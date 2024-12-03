# Result: Data Page 1.1  
conflicted::conflicts_prefer(plotly::layout)
options(jsonlite.keep_vec_names = FALSE)


# ----------------------- Реактивные значения для Results -----------------------|
rv_results <- reactiveValues(
  proceed_data = NULL,           # отсортирован с правильными форматами и столбцами (из all_data): output$results_summary, filters (KITS,Labs, Test)
  all_display_data = NULL,       # сводная статистика данных из proceed_data (output$daily_tests_plot)
  filtered_data = NULL
)


# MySQL connection function with better error handling
mysql_conn <- function() { 
  tryCatch({
    conn <- dbConnect(
      RMySQL::MySQL(),
      dbname = "shiny_app_db",
      host = "127.0.0.1",
      port = 3306,
      user = "root",
      password = "Winter1986+!",
      client.flag = CLIENT_LOCAL_FILES
    )
    return(conn)
  }, error = function(e) {
    log_message(paste("Database connection error:", e$message), "error")
    return(NULL)
  })
}




server <- function(input, output, session) {

MAX_FILE_SIZE <- 20 * 1024^2  # 20MB
  
# Initialize reactive values
data_cache <- reactiveVal(new.env())

  
#---------------------------------------------------------------------------------------------------------------------------------------------------------------|
#_______________________________ ** START 4PL Modeling ** ______________________________________________________________________________________________________|  
#_______________________________________________________________________________________________________________________________________________________________|
  
  
# Универсальная 4PL модель

model_mcd_cps <- function(conc, P1, P2, P3, P4, type = "Increase") {
  if (type == "Increase") {
    P2 + (P1 - P2) / (1 + (conc / P3)^P4)
  } else if (type == "Decrease") {
    P1 + P2 /(1 + exp(-(P3 + P4 * log(conc))))
  } else {
    stop("Invalid type. Must be 'Increase' or 'Decrease'.")
  }
}

inverse_model_cps <- function(cps, P1, P2, P3, P4, type = "Increase") {
  if (type == "Increase") {
    P3 * ((P1 - P2) / (cps - P2) - 1)^(1 / P4)
  } else if (type == "Decrease") {
    exp((-log((P2 / (cps - P1)) - 1) - P3) / P4)
  } else {
    stop("Invalid type. Must be 'Increase' or 'Decrease'.")
  }
}




# Реактивное выражение для типа кривой:

reaction_type <- reactive({
  req(type_of_reaction_global(), input$type_reaction)
  
  selected_test <- input$type_reaction
  
  type_data <- type_of_reaction_global() %>%
    filter(Test == selected_test) %>%
    pull(Type)
  
  if (length(type_data) > 0) {
    return(type_data[1])
  } else {
    return("Unknown")
  }
})





  
  slope_value <- reactive({
    req(input$adg_high_sh, input$adg_low_sh, input$adg_high_lab, input$adg_low_lab)
    (input$adg_high_sh - input$adg_low_sh) / (input$adg_high_lab - input$adg_low_lab)
  })
  
  intercept_value <- reactive({
    req(input$adg_low_sh, input$adg_low_lab)
    input$adg_low_sh - (slope_value() * input$adg_low_lab)
  })
  
#-----------------------   *** Создание графика MCD с использованием plotly ***  -------------------------------------|  
  
  
  output$mcd_plot <- renderPlotly({
    req(input$P1, input$P2, input$P3, input$P4, input$conc_min, input$conc_max)
    
    type <- reaction_type()  # Определяем тип реакции
    
    if (type == "Unknown") {
      showNotification("Unknown reaction type. Defaulting to 'Increase'.", type = "warning")
      type <- "Increase"
    }
    
    cal_conc <- calibrator_concentrations()
    
    # Генерация концентраций
    conc_range <- if (input$x_scale == "log") {
      10^(seq(log10(input$conc_min), log10(input$conc_max), length.out = 1000))
    } else {
      seq(from = input$conc_min, to = input$conc_max, length.out = 1000)
    }
    
    # Расчет значений MCD CPS и Lab CPS
    mcd_cps <- model_mcd_cps(conc_range, input$P1, input$P2, input$P3, input$P4, type)
    lab_cps <- (mcd_cps - intercept_value()) / slope_value()
    
    # Создание данных для графика
    plot_data <- data.frame(
      Concentration = conc_range,
      MCD_CPS = mcd_cps,
      Lab_CPS = lab_cps
    )
    
    # Создание графика
    p <- plot_ly() %>%
      add_trace(data = plot_data, x = ~Concentration, y = ~MCD_CPS, 
                type = 'scatter', mode = 'lines', name = 'MCD CPS', 
                line = list(color = '#009999')) %>%
      add_trace(data = plot_data, x = ~Concentration, y = ~Lab_CPS, 
                type = 'scatter', mode = 'lines', name = 'Lab CPS', 
                line = list(color = '#FF8C00')) %>%
      layout(
        xaxis = list(
          title = "Concentration", 
          type = if (input$x_scale == "log") "log" else "linear"
        ),
        yaxis = list(title = "CPS"),
        showlegend = TRUE
      )
    
    p
  })
  
  #------------------------------------------------------------------------------------------------------------------------|  
  
  
  
  
  # Реактивное выражение для получения последних данных
  latest_kits_data <- reactive({
    req(rv_sql$kits_data_sql, input$type_reaction)
    
    filtered_data <- rv_sql$kits_data_sql %>%
      filter(Reagent_Name == input$type_reaction) %>%  # Фильтрация по выбранному тесту
      filter(Kit_Lot == max(as.numeric(Kit_Lot), na.rm = TRUE))  # Выбор последнего лота
    
    print("Filtered latest_kits_data:")
    print(filtered_data)
    
    return(filtered_data)
  })
  
  
  # Обновляем список лотов при выборе теста
  observe({
    req(rv_sql$kits_data_sql, input$type_reaction)
    
    available_lots <- rv_sql$kits_data_sql %>%
      filter(Reagent_Name == input$type_reaction) %>%
      pull(Kit_Lot) %>%
      unique()
    
    updateSelectInput(
      session,
      "lot_reaction",
      choices = sort(available_lots),
      selected = ifelse(length(available_lots) > 0, available_lots[1], NULL)
    )
  })
  

  output$assay_min_display <- renderText({
    req(rv_sql$kits_data_sql, input$type_reaction, input$lot_reaction)
    
    data <- rv_sql$kits_data_sql %>%
      filter(Reagent_Name == input$type_reaction, Kit_Lot == input$lot_reaction)
    
    if (nrow(data) > 0) {
      paste("Assay Min:", as.numeric(data$Assay_Min[1]))
    } else {
      "Assay Min: Data unavailable"
    }
  })
  
  output$assay_max_display <- renderText({
    req(rv_sql$kits_data_sql, input$type_reaction, input$lot_reaction)
    
    data <- rv_sql$kits_data_sql %>%
      filter(Reagent_Name == input$type_reaction, Kit_Lot == input$lot_reaction)
    
    if (nrow(data) > 0) {
      paste("Assay Max:", as.numeric(data$Assay_Max[1]))
    } else {
      "Assay Max: Data unavailable"
    }
  })
  
  
  observe({
    req(rv_sql$kits_data_sql, input$type_reaction, input$lot_reaction)
    
    data <- rv_sql$kits_data_sql %>%
      filter(Reagent_Name == input$type_reaction, Kit_Lot == input$lot_reaction)
    
    if (nrow(data) > 0) {
      updateNumericInput(session, "assay_min", value = as.numeric(data$Assay_Min[1]))
      updateNumericInput(session, "assay_max", value = as.numeric(data$Assay_Max[1]))
    } else {
      updateNumericInput(session, "assay_min", value = NA)
      updateNumericInput(session, "assay_max", value = NA)
    }
  })
  
  
  # Обновляем значения параметров при изменении теста или лота
  observe({
    req(rv_sql$kits_data_sql, input$type_reaction, input$lot_reaction)
    
    data <- rv_sql$kits_data_sql %>%
      filter(Reagent_Name == input$type_reaction, Kit_Lot == input$lot_reaction)
    
    if (nrow(data) > 0) {
      updateNumericInput(session, "P1", value = as.numeric(data$Curve_Parameter_1[1]))
      updateNumericInput(session, "P2", value = as.numeric(data$Curve_Parameter_2[1]))
      updateNumericInput(session, "P3", value = as.numeric(data$Curve_Parameter_3[1]))
      updateNumericInput(session, "P4", value = as.numeric(data$Curve_Parameter_4[1]))
      updateNumericInput(session, "conc_max", value = as.numeric(data$Curve_Parameter_1[1]*1.2))
    } else {
      updateNumericInput(session, "P1", value = NA)
      updateNumericInput(session, "P2", value = NA)
      updateNumericInput(session, "P3", value = NA)
      updateNumericInput(session, "P4", value = NA)
      updateNumericInput(session, "conc_max", value = NA)
    }
  })
  
  

  output$reaction_type_display <- renderText({
    req(type_of_reaction_global(), input$type_reaction)
    
    selected_test <- input$type_reaction
    
    type_data <- type_of_reaction_global() %>%
      filter(Test == selected_test) %>%
      pull(Type)
    
    if (length(type_data) > 0) {
      return(type_data[1])
    } else {
      return("Unknown Type")
    }
  })
  
#-------------------------------------------   Calculators Outputs  --------------------------------------------------------|
  
  
  
  output$calculated_mcd_cps <- renderText({
    req(input$input_lab_cps)
    lab_cps <- as.numeric(input$input_lab_cps)
    mcd_cps <- lab_cps * slope_value() + intercept_value()
    sprintf("MCD CPS: %.2f", mcd_cps)
  })
  
  output$calculated_concentration_from_lab <- renderText({
    req(input$input_lab_cps)
    lab_cps <- as.numeric(input$input_lab_cps)
    mcd_cps <- lab_cps * slope_value() + intercept_value()
    concentration <- inverse_model_cps(mcd_cps, input$P1, input$P2, input$P3, input$P4)
    sprintf("Concentration: %.6f ng/mL", concentration)
  })
  
  output$calculated_mcd_cps_direct <- renderText({
    req(input$input_concentration)
    conc <- as.numeric(input$input_concentration)
    mcd_cps <- model_mcd_cps(conc, input$P1, input$P2, input$P3, input$P4)
    sprintf("MCD CPS: %.2f", mcd_cps)
  })
  
  output$calculated_lab_cps <- renderText({
    req(input$input_concentration)
    conc <- as.numeric(input$input_concentration)
    mcd_cps <- model_mcd_cps(conc, input$P1, input$P2, input$P3, input$P4)
    lab_cps <- (mcd_cps - intercept_value()) / slope_value()
    sprintf("Lab CPS: %.2f", lab_cps)
  })
  
  
  
  
  
  # Добавляем реактивные выражения для расчета концентраций калибраторов
  calibrator_concentrations <- reactive({
    req(input$adg_low_lab, input$adg_high_lab, 
        input$adg_low_sh, input$adg_high_sh,
        input$P1, input$P2, input$P3, input$P4)
    
    # Расчет концентраций для лабораторных калибраторов
    lab_low_mcd_cps <- input$adg_low_lab * slope_value() + intercept_value()
    lab_high_mcd_cps <- input$adg_high_lab * slope_value() + intercept_value()
    
    lab_low_conc <- inverse_model_cps(lab_low_mcd_cps, input$P1, input$P2, input$P3, input$P4)
    lab_high_conc <- inverse_model_cps(lab_high_mcd_cps, input$P1, input$P2, input$P3, input$P4)
    
    # Расчет концентраций для MCD калибраторов
    mcd_low_conc <- inverse_model_cps(input$adg_low_sh, input$P1, input$P2, input$P3, input$P4)
    mcd_high_conc <- inverse_model_cps(input$adg_high_sh, input$P1, input$P2, input$P3, input$P4)
    
    list(
      lab = list(
        low = list(conc = lab_low_conc, cps = input$adg_low_lab),
        high = list(conc = lab_high_conc, cps = input$adg_high_lab)
      ),
      mcd = list(
        low = list(conc = mcd_low_conc, cps = input$adg_low_sh),
        high = list(conc = mcd_high_conc, cps = input$adg_high_sh)
      )
    )
  })

#-------------------------------------------------------------------------------------------------------------------------------------------------------------|  
#_______________________________ * END 4PL Modeling * ________________________________________________________________________________________________________|
#_____________________________________________________________________________________________________________________________________________________________|

  
  
  
#
#
#
#
#
#
  
  
  
#-------------------------------------------------------------------------------------------------------------------------------------------------------------|   
#_______________________________ ** 222 Work with RESULTS DATA ** ________________________________________________________________________________________________|     
#_____________________________________________________________________________________________________________________________________________________________|
  
  
  

  
#------------------------------------ *** Observers for debugging ***-----------------------------------|
  observe({
    req (rv_results$all_display_data)
    print("all_display_data")
    print(str(rv_results$proceed_data))
  })
  
  observe({
    req (rv_results$filtered_data)
    print('filtered_data')
    print(str(rv_results$filtered_data))
  })

  
  
#_________________________________________________________________________________________|    
  

#________________ 1.1 File validation function __________________________________|
  
  validate_file <- function(file_path) {
    tryCatch({
      if (!file.exists(file_path)) return(FALSE)                 # Наличие файла.
      if (!grepl("\\.xlsx$", file_path)) return(FALSE)           # Расширение файла (.xlsx).
      
      df <- openxlsx::read.xlsx(file_path, rows = 1)
      if (ncol(df) < max(columns_to_load)) return(FALSE)         # Минимальное количество столбцов в первой строке файла.
      
      TRUE
    }, error = function(e) FALSE)
  }
  
#_________________________________________________________________________________|  
  
                  
                 
                 
  
#__________________ 1.2. Загрузка и обработка файлов  ____________________________|
  
columns_to_load <- c(1, 2, 3, 4, 6, 7, 10, 11, 20, 21, 25, 26, 29, 33, 34, 35, 41, 54, 55, 59, 60, 61, 78)
  
observeEvent(input$results_files, {
  req(input$results_files)  # Проверяем, что файлы загружены
  
  withProgress(message = "Processing files", value = 0, {
    output$load_status <- renderText("Starting file processing...")
    
    tryCatch({
      # Validate files
      valid_files <- input$results_files$datapath[sapply(input$results_files$datapath, validate_file)]
      
      if (length(valid_files) == 0) {
        stop("No valid files to process")
      }
      
      # Process files
      total_files <- length(valid_files)
      all_data <- bind_rows(lapply(seq_along(valid_files), function(i) {
        file <- valid_files[i]
        
        incProgress(1 / total_files, 
                    detail = sprintf("Processing file %d of %d", i, total_files))
        
        # Check cache
        cache_key <- digest::digest(file)
        cache_env <- data_cache()
        
        if (exists(cache_key, envir = cache_env)) {
          return(get(cache_key, envir = cache_env))
        }
        
        # Load data
        df <- openxlsx::read.xlsx(file, cols = columns_to_load, colNames = TRUE)
        
        # Process data
        processed_df <- tryCatch({
          df %>% 
            mutate(
              id_lab = as.character(id_lab),
              sn = as.character(sn),
              Unique_Record_ID_Num = as.numeric(Unique_Record_ID_Num),
              Identifier = as.numeric(Identifier),
              Patient_ID_Num = as.character(Patient_ID_Num),
              Name = as.character(Name),
              Physician_Name = as.character(Physician_Name),
              Dilution_Factor = as.numeric(Dilution_Factor),
              Reagent_Name = as.character(Reagent_Name),
              Result = round(as.numeric(Result),1),
              Limit_High = as.numeric(Limit_High),
              Limit_Low = as.numeric(Limit_Low),
              CPS = as.numeric(CPS),
              Date_And_Time_Resulted = ymd_hms(as.character(Date_And_Time_Resulted)),
              Bead_Lot = as.character(Bead_Lot),
              Reagent_Lot_Cycle_1 = as.character(Reagent_Lot_Cycle_1),
              Kit_Lot = as.numeric(Kit_Lot),
              Formula_Number = as.numeric(Formula_Number),
              offLineDilution_Factor = as.numeric(offLineDilution_Factor),
              Assay_Type = as.numeric(Assay_Type),
              Allergen_Code = as.character(Allergen_Code),
              Allergen_Lot = as.character(Allergen_Lot),
              PrincipleDFactor = as.numeric(PrincipleDFactor),
              across(where(is.character), ~if_else(is.na(.), "", as.character(.))),
              file_source = basename(file)
            )
        }, error = function(e) {
          warning(sprintf("Error processing file %s: %s", basename(file), e$message))
          NULL
        })
        
        # Save to cache
        if (!is.null(processed_df)) {
          assign(cache_key, processed_df, envir = cache_env)
        }
        
        processed_df
      }))
      
      if (is.null(all_data) || nrow(all_data) == 0) {
        stop("Failed to load data from files")
      }
      
      # Remove duplicates and sort
      cleaned_data <- all_data %>%
        distinct(Result, CPS,  sn, Date_And_Time_Resulted, Patient_ID_Num, Reagent_Name, .keep_all = TRUE) %>%
        arrange(Reagent_Name)
      
      # Save results
      rv_results$proceed_data <- cleaned_data 
      
      # Calculate duplicates
      duplicate_count <- nrow(all_data) - nrow(cleaned_data)
      
      # Update summary information
      output$results_summary <- renderText({
        req(rv_results$proceed_data)
        
        # Подсчёт количества лабораторий
        num_labs <- length(unique(rv_results$proceed_data$id_lab))
        
        # Общее количество тестов
        num_tests <- nrow(rv_results$proceed_data)
        
        # Диапазон дат
        date_range <- range(rv_results$proceed_data$Date_And_Time_Resulted, na.rm = TRUE)
        date_range_text <- paste(
          format(date_range[1], "%d %b %Y"),
          "to",
          format(date_range[2], "%d %b %Y")
        )
        
        paste(
          "Данные из лабораторий: ", num_labs, "\n",
          "Количество тестов: ", num_tests, "\n",
          "Диапазон дат: ", date_range_text, "\n",
          "Удалено дублирующихся записей: ", duplicate_count
        )
      })
      
      # Update load status
      invalid_files <- length(input$results_files$datapath) - length(valid_files)
      output$load_status <- renderText(sprintf(
        "Loading completed! Processed files: %d, records: %d. Skipped files: %d.",
        length(valid_files),
        nrow(cleaned_data),
        invalid_files
      ))
      
    }, error = function(e) {
      showNotification(paste("Loading error:", e$message), type = "error")
    })
  })
})


#______________________________________________________________________________________|

                    
                   

#--------------------------------------- * FILTERS * -------------------------------------------------|

observe({
  # Если загружены данные, обновляем диапазон выбора
  if (!is.null(rv_results$proceed_data)) {
    updateDateRangeInput(
      session, 
      "date_filter",
      start = min(rv_results$proceed_data$Date_And_Time_Resulted, na.rm = TRUE),
      end = max(rv_results$proceed_data$Date_And_Time_Resulted, na.rm = TRUE)
    )
  }
})


#______________________________________________________________________________________|
  
                   
                   
                   
  
#______________________________________________________________________________________| 
 

observe({
  req(rv_results$proceed_data)
  
  updateSelectInput(session, "test_select",
                    choices = unique(rv_results$proceed_data$Reagent_Name),
                    selected = NULL)
  
  updateSelectInput(session, "lab_select",
                    choices = c("All", unique(rv_results$proceed_data$id_lab)),
                    selected = "All")
  
  updateSelectInput(session, "kit_lot_select",
                    choices = c("All", unique(rv_results$proceed_data$Kit_Lot)),
                    selected = "All")
})

  
  # Обновление списка тестов при загрузке данных
  observe({
    req(rv_results$proceed_data)
    
    updateSelectInput(session, "test_select",
                      choices = unique(rv_results$proceed_data$Reagent_Name),
                      selected = NULL)
  })
  
  # Обновление списка лабораторий при изменении теста
  observe({
    req(rv_results$proceed_data)
    
    selected_test <- input$test_select
    data <- rv_results$proceed_data
    
    if (!is.null(selected_test) && selected_test != "") {
      data <- data %>% filter(Reagent_Name == selected_test)
    }
    
    updateSelectInput(session, "lab_select",
                      choices = c("All", unique(data$id_lab)),
                      selected = "All")
  })
  
  # Обновление списка Kit Lots при изменении теста или лаборатории
  observe({
    req(rv_results$proceed_data)
    
    selected_test <- input$test_select
    selected_labs <- input$lab_select
    data <- rv_results$proceed_data
    
    if (!is.null(selected_test) && selected_test != "") {
      data <- data %>% filter(Reagent_Name == selected_test)
    }
    if (!is.null(selected_labs) && !("All" %in% selected_labs)) {
      data <- data %>% filter(id_lab %in% selected_labs)
    }
    
    updateSelectInput(session, "kit_lot_select",
                      choices = c("All", unique(data$Kit_Lot)),
                      selected = "All")
  })
  
#__________________________________________________________________________________|
  
                  
                 
                 

#____________EXPORT .CSV downloadbutton + downloadHandler_________________________|
  
  
  output$export_results_data <- downloadHandler(
    filename = function() {
      paste("results_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(rv_results$proceed_data)  # Убедитесь, что данные загружены
      write.csv(rv_results$proceed_data, file, row.names = FALSE)
    }
  )
  
  observe({
    if (is.null(rv_results$proceed_data) || nrow(rv_results$proceed_data) == 0) {
      shinyjs::disable("export_results_data")
    } else {
      shinyjs::enable("export_results_data")
    }
  })

#__________________________________________________________________________________|
  
                   
                   
                   
  
#______________________actionButton("reset_results_upload")________________________|
  
  observeEvent(input$reset_results_upload, {
    # Очищаем данные
    rv_results$proceed_data <- NULL
    
    # Сбрасываем сообщение о загрузке
    output$results_load_status <- renderText("")
    output$results_summary <- renderText("Данные были сброшены.")
    
    # Сбрасываем загруженные файлы, добавляя пустую строку в `fileInput`
    shinyjs::reset("results_files")
  })
  
#__________________________________________________________________________________|  
  
                    
                   
                   
  
#__________________Create rv_results$all_display_data and rv_results$filtered_data__________|
  

  # Наблюдение для создания агрегированных данных
  observe({
    req(rv_results$proceed_data)
    rv_results$all_display_data <- rv_results$proceed_data %>%
      group_by(date = as.Date(Date_And_Time_Resulted), id_lab, Kit_Lot, Reagent_Name) %>%
      summarise(
        count = n(),
        mean_result = mean(Result, na.rm = TRUE),
        sd_result = sd(Result, na.rm = TRUE),
        mean_cps = mean(CPS, na.rm = TRUE),
        mean_df = mean(Dilution_Factor, na.rm = TRUE),
        .groups = 'drop'
      )
  })
  
  observe({
  req(rv_results$all_display_data)
  
  # Создание фильтрованных данных и сохранение в rv_results
  rv_results$filtered_data <- rv_results$all_display_data %>%
    # Фильтр по дате
    filter(
      date >= input$date_filter[1],
      date <= input$date_filter[2]
    ) %>%
    # Фильтры по тесту, лаборатории и номеру партии
    filter(
      if (!is.null(input$test_select) && input$test_select != "") 
        Reagent_Name == input$test_select 
      else TRUE,
      if (!is.null(input$lab_select) && !("All" %in% input$lab_select)) 
        id_lab %in% input$lab_select 
      else TRUE,
      if (!is.null(input$kit_lot_select) && !("All" %in% input$kit_lot_select)) 
        Kit_Lot %in% input$kit_lot_select 
      else TRUE
    ) %>%
    arrange(date) %>%
    mutate(across(c(mean_result, sd_result, mean_cps, mean_df), ~ round(., 2)))  # Округление значений
})


#__________________________________________________________________________________|    
 
                  
                 
                 
  
#_____________________ output$data_table (таблица со сводной статистикой из rv_results$rv_results$filtered_data)_________________|
    
    output$data_table <- DT::renderDT({
      req(rv_results$filtered_data)
      
      # Преобразуем в data.frame перед передачей в datatable
      data_to_display <- as.data.frame(rv_results$filtered_data) %>%
        select(
          Date = date,
          Laboratory = id_lab,
          Test = Reagent_Name,
          `Kit Lot` = Kit_Lot,          
          `Test Count` = count,
          `Mean Result` = mean_result,
          `SD Result` = sd_result,
          `Mean CPS` = mean_cps,
          `Mean DF` = mean_df
        )
      
      datatable(
        data_to_display,
        options = list(
          pageLength = 10,  # По умолчанию показывать 10 строк
          lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All")),  # Меню выбора количества строк
          scrollX = TRUE,
          dom = 'lBfrtip',  # Добавляем компонент `l` для отображения выбора количества строк
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'cell-border stripe',
        extensions = c('Buttons')  # Подключаем расширение для кнопок
      ) %>%
        formatDate('Date', method = 'toLocaleDateString') %>%
        formatRound(
          columns = c('Mean Result', 'SD Result', 'Mean CPS', 'Mean DF'),
          digits = 2
        )
    })
    
#__________________________________________________________________________________| 

                    
                   
                    
  
#______________________ Daily tests plot __________________________________________|
  
    output$daily_tests_plot <- renderPlotly({
      data <- if (!is.null(input$test_select) && input$test_select != "") {
        rv_results$filtered_data
      } else {
        rv_results$all_display_data
      }
      
      req(data)  # Убедитесь, что данные существуют
      
      # Преобразование данных в датафрейм, если это необходимо
      data <- as.data.frame(data)
      
      if (nrow(data) == 0) return(NULL)  # Проверка на пустые данные
      
      p <- ggplot(data, 
                  aes(x = date, y = count, color = as.factor(Kit_Lot), 
                      linetype = id_lab, group = interaction(id_lab, Kit_Lot))) +
        geom_line(size = 1.1, alpha = 0.8) +
        geom_point(size = 3, alpha = 0.9) +
        geom_smooth(aes(group = 1), method = "loess", se = FALSE, 
                    color = "black", linetype = "dashed", size = 1) +
        labs(
          title = if (!is.null(input$test_select) && input$test_select != "") {
            paste('Daily Test Count -', input$test_select)
          } else {
            'Daily Test Count - All Tests'
          },
          subtitle = paste("Total Tests:", sum(data$count, na.rm = TRUE)),
          x = 'Date',
          y = 'Test Count',
          color = 'Reagent Name',
          linetype = 'Laboratory'
        ) +
        theme_minimal() +
        scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b %Y") +
        scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
        scale_color_brewer(palette = "Set2") +
        theme(
          panel.background = element_rect(fill = "#2C2C2C", color = NA),
          plot.background = element_rect(fill = "#2C2C2C", color = NA),
          panel.grid.major = element_line(color = "#1A1A1A"),
          panel.grid.minor = element_line(color = "#1A1A1A", size = 0.5),
          axis.text = element_text(color = "#F5F5F5"),
          axis.title = element_text(color = "#F5F5F5"),
          plot.title = element_text(color = "#F5F5F5", size = 14, face = "bold"),
          plot.subtitle = element_text(color = "#F5F5F5", size = 12, face = "italic"),
          legend.background = element_rect(fill = "#2C2C2C", color = NA),
          legend.text = element_text(color = "#F5F5F5"),
          legend.title = element_text(color = "#F5F5F5"),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5)
        )
      
      # Преобразуем ggplot объект в интерактивный график
      ggplotly(p) %>%
        layout(
          hovermode = "x unified",
          margin = list(t = 70),
          annotations = list(
            list(
              x = 0.5,
              y = -0.15,
              text = "Data Source: Your Laboratory Dataset",
              showarrow = FALSE,
              xref = "paper",
              yref = "paper",
              xanchor = "center",
              yanchor = "top",
              font = list(size = 10, color = "#AAAAAA")
            )
          )
        )
    })
    
    
#__________________________________________________________________________________|   
  
                   
                   
                   
  
#_____________________________ STATISTICS TABLE ___________________________________|
  
  output$test_stats_table <- DT::renderDataTable({
    req(rv_results$filtered_data)  # Убедитесь, что данные загружены и фильтрованы
    
    stats_data <- rv_results$filtered_data %>%
      group_by(Reagent_Name, Kit_Lot, id_la) %>%
      summarise(
        Mean_Result = round(mean(mean_result, na.rm = TRUE), 2),
        SD_Result = round(sd(mean_result, na.rm = TRUE), 2),
        Total_Tests = sum(count, na.rm = TRUE),
        .groups = "drop"
      )
    
    datatable(
      stats_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      class = 'cell-border stripe',
      extensions = 'Buttons'
    )
  })
#__________________________________________________________________________________|  

                    
                   
                    
  
#_____________________ STATISTICS PLOT ____________________________________________|
  
  output$test_stats_plot <- renderPlotly({
    req(rv_results$filtered_data)
    
    # Агрегация данных для графика
    data_for_plot <- rv_results$filtered_data %>%
      group_by(Reagent_Name, Kit_Lot, id_lab) %>%  # Включение лабораторий в группировку
      summarise(
        Mean_Result = mean(mean_result, na.rm = TRUE),
        SD_Result = sd(mean_result, na.rm = TRUE),
        Total_Tests = sum(count, na.rm = TRUE),
        .groups = "drop"
      )
    
    if (nrow(data_for_plot) == 0) return(NULL)  # Если данных нет
    
    # Создание графика
    p <- ggplot(data_for_plot, aes(x = Reagent_Name, y = Total_Tests, fill = as.factor(Kit_Lot), color = id_lab)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8, show.legend = TRUE) +  # Группированные столбцы с цветом лабораторий
      geom_text(aes(label = Total_Tests), vjust = -0.5, size = 3) +  # Подписи над столбцами
      labs(
        title = "Test Statistics by Reagent, Kit Lot, and Laboratory",
        subtitle = "Total Tests per Reagent and Kit Lot, grouped by Laboratory",
        x = "Reagent Name",
        y = "Total Tests",
        fill = "Kit Lot",
        color = "Laboratory"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        legend.position = "bottom"
      ) +
      scale_fill_brewer(palette = "Set2") +  # Улучшенная цветовая палитра для Kit Lot
      scale_color_manual(values = c("red", "blue", "green", "purple", "orange"))  # Уникальные цвета для лабораторий
    
    # Преобразование в интерактивный график с подробными подсказками
    ggplotly(p, tooltip = c("x", "y", "fill", "color")) %>%
      layout(
        hovermode = "x unified",  # Подсказки по оси X
        margin = list(t = 50)  # Отступ сверху
      )
  })
  
  
  
  

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
#_______________________________ *2222 END Work with RESULTS DATA * ________________________________________________________________________________________________________| 
#_________________________________________________________________________________________________________________________________________________________________________|




  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
# *******************************  
# * 333 Adjustment: Data 4        **
# *******************************
  
  
  
  
# --------------------------------------** Work with ADJUSTMENTS DATA **------------------------------------------------------------------------------------------------| 
  adjustments_cols <- c(1:7, 9:12, 21:34, 38:39)
  # Реактивные значения
  rv <- reactiveValues(
    calib_data = NULL,
    all_data = NULL
  )
  
  # Вспомогательные функции
  data_cache <- function() {
    if (!exists("._cache", envir = .GlobalEnv)) {
      assign("._cache", new.env(), envir = .GlobalEnv)
    }
    get("._cache", envir = .GlobalEnv)
  }
  
  safe_as_numeric <- function(x) {
    as.numeric(as.character(x))
  }
  
  validate_sheet_data <- function(data) {
    required_cols <- c("id_lab", "sn" , "DateTimeOfAdjustment", "Test_Name", "Kit_Lot", "Slope", "Intercept")
    missing_cols <- setdiff(required_cols, names(data))
    
    if (length(missing_cols) > 0) {
      stop(sprintf("Отсутствуют обязательные столбцы: %s", 
                   paste(missing_cols, collapse = ", ")))
    }
    
    # Проверка формата строки даты
    invalid_dates <- !stringr::str_detect(data$DateTimeOfAdjustment, "^\\d{14}$")
    if (any(invalid_dates)) {
      stop("Некорректный формат в столбце DateTimeOfAdjustment (ожидается 'YYYYMMDDHHMMSS')")
    }
    
    TRUE
  }
  
  # Функция обработки файлов
  process_calibration_files <- function(files, rv) {
    tryCatch({
      # Базовая проверка
      if (is.null(files) || nrow(files) == 0) {
        stop("Файлы не выбраны")
      }
      if (any(files$size > MAX_FILE_SIZE)) {
        stop("Файл слишком большой (максимум 20MB)")
      }
      if (!all(tools::file_ext(files$name) == "xlsx")) {
        stop("Допустимы только Excel файлы (.xlsx)")
      }
      
      withProgress(message = 'Обработка файлов', value = 0, {
        all_data <- purrr::map_df(seq_along(files$datapath), function(i) {
          file_path <- files$datapath[i]
          file_name <- files$name[i]
          
          incProgress(1/length(files$datapath), 
                      detail = paste("Обрабатывается", file_name))
          
          # Проверка кэша
          cache_key <- paste0("calib_", digest::digest(file_path))
          cache_env <- data_cache()
          
          if (exists(cache_key, envir = cache_env)) {
            return(get(cache_key, envir = cache_env))
          }
          
          # Чтение и преобразование данных
          tryCatch({
            wb <- openxlsx::loadWorkbook(file_path)
            data <- openxlsx::read.xlsx(wb, sheet = 1, cols = adjustments_cols)
            
            # Убедитесь, что обязательные столбцы присутствуют
            validate_sheet_data(data)
            
            # Преобразование и очистка данных
            transformed_data <- data %>%
              mutate(
                id_lab = as.character(id_lab),
                sn = as.character(sn),
                DateTimeOfAdjustment = lubridate::ymd_hms(DateTimeOfAdjustment, quiet = TRUE),
                Slope = safe_as_numeric(Slope),
                Intercept = safe_as_numeric(Intercept),
                CV_Counts_Low = safe_as_numeric(CV_Counts_Low),
                CV_Counts_High = safe_as_numeric(CV_Counts_High),
                CPS_Low_1 = safe_as_numeric(CPS_Low_1),
                CPS_Low_2 = safe_as_numeric(CPS_Low_2),
                CPS_Low_3 = safe_as_numeric(CPS_Low_3),
                CPS_Low_4 = safe_as_numeric(CPS_Low_4),
                CPS_High_1 = safe_as_numeric(CPS_High_1),
                CPS_High_2 = safe_as_numeric(CPS_High_2),
                CPS_High_3 = safe_as_numeric(CPS_High_3),
                CPS_High_4 = safe_as_numeric(CPS_High_4),
                Adjustor_Concentration_Low = safe_as_numeric(Adjustor_Concentration_Low),
                Adjustor_Concentration_High = safe_as_numeric(Adjustor_Concentration_High),
                Mean_CPS_Low = safe_as_numeric(Mean_CPS_Low),
                Mean_CPS_High = safe_as_numeric(Mean_CPS_High),
                Reagent_Name = as.character(Test_Name),
                Kit_Lot = as.character(Kit_Lot),
                Adjustor_Lot_Low = as.character(Adjustor_Lot_Low),
                Adjustor_Lot_High = as.character(Adjustor_Lot_High),
                FileName = file_name
              ) %>%
              select(-Test_Name)
            
            assign(cache_key, transformed_data, envir = cache_env)
            transformed_data
            
          }, error = function(e) {
            stop(sprintf("Ошибка в файле %s: %s", file_name, e$message))
          })
        })
        
        # Обработка и агрегация данных
        all_data <- all_data %>%
          distinct(Reagent_Name, Kit_Lot, DateTimeOfAdjustment, Slope, Intercept, .keep_all = TRUE)
        print(nrow(all_data))

        grouped_data <- all_data %>%
          group_by(Reagent_Name, Kit_Lot) %>%
          summarise(
            Count = n(),
            Mean_Slope = round(mean(Slope, na.rm = TRUE), 4),
            SD_Slope = round(sd(Slope, na.rm = TRUE), 4),
            CV_Slope = round((SD_Slope / Mean_Slope) * 100, 2),
            Mean_Intercept = round(mean(Intercept, na.rm = TRUE), 4),
            SD_Intercept = round(sd(Intercept, na.rm = TRUE), 4),
            CV_Intercept = round((SD_Intercept / Mean_Intercept) * 100, 2),
            Files_Processed = paste(unique(FileName), collapse = ", "),
            Last_Update = max(DateTimeOfAdjustment, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          arrange(Reagent_Name, Kit_Lot)
        
        
        # Сохранение результатов
        rv$calib_data <- list(
          raw_data = all_data,
          grouped_data = grouped_data,
          last_update = Sys.time()
        )
      })
      
      str(rv$calib_data$raw_data)
      summary(rv$calib_data$raw_data)
      print(nrow(all_data)) 
      
      
      showNotification(
        sprintf("Успешно загружено %d файлов", length(files$datapath)),
        type = "message"
      )
      
    }, error = function(e) {
      showNotification(
        paste("Ошибка загрузки файлов:", e$message),
        type = "error",
        duration = NULL
      )
    })
  }
  
  
  
  # Наблюдатели событий
  observeEvent(input$calib_files, {
    req(input$calib_files)
    process_calibration_files(input$calib_files, rv)
  })
  
  observeEvent(input$reset_upload, {
    rv$calib_data <- NULL
    output$calib_load_status <- renderText("")
    output$validation_summary <- renderText("")
    shinyjs::reset("calib_files")
  })
  
  
  
  # Выводы
  output$export_results <- downloadHandler(
    filename = function() {
      paste0("calibration_results_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx")
    },
    content = function(file) {
      req(rv$calib_data)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Raw Data")
      openxlsx::writeData(wb, "Raw Data", rv$calib_data$raw_data)
      openxlsx::addWorksheet(wb, "Summary")
      openxlsx::writeData(wb, "Summary", rv$calib_data$grouped_data)
      style <- openxlsx::createStyle(textDecoration = "bold", border = "Bottom", borderStyle = "medium")
      openxlsx::addStyle(wb, "Raw Data", style = style, rows = 1, cols = 1:ncol(rv$calib_data$raw_data))
      openxlsx::addStyle(wb, "Summary", style = style, rows = 1, cols = 1:ncol(rv$calib_data$grouped_data))
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$calib_load_status <- renderText({
    if (is.null(rv$calib_data)) return("Ожидание файлов...")
    sprintf("Последнее обновление: %s", format(rv$calib_data$last_update, "%Y-%m-%d %H:%M:%S"))
  })
  
  output$validation_summary <- renderText({
    req(rv$calib_data)
    data <- rv$calib_data$raw_data
    
    num_tests <- length(unique(data$Reagent_Name))
    num_kits <- length(unique(data$Kit_Lot))
    date_range <- range(data$DateTimeOfAdjustment, na.rm = TRUE)
    
    paste(
      "Статистика загрузки:",
      sprintf("Количество тестов: %d", num_tests),
      sprintf("Количество партий: %d", num_kits),
      sprintf("Количество записей: %d", nrow(data)),
      sprintf("Диапазон дат: %s - %s", 
              format(date_range[1], "%d.%m.%Y"),
              format(date_range[2], "%d.%m.%Y")),
      "",
      "Обзор качества данных:",
      sprintf("Обработано файлов: %d", length(unique(data$FileName))),
      sprintf("Пропущенные значения: %d", sum(is.na(data))),
      sep = "\n"
    )
  })
  
  observeEvent(input$reset_upload, {
    rv$calib_data <- NULL
    cache_env <- data_cache()
    rm(list = ls(envir = cache_env), envir = cache_env)
    output$calib_load_status <- renderText("Ожидание файлов...")
    shinyjs::reset("calib_files")
  })
  
  data_cache <- function() {
    if (!exists("._cache", envir = .GlobalEnv)) {
      assign("._cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
    }
    get("._cache", envir = .GlobalEnv)
  }
  
#__________________________OUTPUTS_________________________________________________________________|
  
  
  
#_______________________ Таблица: Summary by Kits ______________________________|
  
  
  output$calib_table2 <- DT::renderDT({
    req(filtered_calib_data())
    
    summary_data <- filtered_calib_data() %>%
      group_by(Reagent_Name, Kit_Lot) %>%
      summarise(
        Count = n(),
        Mean_Slope = round(mean(Slope, na.rm = TRUE), 4),
        SD_Slope = round(sd(Slope, na.rm = TRUE), 4),
        CV_Slope = round((SD_Slope / Mean_Slope) * 100, 2),
        Mean_Intercept = round(mean(Intercept, na.rm = TRUE), 4),
        SD_Intercept = round(sd(Intercept, na.rm = TRUE), 4),
        CV_Intercept = round((SD_Intercept / Mean_Intercept) * 100, 2),
        .groups = "drop"
      )
    
    datatable(
      summary_data,
      options = list(
        pageLength = 10,
        lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All")),
        scrollX = TRUE,
        dom = 'lBfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      class = 'cell-border stripe',
      extensions = 'Buttons'
    )
  })

#_______________________________________________________________________________|  
  
  # Таблица: Detailed Data
  output$calib_table1 <- DT::renderDT({
    req(filtered_calib_data())  # Убедитесь, что данные после фильтрации существуют
    
    detailed_data <- filtered_calib_data()  # Используем отфильтрованные данные
    
    datatable(
      detailed_data,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      class = 'cell-border stripe',
      extensions = 'Buttons'
    )
  })
  


  # График: Validation
  output$validation_plot <- renderPlotly({
    req(rv$calib_data)  # Убедитесь, что данные существуют
    
    validation_data <- rv$calib_data$raw_data %>%
      group_by(Reagent_Name, Kit_Lot, id_lab) %>%
      summarise(
        Mean_Slope = mean(Slope, na.rm = TRUE),
        SD_Slope = sd(Slope, na.rm = TRUE),
        CV_Slope = ifelse(
          !is.na(Mean_Slope) & Mean_Slope != 0,
          (SD_Slope / Mean_Slope) * 100,
          NA
        ),
        .groups = 'drop'
      )
    
    if (nrow(validation_data) == 0) return(NULL)  # Проверка на отсутствие данных
    
    p <- ggplot(validation_data, aes(x = Reagent_Name, y = CV_Slope, color = id_lab)) +
      geom_bar(stat = "identity", position = "dodge", ) +
      labs(
        title = "Validation of Slope Coefficient",
        x = "Test Name",
        y = "Coefficient of Variation (%)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })

  # Обновить график валидации : Если график использует данные, подключите filtered_calib_data().
  
  output$validation_plot <- renderPlotly({
    req(filtered_calib_data())
    
    data <- filtered_calib_data()
    
    p <- ggplot(data, aes(x = Slope, y = Intercept, color = Kit_Lot)) +
      geom_point() +
      labs(title = "Validation Plot", x = "Slope", y = "Intercept") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
    
  # Текст: Validation Details
  output$validation_details <- renderText({
    req(rv$calib_data)  # Убедитесь, что данные существуют
    
    data <- rv$calib_data$raw_data
    
    num_tests <- length(unique(data$Reagent_Name))
    num_kits <- length(unique(data$Kit_Lot))
    num_records <- nrow(data)
    num_missing <- sum(is.na(data))
    date_range <- range(data$DateTimeOfAdjustment, na.rm = TRUE)
    
    paste(
      "Детали проверки данных:",
      sprintf("Количество тестов: %d", num_tests),
      sprintf("Количество наборов: %d", num_kits),
      sprintf("Количество записей: %d", num_records),
      sprintf("Количество пропущенных значений: %d", num_missing),
      sprintf("Диапазон дат: %s - %s", 
              format(date_range[1], "%d.%m.%Y"),
              format(date_range[2], "%d.%m.%Y")),
      sep = "\n"
    )
  })
  
  
  
  
#____________________________FILTRATION_________________________________________|
  
  
  
  
  #  SERVER: Реактивное обновление списка тестов
  observe({
    req(rv$calib_data)  # Убедитесь, что данные загружены
    
    # Уникальные значения Test Name
    Reagent_Names <- c("All", sort(unique(rv$calib_data$raw_data$Reagent_Name)))
    
    # Обновление выбора в UI
    updateSelectInput(
      session = session, 
      inputId = "calib_test_select", 
      choices = Reagent_Names,
      selected = "All"  # По умолчанию выбрано "All"
    )
  })
  
  
  observe({
    req(rv$calib_data)
    
    # Получаем выбранный тест
    selected_test <- input$calib_test_select
    data <- rv$calib_data$raw_data
    
    # Фильтруем данные по выбранному тесту
    if (!is.null(selected_test) && selected_test != "") {
      data <- data %>% filter(Reagent_Name == selected_test)
    }
    
    # Обновление списка Kit Lots
    updateSelectInput(session, "calib_kit_lot_select",
                      choices = c("All", unique(data$Kit_Lot)),
                      selected = "All")
  })
  
  
  # SERVER: Фильтрация данных
  filtered_calib_data <- reactive({
    req(rv$calib_data)  # Убедитесь, что данные загружены
    
    data <- rv$calib_data$raw_data
    
    # Фильтрация по Test Name
    if (!is.null(input$calib_test_select) && input$calib_test_select != "" && input$calib_test_select != "All") {
      data <- data %>% filter(Reagent_Name == input$calib_test_select)
    }
    
    # Фильтрация по Kit Lot
    if (!is.null(input$calib_kit_lot_select) && !("All" %in% input$calib_kit_lot_select)) {
      data <- data %>% filter(Kit_Lot %in% input$calib_kit_lot_select)
    }
    
    data
  })
  
  
  #___________________ adjustments Upload to SQL  ________________________|
  
  log_message <- function(message, type = "info") {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_line <- sprintf("[%s] [%s] %s", timestamp, type, message)
    cat(log_line, "\n")  # Лог выводится в консоль
    
  }
  
  
  
  observeEvent(input$upload_adjustments_to_sql, {
    req(rv$calib_data$raw_data)
    
    rv$calib_data$raw_data <- rv$calib_data$raw_data %>%
      mutate(
        id_lab = if_else(is.na(id_lab), "Unknown", id_lab),
        sn = if_else(is.na(sn), "Unknown", sn),
        Reagent_Name = if_else(is.na(Reagent_Name), "Unknown", Reagent_Name),
        Kit_Lot = if_else(is.na(Kit_Lot), 0, as.numeric(Kit_Lot)),
        DateTimeOfAdjustment = if_else(is.na(DateTimeOfAdjustment), Sys.time(), DateTimeOfAdjustment)
      )
    
    # Ensure proper data types
    rv$calib_data$raw_data$DateTimeOfAdjustment <- as.POSIXct(rv$calib_data$raw_data$DateTimeOfAdjustment, format="%Y-%m-%d %H:%M:%S", tz="UTC")
    rv$calib_data$raw_data$Kit_Lot <- as.numeric(rv$calib_data$raw_data$Kit_Lot)
    
    conn <- mysql_conn()
    
    tryCatch({
      withProgress(
        message = "Uploading adjustment data to SQL...",
        detail = "Checking for duplicates...",
        value = 0, {
          
          # Function to check for existing records using the correct composite key
          check_existing_records <- function(chunk) {
            # Ensure Kit_Lot is numeric
            chunk$Kit_Lot <- as.numeric(chunk$Kit_Lot)
            
            # Create a query to check existing records
            placeholders <- paste(
              sprintf("('%s', '%s', '%s', %.0f, '%s')",  # Changed to %.0f for numeric Kit_Lot
                      chunk$id_lab,
                      chunk$sn,
                      chunk$Reagent_Name,
                      chunk$Kit_Lot,
                      format(as.POSIXct(chunk$DateTimeOfAdjustment), "%Y-%m-%d %H:%M:%S")),
              collapse = ", "
            )
            
            query <- sprintf("
            SELECT id_lab, sn, Reagent_Name, Kit_Lot, DateTimeOfAdjustment
            FROM adjustments_data_sql
            WHERE (id_lab, sn, Reagent_Name, Kit_Lot, DateTimeOfAdjustment)
            IN (%s)", placeholders)
            
            existing <- dbGetQuery(conn, query)
            
            if (nrow(existing) > 0) {
              existing$DateTimeOfAdjustment <- as.POSIXct(existing$DateTimeOfAdjustment)
              existing$Kit_Lot <- as.numeric(existing$Kit_Lot)  # Convert to numeric after query
            }
            
            return(existing)
          }
          
          chunk_size <- 1000
          total_rows <- nrow(rv$calib_data$raw_data)
          total_chunks <- ceiling(total_rows / chunk_size)
          new_records_count <- 0
          

          
          for (i in seq_len(total_chunks)) {
            start_idx <- ((i - 1) * chunk_size + 1)
            end_idx <- min(i * chunk_size, total_rows)
            chunk <- rv$calib_data$raw_data[start_idx:end_idx, ]
            
            # Check for existing records
            existing_records <- check_existing_records(chunk)
            
            # Filter out existing records
            if (nrow(existing_records) > 0) {
              chunk <- anti_join(
                chunk,
                existing_records,
                by = c("id_lab", "sn", "Reagent_Name", "Kit_Lot", "DateTimeOfAdjustment")
              )
            }
            
            # Only proceed if there are new records to insert
            if (nrow(chunk) > 0) {
              # Format datetime for SQL insertion
              chunk$DateTimeOfAdjustment <- format(as.POSIXct(chunk$DateTimeOfAdjustment), "%Y-%m-%d %H:%M:%S")
              
              # Prepare INSERT query
              sql_query <- paste(
                "INSERT IGNORE INTO adjustments_data_sql (
                id_lab, sn, Reagent_Name, Kit_Lot, DateTimeOfAdjustment, 
                Adjustor_Lot_Low, Adjustor_Lot_High, Slope, Intercept, 
                CV_Counts_Low, CV_Counts_High, CPS_Low_1, CPS_Low_2, 
                CPS_Low_3, CPS_Low_4, CPS_High_1, CPS_High_2, CPS_High_3, 
                CPS_High_4, Adjustor_Concentration_Low, Adjustor_Concentration_High, 
                Mean_CPS_Low, Mean_CPS_High, CPS_Low_Excluded, CPS_High_Excluded, 
                Adjustment_Index, Intercept_Guide, FileName
              ) VALUES",
                paste(
                  apply(chunk, 1, function(row) {
                    paste0(
                      "(",
                      paste(sapply(row, function(x) {
                        if (is.logical(x)) as.integer(ifelse(is.na(x), 0, x))
                        else if (is.numeric(x)) ifelse(is.na(x), "NULL", sprintf("%.0f", x))  # Format numeric as integer
                        else if (is.na(x)) "NULL"
                        else shQuote(as.character(x), type = "sh")
                      }), collapse = ", "),
                      ")"
                    )
                  }),
                  collapse = ", "
                )
              )
              
              # Execute the query
              dbExecute(conn, sql_query)
              new_records_count <- new_records_count + nrow(chunk)
            }
            
            incProgress(
              1/total_chunks, 
              detail = sprintf("Processed chunk %d of %d (%d new records)", 
                               i, total_chunks, new_records_count)
            )
          }
          
          showNotification(
            sprintf("Upload complete! Added %d new adjustment records.", new_records_count),
            type = "message"
          )
        }
      )
    }, error = function(e) {
      showNotification(sprintf("Error uploading adjustment data: %s", e$message), type = "error")
    }, finally = {
      dbDisconnect(conn)
    })
  })
  
  #_________________________________________________________________________________________________________________|
  
  
#_______________________________ **END LOADING Adjustment Data** _____________________________________________|  


  
  
  
  
  
  
  
#
#
#
#
#
#
#
  
  
  
  
  
  
  


#----------------------------------------------------------------------------------------------------------------------------------------------|
#_______________________________ ** Start working with Kits Data** ____________________________________________________________________________|
#______________________________________________________________________________________________________________________________________________|

  
 
  
  
  

  
  
  # -------------------------- Константы и настройки --------------------------
  

    MAX_FILE_SIZE = 200 * 1024^2
  
  columns_to_load_kits = c(1,2,4,8,13,
                             14,18,19,20,21, 
                             33,34,51,
                             62:69, 82:89, 102,105,108,
                             109,134,137, 164, 165)
  
    required_cols = c("Reagent_Name", "Kit_Lot")
  
  
  
  
  rv_kits <- reactiveValues(
    kits_data = NULL,           # Raw data storage
    filtered_data_kits = NULL,  # Filtered data storage
    load_status = NULL,         # Status of data loading
    summary_stats = NULL,       # Storage for summary statistics
    validation_status = NULL    # Status of data validation
  )
  
  
  
  validate_kits_data <- function(data) {
    required_cols <- c("id_lab", "sn" ,"Reagent_Name", "Kit_Lot")
    missing_cols <- setdiff(required_cols, names(data))
    
    if (length(missing_cols) > 0) {
      stop(sprintf("Отсутствуют обязательные столбцы: %s", 
                   paste(missing_cols, collapse = ", ")))
    }
    
    if (nrow(data) == 0) {
      stop("Файл не содержит данных")
    }
    
    TRUE
  }
  

                    
                   
  

#------------------------------------ ... File upload handler  ... --------------|
  
  
  # Приведение типов и обработка пропущенных значений для данных Kits
  observeEvent(input$kits_files, {
    req(input$kits_files)  # Проверяем, что файлы загружены
    
    withProgress(message = "Обработка файлов Kits...", value = 0, {
      output$kits_load_status <- renderText("Началась обработка файлов...")
      
      tryCatch({
        # Проверка и обработка файлов
        valid_files <- input$kits_files$datapath[sapply(input$kits_files$datapath, validate_file)]
        
        if (length(valid_files) == 0) {
          stop("Нет подходящих файлов для обработки")
        }
        
        # Обрабатываем файлы
        total_files <- length(valid_files)
        all_data <- bind_rows(lapply(seq_along(valid_files), function(i) {
          file <- valid_files[i]
          
          incProgress(1 / total_files, 
                      detail = sprintf("Обработка файла %d из %d", i, total_files))
          
          # Проверка кэша
          cache_key <- digest::digest(file)
          cache_env <- data_cache()
          
          if (exists(cache_key, envir = cache_env)) {
            return(get(cache_key, envir = cache_env))
          }
          
          # Загружаем данные
          df <- openxlsx::read.xlsx(file, cols = columns_to_load_kits, colNames = TRUE)
          
          # Проверка наличия данных
          if (nrow(df) == 0) {
            warning(sprintf("Файл %s пустой", basename(file)))
            return(NULL)
          }
          
          # Преобразуем типы данных и заменяем NA в строковых столбцах на пустые строки
          df <- df %>%
            mutate(across(where(is.character), ~if_else(is.na(.), NA_character_, .)),  # Заменяем NA на NA_character_ в строках
                   across(where(is.numeric), ~if_else(is.na(.), NA_real_, .)))  # Заменяем NA на NA_real_ в числовых столбцах
          
          # Преобразуем типы данных
          processed_df <- tryCatch({
            df %>% 
              mutate(
                id_lab = as.character(id_lab),
                sn = as.character(sn),
                Reagent_Name = as.character(Reagent_Name),
                Kit_Lot = as.numeric(Kit_Lot),
                Bead_Lot = as.character(Bead_Lot),
                Adjustor_Lot_1 = as.character(Adjustor_Lot_1),
                Adjustor_Lot_2 = as.character(Adjustor_Lot_2),
                Adjustor_Concentration_1 = as.numeric(Adjustor_Concentration_1),
                Adjustor_Concentration_2 = as.numeric(Adjustor_Concentration_2),
                Times_Adjusted = as.numeric(Times_Adjusted),
                Curve_Parameter_1 = as.numeric(Curve_Parameter_1),
                Curve_Parameter_2 = as.numeric(Curve_Parameter_2),
                Curve_Parameter_3 = as.numeric(Curve_Parameter_3),
                Curve_Parameter_4 = as.numeric(Curve_Parameter_4),
                Curve_Parameter_5 = as.numeric(Curve_Parameter_5),
                Curve_Parameter_6 = as.numeric(Curve_Parameter_6),
                Curve_Parameter_7 = as.numeric(Curve_Parameter_7),
                Curve_Parameter_8 = as.numeric(Curve_Parameter_8),
                Dose_Parameter_1 = as.numeric(Dose_Parameter_1),
                Dose_Parameter_2 = as.numeric(Dose_Parameter_2),
                Dose_Parameter_3 = as.numeric(Dose_Parameter_3),
                Dose_Parameter_4 = as.numeric(Dose_Parameter_4),
                Dose_Parameter_5 = as.numeric(Dose_Parameter_5),
                Dose_Parameter_6 = as.numeric(Dose_Parameter_6),
                Dose_Parameter_7 = as.numeric(Dose_Parameter_7),
                Dose_Parameter_8 = as.numeric(Dose_Parameter_8),
                Kit_Expiration = ymd(Kit_Expiration),
                Adjust_Frequency = as.numeric(Adjust_Frequency),
                Assay_Max = as.numeric(Assay_Max),
                Assay_Min = as.numeric(Assay_Min),
                Default_Units = as.character(Default_Units),
                Reported_Units = as.character(Reported_Units),
                Pre_Dilution_Factor = as.numeric(Pre_Dilution_Factor),
                CPS_Cutoff = as.numeric(CPS_Cutoff),
                file_source = basename(file)
              )
          }, error = function(e) {
            warning(sprintf("Ошибка при обработке файла %s: %s", basename(file), e$message))
            return(NULL)
          })
          
          # Сохраняем в кэш
          if (!is.null(processed_df)) {
            assign(cache_key, processed_df, envir = cache_env)
          }
          
          processed_df
        }))
        
        # Проверка на наличие данных
        if (is.null(all_data) || nrow(all_data) == 0) {
          stop("Не удалось загрузить данные из файлов")
        }
        
        # Удаляем дубли и сортируем
        cleaned_data <- all_data %>%
          distinct(Kit_Lot, Bead_Lot, Adjustor_Lot_1, Adjustor_Lot_2, .keep_all = TRUE) %>%
          arrange(Reagent_Name)
        
        # Сохраняем обработанные данные
        rv_kits$kits_data <- cleaned_data 
        
        # Подсчёт дубликатов
        duplicate_count <- nrow(all_data) - nrow(cleaned_data)
        
        # Обновляем статистику
        output$kits_summary <- renderText({
          req(rv_kits$kits_data)
          
          num_labs <- length(unique(rv_kits$kits_data$id_lab))
          num_tests <- nrow(rv_kits$kits_data)
          date_range <- range(rv_kits$kits_data$Kit_Expiration, na.rm = TRUE)
          date_range_text <- paste(
            format(date_range[1], "%d %b %Y"),
            "по",
            format(date_range[2], "%d %b %Y")
          )
          
          paste(
            "Лабораторий: ", num_labs, "\n",
            "Тестов: ", num_tests, "\n",
            "Диапазон дат: ", date_range_text, "\n",
            "Удалено дублирующихся записей: ", duplicate_count
          )
        })
        
        # Статус загрузки
        invalid_files <- length(input$kits_files$datapath) - length(valid_files)
        output$kits_load_status <- renderText(sprintf(
          "Загрузка завершена! Обработано файлов: %d, записей: %d. Пропущено файлов: %d.",
          length(valid_files),
          nrow(cleaned_data),
          invalid_files
        ))
        
      }, error = function(e) {
        showNotification(paste("Ошибка при загрузке файлов:", e$message), type = "error")
      })
    })
  })
  
  
  

  


#--------------------------------------------------------------------------------|
  
  # Обработчик кнопки сброса
  observeEvent(input$reset_kits_upload, {
    rv_kits$kits_data <- NULL
    rv_kits$load_status <- NULL
    
    # Сброс фильтров
    updateSelectInput(session, "kits_test_select", choices = c("Все"))
    updateSelectInput(session, "kits_lot_select", choices = c("Все"))
    updateSelectInput(session, "kits_manufacturer_select", choices = c("Все"))
    
    # Очистка загруженного файла
    runjs('document.getElementById("kits_files").value = "";')
    
    showNotification("Данные сброшены", type = "warning")
  })
  
  # Статус загрузки
  output$kits_load_status <- renderText({
    if (is.null(rv_kits$load_status)) {
      "Ожидание загрузки файла..."
    } else if (rv_kits$load_status == "success") {
      sprintf("Загружено записей: %d", nrow(rv_kits$kits_data))
    } else {
      "Ошибка загрузки файла"
    }
  })
  
  # Сводка данных
  output$kits_summary <- renderText({
    req(rv_kits$kits_data)
    req(rv_kits$load_status == "success")
    
    data <- rv_kits$kits_data
    sprintf(
      "Всего записей: %d\nУникальных тестов: %d\nУникальных лотов: %d",
      nrow(data),
      length(unique(data$Reagent_Name)),
      length(unique(data$Kit_Lot))
    )
  })
  
  
  

  

#------------------------------- FILTERS ---------------------------------------------|

observe({
  req(rv_kits$kits_data)
  
  updateSelectInput(session, "kits_test_select",
                    choices = unique(rv_kits$kits_data$Reagent_Name),
                    selected = NULL
  )
  
  updateSelectInput(session, "lab_select",
                    choices = c("All", unique(rv_kits$kits_data$id_lab)),
                    selected = "All"
  )
  
  updateSelectInput(session, "kits_lot_select",
                    choices = c("All", unique(rv_kits$kits_data$Kit_Lot)),
                    selected = "All"
  )
  
  # Установка дат на основе данных
  valid_dates <- rv_kits$kits_data$Kit_Expiration[!is.na(rv_kits$kits_data$Kit_Expiration)]
  if (length(valid_dates) > 0) {
    updateDateRangeInput(session, "kits_date_filter",
                         start = min(valid_dates),
                         end = max(valid_dates)
    )
  }
})

# Обновление списка лабораторий при изменении теста
observe({
  req(rv_kits$kits_data)
  
  selected_test <- input$kits_test_select
  data <- rv_kits$kits_data
  
  if (!is.null(selected_test) && selected_test != "") {
    data <- data %>% filter(Reagent_Name == selected_test)
  }
  
  updateSelectInput(session, "lab_select",
                    choices = c("All", unique(data$id_lab)),
                    selected = if(is.null(input$lab_select)) "All" else input$lab_select
  )
})

# Обновление списка Kit Lots при изменении теста или лаборатории
observe({
  req(rv_kits$kits_data)
  
  selected_test <- input$kits_test_select
  selected_labs <- input$lab_select
  data <- rv_kits$kits_data
  
  if (!is.null(selected_test) && selected_test != "") {
    data <- data %>% filter(Reagent_Name == selected_test)
  }
  if (!is.null(selected_labs) && !("All" %in% selected_labs)) {
    data <- data %>% filter(id_lab %in% selected_labs)
  }
  
  updateSelectInput(session, "kits_lot_select",
                    choices = c("All", unique(data$Kit_Lot)),
                    selected = if(is.null(input$kits_lot_select)) "All" else input$kits_lot_select
  )
})

# Фильтрация данных
filtered_kits_data <- reactive({
  req(rv_kits$kits_data)
  
  data <- rv_kits$kits_data
  
  # Фильтр по тесту
  if (!is.null(input$kits_test_select) && input$kits_test_select != "") {
    data <- data %>% filter(Reagent_Name == input$kits_test_select)
  }
  
  # Фильтр по лабораториям
  if (!is.null(input$lab_select) && !("All" %in% input$lab_select)) {
    data <- data %>% filter(id_lab %in% input$lab_select)
  }
  
  # Фильтр по Kit Lot
  if (!is.null(input$kits_lot_select) && !("All" %in% input$kits_lot_select)) {
    data <- data %>% filter(Kit_Lot %in% input$kits_lot_select)
  }
  
  # Фильтр по датам
  if (!is.null(input$kits_date_filter)) {
    data <- data %>%
      filter(
        !is.na(Kit_Expiration),
        Kit_Expiration >= as.Date(input$kits_date_filter[1]),
        Kit_Expiration <= as.Date(input$kits_date_filter[2])
      )
  }
  
  data
})

# Сохранение отфильтрованных данных
observe({
  rv_kits$filtered_data_kits <- filtered_kits_data()
  
  # Уведомление, если нет данных
  if (nrow(rv_kits$filtered_data_kits) == 0) {
    showNotification(
      "No data matches the current filters",
      type = "warning",
      duration = 5
    )
  }
})

# Сброс фильтров
observeEvent(input$reset_filters, {
  updateSelectInput(session, "kits_test_select", selected = NULL)
  updateSelectInput(session, "lab_select", selected = "All")
  updateSelectInput(session, "kits_lot_select", selected = "All")
  
  # Сброс дат на полный диапазон
  valid_dates <- rv_kits$kits_data$Kit_Expiration[!is.na(rv_kits$kits_data$Kit_Expiration)]
  if (length(valid_dates) > 0) {
    updateDateRangeInput(session, "kits_date_filter",
                         start = min(valid_dates),
                         end = max(valid_dates)
    )
  }
})



#-------------------------------------------------------------------------------------|


                    
                    


# ----------------------- Таблицы для Kits (+) ---------------------------------------|


  
output$kits_data_table <- DT::renderDT({
  req(rv_kits$filtered_data_kits)  # Убедиться, что данные отфильтрованы
  
  datatable(
    rv_kits$filtered_data_kits,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = 'lBfrtip',
      buttons = c('copy', 'csv', 'excel'),
      lengthMenu = list(c(10, 25, 50, -1), c("10", "25", "50", "Все")),
      columnDefs = list(list(targets = "_all", className = "dt-center"))
    ),
    rownames = FALSE,
    class = 'display cell-border compact'
  )
})




                   
                   


  
  # ----------------------- Графики для Kits output$kits_usage_plot -------------|

  
  
  
  output$kits_usage_plot <- renderPlotly({
    req(rv_kits$filtered_data_kits)
    
    tryCatch({
      # Подготовка данных с дополнительной проверкой
      validation_data <- rv_kits$filtered_data_kits %>%
        group_by(Reagent_Name, Kit_Lot) %>%
        # Добавляем проверку на минимальное количество наблюдений
        filter(n() >= 2) %>%  # Нужно минимум 2 наблюдения для расчета SD
        summarise(
          N = n(),  # Добавляем подсчет количества наблюдений
          Mean_Intercept = round(mean(Intercept, na.rm = TRUE), 4),
          SD_Intercept = round(sd(Intercept, na.rm = TRUE), 4),
          CV_Intercept = round((SD_Intercept / Mean_Intercept) * 100, 2),
          .groups = 'drop'
        ) %>%
        filter(!is.na(CV_Intercept)) %>%  # Удаляем строки с NA в CV
        arrange(desc(CV_Intercept))
      
      if (nrow(validation_data) == 0) {
        showNotification("No sufficient data for CV calculation", type = "warning")
        return(NULL)
      }
      
      # Создание графика с дополнительной информацией
      p <- ggplot(validation_data, aes(
        x = reorder(Reagent_Name, -CV_Intercept),
        y = CV_Intercept,
        fill = as.factor(Kit_Lot),
        text = sprintf(
          "Test: %s\nLot: %s\nCV: %.1f%%\nN: %d",
          Reagent_Name, Kit_Lot, CV_Intercept, N
        )
      )) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", CV_Intercept, N)), 
                  position = position_dodge(0.7), 
                  vjust = -0.5, size = 3) +
        scale_fill_viridis_d(name = "Kit Lot") +
        labs(
          title = paste("Coefficient of Variation by Reagent\n",
                        "Total tests:", length(unique(validation_data$Reagent_Name)),
                        "Total lots:", length(unique(validation_data$Kit_Lot))),
          x = "Reagent Name",
          y = "Coefficient of Variation (%)"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(showlegend = TRUE,
               margin = list(b = 100))  # Увеличиваем нижний отступ для длинных подписей
      
    }, error = function(e) {
      showNotification(
        sprintf("Error creating plot: %s", conditionMessage(e)),
        type = "error"
      )
      return(NULL)
    })
  })

  # Добавляем вывод таблицы
  output$kits_usage_table <- renderDT({
    req(rv_kits$filtered_data_kits)
    
    tryCatch({
      # Подготовка данных для таблицы
      table_data <- rv_kits$filtered_data_kits %>%
        group_by(Reagent_Name, Kit_Lot) %>%
        filter(n() >= 2) %>%
        summarise(
          N = n(),
          Mean_Intercept = round(mean(Intercept, na.rm = TRUE), 4),
          SD_Intercept = round(sd(Intercept, na.rm = TRUE), 4),
          CV_Intercept = round((SD_Intercept / Mean_Intercept) * 100, 2),
          .groups = 'drop'
        ) %>%
        filter(!is.na(CV_Intercept)) %>%
        arrange(desc(CV_Intercept)) %>%
        rename(
          "Test Name" = Reagent_Name,
          "Kit Lot" = Kit_Lot,
          "Sample Count" = N,
          "Mean" = Mean_Intercept,
          "SD" = SD_Intercept,
          "CV (%)" = CV_Intercept
        )
      
      # Форматирование и вывод таблицы
      datatable(
        table_data,
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          pageLength = 10,
          order = list(list(5, 'desc')),  # Сортировка по CV по убыванию
          scrollX = TRUE
        ),
        class = 'cell-border stripe',
        rownames = FALSE
      ) %>%
        formatRound(
          columns = c('Mean', 'SD', 'CV (%)'),
          digits = 2
        )
      
    }, error = function(e) {
      showNotification(
        sprintf("Error creating table: %s", conditionMessage(e)),
        type = "error"
      )
      return(NULL)
    })
  })

                   
                   


  
# -------------------- Дополнительные детали  output$kits_validation_details ---|
  

  # Вывод детальной информации о валидации
  output$kits_validation_details <- renderPrint({
    req(rv_kits$filtered_data_kits)
    
    tryCatch({
      # Подсчет основных метрик
      validation_summary <- list(
        total_tests = length(unique(rv_kits$filtered_data_kits$Reagent_Name)),
        total_kits = length(unique(rv_kits$filtered_data_kits$Kit_Lot)),
        total_records = nrow(rv_kits$filtered_data_kits),
        missing_values = sum(is.na(rv_kits$filtered_data_kits$Intercept)) + 
          sum(is.na(rv_kits$filtered_data_kits$Slope))
      )
      
      # Статистика по тестам
      test_stats <- rv_kits$filtered_data_kits %>%
        group_by(Reagent_Name) %>%
        summarise(
          Kits_Count = n_distinct(Kit_Lot),
          Records_Count = n(),
          Avg_Slope = mean(Slope, na.rm = TRUE),
          Avg_Intercept = mean(Intercept, na.rm = TRUE),
          .groups = 'drop'
        )
      
      # Формируем вывод
      cat("=== Kits Validation Summary ===\n\n")
      cat(sprintf("Total Tests: %d\n", validation_summary$total_tests))
      cat(sprintf("Total Kits: %d\n", validation_summary$total_kits))
      cat(sprintf("Total Records: %d\n", validation_summary$total_records))
      cat(sprintf("Missing Values: %d\n\n", validation_summary$missing_values))
      
      cat("=== Detailed Statistics by Test ===\n\n")
      print(test_stats)
      
    }, error = function(e) {
      cat("Error generating validation details:", conditionMessage(e))
    })
  })
  
  # Таблица с детальной информацией
  output$kits_validation_table <- renderDT({
    req(rv_kits$filtered_data_kits)
    
    tryCatch({
      # Создаем детальную таблицу без дат
      validation_table <- rv_kits$filtered_data_kits %>%
        group_by(Reagent_Name, Kit_Lot) %>%
        summarise(
          Records = n(),
          Mean_Slope = mean(Slope, na.rm = TRUE),
          SD_Slope = sd(Slope, na.rm = TRUE),
          CV_Slope = (SD_Slope / Mean_Slope) * 100,
          Mean_Intercept = mean(Intercept, na.rm = TRUE),
          SD_Intercept = sd(Intercept, na.rm = TRUE),
          CV_Intercept = (SD_Intercept / Mean_Intercept) * 100,
          .groups = 'drop'
        ) %>%
        arrange(Reagent_Name, Kit_Lot) %>%
        rename(
          "Test Name" = Reagent_Name,
          "Kit Lot" = Kit_Lot,
          "Record Count" = Records
        )
      
      # Создаем таблицу с форматированием
      datatable(
        validation_table,
        extensions = c('Buttons', 'Scroller'),
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            list(extend = 'copy'),
            list(extend = 'csv'),
            list(extend = 'excel')
          ),
          pageLength = 10,
          scrollX = TRUE,
          scrollY = 400,
          scroller = TRUE
        ),
        class = 'cell-border stripe',
        rownames = FALSE
      ) %>%
        formatRound(
          columns = c("Mean_Slope", "SD_Slope", "CV_Slope",
                      "Mean_Intercept", "SD_Intercept", "CV_Intercept"),
          digits = 2
        )
      
    }, error = function(e) {
      showNotification(
        sprintf("Error creating validation table: %s", conditionMessage(e)),
        type = "error"
      )
      return(NULL)
    })
  })
  

                    
                   

# ---------------------- График параметров калибровочной кривой output$calibration_params_plot -----------------------|

  
  output$calibration_trend_plot <- renderPlotly({
    req(rv_kits$filtered_data_kits)
    
    tryCatch({
      # Подготовка данных для графика
      plot_data <- rv_kits$filtered_data_kits %>%
        mutate(
          Curve_Parameter_2 = as.numeric(Curve_Parameter_2),
          Curve_Parameter_3 = as.numeric(Curve_Parameter_3)
        ) %>%
        group_by(Reagent_Name, Kit_Lot) %>%
        summarise(
          CP1 = mean(Curve_Parameter_1, na.rm = TRUE),
          CP2 = mean(Curve_Parameter_2, na.rm = TRUE),
          CP3 = mean(Curve_Parameter_3, na.rm = TRUE),
          CP4 = mean(Curve_Parameter_4, na.rm = TRUE),
          Slope = mean(Slope, na.rm = TRUE),
          Intercept = mean(Intercept, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(Reagent_Name, Kit_Lot)
      
      # Создание графика
      p <- plot_ly(data = plot_data, type = 'scatter', mode = 'lines+markers') %>%
        # Параметр 1
        add_trace(
          x = ~Kit_Lot,
          y = ~CP1,
          color = ~Reagent_Name,
          name = ~paste(Reagent_Name, "- P1"),
          visible = "legendonly",
          text = ~sprintf(
            "Test: %s<br>Lot: %d<br>P1: %.2e<br>Slope: %.3f<br>Intercept: %.2f",
            Reagent_Name, Kit_Lot, CP1, Slope, Intercept
          ),
          hoverinfo = 'text'
        ) %>%
        # Параметр 2
        add_trace(
          x = ~Kit_Lot,
          y = ~CP2,
          color = ~Reagent_Name,
          name = ~paste(Reagent_Name, "- P2"),
          visible = "legendonly",
          text = ~sprintf(
            "Test: %s<br>Lot: %d<br>P2: %.2e<br>Slope: %.3f<br>Intercept: %.2f",
            Reagent_Name, Kit_Lot, CP2, Slope, Intercept
          ),
          hoverinfo = 'text'
        ) %>%
        # Параметр 3
        add_trace(
          x = ~Kit_Lot,
          y = ~CP3,
          color = ~Reagent_Name,
          name = ~paste(Reagent_Name, "- P3"),
          visible = "legendonly",
          text = ~sprintf(
            "Test: %s<br>Lot: %d<br>P3: %.2e<br>Slope: %.3f<br>Intercept: %.2f",
            Reagent_Name, Kit_Lot, CP3, Slope, Intercept
          ),
          hoverinfo = 'text'
        ) %>%
        # Slope
        add_trace(
          x = ~Kit_Lot,
          y = ~Slope,
          color = ~Reagent_Name,
          name = ~paste(Reagent_Name, "- Slope"),
          text = ~sprintf(
            "Test: %s<br>Lot: %d<br>Slope: %.3f<br>Intercept: %.2f",
            Reagent_Name, Kit_Lot, Slope, Intercept
          ),
          hoverinfo = 'text'
        ) %>%
        # Настройка макета
        layout(
          title = list(
            text = "Calibration Parameters Trends by Lot",
            x = 0.5
          ),
          xaxis = list(
            title = "Kit Lot",
            tickangle = 45
          ),
          yaxis = list(
            title = "Parameter Value",
            type = "linear"
          ),
          hovermode = "closest",
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            y = -0.2
          ),
          margin = list(b = 100)  # Увеличиваем нижний отступ для легенды
        )
      
      p
      
    }, error = function(e) {
      showNotification(sprintf("Error in plot: %s", e$message), type = "error")
      return(NULL)
    })
  })

                   
                   


#---------------      Performance Metrics            --------------------------|

# График "Performance Metrics"
output$kits_performance_plot <- renderPlotly({
  req(rv_kits$filtered_data_kits)
  
  tryCatch({
    performance_data <- rv_kits$filtered_data_kits %>%
      group_by(Reagent_Name, Kit_Lot) %>%
      summarise(
        Mean_Slope = round(mean(Slope, na.rm = TRUE), 4),
        Mean_Intercept = round(mean(Intercept, na.rm = TRUE), 4),
        SD_Slope = round(sd(Slope, na.rm = TRUE), 4),
        SD_Intercept = round(sd(Intercept, na.rm = TRUE), 4),
        .groups = 'drop'
      )
    
    if (nrow(performance_data) == 0) {
      showNotification("No data available for plotting", type = "warning")
      return(NULL)
    }
    
    p <- ggplot(performance_data, 
                aes(x = Mean_Slope, 
                    y = Mean_Intercept, 
                    color = Reagent_Name,
                    text = sprintf(
                      "Test: %s\nLot: %s\nSlope: %.4f\nIntercept: %.4f",
                      Reagent_Name, Kit_Lot, Mean_Slope, Mean_Intercept
                    ))) +
      geom_point(size = 3, alpha = 0.7) +
      labs(
        title = "Performance Metrics: Mean Slope vs Mean Intercept",
        x = "Mean Slope",
        y = "Mean Intercept"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = "text")
    
  }, error = function(e) {
    showNotification(sprintf("Error in plot: %s", e$message), type = "error")
    return(NULL)
  })
})

# Таблица "Performance Metrics"
output$kits_performance_table <- renderDT({
  req(rv_kits$filtered_data_kits)
  
  tryCatch({
    # Проверяем данные
    if (nrow(rv_kits$filtered_data_kits) == 0) {
      showNotification("No data available", type = "warning")
      return(NULL)
    }
    
    # Создаем таблицу данных
    performance_data <- rv_kits$filtered_data_kits %>%
      group_by(Reagent_Name, Kit_Lot) %>%
      summarise(
        N = n(),
        Mean_Slope = mean(Slope, na.rm = TRUE),
        SD_Slope = sd(Slope, na.rm = TRUE),
        CV_Slope = (SD_Slope / Mean_Slope) * 100,
        Mean_Intercept = mean(Intercept, na.rm = TRUE),
        SD_Intercept = sd(Intercept, na.rm = TRUE),
        CV_Intercept = (SD_Intercept / Mean_Intercept) * 100,
        .groups = 'drop'
      )
    
    # Проверяем результат агрегации
    if (nrow(performance_data) == 0) {
      showNotification("No data after aggregation", type = "warning")
      return(NULL)
    }
    
    # Создаем таблицу
    DT::datatable(
      performance_data,
      colnames = c(
        "Test Name" = "Reagent_Name",
        "Kit Lot" = "Kit_Lot",
        "Sample Count" = "N",
        "Mean Slope" = "Mean_Slope",
        "SD Slope" = "SD_Slope",
        "CV Slope (%)" = "CV_Slope",
        "Mean Intercept" = "Mean_Intercept",
        "SD Intercept" = "SD_Intercept",
        "CV Intercept (%)" = "CV_Intercept"
      ),
      extensions = c('Buttons', 'Scroller'),
      options = list(
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy'),
          list(extend = 'csv'),
          list(extend = 'excel')
        ),
        pageLength = 10,
        scrollX = TRUE,
        scrollY = 400,
        scroller = TRUE,
        searching = TRUE
      ),
      class = 'cell-border stripe'
    ) %>%
      formatRound(
        columns = c(4:9),  # Форматируем числовые колонки
        digits = 2
      )
    
  }, error = function(e) {
    # Более подробный вывод ошибки для отладки
    message("Error in performance table: ", e$message)
    showNotification(
      sprintf("Error creating table: %s", conditionMessage(e)),
      type = "error",
      duration = NULL
    )
    return(NULL)
  })
})





  
                   
                   # Обработчик экспорта данных         
  
  

  output$export_kits_data <- downloadHandler(
    filename = function() {
      paste("kits_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv_kits$kits_data)
      tryCatch({
        openxlsx::write.xlsx(rv_kits$kits_data, file)
      }, error = function(e) {
        showNotification(
          sprintf("Ошибка при экспорте данных: %s", conditionMessage(e)),
          type = "error"
        )
      })
    }
  )
  
  
#---------------------------------------------------------------------------------------------------------------------------------|
#__________________________________________**** END WORK WITH KITS **** __________________________________________________________|
#_________________________________________________________________________________________________________________________________|


  
  
  
#
#
#
#
#
#
#
#
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # -----------------------------------------------------------------------------------------------------------------------------------|
  # __________________________________________**** START Analyzing Data from SQL ****________________________________________________|
  # ___________________________________________________________________________________________________________________________________|
  
  # Реактивные данные
  rv_sql <- reactiveValues(
    kits_data_sql = NULL,
    results_data_sql = NULL,
    adjustments_data_sql = NULL
  )
  
  # Логирование сообщений
  log_message_sql <- function(message, type = "info") {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    writeLines(sprintf("[%s] [%s] %s", timestamp, type, message), con = "app_sql.log", useBytes = TRUE)
  }
  
  # Тест соединения с базой данных
  test_conn <- mysql_conn()
  if (!is.null(test_conn)) {
    print("Connection successful!")
    dbDisconnect(test_conn)
  } else {
    print("Connection failed.")
  }
  
  # ------------------------------------ Загрузка Данных ----------------------------------------------------------|
  
  observeEvent(input$load_sql_data, {
    withProgress(message = 'Loading data...', value = 0, {
      conn <- mysql_conn()
      if (is.null(conn)) {
        showNotification("Failed to connect to database", type = "error")
        return()
      }
      
      tryCatch({
        # Загружаем данные
        rv_sql$kits_data_sql <- dbGetQuery(conn, "SELECT * FROM kits_data_sql")
        rv_sql$results_data_sql <- dbGetQuery(conn, "SELECT * FROM results_data_sql")
        rv_sql$adjustments_data_sql <- dbGetQuery(conn, "SELECT * FROM adjustments_data_sql")
        
        log_message_sql(paste("Loaded rows: Kits =", nrow(rv_sql$kits_data_sql), 
                              ", Results =", nrow(rv_sql$results_data_sql), 
                              ", Adjustments =", nrow(rv_sql$adjustments_data_sql)))
        showNotification("Data loaded successfully", type = "message")
        
        # Инициализация фильтров
        test_choices <- rv_sql$kits_data_sql %>%
          pull(Reagent_Name) %>%
          unique() %>%
          sort()
        
        if (length(test_choices) > 0) {
          # Устанавливаем первый тест после загрузки
          updateSelectInput(session, "sql_test_select", choices = test_choices, selected = test_choices[1])
        } else {
          # Если тестов нет, оставляем фильтр пустым
          updateSelectInput(session, "sql_test_select", choices = NULL, selected = NULL)
        }
        
        # Обновляем лаборатории и лоты
        updateSelectInput(session, "sql_lab_select", choices = c("All", sort(unique(rv_sql$kits_data_sql$id_lab))), selected = "All")
        updateSelectInput(session, "sql_lot_select", choices = c("All", sort(unique(rv_sql$kits_data_sql$Kit_Lot))), selected = "All")
        
      }, error = function(e) {
        log_message_sql(paste("Error loading data:", e$message), "error")
        showNotification(paste("Error loading data:", e$message), type = "error")
      }, finally = {
        if (!is.null(conn) && dbIsValid(conn)) {
          dbDisconnect(conn)
        }
      })
    })
  })
  
  
  
  
  # ------------------------------------ Фильтрация по лабораториям, тестам и лотам --------------------------------|
  
  observe({
    req(rv_sql$kits_data_sql)
    
    # Начальные данные
    filtered_kits <- rv_sql$kits_data_sql
    
    # Фильтрация по тесту
    if (!is.null(input$sql_test_select) && input$sql_test_select != "All") {
      filtered_kits <- filtered_kits %>% filter(Reagent_Name == input$sql_test_select)
    }
    
    # Обновляем доступные лаборатории
    lab_choices <- filtered_kits %>%
      pull(id_lab) %>%
      unique() %>%
      sort()
    
    current_labs <- input$sql_lab_select
    if (!is.null(current_labs) && "All" %in% current_labs && length(current_labs) > 1) {
      current_labs <- setdiff(current_labs, "All") # Убираем "All", если выбраны другие лаборатории
    }
    current_labs <- current_labs[current_labs %in% lab_choices]
    
    updateSelectInput(
      session, 
      "sql_lab_select", 
      choices = c("All", lab_choices), 
      selected = if (length(current_labs) == 0) "All" else current_labs
    )
    
    # Фильтрация по лабораториям
    if (!is.null(input$sql_lab_select) && !"All" %in% input$sql_lab_select) {
      filtered_kits <- filtered_kits %>% filter(id_lab %in% input$sql_lab_select)
    }
    
    # Обновляем доступные лоты
    lot_choices <- filtered_kits %>%
      pull(Kit_Lot) %>%
      unique() %>%
      sort()
    
    current_lots <- input$sql_lot_select
    if (!is.null(current_lots) && "All" %in% current_lots && length(current_lots) > 1) {
      current_lots <- setdiff(current_lots, "All") # Убираем "All", если выбраны другие лоты
    }
    current_lots <- current_lots[current_lots %in% lot_choices]
    
    updateSelectInput(
      session, 
      "sql_lot_select", 
      choices = c("All", lot_choices), 
      selected = if (length(current_lots) == 0) "All" else current_lots
    )
  })
  
  
  
  
  # ------------------------------------ Обновление диапазона доступных дат -----------------------------------------|  
  
  observe({
    req(rv_sql$adjustments_data_sql)
    
    # Получаем отфильтрованные данные
    filtered_adjustments <- rv_sql$adjustments_data_sql
    
    # Применяем фильтры теста, лаборатории и лота
    if (!is.null(input$sql_test_select) && input$sql_test_select != "All") {
      filtered_adjustments <- filtered_adjustments %>% filter(Reagent_Name == input$sql_test_select)
    }
    if (!is.null(input$sql_lab_select) && !"All" %in% input$sql_lab_select) {
      filtered_adjustments <- filtered_adjustments %>% filter(id_lab %in% input$sql_lab_select)
    }
    if (!is.null(input$sql_lot_select) && !"All" %in% input$sql_lot_select) {
      filtered_adjustments <- filtered_adjustments %>% filter(Kit_Lot %in% input$sql_lot_select)
    }
    
    # Находим минимальную и максимальную даты
    if (nrow(filtered_adjustments) > 0) {
      min_date <- min(as.Date(filtered_adjustments$DateTimeOfAdjustment, format = "%Y-%m-%d %H:%M:%S"), na.rm = TRUE)
      max_date <- max(as.Date(filtered_adjustments$DateTimeOfAdjustment, format = "%Y-%m-%d %H:%M:%S"), na.rm = TRUE)
    } else {
      min_date <- NULL
      max_date <- NULL
    }
    
    # Обновляем фильтр по диапазону дат
    updateDateRangeInput(
      session, 
      "sql_date_filter",
      start = min_date,
      end = max_date,
      min = min_date,
      max = max_date
    )
  })
  
  # -------------------- Основной процесс фильтрации ---------------------------------------------------------------|
  
  filtered_data_test <- reactive({
    req(rv_sql$kits_data_sql, rv_sql$results_data_sql, rv_sql$adjustments_data_sql)
    
    filtered_kits <- rv_sql$kits_data_sql
    filtered_results <- rv_sql$results_data_sql
    filtered_adjustments <- rv_sql$adjustments_data_sql
    
    # Фильтрация по лабораториям
    if (!is.null(input$sql_lab_select) && !"All" %in% input$sql_lab_select) {
      filtered_kits <- filtered_kits %>% filter(id_lab %in% input$sql_lab_select)
      filtered_results <- filtered_results %>% filter(id_lab %in% input$sql_lab_select)
      filtered_adjustments <- filtered_adjustments %>% filter(id_lab %in% input$sql_lab_select)
    }
    
    # Фильтрация по тесту
    if (!is.null(input$sql_test_select) && input$sql_test_select != "All") {
      filtered_kits <- filtered_kits %>% filter(Reagent_Name == input$sql_test_select)
      filtered_results <- filtered_results %>% filter(Reagent_Name == input$sql_test_select)
      filtered_adjustments <- filtered_adjustments %>% filter(Reagent_Name == input$sql_test_select)
    }
    
    # Фильтрация по лоту
    if (!is.null(input$sql_lot_select) && !"All" %in% input$sql_lot_select) {
      filtered_kits <- filtered_kits %>% filter(Kit_Lot %in% input$sql_lot_select)
      filtered_results <- filtered_results %>% filter(Kit_Lot %in% input$sql_lot_select)
      filtered_adjustments <- filtered_adjustments %>% filter(Kit_Lot %in% input$sql_lot_select)
    }
    
    # Фильтрация по дате
    if (!is.null(input$sql_date_filter) && !is.na(input$sql_date_filter[1]) && !is.na(input$sql_date_filter[2])) {
      date_start <- as.Date(input$sql_date_filter[1])
      date_end <- as.Date(input$sql_date_filter[2])
      
      filtered_adjustments <- filtered_adjustments %>%
        filter(as.Date(DateTimeOfAdjustment, format = "%Y-%m-%d %H:%M:%S") >= date_start & 
                 as.Date(DateTimeOfAdjustment, format = "%Y-%m-%d %H:%M:%S") <= date_end)
    }
    
    list(kits = filtered_kits, results = filtered_results, adjustments = filtered_adjustments)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ------------------------------------ Сброс фильтров ------------------------------------------------------------|
  
  observeEvent(input$sql_reset_filters, {
    req(rv_sql$kits_data_sql)
    
    # Получаем список тестов
    test_choices <- rv_sql$kits_data_sql %>%
      pull(Reagent_Name) %>%
      unique() %>%
      sort()
    
    # Проверяем, есть ли доступные тесты
    selected_test <- if (length(test_choices) > 0) {
      test_choices[1]  # Выбираем первый тест
    } else {
      NULL  # Оставляем выбор пустым, если нет тестов
    }
    
    # Сброс фильтров тестов
    updateSelectInput(
      session, 
      "sql_test_select", 
      choices = test_choices, 
      selected = selected_test
    )
    
    # Сброс фильтров лабораторий
    updateSelectInput(
      session, 
      "sql_lab_select", 
      choices = c("All", sort(unique(rv_sql$kits_data_sql$id_lab))), 
      selected = "All"
    )
    
    # Сброс фильтров лотов
    updateSelectInput(
      session, 
      "sql_lot_select", 
      choices = c("All", sort(unique(rv_sql$kits_data_sql$Kit_Lot))), 
      selected = "All"
    )
    
    # Логирование события сброса
    log_message_sql("Filters have been reset to default.")
  })
  
  
  
  
  # -------------------- Вывод данных ------------------------------------------------------------------------------|
  
  
  

  
#------------------------ output$kits_curve_params_plot -----------------------------------|
  
  
  # Построение графика параметров кривой (P1-P4) в зависимости от выбранного лота
  output$kits_curve_params_plot <- renderPlotly({
    req(rv_sql$kits_data_sql, input$sql_lot_select)
    
    # Фильтруем данные на основе выбранного теста и лота
    filtered_data <- rv_sql$kits_data_sql
    
    if (!is.null(input$sql_test_select) && input$sql_test_select != "All") {
      filtered_data <- filtered_data %>% filter(Reagent_Name == input$sql_test_select)
    }
    
    if (!is.null(input$sql_lot_select) && !"All" %in% input$sql_lot_select) {
      filtered_data <- filtered_data %>% filter(Kit_Lot %in% input$sql_lot_select)
    }
    
    # Проверяем, есть ли данные для построения графика
    validate(
      need(nrow(filtered_data) > 0, "Нет данных для отображения")
    )
    
    # Преобразуем данные для графика (P1-P4)
    plot_data <- filtered_data %>%
      select(Kit_Lot, Curve_Parameter_1, Curve_Parameter_2, Curve_Parameter_3, Curve_Parameter_4) %>%
      pivot_longer(
        cols = starts_with("Curve_Parameter"),
        names_to = "Parameter",
        values_to = "Value"
      ) %>%
      mutate(
        Parameter = case_when(
          Parameter == "Curve_Parameter_1" ~ "P1",
          Parameter == "Curve_Parameter_2" ~ "P2",
          Parameter == "Curve_Parameter_3" ~ "P3",
          Parameter == "Curve_Parameter_4" ~ "P4",
          TRUE ~ Parameter
        )
      )
    
    # Создаем график с trace для каждого параметра
    plot <- plot_ly()
    parameters <- unique(plot_data$Parameter)
    
    for (param in parameters) {
      param_data <- plot_data %>% filter(Parameter == param) %>% arrange(Kit_Lot)
      
      plot <- plot %>%
        add_trace(
          data = param_data,
          x = ~Kit_Lot,
          y = ~Value,
          type = "scatter",
          mode = "lines+markers",
          name = param,
          text = ~paste("Лот:", Kit_Lot, "<br>Параметр:", param, "<br>Значение:", round(Value, 2)),
          yaxis = ifelse(param == "P4", "y2", "y") # Используем y2 для P4
        )
    }
    
    # Настройка макета
    plot %>%
      layout(
        title = paste0("Параметры кривой (P1-P4) для теста: ", 
                       ifelse(input$sql_test_select == "All", "Все", input$sql_test_select)),
        xaxis = list(title = "Лот"),
        yaxis = list(
          title = "Логарифмированные значения параметров (P1-P3)",
          type = "log" # Логарифмируем основную ось Y
        ),
        yaxis2 = list(
          title = "Значения параметра P4",
          overlaying = "y",
          side = "right" # Размещаем вторую ось справа
        ),
        legend = list(title = list(text = "Параметры"))
      )
  })
  
  
  
  
  
  
  # Построение графика Master Curve (MCD CPS) для выбранных лотов
  output$mcd_kit_plot <- renderPlotly({
    req(rv_sql$kits_data_sql, input$sql_lot_select)
    
    # Фильтруем данные на основе выбранного теста и лота
    filtered_data <- rv_sql$kits_data_sql
    
    if (!is.null(input$sql_test_select) && input$sql_test_select != "All") {
      filtered_data <- filtered_data %>% filter(Reagent_Name == input$sql_test_select)
    }
    
    if (!is.null(input$sql_lot_select) && !"All" %in% input$sql_lot_select) {
      filtered_data <- filtered_data %>% filter(Kit_Lot %in% input$sql_lot_select)
    }
    
    # Проверяем, есть ли данные после фильтрации
    validate(
      need(nrow(filtered_data) > 0, "Нет данных для отображения")
    )
    
    # Генерация концентраций
    conc_range <- if (input$x_scale == "log") {
      10^(seq(log10(input$conc_min), log10(input$conc_max), length.out = 1000))
    } else {
      seq(from = input$conc_min, to = input$conc_max, length.out = 1000)
    }
    
    # Инициализация графика
    plot <- plot_ly()
    
    # Для каждого выбранного лота строим отдельную линию
    unique_lots <- unique(filtered_data$Kit_Lot)
    
    for (lot in unique_lots) {
      lot_data <- filtered_data %>% filter(Kit_Lot == lot)
      
      # Извлечение параметров P1-P4 для текущего лота
      p1 <- lot_data$Curve_Parameter_1
      p2 <- lot_data$Curve_Parameter_2
      p3 <- lot_data$Curve_Parameter_3
      p4 <- lot_data$Curve_Parameter_4
      
      # Расчет значений MCD CPS
      mcd_cps <- model_mcd_cps(conc_range, p1, p2, p3, p4, "Increase")
      
      # Добавление линии для текущего лота
      plot <- plot %>%
        add_trace(
          x = conc_range,
          y = mcd_cps,
          type = 'scatter',
          mode = 'lines',
          name = paste("Lot", lot),
          line = list(width = 2)
        )
    }
    
    # Настройка осей и легенды
    plot <- plot %>%
      layout(
        title = paste("Master Curve (MCD CPS) для теста:", 
                      ifelse(input$sql_test_select == "All", "Все", input$sql_test_select)),
        xaxis = list(
          title = "Концентрация", 
          type = if (input$x_scale == "log") "log" else "linear"
        ),
        yaxis = list(title = "MCD CPS"),
        showlegend = TRUE
      )
    
    plot
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$kits_all_data <- renderDT({
    filtered <- filtered_data_test()$kits
    datatable(filtered, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$results_all_data_sql <- renderDT({
    filtered <- filtered_data_test()$results
    datatable(filtered, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$adjustments_all_data_sql <- renderDT({
    filtered <- filtered_data_test()$adjustments
    datatable(filtered, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  
  # __________________________________________________________________________________________________________________|
  # __________________________________________**** END Analyzing Data from SQL ****___________________________________|
  # __________________________________________________________________________________________________________________|
  
 
                                                                                                       
                                                                                                       
                                                                                                        
  
  
  

#---------------------------------------------------------------------------------------------------------------------------------|
#-------------------------- ***** 666 START  Работа по отправке данных в MySQL  ***** -------------------------------------------------|
#_________________________________________________________________________________________________________________________________|


#_____________________________ Загрузка данных results из (proceed_data) в SQL  ____________________|

observeEvent(input$upload_to_sql, {
  req(rv_results$proceed_data)
  
  conn <- mysql_conn()
  
  tryCatch({
    withProgress(
      message = "Uploading data to SQL...",
      detail = "Checking for duplicates...",
      value = 0, {
        
        # Function to check for existing records
        check_existing_records <- function(chunk) {
          # Create a temporary table with unique identifiers
          temp_query <- paste0(
            "CREATE TEMPORARY TABLE temp_ids (
              id_lab VARCHAR(255),
              Unique_Record_ID_Num VARCHAR(255),
              Reagent_Lot_Cycle_1 VARCHAR(255),
              PRIMARY KEY (id_lab, Unique_Record_ID_Num, Reagent_Lot_Cycle_1)
            )"
          )
          dbExecute(conn, temp_query)
          
          # Insert chunk identifiers into temp table
          ids_data <- chunk[, c("id_lab", "Unique_Record_ID_Num", "Reagent_Lot_Cycle_1")]
          ids_query <- paste(
            "INSERT INTO temp_ids VALUES",
            paste(
              apply(ids_data, 1, function(row) {
                paste0("(", paste(shQuote(row, type = "sh"), collapse = ", "), ")")
              }),
              collapse = ", "
            )
          )
          dbExecute(conn, ids_query)
          
          # Get existing records
          existing <- dbGetQuery(conn, "
            SELECT t.id_lab, t.Unique_Record_ID_Num, t.Reagent_Lot_Cycle_1
            FROM temp_ids t
            INNER JOIN results_data_sql r
            ON t.id_lab = r.id_lab 
            AND t.Unique_Record_ID_Num = r.Unique_Record_ID_Num
            AND t.Reagent_Lot_Cycle_1 = r.Reagent_Lot_Cycle_1
          ")
          
          # Clean up
          dbExecute(conn, "DROP TEMPORARY TABLE IF EXISTS temp_ids")
          
          return(existing)
        }
        
        # Process data in chunks
        chunk_size <- 1000
        total_rows <- nrow(rv_results$proceed_data)
        total_chunks <- ceiling(total_rows / chunk_size)
        new_records_count <- 0
        
        for (i in seq_len(total_chunks)) {
          # Get current chunk
          start_idx <- ((i - 1) * chunk_size + 1)
          end_idx <- min(i * chunk_size, total_rows)
          chunk <- rv_results$proceed_data[start_idx:end_idx, ]
          
          # Check for existing records
          existing_records <- check_existing_records(chunk)
          
          # Filter out existing records
          if (nrow(existing_records) > 0) {
            chunk <- anti_join(
              chunk,
              existing_records,
              by = c("id_lab", "Unique_Record_ID_Num", "Reagent_Lot_Cycle_1")
            )
          }
          
          # Skip if no new records to insert
          if (nrow(chunk) > 0) {
            # Generate and execute INSERT query for new records
            sql_query <- paste(
              "INSERT INTO results_data_sql (id_lab, sn, Unique_Record_ID_Num, Identifier, 
               Patient_ID_Num, Name, Physician_Name, Dilution_Factor, Reagent_Name, Result, 
               Limit_High, Limit_Low, CPS, Date_And_Time_Resulted, Bead_Lot, 
               Reagent_Lot_Cycle_1, Kit_Lot, Formula_Number, offLineDilution_Factor, 
               Assay_Type, Allergen_Code, Allergen_Lot, PrincipleDFactor, file_source) VALUES",
              paste(
                apply(chunk, 1, function(row) {
                  paste0("(", paste(shQuote(row, type = "sh"), collapse = ", "), ")")
                }),
                collapse = ", "
              )
            )
            
            dbExecute(conn, sql_query)
            new_records_count <- new_records_count + nrow(chunk)
          }
          
          # Update progress
          incProgress(
            1/total_chunks, 
            detail = sprintf("Processed chunk %d of %d (%d new records)", 
                             i, total_chunks, new_records_count)
          )
        }
        
        showNotification(
          sprintf("Upload complete! Added %d new records.", new_records_count),
          type = "message"
        )
      }
    )
  }, error = function(e) {
    showNotification(sprintf("Error uploading to SQL: %s", e$message), type = "error")
  }, finally = {
    dbDisconnect(conn)
  })
})


#___________________ Загрузка данных kits из (rv_kits$kits_data_sql) в SQL  ____________________|

# Database connection pool
db_pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "shiny_app_db",
  host = "127.0.0.1",
  port = 3306,
  user = "root",
  password = "Winter1986+!",
  client.flag = CLIENT_LOCAL_FILES
)

# Helper function to generate SQL query
generate_insert_query <- function(chunk) {
  # First, check which records already exist
  existing_records <- dbGetQuery(db_pool, sprintf(
    "SELECT id_lab, Kit_Lot, Reagent_Name 
     FROM kits_data_sql 
     WHERE (id_lab, Kit_Lot, Reagent_Name) IN (%s)",
    paste(sprintf("(%s, %s, %s)",
                  shQuote(chunk$id_lab),
                  chunk$Kit_Lot,
                  shQuote(chunk$Reagent_Name)),
          collapse = ", ")
  ))
  
  # Filter out existing records
  if (nrow(existing_records) > 0) {
    chunk <- anti_join(chunk, existing_records,
                       by = c("id_lab", "Kit_Lot", "Reagent_Name"))
  }
  
  # If no new records to insert, return NULL
  if (nrow(chunk) == 0) {
    return(NULL)
  }
  
  # Generate INSERT query for new records only
  paste(
    "INSERT INTO kits_data_sql (id_lab, sn, Full_Test_Name, Reagent_Name, Kit_Lot, Bead_Lot,
                               Num_Of_Adjustors, Adjustment_Method, Adjustor_Lot_1, Adjustor_Lot_2,
                               Adjustor_Concentration_1, Adjustor_Concentration_2, Times_Adjusted,
                               Curve_Parameter_1, Curve_Parameter_2, Curve_Parameter_3, Curve_Parameter_4,
                               Curve_Parameter_5, Curve_Parameter_6, Curve_Parameter_7, Curve_Parameter_8,
                               Dose_Parameter_1, Dose_Parameter_2, Dose_Parameter_3, Dose_Parameter_4,
                               Dose_Parameter_5, Dose_Parameter_6, Dose_Parameter_7, Dose_Parameter_8,
                               Kit_Expiration, Adjust_Frequency, Assay_Max, Assay_Min,
                               Default_Units, Reported_Units, Pre_Dilution_Factor, CPS_Cutoff, file_source) VALUES",
    paste(
      apply(chunk, 1, function(row) {
        paste0(
          "(",
          paste(sapply(row, function(x) {
            if (is.na(x)) "NULL" else shQuote(x, type = "sh")
          }), collapse = ", "),
          ")"
        )
      }),
      collapse = ", "
    )
  )
}

# Data upload observer
observeEvent(input$upload_kits_to_sql, {
  req(rv_kits$kits_data)
  
  tryCatch({
    withProgress(message = "Uploading Kits data to SQL...", value = 0, {
      dbExecute(db_pool, "SET FOREIGN_KEY_CHECKS = 0;")
      
      kits_data <- rv_kits$kits_data
      
      # Process in chunks
      chunk_size <- 1000
      total_rows <- nrow(kits_data)
      total_chunks <- ceiling(total_rows / chunk_size)
      
      new_records_count <- 0
      
      for (i in seq_len(total_chunks)) {
        chunk_start <- ((i - 1) * chunk_size + 1)
        chunk_end <- min(i * chunk_size, total_rows)
        chunk <- kits_data[chunk_start:chunk_end, ]
        
        # Generate and execute SQL query
        sql_query <- generate_insert_query(chunk)
        if (!is.null(sql_query)) {
          dbExecute(db_pool, sql_query)
          new_records_count <- new_records_count + nrow(chunk)
        }
        
        incProgress(1/total_chunks, 
                    detail = sprintf("Processed: %d of %d chunks", i, total_chunks))
      }
      
      dbExecute(db_pool, "SET FOREIGN_KEY_CHECKS = 1;")
    })
    
    showNotification(
      sprintf("Upload complete! Added %d new records.", new_records_count),
      type = "message"
    )
    
  }, error = function(e) {
    dbExecute(db_pool, "SET FOREIGN_KEY_CHECKS = 1;")
    showNotification(
      sprintf("Error uploading data: %s", conditionMessage(e)),
      type = "error"
    )
  })
})

# Close connection when app stops
onStop(function() {
  pool::poolClose(db_pool)
})







#______________________________________ APP to SQL : "type_of_reaction" __________________________________________________|


# Загрузка данных из SQL при старте приложения
observe({
  conn <- mysql_conn()
  type_of_reaction_data <- dbReadTable(conn, "type_of_reaction")
  dbDisconnect(conn)
  
  if (nrow(type_of_reaction_data) == 0) {
    type_of_reaction_data <- data.frame(
      Test = character(),
      Formula_Number = integer(),
      Type = character(),
      stringsAsFactors = FALSE
    )
  }
  
  type_of_reaction_global(type_of_reaction_data)
})



# Таблица для отображения
output$type_of_reaction_table <- renderDT({
  req(type_of_reaction_global())
  data <- type_of_reaction_global()
  
  if (nrow(data) == 0) {
    data <- data.frame(
      Test = "No Data",
      Formula_Number = NA,
      Type = "No Data"
    )
  }
  
  datatable(
    data,
    options = list(
      pageLength = 10,     # Количество строк на странице
      scrollX = TRUE,      # Горизонтальная прокрутка
      autoWidth = TRUE,    # Автоматическая ширина столбцов
      columnDefs = list(
        list(className = 'dt-center', targets = "_all")  # Центрирование текста для всех столбцов
      )
    ),
    rownames = FALSE,
    class = 'cell-border stripe hover'  # Включает базовые стили
  )
})

# Обновление selectInput на основе данных
observe({
  req(type_of_reaction_global())
  updateSelectInput(
    session,
    "type_reaction",
    choices = unique(type_of_reaction_global()$Test),
    selected = NULL
  )
})





output$reaction_type_display <- renderText({
  req(input$type_reaction)  # Убедимся, что есть выбранное значение
  req(type_of_reaction_global())  # Убедимся, что данные загружены
  
  # Фильтруем данные для выбранного теста
  selected_test <- type_of_reaction_global() %>%
    filter(Test == input$type_reaction)
  
  # Если тест найден, возвращаем его тип
  if (nrow(selected_test) > 0) {
    return(selected_test$Type[1])  # Возвращаем тип первой строки
  } else {
    return("No Type Available")  # Если теста нет, возвращаем заглушку
  }
})

#_________________________________________________________________________________________________________________|


# Observer for lot selection
observeEvent(input$load_sql_data, {
  print("results_data_sql: ")
  str(rv$results_data_sql)
  
  print("adjustments_data_sql: ")
  str(rv$adjustments_data_sql)
  
  print("kits_data_sql: ")
  str(rv$kits_data_sql)
}, ignoreInit = TRUE)

}