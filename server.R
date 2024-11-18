# Result: Data Page 1.1  

server <- function(input, output, session) {

  
  MAX_FILE_SIZE <- 20 * 1024^2  # 20MB
  rv_results <- reactiveValues(
    proceed_data = NULL,           # отсортирован с правильными форматами и столбцами (из all_data): output$results_summary, filters (KITS,Labs, Test)
    all_display_data = NULL        # сводная статистика данных из proceed_data (output$daily_tests_plot)
  )
  
  # Initialize reactive values
  data_cache <- reactiveVal(new.env())


  
  
  

#_______________________________ ** Work with RESULTS DATA ** ________________________________________________________________________________________________________|   

  
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
          
          incProgress(1/total_files, 
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
                id_lab = as.factor(id_lab),
                sn = as.character(sn),
                Unique_Record_ID_Num = as.numeric(Unique_Record_ID_Num),
                Identifier = as.numeric(Identifier),
                Patient_ID_Num = as.character(Patient_ID_Num),
                Name = as.character(Name),
                Physician_Name = as.character(Physician_Name),
                Dilution_Factor = as.numeric(Dilution_Factor),
                Reagent_Name = as.character(Reagent_Name),
                Result = as.numeric(Result),
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
        
        # Save results
        rv_results$proceed_data <- all_data %>% arrange(Reagent_Name)
        
        
        # Update summary information
        output$results_summary <- renderText({
          req(rv_results$proceed_data)
          num_labs <- length(unique(rv_results$proceed_data$id_lab))
          num_tests <- nrow(rv_results$proceed_data)
          date_range <- range(rv_results$proceed_data$Date_And_Time_Resulted, na.rm = TRUE)
          date_range_text <- paste(
            format(date_range[1], "%d %b %Y"),
            "to",
            format(date_range[2], "%d %b %Y")
          )
          paste(
            "Данные из лабораторий: ", num_labs, "\n",
            "Количество тестов: ", num_tests, "\n",
            "Диапазон дат: ", date_range_text
          )
        })
        
        # Update load status
        invalid_files <- length(input$results_files$datapath) - length(valid_files)
        output$load_status <- renderText(sprintf(
          "Loading completed! Processed files: %d, records: %d. Skipped files: %d.",
          length(valid_files),
          nrow(all_data),
          invalid_files
        ))
        
      }, error = function(e) {
        output$load_status <- renderText(paste("Loading error:", e$message))
      })
    })
  })
  
#______________________________________________________________________________________|
  
                   
                   
                   
  
#______________________________________________________________________________________| 
  
  
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
  
                    
                   
                   
  
#__________________Create rv_results$all_display_data and filtered_data()__________|
  

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


    filtered_data <- reactive({
      req(rv_results$proceed_data)
      data <- rv_results$all_display_data
      
      # Используйте dplyr::filter с условными выражениями
      data %>%
        dplyr::filter(
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
        mutate(across(c(mean_result, sd_result, mean_cps, mean_df), ~round(., 2)))
    })

#__________________________________________________________________________________|    
 
                  
                 
                 
  
#_____________________ output$data_table (таблица со сводной статистикой из filtered_data())_________________|
    
    output$data_table <- DT::renderDT({
      req(filtered_data())
      
      # Преобразуем в data.frame перед передачей в datatable
      data_to_display <- as.data.frame(filtered_data()) %>%
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
      filtered_data()
    } else {
      rv_results$all_display_data
    }
    
    print("Rendering plot...")
    
    if (nrow(data) == 0) return(NULL)
    
    p <- ggplot(data, 
                aes(x = date, y = count, color = as.factor(Reagent_Name), 
                    linetype = id_lab, group = interaction(id_lab, Kit_Lot))) +
      geom_line() +
      geom_point() +
      labs(
        title = if (!is.null(input$test_select) && input$test_select != "") {
          paste('Daily Test Count -', input$test_select)
        } else {
          'Daily Test Count - All Tests'
        },
        x = 'Date',
        y = 'Test Count',
        color = 'Kit Lot'
      ) +
      theme_minimal() +
      scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b %Y") +
      scale_y_continuous(limits = c(0, NA)) +
      theme(
        panel.background = element_rect(fill = "#2C2C2C", color = NA),
        plot.background = element_rect(fill = "#2C2C2C", color = NA),
        panel.grid.major = element_line(color = "#1A1A1A"),
        panel.grid.minor = element_line(color = "#1A1A1A", size = 0.5),
        axis.text = element_text(color = "#F5F5F5"),
        axis.title = element_text(color = "#F5F5F5"),
        plot.title = element_text(color = "#F5F5F5", size = 14, face = "bold"),
        legend.background = element_rect(fill = "#2C2C2C", color = NA),
        legend.text = element_text(color = "#F5F5F5"),
        legend.title = element_text(color = "#F5F5F5"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5)
      )
    
    ggplotly(p)
  })
#__________________________________________________________________________________|   
  
                   
                   
                   
  
#_____________________________ STATISTICS TABLE ___________________________________|
  
  output$test_stats_table <- DT::renderDataTable({
    req(filtered_data())  # Убедитесь, что данные загружены и фильтрованы
    
    stats_data <- filtered_data() %>%
      group_by(Reagent_Name, Kit_Lot) %>%
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
    req(filtered_data())
    
    data_for_plot <- filtered_data() %>%
      group_by(Reagent_Name, Kit_Lot) %>%
      summarise(
        Mean_Result = mean(mean_result, na.rm = TRUE),
        SD_Result = sd(mean_result, na.rm = TRUE),
        Total_Tests = sum(count, na.rm = TRUE),
        .groups = "drop"
      )
    
    if (nrow(data_for_plot) == 0) return(NULL)  # Если данных нет
    
    p <- ggplot(data_for_plot, aes(x = Reagent_Name, y = Total_Tests, fill = Kit_Lot)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Test Statistics by Reagent and Kit Lot",
        x = "Reagent Name",
        y = "Total Tests"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p)
  })
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
#_______________________________ * END Work with RESULTS DATA * ________________________________________________________________________________________________________| 
#_________________________________________________________________________________________________________________________________________________________________________|




  
# *******************************  
# *  Adjustment: Data Page 1.2 **
# *******************************
  
  
  
  
# --------------------------------------** Work with ADJUSTMENTS DATA **------------------------------------------------------------------------------------------------| 
  adjustments_cols <- c(1:12, 21:34, 38:39)
  # Реактивные значения
  rv <- reactiveValues(
    calib_data = NULL    # Обработанные данные калибровки
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
    required_cols <- c("DateTimeOfAdjustment", "Test_Name", "Kit_Lot", "Slope", "Intercept")
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
                Test_Name = as.character(Test_Name),
                Kit_Lot = as.character(Kit_Lot),
                Adjustor_Lot_Low = as.character(Adjustor_Lot_Low),
                Adjustor_Lot_High = as.character(Adjustor_Lot_High),
                Error = as.logical(Error),
                FileName = file_name
              )
            
            assign(cache_key, transformed_data, envir = cache_env)
            transformed_data
            
          }, error = function(e) {
            stop(sprintf("Ошибка в файле %s: %s", file_name, e$message))
          })
        })
        
        # Обработка и агрегация данных
        all_data <- all_data %>%
          distinct(Test_Name, Kit_Lot, DateTimeOfAdjustment, Slope, Intercept, .keep_all = TRUE)
        
        grouped_data <- all_data %>%
          group_by(Test_Name, Kit_Lot) %>%
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
          arrange(Test_Name, Kit_Lot)
        
        # Сохранение результатов
        rv$calib_data <- list(
          raw_data = all_data,
          grouped_data = grouped_data,
          last_update = Sys.time()
        )
      })
      
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
    
    num_tests <- length(unique(data$Test_Name))
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
  

#__________________________OUTPUTS_________________________________________________________________|
  
  
  
#_______________________ Таблица: Summary by Kits ______________________________|
  
  
  output$calib_table2 <- DT::renderDT({
    req(filtered_calib_data())
    
    summary_data <- filtered_calib_data() %>%
      group_by(Test_Name, Kit_Lot) %>%
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
    req(rv$calib_data)  # Убедитесь, что данные существуют
    
    detailed_data <- rv$calib_data$raw_data  # Необработанные данные
    
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
      group_by(Test_Name, Kit_Lot) %>%
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
    
    p <- ggplot(validation_data, aes(x = Test_Name, y = CV_Slope, fill = Kit_Lot)) +
      geom_bar(stat = "identity", position = "dodge") +
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
    
    num_tests <- length(unique(data$Test_Name))
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
  
  
  
  
  # Добавить серверные функции для обновления фильтров
  
  observe({
    req(rv$calib_data)
    
    # Обновление списка тестов
    updateSelectInput(session, "calib_test_select",
                      choices = unique(rv$calib_data$raw_data$Test_Name),
                      selected = NULL)
  })
  
  observe({
    req(rv$calib_data)
    
    # Получаем выбранный тест
    selected_test <- input$calib_test_select
    data <- rv$calib_data$raw_data
    
    # Фильтруем данные по выбранному тесту
    if (!is.null(selected_test) && selected_test != "") {
      data <- data %>% filter(Test_Name == selected_test)
    }
    
    # Обновление списка Kit Lots
    updateSelectInput(session, "calib_kit_lot_select",
                      choices = c("All", unique(data$Kit_Lot)),
                      selected = "All")
  })
  
  
  # Добавить реактивные данные с фильтрацией
  
  filtered_calib_data <- reactive({
    req(rv$calib_data)
    
    data <- rv$calib_data$raw_data
    
    # Фильтрация по Test Name
    if (!is.null(input$calib_test_select) && input$calib_test_select != "") {
      data <- data %>% filter(Test_Name == input$calib_test_select)
    }
    
    # Фильтрация по Kit Lot
    if (!is.null(input$calib_kit_lot_select) && !("All" %in% input$calib_kit_lot_select)) {
      data <- data %>% filter(Kit_Lot %in% input$calib_kit_lot_select)
    }
    
    data
  })
  
#_______________________________ **END LOADING Adjustment Data** _____________________________________________|  



#----------------------------------------------------------------------------------------------------------------------------------------------|
#_______________________________ ** Start working with Kits Data** ____________________________________________________________________________|
#______________________________________________________________________________________________________________________________________________|

  # -------------------------- Константы и настройки --------------------------
  MAX_FILE_SIZE <- 20 * 1024^2  # Максимальный размер файла (20 MB)
  kits_cols <- c(
    1, 2, 8, 13, 14, 18, 19, 20, 21,
    33, 34, 52, 53, 54, 55, 56,
    62, 63, 64, 65, 66, 67, 68, 69,
    82, 83, 84, 85, 86, 87, 88, 89,
    102, 105, 108, 109,
    134, 137, 139, 140, 141, 142, 143, 144, 145, 146,
    164, 165, 168, 169, 170, 171, 172
  )
  
  # ----------------------- Реактивные значения для Kits -----------------------
  rv_kits <- reactiveValues(
    kits_data = NULL  # Данные для `Kits`
  )
  
  # --------------------- Функция обработки файлов Kits ---------------------
  validate_kits_data <- function(data) {
    # Обязательные столбцы для `Kits`
    required_cols <- c("Test_Name", "Kit_Lot", "Intercept", "CV_High_Limit", "CV_Low_Limit")
    missing_cols <- setdiff(required_cols, names(data))
    
    if (length(missing_cols) > 0) {
      stop(sprintf("Отсутствуют обязательные столбцы: %s", 
                   paste(missing_cols, collapse = ", ")))
    }
    
    # Дополнительная проверка форматов
    if (any(is.na(data$Test_Name))) {
      stop("Пропущены значения в столбце Test_Name")
    }
    if (any(is.na(data$Kit_Lot))) {
      stop("Пропущены значения в столбце Kit_Lot")
    }
    
    TRUE
  }
  
  process_kits_files <- function(files, rv) {
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
          
          incProgress(1 / length(files$datapath), 
                      detail = paste("Обрабатывается", file_name))
          
          # Проверка кэша
          cache_key <- paste0("kits_", digest::digest(file_path))
          cache_env <- data_cache()
          
          if (exists(cache_key, envir = cache_env)) {
            return(get(cache_key, envir = cache_env))
          }
          
          # Чтение и обработка данных
          tryCatch({
            wb <- openxlsx::loadWorkbook(file_path)
            data <- openxlsx::read.xlsx(wb, sheet = 1, cols = kits_cols)
            
            # Убедитесь, что обязательные столбцы присутствуют
            validate_kits_data(data)
            
            # Преобразование данных
            transformed_data <- data %>%
              mutate(
                Intercept = safe_as_numeric(Intercept),
                CV_High_Limit = safe_as_numeric(CV_High_Limit),
                CV_Low_Limit = safe_as_numeric(CV_Low_Limit),
                Conversion_Factor_1 = safe_as_numeric(Conversion_Factor_1),
                Conversion_Factor_2 = safe_as_numeric(Conversion_Factor_2),
                Test_Name = as.character(Test_Name),
                Kit_Lot = as.character(Kit_Lot),
                FileName = file_name
              ) %>%
              distinct()  # Удаление дубликатов
            
            assign(cache_key, transformed_data, envir = cache_env)
            transformed_data
            
          }, error = function(e) {
            stop(sprintf("Ошибка в файле %s: %s", file_name, e$message))
          })
        })
        
        # Сохранение результатов
        rv$kits_data <- list(
          raw_data = all_data,
          last_update = Sys.time()
        )
      })
      
      showNotification(sprintf("Успешно загружено %d файлов", length(files$datapath)), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Ошибка загрузки файлов:", e$message), type = "error", duration = NULL)
    })
  }
  
  
  # ----------------------- Наблюдатели для Kits -----------------------
  observeEvent(input$kits_files, {
    req(input$kits_files)
    process_kits_files(input$kits_files, rv_kits)
  })
  
  observeEvent(input$reset_kits_upload, {
    rv_kits$kits_data <- NULL
    output$kits_load_status <- renderText("")
    output$kits_summary <- renderText("")
    shinyjs::reset("kits_files")
  })
  
  # ----------------------- Таблицы для Kits -----------------------
  output$kits_table2 <- DT::renderDT({
    req(rv_kits$kits_data)
    
    summary_data <- rv_kits$kits_data$raw_data %>%
      group_by(Test_Name, Kit_Lot) %>%
      summarise(
        Count = n(),
        Max_CV_High = round(max(CV_High_Limit, na.rm = TRUE), 2),
        Min_CV_Low = round(min(CV_Low_Limit, na.rm = TRUE), 2),
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
  
  output$kits_table1 <- DT::renderDT({
    req(rv_kits$kits_data)
    
    detailed_data <- rv_kits$kits_data$raw_data
    
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
  
  # ----------------------- Графики для Kits -----------------------
  output$kits_validation_plot <- renderPlotly({
    req(rv_kits$kits_data)
    
    validation_data <- rv_kits$kits_data$raw_data %>%
      group_by(Test_Name, Kit_Lot) %>%
      summarise(
        Mean_Intercept = round(mean(Intercept, na.rm = TRUE), 4),
        SD_Intercept = round(sd(Intercept, na.rm = TRUE), 4),
        CV_Intercept = round((SD_Intercept / Mean_Intercept) * 100, 2),
        .groups = 'drop'
      )
    
    if (nrow(validation_data) == 0) return(NULL)
    
    p <- ggplot(validation_data, aes(x = Test_Name, y = CV_Intercept, fill = Kit_Lot)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Validation of Intercept Coefficient",
        x = "Test Name",
        y = "Coefficient of Variation (%)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$kits_slope_intercept_plot <- renderPlotly({
    req(rv_kits$kits_data)
    
    data <- rv_kits$kits_data$raw_data
    
    p <- ggplot(data, aes(x = CV_High_Limit, y = CV_Low_Limit, color = Kit_Lot)) +
      geom_point() +
      labs(
        title = "High vs Low CV Limits",
        x = "High CV Limit",
        y = "Low CV Limit"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ----------------------- Дополнительные детали -----------------------
  output$kits_validation_details <- renderText({
    req(rv_kits$kits_data)
    
    data <- rv_kits$kits_data$raw_data
    
    num_tests <- length(unique(data$Test_Name))
    num_kits <- length(unique(data$Kit_Lot))
    num_records <- nrow(data)
    num_missing <- sum(is.na(data))
    
    paste(
      "Детали проверки данных Kits:",
      sprintf("Количество тестов: %d", num_tests),
      sprintf("Количество наборов: %d", num_kits),
      sprintf("Количество записей: %d", num_records),
      sprintf("Количество пропущенных значений: %d", num_missing),
      sep = "\n"
    )
  })
  
 
  
#_________________________________________________________________________________________________________________________________|
#_________________________________________________________________________________________________________________________________|


  
  
  
  
  
  
  
  
  
  
  
  
#---------------------------------------------------------------------------------------------------------------------------------------------------------------|
#_______________________________ ** START 4PL Modeling ** ______________________________________________________________________________________________________|  
#_______________________________________________________________________________________________________________________________________________________________|


# 4PL Model
model_mcd_cps <- function(conc, P1, P2, P3, P4) {
  P2 + (P1 - P2) / (1 + (conc / P3)^P4)
}

inverse_model_cps <- function(cps, P1, P2, P3, P4) {
  P3 * ((P1 - P2) / (cps - P2) - 1)^(1 / P4)
}

slope_value <- reactive({
  req(input$adg_high_sh, input$adg_low_sh, input$adg_high_lab, input$adg_low_lab)
  (input$adg_high_sh - input$adg_low_sh) / (input$adg_high_lab - input$adg_low_lab)
})

intercept_value <- reactive({
  req(input$adg_low_sh, input$adg_low_lab)
  input$adg_low_sh - (slope_value() * input$adg_low_lab)
})


output$mcd_plot <- renderPlotly({
  req(input$P1, input$P2, input$P3, input$P4, input$conc_min, input$conc_max)
  
  cal_conc <- calibrator_concentrations()
  
  # Генерация концентраций
  conc_range <- if (input$x_scale == "log") {
    10^(seq(log10(input$conc_min), log10(input$conc_max), length.out = 1000))
  } else {
    seq(from = input$conc_min, to = input$conc_max, length.out = 1000)
  }
  
  # Расчет значений MCD CPS и Lab CPS
  mcd_cps <- model_mcd_cps(conc_range, input$P1, input$P2, input$P3, input$P4)
  lab_cps <- (mcd_cps - intercept_value()) / slope_value()
  
  # Создание данных для графика
  plot_data <- data.frame(
    Concentration = conc_range,
    MCD_CPS = mcd_cps,
    Lab_CPS = lab_cps
  )
  
  # Создание графика с использованием plotly
  p <- plot_ly() %>%
    # Основные кривые
    add_trace(data = plot_data, x = ~Concentration, y = ~MCD_CPS, 
              type = 'scatter', mode = 'lines', name = 'MCD CPS', 
              line = list(color = '#009999')) %>%
    add_trace(data = plot_data, x = ~Concentration, y = ~Lab_CPS, 
              type = 'scatter', mode = 'lines', name = 'Lab CPS', 
              line = list(color = '#FF8C00')) %>%
    
    # Калибраторы MCD
    add_trace(x = c(cal_conc$mcd$low$conc, cal_conc$mcd$high$conc),
              y = c(cal_conc$mcd$low$cps, cal_conc$mcd$high$cps),
              type = 'scatter', mode = 'lines+markers',
              name = 'MCD Calibrators',
              line = list(color = '#009999', dash = 'dot'),
              marker = list(color = '#009999', size = 10)) %>%
    
    # Калибраторы Lab
    add_trace(x = c(cal_conc$lab$low$conc, cal_conc$lab$high$conc),
              y = c(cal_conc$lab$low$cps, cal_conc$lab$high$cps),
              type = 'scatter', mode = 'lines+markers',
              name = 'Lab Calibrators',
              line = list(color = '#FF8C00', dash = 'dot'),
              marker = list(color = '#FF8C00', size = 10))
  
  # Настройка осей
  plotly::layout(p,
                 xaxis = list(
                   title = "Concentration", 
                   type = if (input$x_scale == "log") "log" else "linear"
                 ),
                 yaxis = list(title = "CPS"),
                 showlegend = TRUE
  )
})






# Calculators Outputs
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
#_______________________________ ** END 4PL Modeling ** ______________________________________________________________________________________________________|




}