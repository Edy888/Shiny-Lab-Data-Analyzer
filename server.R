# Result: Data Page 1.1  

server <- function(input, output, session) {
  rv_results <- reactiveValues(
    raw_data = NULL,
    daily_tests = NULL,
    test_statistics = NULL
  )

#_______________________________ ** Work with RESULTS DATA ** ________________________________________________________________________________________________________|   

  MAX_FILE_SIZE <- 20 * 1024^2  # 20MB
  columns_to_load <- c(1, 2, 3, 4, 6, 7, 10, 11, 20, 21, 25, 26, 29, 33, 34, 35, 41, 54, 55, 59, 60, 61, 78)
  # Initialize reactive values
  data_cache <- reactiveVal(new.env())
  results_data <- reactiveVal(NULL)
  
  # File validation function
  validate_file <- function(file_path) {
    tryCatch({
      if (!file.exists(file_path)) return(FALSE)
      if (!grepl("\\.xlsx$", file_path)) return(FALSE)
      
      df <- openxlsx::read.xlsx(file_path, rows = 1)
      if (ncol(df) < max(columns_to_load)) return(FALSE)
      
      TRUE
    }, error = function(e) FALSE)
  }
  
  
  
  
  
  
  
  
  # Обновление списка тестов при загрузке данных
  observe({
    updateSelectizeInput(session, "test_select",
                         choices = if (!is.null(results_data())) {
                           unique(results_data()$Reagent_Name)
                         } else {
                           NULL
                         },
                         options = list(
                           placeholder = if (is.null(results_data())) 
                             'Upload files first...' else 'Select test...'
                         ))
  })
  
  # Обновление списка лабораторий при изменении теста
  observe({
    updateSelectizeInput(session, "lab_select",
                         choices = if (!is.null(results_data())) {
                           c("All", sort(unique(results_data()$id_lab)))
                         } else {
                           c("All")
                         },
                         selected = "All",
                         options = list(
                           placeholder = if (is.null(results_data())) 
                             'Upload files first...' else 'Select laboratory...'
                         ))
  })
  
  # Обновление списка Kit Lots при изменении теста или лаборатории
  observe({
    updateSelectizeInput(session, "kit_lot_select",
                         choices = if (!is.null(results_data())) {
                           data <- results_data()
                           if (!is.null(input$test_select) && input$test_select != "") {
                             data <- data %>% filter(Reagent_Name == input$test_select)
                           }
                           if (!is.null(input$lab_select) && !("All" %in% input$lab_select)) {
                             data <- data %>% filter(id_lab %in% input$lab_select)
                           }
                           c("All", sort(unique(data$Kit_Lot)))
                         } else {
                           c("All")
                         },
                         selected = "All",
                         options = list(
                           placeholder = if (is.null(results_data())) 
                             'Upload files first...' else 'Select Kit Lot...'
                         ))
  })
  
  # File upload handler
  observeEvent(input$results_files, {
    req(input$results_files)
    
    
    
    
    
    
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
                id_lab = as.character(id_lab),
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
        results_data(all_data)
        
        output$load_status <- renderText(sprintf(
          "Loading completed successfully! Processed files: %d, records: %d",
          length(valid_files),
          nrow(all_data)
        ))
        
      }, error = function(e) {
        output$load_status <- renderText(paste("Loading error:", e$message))
      })
    })
  })
  
  # Data info output
  output$data_info <- renderText({
    req(results_data())
    paste("Data from", length(unique(results_data()$id_lab)), 
          "laboratories. Total", nrow(results_data()), "records.")
  })
  
  
  
  
  
  all_display_data <- reactive({
    req(results_data())
    data <- results_data() %>%
      group_by(date = as.Date(Date_And_Time_Resulted), id_lab, Kit_Lot, Reagent_Name) %>%
      summarise(
        count = n(),
        mean_result = mean(Result, na.rm = TRUE),
        sd_result = sd(Result, na.rm = TRUE),
        mean_cps = mean(CPS, na.rm = TRUE),
        mean_df = mean(Dilution_Factor, na.rm = TRUE),
        .groups = 'drop'
      )
    data
  })
  
  filtered_data <- reactive({
    req(results_data())
    data <- all_display_data()
    
    # Apply all filters
    if (!is.null(input$test_select) && input$test_select != "") {
      data <- data %>% filter(Reagent_Name == input$test_select)
    }
    
    if (!is.null(input$lab_select) && !("All" %in% input$lab_select)) {
      data <- data %>% filter(id_lab %in% input$lab_select)
    }
    
    if (!is.null(input$kit_lot_select) && !("All" %in% input$kit_lot_select)) {
      data <- data %>% filter(Kit_Lot %in% input$kit_lot_select)
    }
    
    # Sort and format
    data %>% 
      arrange(date) %>%
      mutate(across(c(mean_result, sd_result, mean_cps, mean_df), ~round(., 2)))
  })
  
  
  # Data table output
  output$data_table <- DT::renderDataTable({
    req(filtered_data())
    print("Rendering data table...")
    
    data_to_display <- filtered_data() %>%
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
        pageLength = 10,
        scrollX = TRUE,
        order = list(list(0, 'desc')),
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      class = 'cell-border stripe',
      extensions = 'Buttons'
    ) %>%
      formatDate('Date', method = 'toLocaleDateString') %>%
      formatRound(
        columns = c('Mean Result', 'SD Result', 'Mean CPS', 'Mean DF'),
        digits = 2
      )
  })
  
  # Daily tests plot
  output$daily_tests_plot <- renderPlotly({
    data <- if (!is.null(input$test_select) && input$test_select != "") {
      filtered_data()
    } else {
      all_display_data()
    }
    
    print("Rendering plot...")
    
    if (nrow(data) == 0) return(NULL)
    
    p <- ggplot(data, 
                aes(x = date, y = count, color = as.factor(Kit_Lot), 
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
  
  
  # STATISTICS TABLE _ WORK!!!
  
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
  
  
  # STATISTICS PLOT _ WORK!!!
  
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
#_______________________________ ** END Work with RESULTS DATA ** ________________________________________________________________________________________________________| 







# Adjustment: Data Page 1.2

# --------------------------------------* Work with ADJUSTMENTS DATA *------------------------------------------------------------------------------------------------| 
# Constants
MAX_FILE_SIZE <- 20 * 1024^2  # 20MB
adjustments_cols <- c(1:12, 21:34, 38:39)

# Reactive values for storing data and cache
# rv$calib_data: Хранит обработанные данные, включая исходные и агрегированные данные.
# rv$processing: Указывает, идет ли в данный момент процесс обработки данных.

rv <- reactiveValues(
  calib_data = NULL,
  processing = FALSE
)

# Validate sheet data function
validate_sheet_data <- function(sheet_data) {
  required_cols <- c("DateTimeOfAdjustment", "Slope", "Intercept", "Test_Name", "Kit_Lot")
  missing_cols <- setdiff(required_cols, names(sheet_data))
  
  
  # Если хотя бы один столбец отсутствует, генерирует ошибку с указанием недостающих столбцов.
  if (length(missing_cols) > 0) {
    stop(paste("Отсутствуют обязательные столбцы:", paste(missing_cols, collapse = ", ")))
  }
}

# Helper function to safely convert to numeric
# Безопасное преобразование в числовой формат
# - Преобразует значения в числовой формат, безопасно обрабатывая пустые или некорректные значения, возвращая NA для таких случаев.
safe_as_numeric <- function(x) {
  if (is.null(x)) return(NA_real_)
  as.numeric(as.character(x))
}


# --------------------------------------* PROCESS FILES FUNCTION / ФУНКЦИЯ ОБРАБОТКИ ФАЙЛОВ *------------------------------------------------|  
#  
#   Этапы обработки:
#   1. Базовая проверка
#     1.1 Проверяет, выбраны ли файлы.
#     1.2 Убедится, что их размер не превышает MAX_FILE_SIZE.
#     1.3 Удостоверяется, что файлы имеют расширение 
#   2. Чтение и преобразование данных: sheet_data <-                                                                                         |
#     2.1 Загружает данные из первого листа файла Excel.                                                                                                            
#     2.2 Сохраняет только нужные столбцы (adjustments_cols).                                                                                                       
#     2.3 Преобразует данные: даты  и числовые значения (Slope, Intercept) обрабатываются с использованием функций ymd hms и safe_as_numeric. |
#   3. Проверка данных
#     3.1 Удостоверяется, что файл содержит все обязательные столбцы: validate_sheet_data(sheet_data)
#   4. Фильтрация и анализ данных
#     4.1 Удаляет дубликаты строк по ключевым полям : all_data <- all_data %>% distinct(...)  # Удаляет дубликаты
#     4.2 Агрегирует данные, вычисляя: grouped_data <- all_data %>% group_by(...) 
#        4.2.1 Количество записей (Count).
#        4.2.2 Средние и стандартные отклонения (Mean_Slope, SD_Slope и др.).
#        4.2.3 Коэффициенты вариации (CV_Slope, CV_Intercept).
#   5. Сохранение данных: 
#     5.1 Сохраняет сырые данные без дубликатов и агрегированные данные в 
#   6. Уведомление об успехе или ошибке:
#     6.1 Отправляет уведомления в интерфейс Shiny о статусе обработки файлов: 
#   7. Обработчик загрузки файлов:
#     7.1 Реагирует на загрузку новых файлов и вызывает process_files для их обработки: 
#   8. Обработчик сброса данных:
#     8.1 Сбрасывает все данные  и восстанавливает интерфейс загрузки в исходное состояние: 
#   9. Пример интерфейсной логики
#     9.1 Вывод статуса загрузки: 
#        9.1.2 Отображает текущий статус загрузки файлов (например, успешное завершение или ошибки): 
#     9.2 Уведомления: 
#        9.2.2 Выводит всплывающие сообщения для информирования пользователя: 
# 
# ___________________________________________________________________________________________________________________________________________|


process_files <- function(files) {
  if (rv$processing) return()
  rv$processing <- TRUE
  on.exit(rv$processing <- FALSE)
  
  output$calib_load_status <- renderText("Загрузка файлов калибровок...")
  
  tryCatch({
    # | 1. Базовая проверка
    # |    1.1 Проверяет, выбраны ли файлы.
    if (is.null(files) || nrow(files) == 0) {
      stop("Файлы не выбраны")
    }
    
    # |    1.2 Убедится, что их размер не превышает MAX_FILE_SIZE.
    if (any(files$size > MAX_FILE_SIZE)) {
      stop("Файл слишком большой (максимум 20MB)")
    }
    # |    1.3 Удостоверяется, что файлы имеют расширение .xlsx.
    if (!all(tools::file_ext(files$name) == "xlsx")) {
      stop("Допускаются только файлы Excel (.xlsx)")
    }
    
    # Process files with progress indicator
    withProgress(message = 'Загрузка и обработка файлов', value = 0, {
      all_data <- map_df(seq_along(files$datapath), function(i) {
        file_path <- files$datapath[i]
        file_name <- files$name[i]
        
        if (!file.exists(file_path)) {
          stop(sprintf("Файл не найден: %s", file_name))
        }
        
        incProgress(1/length(files$datapath), 
                    detail = paste("Обработка", file_name))
        
        # |  2. Чтение и преобразование данных:
        sheet_data <- tryCatch({
          
          # |    2.1 Загружает данные из первого листа файла Excel.
          wb <- openxlsx::loadWorkbook(file_path)
          data <- openxlsx::read.xlsx(wb, sheet = 1, colNames = TRUE)
          
          
          # |    2.2 Сохраняет только нужные столбцы (adjustments_cols).
          cols_to_keep <- names(data)[as.numeric(adjustments_cols[adjustments_cols <= ncol(data)])]
          data <- data[, cols_to_keep, drop = FALSE]
          
          data
        }, error = function(e) {                                                # Обработка ошибок при чтении файла
          stop(sprintf("Ошибка чтения файла %s: %s", file_name, e$message))     # Функция stop: Прерывает выполнение текущего процесса и выдает сообщение об ошибке.
        })
        # sprintf: Форматирует строку, добавляя имя файла и сообщение об ошибке (e$message), чтобы пользователь понимал, какой файл вызвал проблему и почему. 
        if (is.null(sheet_data) || nrow(sheet_data) == 0) {
          stop(sprintf("Файл %s пуст или содержит некорректные данные", file_name))
        }
        
        # |  3. Проверка данных
        # |    3.1 Удостоверяется, что файл содержит все обязательные столбцы: validate_sheet_data(sheet_data)  
        validate_sheet_data(sheet_data)
        
        # |    2.3 Преобразует данные: даты (DateTimeOfAdjustment) и числовые значения (Slope, Intercept) обрабатываются с использованием функций ymd_hms и safe_as_numeric.
        transformed_data <- sheet_data  %>%
          mutate(
            DateTimeOfAdjustment = ymd_hms(as.character(DateTimeOfAdjustment)),
            Slope = safe_as_numeric(Slope),
            Intercept = safe_as_numeric(Intercept),
            CV_Counts_Low = safe_as_numeric(CV_Counts_Low),
            CV_Counts_High = safe_as_numeric(CV_Counts_High),
            CPS_Low_1 = if("CPS_Low_1" %in% names(.)) safe_as_numeric(CPS_Low_1) else NA_real_,
            CPS_Low_2 = if("CPS_Low_2" %in% names(.)) safe_as_numeric(CPS_Low_2) else NA_real_,
            CPS_Low_3 = if("CPS_Low_3" %in% names(.)) safe_as_numeric(CPS_Low_3) else NA_real_,
            CPS_Low_4 = if("CPS_Low_4" %in% names(.)) safe_as_numeric(CPS_Low_4) else NA_real_,
            CPS_High_1 = if("CPS_High_1" %in% names(.)) safe_as_numeric(CPS_High_1) else NA_real_,
            CPS_High_2 = if("CPS_High_2" %in% names(.)) safe_as_numeric(CPS_High_2) else NA_real_,
            CPS_High_3 = if("CPS_High_3" %in% names(.)) safe_as_numeric(CPS_High_3) else NA_real_,
            CPS_High_4 = if("CPS_High_4" %in% names(.)) safe_as_numeric(CPS_High_4) else NA_real_,
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
        
        # Проверка числовых данных
        numeric_cols <- c("Slope", "Intercept", "Mean_CPS_Low", "Mean_CPS_High")
        if (any(sapply(transformed_data[numeric_cols], function(x) all(is.na(x))))) {
          stop(sprintf("Ошибка в файле %s: отсутствуют числовые данные в важных столбцах", file_name))
        }
        
        transformed_data
      })
      
      if (is.null(all_data) || nrow(all_data) == 0) {
        stop("Не удалось получить данные из файлов")
      }
      
      # |    4.1 Удаляет дубликаты строк по ключевым полям (Test_Name, Kit_Lot, DateTimeOfAdjustment):
      all_data <- all_data %>%
        distinct(Test_Name, Kit_Lot, DateTimeOfAdjustment, Slope, Intercept, .keep_all = TRUE)  # Удаляем дубликаты в сырых данных
      
      # |    4.2 Агрегирует данные, вычисляя: grouped_data <- all_data %>% group_by(...) %>% summarise(...)
      # |       4.2.1 Количество записей (Count).
      # |       4.2.2 Средние и стандартные отклонения (Mean_Slope, SD_Slope и др.).
      # |       4.2.3 Коэффициенты вариации (CV_Slope, CV_Intercept).
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
      
      # |  5. Сохранение данных: 
      # |    5.1 Сохраняет сырые данные без дубликатов и агрегированные данные в rv$calib_data: rv$calib_data <- list(...)
      # |  6. Уведомление об успехе или ошибке:
      rv$calib_data <- list(
        raw_data = all_data,  # Теперь raw_data будет без дубликатов
        grouped_data = grouped_data,
        last_update = Sys.time()
      )
      
      # |  6. Уведомление об успехе или ошибке:
      showNotification(
        sprintf("Успешно загружено %d файлов", length(files$datapath)),
        type = "message"
      )
      
      output$calib_load_status <- renderText(
        sprintf("Успешно загружено %d файлов", length(files$datapath))
      )
    })
    
  }, error = function(e) {
    showNotification(
      paste("Ошибка загрузки файлов:", e$message),
      type = "error",
      duration = NULL
    )
    output$calib_load_status <- renderText(paste("Ошибка загрузки файлов:", e$message))
  })
}


# |  7. Обработчик загрузки файлов:
# |    7.1 Реагирует на загрузку новых файлов и вызывает process_files для их обработки: observeEvent(input$calib_files, { ... })  

# File upload handler
observeEvent(input$calib_files, {
  req(input$calib_files)
  process_files(input$calib_files)
})

# |  8. Обработчик сброса данных:
# |    8.1 Сбрасывает все данные (rv$calib_data) и восстанавливает интерфейс загрузки в исходное состояние: observeEvent(input$reset_upload, { ... })
# |  9. Пример интерфейсной логики
# |    9.1 Вывод статуса загрузки: 
# |       9.1.2 Отображает текущий статус загрузки файлов (например, успешное завершение или ошибки): output$calib_load_status <- renderText(...)
# |    9.2 Уведомления: 
# |       9.2.2 Выводит всплывающие сообщения для информирования пользователя: showNotification(...)  
observeEvent(input$reset_upload, {
  rv$calib_data <- NULL
  rv$processing <- FALSE
  shinyjs::reset("calib_files")
  output$calib_load_status <- renderText("")
  showNotification("Данные сброшены", type = "warning")
})

#_______________________________ **END LOADING Adjustment Data** _____________________________________________|  















#_______________________________ ** START 4PL Modeling ** ______________________________________________________________________________________________________|  



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