# Result: Data Page 1.1  

#_______________________________ ** Work with RESULTS DATA ** ________________________________________________________________________________________________________|   

# Constants
MAX_FILE_SIZE <- 20 * 1024^2  # 20MB
columns_to_load <- c(1, 2, 3, 4, 6, 7, 10, 11, 20, 21, 25, 26, 29, 33, 34, 35, 41, 54, 55, 59, 60, 61, 78)

# Initialize reactive values
data_cache <- reactiveVal(new.env())
results_data <- reactiveVal(NULL)

# Улучшенная функция валидации файла
validate_file <- function(file_path) {
  tryCatch({
    if (!file.exists(file_path)) {
      return(FALSE)
    }
    
    df <- openxlsx::read.xlsx(file_path, cols = columns_to_load, colNames = TRUE)
    
    # Удаляем строки с пропущенными значениями в критических колонках
    df <- df[!is.na(df$Date_And_Time_Resulted) & !is.na(df$Result), ]
    
    # Проверяем, остались ли данные после фильтрации
    if (nrow(df) == 0) {
      return(FALSE)
    }
    
    TRUE
  }, error = function(e) {
    FALSE
  })
}


output$dynamic_lab_select <- renderUI({
  req(results_data(), input$test_select, input$test_select != "")
  
  data <- results_data()
  data <- data %>% filter(Reagent_Name == input$test_select)
  lab_choices <- sort(unique(data$id_lab))
  
  selectInput(
    inputId = "lab_select",
    label = "Laboratory",
    choices = lab_choices,
    selected = NULL,
    multiple = TRUE,
    selectize = FALSE
  )
})

# Динамический вывод загрузки калибровочных файлов
output$dynamic_calib_upload <- renderUI({
  tagList(
    fileInput(
      "calib_files",
      span(
        "Upload Calibration Files",
        span(class = "text-muted d-block small", "Max size: 20MB")
      ),
      multiple = TRUE,
      accept = c(".xlsx"),
      buttonLabel = "Browse...",
      width = "100%"
    ),
    div(
      class = "d-flex gap-2 my-3",
      actionButton("reset_upload", "Reset", 
                   class = "btn-warning btn-sm"),
      downloadButton("export_results", "Export",
                     class = "btn-sm")
    ),
    textOutput("calib_load_status") %>%
      tagAppendAttributes(class = "text-info small mt-2"),
    verbatimTextOutput("validation_summary") %>%
      tagAppendAttributes(class = "small mt-3")
  )
})


# Reactive observers for input updates
observe({
  updateSelectizeInput(session, "test_select",
                       choices = if (!is.null(results_data())) {
                         unique(results_data()$Reagent_Name)
                       } else {
                         NULL
                       },
                       options = list(
                         placeholder = if (is.null(results_data())) 
                           'Загрузите файлы...' else 'Выберите тест...'
                       ))
})


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
                           'Загрузите файлы...' else 'Выберите Kit Lot...'
                       ))
})


# File upload handler with type conversion
observeEvent(input$results_files, {
  req(input$results_files)
  
  if (any(sapply(input$results_files$size, function(x) x > MAX_FILE_SIZE))) {
    output$load_status <- renderText("Ошибка: Один или несколько файлов превышают максимальный размер (20MB)")
    return()
  }
  
  withProgress(message = "Обработка файлов", value = 0, {
    output$load_status <- renderText("Начало обработки файлов...")
    
    tryCatch({
      valid_files <- input$results_files$datapath[sapply(input$results_files$datapath, validate_file)]
      
      if (length(valid_files) == 0) {
        stop("Нет валидных файлов для обработки")
      }
      
      total_files <- length(valid_files)
      all_data <- bind_rows(lapply(seq_along(valid_files), function(i) {
        file <- valid_files[i]
        
        incProgress(1/total_files, 
                    detail = sprintf("Обработка файла %d из %d", i, total_files))
        
        cache_key <- digest::digest(file)
        cache_env <- data_cache()
        
        if (exists(cache_key, envir = cache_env)) {
          return(get(cache_key, envir = cache_env))
        }
        
        df <- openxlsx::read.xlsx(file, cols = columns_to_load, colNames = TRUE)
        
        # Удаляем строки с пропущенными значениями в критических колонках
        df <- df[!is.na(df$Date_And_Time_Resulted) & !is.na(df$Result), ]
        
        if (nrow(df) > 0) {
          processed_df <- df %>% 
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
              # Обработка NA значений для character колонок
              across(where(is.character), ~if_else(is.na(.), "", as.character(.))),
              # Обработка NA значений для numeric колонок
              across(where(is.numeric), ~if_else(is.na(.), 0, as.numeric(.))),
              file_source = basename(file)
            )
          
          assign(cache_key, processed_df, envir = cache_env)
          return(processed_df)
        }
        
        return(NULL)
      }))
      
      if (is.null(all_data) || nrow(all_data) == 0) {
        stop("Не удалось загрузить данные из файлов")
      }
      
      results_data(all_data)
      
      output$load_status <- renderText(sprintf(
        "Загрузка успешно завершена! Обработано файлов: %d, загружено записей: %d",
        length(valid_files),
        nrow(all_data)
      ))
      
    }, error = function(e) {
      output$load_status <- renderText(paste("Ошибка загрузки:", e$message))
    })
  })
})



filtered_data <- reactive({
  req(results_data())
  data <- all_display_data()
  
  # Create filter conditions
  filters <- list()
  
  if (!is.null(input$test_select) && input$test_select != "") {
    filters$test <- quo(Reagent_Name == !!input$test_select)
  }
  
  if (!is.null(input$lab_select) && !("All" %in% input$lab_select)) {
    filters$lab <- quo(id_lab %in% !!input$lab_select)
  }
  
  if (!is.null(input$kit_lot_select) && !("All" %in% input$kit_lot_select)) {
    filters$kit <- quo(Kit_Lot %in% !!input$kit_lot_select)
  }
  
  # Apply filters efficiently
  if (length(filters) > 0) {
    data <- data %>% filter(!!!filters)
  }
  
  data %>% 
    arrange(date) %>%
    mutate(across(c(mean_result, sd_result, mean_cps, mean_df), ~round(., 2)))
})

# Improved data table output
output$data_table <- DT::renderDataTable({
  req(filtered_data())
  
  data_to_display <- filtered_data() %>%
    select(
      Date = date,
      Laboratory = id_lab,
      Test = Reagent_Name,
      Kit Lot = Kit_Lot,
      Test Count = count,
      Mean Result = mean_result,
      SD Result = sd_result,
      Mean CPS = mean_cps,
      Mean DF = mean_df
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

# Improved plot output with better performance
output$daily_tests_plot <- renderPlotly({
  data <- if (!is.null(input$test_select) && input$test_select != "") {
    filtered_data()
  } else {
    all_display_data()
  }
  req(nrow(data) > 0)
  
  print("Отрисовка графика...")
  
  # Оптимизированная подготовка данных для графика
  plot_data <- data %>%
    group_by(date, id_lab, Kit_Lot) %>%
    summarise(
      count = sum(count),
      .groups = 'drop'
    )
  
  p <- ggplot(plot_data, 
              aes(x = date, y = count, color = as.factor(Kit_Lot), 
                  linetype = id_lab, group = interaction(id_lab, Kit_Lot))) +
    geom_line() +
    geom_point(size = 2, alpha = 0.7) +
    labs(
      title = if (!is.null(input$test_select) && input$test_select != "") {
        paste('Количество тестов по дням -', input$test_select)
      } else {
        'Количество тестов по дням - Все тесты'
      },
      x = 'Дата',
      y = 'Количество тестов',
      color = 'Kit Lot',
      linetype = 'Лаборатория'
    ) +
    theme_minimal() +
    scale_x_date(
      date_breaks = "2 weeks",
      date_labels = "%d %b %Y",
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    ) +
    theme(
      panel.background = element_rect(fill = "#2C2C2C", color = NA),
      plot.background = element_rect(fill = "#2C2C2C", color = NA),
      panel.grid.major = element_line(color = "#1A1A1A"),
      panel.grid.minor = element_line(color = "#1A1A1A", size = 0.5),
      axis.text = element_text(color = "#F5F5F5"),
      axis.title = element_text(color = "#F5F5F5", face = "bold"),
      plot.title = element_text(color = "#F5F5F5", size = 14, face = "bold"),
      legend.background = element_rect(fill = "#2C2C2C", color = NA),
      legend.text = element_text(color = "#F5F5F5"),
      legend.title = element_text(color = "#F5F5F5", face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.position = "bottom",
      legend.box = "vertical"
    )
  
  # Конвертация в plotly с оптимизированными настройками
  ggplotly(p, tooltip = c("x", "y", "colour", "linetype")) %>%
    layout(
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        y = -0.2
      ),
      margin = list(
        l = 50,
        r = 50,
        b = 100,
        t = 50
      )
    ) %>%
    config(displayModeBar = FALSE)
})

# Отображение информации о данных
output$data_info <- renderText({
  req(results_data())
  sprintf(
    "Данные из %d лабораторий. Всего %d записей.",
    length(unique(results_data()$id_lab)),
    nrow(results_data())
  )
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




# UI outputs
output$calib_table1 <- renderDT({
  req(rv$calib_data)
  datatable(
    rv$calib_data$raw_data,
    extensions = c('Buttons', 'Scroller'),
    options = list(
      scrollX = TRUE,
      scrollY = '70vh',
      scroller = TRUE
    )
  )
})

output$calib_table2 <- renderDT({
  req(rv$calib_data)
  datatable(
    rv$calib_data$grouped_data,
    extensions = c('Buttons', 'Scroller'),
    options = list(
      scrollX = TRUE,
      scrollY = '70vh',
      scroller = TRUE
    )
  )
})



output$validation_plot <- renderPlotly({
  req(rv$calib_data)
  plot_data <- rv$calib_data$grouped_data
  
  # Используем стандартную палитру цветов если RColorBrewer недоступен
  n_colors <- length(unique(plot_data$Kit_Lot))
  custom_colors <- tryCatch({
    colorRampPalette(brewer.pal(min(8, n_colors), "Set3"))(n_colors)
  }, error = function(e) {
    # Fallback к базовым цветам если RColorBrewer недоступен
    palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
                 "#9467bd", "#8c564b", "#e377c2", "#7f7f7f")
    rep_len(palette, n_colors)
  })
  
  p <- plot_ly(plot_data, 
               x = ~Test_Name, 
               y = ~CV_Slope,
               type = 'scatter',
               mode = 'markers',
               marker = list(size = 10),
               color = ~Kit_Lot,
               colors = custom_colors,
               text = ~paste("Kit Lot:", Kit_Lot,
                             "<br>CV Slope:", sprintf("%.2f%%", CV_Slope),
                             "<br>Count:", Count)) %>%
    layout(
      title = list(
        text = "CV Slope по тестам и лотам",
        font = list(size = 16)
      ),
      xaxis = list(
        title = "Тест",
        tickangle = 45
      ),
      yaxis = list(
        title = "CV Slope (%)",
        zeroline = TRUE
      ),
      showlegend = TRUE,
      legend = list(x = 1.02, y = 0.9),
      margin = list(b = 100),  # Увеличиваем нижний отступ для наклонных меток
      hoverlabel = list(bgcolor = "white"),
      plot_bgcolor = '#f8f9fa',
      paper_bgcolor = '#f8f9fa'
    )
  
  p
})

output$validation_summary <- renderText({
  req(rv$calib_data)
  data <- rv$calib_data$raw_data
  
  sprintf(
    "Загружено записей: %d\nУникальных тестов: %d\nПериод данных: %s - %s",
    nrow(data),
    length(unique(data$Test_Name)),
    format(min(data$DateTimeOfAdjustment), "%Y-%m-%d"),
    format(max(data$DateTimeOfAdjustment), "%Y-%m-%d")
  )
})

output$validation_details <- renderText({
  req(rv$calib_data)
  data <- rv$calib_data$grouped_data
  
  # Создаем статистику по каждому тесту
  test_stats <- lapply(unique(data$Test_Name), function(test_name) {
    test_data <- data[data$Test_Name == test_name, ]
    cv_slope_mean <- mean(test_data$CV_Slope, na.rm = TRUE)
    
    # Проверяем, является ли среднее значение NA или NaN
    if(is.na(cv_slope_mean) || is.nan(cv_slope_mean)) {
      cv_slope_text <- "Нет данных"
    } else {
      cv_slope_text <- sprintf("%.2f%%", cv_slope_mean)
    }
    
    sprintf("- %s:\n  Количество записей: %d\n  Среднее CV Slope: %s",
            test_name,
            sum(test_data$Count),
            cv_slope_text)
  })
  
  paste("Статистика по тестам:\n\n",
        paste(test_stats, collapse = "\n\n"))
})

output$export_results <- downloadHandler(
  filename = function() {
    paste("calibration_results_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx", sep="")
  },
  content = function(file) {
    req(rv$calib_data)
    
    withProgress(message = 'Подготовка файла для экспорта', value = 0, {
      wb <- createWorkbook()
      addWorksheet(wb, "Сводные данные")
      writeData(wb, "Сводные данные", rv$calib_data$grouped_data)
      addWorksheet(wb, "Исходные данные")
      writeData(wb, "Исходные данные", rv$calib_data$raw_data)
      saveWorkbook(wb, file, overwrite = TRUE)
    })
  }
)

output$last_update_time <- renderText({
  req(rv$calib_data)
  format(rv$calib_data$last_update, "%Y-%m-%d %H:%M:%S")
})

#_______________________________ **END Work with Adjustment Data** ______________________________________________________________________________|   














