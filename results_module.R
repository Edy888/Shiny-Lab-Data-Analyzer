results_module <- function(input, output, session, rv_results) {
  # Константы
  RESULTS_FILE_SIZE_LIMIT <- 20 * 1024^2  # 20MB
  
  # Валидация данных
  validate_results_sheet <- function(sheet_data) {
    required_cols <- c("DateTimeOfResult", "Result", "Test_Name", "Lab_ID", "Kit_Lot")
    missing_cols <- setdiff(required_cols, names(sheet_data))
    if (length(missing_cols) > 0) {
      stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    }
  }
  
  # Преобразование в числовой формат
  safe_as_numeric <- function(x) {
    if (is.null(x)) return(NA_real_)
    as.numeric(as.character(x))
  }
  
  # Обработка файлов
  process_results_files <- function(files, rv) {
    if (is.null(files) || nrow(files) == 0) {
      stop("Файлы не выбраны.")
    }
    
    if (!all(tools::file_ext(files$name) == "xlsx")) {
      stop("Допускаются только файлы Excel (.xlsx)")
    }
    
    rv$processing <- TRUE
    on.exit(rv$processing <- FALSE)
    
    withProgress(message = "Обработка файлов...", value = 0, {
      all_data <- purrr::map_dfr(seq_along(files$datapath), function(i) {
        file_path <- files$datapath[i]
        file_name <- files$name[i]
        
        if (!file.exists(file_path)) {
          stop(sprintf("Файл не найден: %s", file_name))
        }
        
        incProgress(1 / length(files$datapath), detail = sprintf("Обработка %s", file_name))
        
        tryCatch({
          # Читаем только указанные столбцы
          data <- openxlsx::read.xlsx(file_path, sheet = 1, colNames = TRUE, cols = col_results)
          
          validate_results_sheet(data)
          
          data %>%
            dplyr::rename(
              id_lab = "id_lab",
              Reagent_Name = "Reagent_Name",
              Result = "Result",
              Date_And_Time_Resulted = "Date_And_Time_Resulted",
              Kit_Lot = "Kit_Lot"
            ) %>%
            dplyr::mutate(
              Date_And_Time_Resulted = lubridate::ymd_hms(as.character(Date_And_Time_Resulted)),
              Result = safe_as_numeric(Result),
              Kit_Lot = as.character(Kit_Lot),
              FileName = file_name
            )
        }, error = function(e) {
          stop(sprintf("Ошибка при обработке %s: %s", file_name, e$message))
        })
      })
      
      if (nrow(all_data) == 0) {
        stop("Загруженные файлы не содержат данных.")
      }
      
      rv$raw_data <- all_data
      rv$daily_tests <- all_data %>%
        dplyr::group_by(Date = as.Date(Date_And_Time_Resulted), Reagent_Name) %>%
        dplyr::summarise(Count = n(), .groups = "drop")
      
      rv$test_statistics <- all_data %>%
        dplyr::group_by(Reagent_Name, Kit_Lot, id_lab) %>%
        dplyr::summarise(
          Count = n(),
          Mean_Result = round(mean(Result, na.rm = TRUE), 2),
          SD_Result = round(sd(Result, na.rm = TRUE), 2),
          CV_Result = round((SD_Result / Mean_Result) * 100, 2),
          .groups = "drop"
        )
      
      showNotification("Файлы успешно обработаны!", type = "message")
    })
  }
  
  # Обработчик загрузки файлов
  observeEvent(input$results_files, {
    req(input$results_files)
    process_results_files(input$results_files, rv_results)
  })
  
  # Обработчик сброса данных
  observeEvent(input$reset_results_upload, {
    rv_results$raw_data <- NULL
    rv_results$daily_tests <- NULL
    rv_results$test_statistics <- NULL
    rv_results$processing <- FALSE
    shinyjs::reset("results_files")
    output$results_load_status <- renderText("")
    showNotification("Данные сброшены", type = "warning")
  })
  
  # Таблица данных
  output$data_table <- renderDT({
    req(rv_results$raw_data)
    datatable(rv_results$raw_data, extensions = c('Scroller'),
              options = list(scrollX = TRUE, scrollY = '70vh', scroller = TRUE))
  })
}

  
  # График ежедневных тестов
  output$daily_tests_plot <- renderPlotly({
    req(rv_results$daily_tests)
    daily_tests <- rv_results$daily_tests
    
    ggplot(daily_tests, aes(x = Date, y = Count, color = Reagent_Name)) +
      geom_line() +
      geom_point(size = 2) +
      labs(title = "Daily Test Counts", x = "Date", y = "Test Count", color = "Reagent Name") +
      theme_minimal() %>%
      ggplotly()
  })
  
  # Таблица статистики
  output$test_stats_table <- renderDT({
    req(rv_results$test_statistics)
    datatable(rv_results$test_statistics, extensions = c('Buttons', 'Scroller'),
              options = list(scrollX = TRUE, scrollY = '70vh', scroller = TRUE,
                             dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')))
  })
}
