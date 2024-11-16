
# Constants
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