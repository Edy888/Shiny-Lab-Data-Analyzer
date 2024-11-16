library(shiny)
options(shiny.maxRequestSize = 100 * 1024^2)


ui <- page_navbar(
  theme = my_theme,
  
  header = tagList(
    div(
      style = "padding: 0 0 0 35px; margin-top: 30px;",
      span(
        "Analysis v 1.0",
        style = "color: #FF8C00; font-size: 18px;"
      )
    ),
    tags$style(HTML("
      .form-control, .control-label { 
        font-size: 0.85rem !important;
      }
      .small-text .form-control,
      .small-text .control-label {
        font-size: 0.85rem !important;
      }
      .card .selectize-dropdown {
        position: fixed !important;
        top: auto !important;
        left: auto !important;
        z-index: 9999 !important;
      }
      .card .selectize-control {
        position: relative !important;
      }
    "))
  ),
  
  # Data Upload Panel
  nav_panel(
    title = "Data Upload",
    page_sidebar(
      sidebar = sidebar(
        width = 470,
        
        # Data Import Card
        card(
          card_header(
            "Data Import", 
            class = "bg-primary text-white fw-bold"
          ),
          style = "background-color: #808080;",
          
          accordion(
            accordion_panel(
              "Test Results",
              fileInput(
                "results_files",
                span(
                  "Upload Test Results Files",
                  span(class = "text-muted d-block small", "Max size: 20MB")
                ),
                multiple = TRUE,
                accept = c(".xlsx"),
                buttonLabel = "Browse...",
                width = "100%"
              ),
              div(
                class = "d-flex gap-2 my-3",
                actionButton("reset_results_upload", "Reset", 
                             class = "btn-warning btn-sm"),
                downloadButton("export_results_data", "Export",
                               class = "btn-sm")
              ),
              textOutput("results_load_status") %>%
                tagAppendAttributes(class = "text-info small mt-2"),
              verbatimTextOutput("results_summary") %>%
                tagAppendAttributes(class = "small mt-3")
            ),
            
            accordion_panel(
              "Test Selection",
              uiOutput("dynamic_test_select"),
              uiOutput("dynamic_lab_select"),
              uiOutput("dynamic_kitlot_select")
            ),
            
            accordion_panel(
              "Calibration Data",
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
            ),
            
            accordion_panel(
              "Kits Data",
              fileInput(
                "kits_files",
                span("Upload Kits Files", span(class = "text-muted", "(xlsx)")),
                multiple = TRUE,
                accept = c(".xlsx"),
                buttonLabel = "Browse..."
              )
            )
          )
        )
      ),
      
      # Main content
      layout_column_wrap(
        width = 1,
        
        # Test Results Card
        card(
          full_screen = TRUE,
          card_header(
            "Test Results",
            class = "bg-primary text-white"
          ),
          navset_card_tab(
            nav_panel(
              "Data Table",
              DTOutput("data_table") %>%
                withSpinner(type = 4, color = "#0d6efd")
            ),
            nav_panel(
              "Daily Tests Plot",
              plotlyOutput("daily_tests_plot") %>%
                withSpinner(type = 4, color = "#0d6efd")
            ),
            nav_panel(
              "Statistics",
              layout_column_wrap(
                width = 1,
                card(
                  card_header("Statistics Plot", class = "bg-primary text-white"),
                  card_body(
                    plotlyOutput("test_stats_plot", height = "500px") %>%
                      withSpinner(type = 4, color = "#0d6efd")
                  )
                ),
                card(
                  card_header("Statistics Table", class = "bg-primary text-white"),
                  card_body(
                    DTOutput("test_stats_table") %>%
                      withSpinner(type = 4, color = "#0d6efd")
                  )
                )
              )
            )
          )
        ),
        
        # Calibration Data Card
        card(
          full_screen = TRUE,
          card_header(
            "Calibration Data",
            class = "bg-primary text-white"
          ),
          navset_card_tab(
            nav_panel(
              "Summary by Kits",
              DTOutput("calib_table2") %>%
                withSpinner(type = 4, color = "#0d6efd")
            ),
            nav_panel(
              "Detailed Data",
              DTOutput("calib_table1") %>%
                withSpinner(type = 4, color = "#0d6efd")
            ),
            nav_panel(
              "Validation",
              layout_column_wrap(
                width = 1,
                plotlyOutput("validation_plot", height = "600px") %>%
                  withSpinner(type = 4, color = "#0d6efd"),
                hr(),
                verbatimTextOutput("validation_details") %>%
                  tagAppendAttributes(class = "small")
              )
            )
          )
        ),
        
        # Kits Information Card
        card(
          full_screen = TRUE,
          card_header(
            "Kits Information",
            class = "bg-primary text-white"
          ),
          navset_card_tab(
            nav_panel(
              "Basic Info",
              DTOutput("kits_info_table") %>%
                withSpinner(type = 4, color = "#0d6efd")
            ),
            nav_panel(
              "Usage Analysis",
              layout_column_wrap(
                width = 1,
                plotlyOutput("kits_usage_plot") %>%
                  withSpinner(type = 4, color = "#0d6efd"),
                DTOutput("kits_usage_table") %>%
                  withSpinner(type = 4, color = "#0d6efd")
              )
            ),
            nav_panel(
              "Performance Metrics",
              layout_column_wrap(
                width = 1,
                plotlyOutput("kits_performance_plot") %>%
                  withSpinner(type = 4, color = "#0d6efd"),
                DTOutput("kits_performance_table") %>%
                  withSpinner(type = 4, color = "#0d6efd")
              )
            )
          )
        )
      )
    )
  ),
  
  # Master Curve Panel
  nav_panel(
    title = "Master Curve",
    page_sidebar(
      sidebar = sidebar(
        card(
          card_header("Parameters", class = "bg-primary text-white"),
          div(
            style = "padding: 1rem;",
            numericInput("P1", "P1:", value = 3.404367e+19),
            numericInput("P2", "P2:", value = 61440),
            numericInput("P3", "P3:", value = 1.280082e+14),
            numericInput("P4", "P4:", value = -0.9975617),
            hr(),
            numericInput("conc_min", "Min Conc:", value = 0.1, min = 0.001),
            numericInput("conc_max", "Max Conc:", value = 50000, min = 1),
            hr(),
            numericInput("adg_low_sh", "Adjustor_Low_MCD, CPS", value = 711703),
            numericInput("adg_high_sh", "Adjustor_High_MCD, CPS", value = 10993840),
            numericInput("adg_low_lab", "Adjustor_Low_Lab, CPS", value = 777867),
            numericInput("adg_high_lab", "Adjustor_High_Lab, CPS", value = 12597374),
            numericInput("intercept_guide", "Guide Intercept", value = 213510.891)
          )
        ),
        width = "300px"
      ),
      
      layout_column_wrap(
        width = 1,
        card(
          card_header(
            "Master Curve Analysis Tool",
            class = "fw-bold fs-4 text-center"
          ),
          card_body(
            tags$h5("Features:"),
            tags$ul(
              tags$li("Visualize and calculate the master curve based on input parameters"),
              tags$li("Calculate SLOPE and INTERCEPT"),
              tags$li("Calculate CPS based on Master Curve Definition"),
              tags$li("Calculate Concentration using CPS")
            )
          )
        ),
        
        layout_columns(
          col_widths = c(8, 4),
          card(
            full_screen = TRUE,
            card_header("Master Curve", class = "bg-primary text-white"),
            card_body(
              plotlyOutput("mcd_plot", height = "350px"),
              radioButtons("x_scale", "Scale:",
                           choices = c("Original" = "original", "Log" = "log"),
                           selected = "log",
                           inline = TRUE)
            )
          ),
          card(
            full_screen = TRUE,
            card_header("Adjustors Calculation Results", class = "bg-primary text-white"),
            card_body(
              tags$h5("Formula:"),
              tags$pre(
                "slope = (adj_mcd_high - adj_mcd_low) / (adj_lab_high - adj_lab_low)
intercept = adj_mcd_low - (slope * adj_lab_low)"
              ),
              card(
                card_header("Calculated Slope"),
                textOutput("calculated_slope")
              ),
              card(
                card_header("Calculated Intercept"),
                textOutput("calculated_intercept")
              )
            )
          )
        )
      )
    )
  )
)