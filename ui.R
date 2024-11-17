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
  

  nav_panel(
    title = "Master Curve",
    page_sidebar(
      sidebar = sidebar(
        width = 400,
        card(
          card_header("Parameters", class = "bg-primary text-white"),
          card_body(
            numericInput("P1", "P1 (Max CPS):", value = 3.404367e+19, width = "100%"),
            numericInput("P2", "P2 (Min CPS):", value = 61440, width = "100%"),
            numericInput("P3", "P3 (EC50):", value = 1.280082e+14, width = "100%"),
            numericInput("P4", "P4 (Slope):", value = -0.9975617, width = "100%"),
            hr(),
            numericInput("conc_min", "Min Concentration:", value = 0.1, min = 0.001, width = "100%"),
            numericInput("conc_max", "Max Concentration:", value = 50000, min = 1, width = "100%"),
            hr(),
            numericInput("adg_low_sh", "Adjustor Low (MCD CPS):", value = 711703, width = "100%"),
            numericInput("adg_high_sh", "Adjustor High (MCD CPS):", value = 10993840, width = "100%"),
            numericInput("adg_low_lab", "Adjustor Low (Lab CPS):", value = 777867, width = "100%"),
            numericInput("adg_high_lab", "Adjustor High (Lab CPS):", value = 12597374, width = "100%"),
            numericInput("intercept_guide", "Guide Intercept:", value = 213510.891, width = "100%")
          )
        )
      ),
      # Main content area
      card(
        full_screen = TRUE,
        style = "min-height: 800px;",
        card_header("Master Curve Analysis", class = "bg-primary text-white"),
        card_body(
          # График
          plotlyOutput("mcd_plot", height = "600px"),
          
          # Контейнер для элементов управления
          div(
            style = "display: flex; gap: 20px; align-items: start; margin-top: 20px;",
            
            # X-Scale controls
            div(
              style = "flex: 0 0 200px;",
              radioButtons("x_scale", "X-Axis Scale:",
                           choices = c("Original" = "original", "Log" = "log"),
                           selected = "log", inline = TRUE)
            ),
            
            # Laboratory CPS → Concentration
            div(
              style = "flex: 1;",
              h6("Laboratory CPS → Concentration", style = "color: #0d6efd; font-weight: bold;"),
              numericInput("input_lab_cps", "Enter Laboratory CPS Value:", value = NA, width = "100%"),
              textOutput("calculated_mcd_cps"),
              textOutput("calculated_concentration_from_lab")
            ),
            
            # Concentration → CPS
            div(
              style = "flex: 1;",
              h6("Concentration → CPS", style = "color: #0d6efd; font-weight: bold;"),
              numericInput("input_concentration", "Enter Concentration Value:", value = 10, width = "100%"),
              textOutput("calculated_mcd_cps_direct"),
              textOutput("calculated_lab_cps")
            )
          )
        )
      )
    )
  )
)