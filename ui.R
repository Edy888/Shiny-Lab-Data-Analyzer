
#Напиши все составляющие, которые мне нужны.
#Здесь описаны все особенности:
  
#1. есть приложение Shiny
#2. для развертывания буду использовать shinyapps.io
#3. база данных находится в My SQL 
#4. есть домен: shinylab.pro (godaddy)
#5. нет белого IP для проброса портов и удаленного подключения
#6. Цель: любой пользователь может зайти на сайт и использовать приложение. При загрузке своей базы данных (.accdb) пользователем - данные по скрипту в коде приложения должны подгрузиться в мою базу данных MySQL : shiny_app_db.
#- после этого данные возвращаются в приложение в зависмости от фильтров которые выбирает пользователь.
#Также проиллюстрируй всю схему для успешного развертывания приложения с такими особенностями

# [Пользователь] --> [Домен (shinylab.pro)] --> [shinyapps.io] --> [Shiny-приложение] --> [MySQL-сервер]



library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(shiny.i18n)     # translation
library(rsconnect)
source("global.R")




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
      /* Общие стили для форм */
      .form-control, .control-label { 
        font-size: 0.85rem !important;
      }
      .small-text .form-control,
      .small-text .control-label {
        font-size: 0.85rem !important;
      }

      /* Обновленные стили для selectize */
      .selectize-dropdown {
        position: absolute !important;
        top: 100% !important;
        left: 0 !important;
        z-index: 1000 !important;
        max-width: 300px !important;
        width: 100% !important;
        background: #212529 !important;
        border: 1px solid rgba(255,255,255,0.1);
        border-radius: 4px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.2);
      }
      
      .selectize-dropdown-content {
        max-height: 200px;
        overflow-y: auto;
        color: #fff !important;
      }

      .selectize-dropdown .option {
        color: #fff !important;
        padding: 8px 12px;
      }

      .selectize-dropdown .option:hover,
      .selectize-dropdown .option.active {
        background-color: #495057 !important;
      }
      
      .selectize-control {
        position: relative !important;
        max-width: 100%;
        width: 100%;
      }

      /* Адаптивный дизайн */
      @media (max-width: 768px) {
        .form-control, .control-label {
          font-size: 0.75rem !important;
        }
        .selectize-control {
          width: 100% !important;
        }
      }
      
      /* Стили аккордеона */
      .accordion-body {
        padding: 1rem;
      }
      
      /* Стили подписей */
      .control-label {
        font-weight: 500;
        margin-bottom: 0.5rem;
      }
    "))
  ),
  
  # Data Upload Panel
  nav_panel(
    title = "Data Upload",
    page_sidebar(
      sidebar = sidebar(
        width = 500,
        card(
          card_header(
            "Data Import", 
            class = "bg-primary text-white fw-bold"
          ),
          style = "background-color: #808080;",
          
          accordion(
            # Unified Data Upload Panel
            accordion_panel(
              "Data Upload",
              fileInput(
                "db_files",
                span(
                  "Upload Access Files (.mdb, .accdb)",
                  span(class = "text-muted d-block small", "Max size: 100MB")
                ),
                multiple = TRUE,
                accept = c(".mdb", ".accdb"),
                buttonLabel = "Browse...",
                width = "100%"
              ),
              
              uiOutput("table_selection"),  # Динамический выбор таблицы
              
              div(
                class = "d-flex gap-2 my-3",
                actionButton("reset_upload", "Reset", class = "btn-warning btn-sm"),
                downloadButton("export_combined_data", "Export Data", class = "btn-sm")
              ),
              textOutput("load_status") %>% 
                tagAppendAttributes(class = "text-info small mt-2"),
              verbatimTextOutput("summary") %>%
                tagAppendAttributes(class = "small mt-3"),
              actionButton("upload_to_sql", "Upload to SQL", class = "btn-success btn-sm"),
              textOutput("upload_status") %>% 
                tagAppendAttributes(class = "text-success small mt-2")
              
              
            ),
            
            # Unified Filters Panel
            accordion_panel(
              "Data Filters",
              div(
                style = "max-width: 300px;",
                div(
                  class = "mb-3",
                  selectInput(
                    "sn_select", 
                    "Select SN:", 
                    choices = NULL, 
                    multiple = TRUE,
                    width = "100%"
                  )
                ),
                div(
                  class = "mb-3",
                  selectInput(
                    "test_select", 
                    "Select Test Name:", 
                    choices = NULL, 
                    multiple = TRUE,
                    width = "100%"
                  )
                ),
                div(
                  class = "mb-3",
                  selectInput(
                    "kit_lot_select", 
                    "Select Kit Lot:", 
                    choices = NULL, 
                    multiple = TRUE,
                    width = "100%"
                  )
                ),
                div(
                  class = "mb-3",
                  dateRangeInput(
                    "date_range", 
                    "Select Date Range:", 
                    start = Sys.Date() - 30,
                    end = Sys.Date(),
                    width = "100%"
                  )
                )
              )
            )
          )
        )
      ),
      
      
      # Main content area
      layout_column_wrap(
        width = 1,
        
        # Test Results Card
        card(
          full_screen = TRUE,
          card_header("Test Results", class = "bg-primary text-white"),
          navset_card_tab(
            nav_panel(
              "Data Table",
              card(
                card_body(
                  DTOutput("data_table") %>% withSpinner(type = 4, color = "#0d6efd")
                )
              )
            ),
            nav_panel(
              "Daily Tests Plot",
              card(
                style = "height: 100vh; width: 100%;",
                card_body(
                  style = "height: 100%; width: 100%; padding: 0; margin: 0;",
                  plotlyOutput("daily_tests_plot", height = "100%", width = "100%") %>%
                    withSpinner(type = 4, color = "#0d6efd")
                )
              )
            ),
            nav_panel(
              "Statistics Plot",
              card(
                card_body(
                  plotlyOutput("test_stats_plot", height = "500px") %>%
                    withSpinner(type = 4, color = "#0d6efd")
                )
              )
            ),
            nav_panel(
              "Statistics Table",
              card(
                full_screen = TRUE,
                card_body(
                  DTOutput("test_stats_table") %>% withSpinner(type = 4, color = "#0d6efd")
                )
              )
            )
          )
        ),
        
        # Calibration Data Card
        card(
          full_screen = TRUE,
          card_header("Calibration Data", class = "bg-primary text-white"),
          navset_card_tab(
            nav_panel(
              "Summary by Kits",
              card(
                full_screen = TRUE,
                card_body(
                  style = "height: 100%; width: 100%; padding: 0; margin: 0;",
                  DTOutput("calib_table2") %>% withSpinner(type = 4, color = "#0d6efd")
                )
              )
            ),
            nav_panel(
              "Detailed Data",
              card(
                full_screen = TRUE,
                card_body(
                  style = "height: 100%; width: 100%; padding: 0; margin: 0;",
                  DTOutput("calib_table1") %>% withSpinner(type = 4, color = "#0d6efd")
                )
              )
            ),
            nav_panel(
              "Validation",
              card(
                full_screen = TRUE,
                card_body(
                  style = "height: 100%; width: 100%; padding: 0; margin: 0;",
                  layout_column_wrap(
                    width = 1,
                    plotlyOutput("validation_plot", height = "600px", width = "100%") %>%
                      withSpinner(type = 4, color = "#0d6efd"),
                    hr(),
                    verbatimTextOutput("validation_details") %>%
                      tagAppendAttributes(class = "small")
                  )
                )
              )
            )
          )
        ),
        
        # Kits Information Card
        card(
          full_screen = TRUE,
          card_header("Kits Data", class = "bg-primary text-white"),
          navset_card_tab(
            nav_panel(
              "Data Table",
              card(
                full_screen = TRUE,
                card_body(
                  DTOutput("kits_data_table") %>% withSpinner(type = 4, color = "#0d6efd")
                )
              )
            ),
            nav_panel(
              "Usage Analysis",
              card(
                full_screen = TRUE,
                card_body(
                  layout_column_wrap(
                    width = 1,
                    plotlyOutput("kits_usage_plot", height = "600px", width = "100%") %>%
                      withSpinner(type = 4, color = "#0d6efd"),
                    DTOutput("kits_usage_table") %>% withSpinner(type = 4, color = "#0d6efd")
                  )
                )
              )
            ),
            nav_panel(
              "Performance Metrics",
              card(
                full_screen = TRUE,
                card_body(
                  layout_column_wrap(
                    width = 1,
                    plotlyOutput("kits_performance_plot", height = "600px", width = "100%") %>%
                      withSpinner(type = 4, color = "#0d6efd"),
                    DTOutput("kits_performance_table") %>% withSpinner(type = 4, color = "#0d6efd")
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # Master Curve Panel
  nav_panel(
    title = "Master Curve Calculation",
    page_sidebar(
      sidebar = sidebar(
        width = 350,
        card(
          card_header(
            div(
              class = "d-flex align-items-center gap-2",
              "Parameters",
              bsicons::bs_icon("info-circle-fill", class = "text-white") %>%
                tooltip(
                  "Здесь вы можете настроить параметры 4PL модели и калибровочные значения"
                )
            ),
            class = "bg-primary text-white"
          ),
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
          plotlyOutput("mcd_plot", height = "600px"),
          div(
            style = "display: flex; gap: 20px; align-items: start; margin-top: 20px;",
            div(
              style = "flex: 0 0 200px;",
              radioButtons("x_scale", "X-Axis Scale:",
                           choices = c("Original" = "original", "Log" = "log"),
                           selected = "log", inline = TRUE)
            ),
            div(
              style = "flex: 1;",
              h6("Laboratory CPS → Concentration", style = "color: #0d6efd; font-weight: bold;"),
              numericInput("input_lab_cps", "Enter Laboratory CPS Value:", value = NA, width = "100%"),
              textOutput("calculated_mcd_cps"),
              textOutput("calculated_concentration_from_lab")
            ),
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