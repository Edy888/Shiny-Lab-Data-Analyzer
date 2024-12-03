library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(plotly)
library(shinycssloaders)
library(shinyjs)

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
  
  
  
  #--------------------------------------------------------------------------------------------------------------------|  
  #------------------------------------***1. Master Curve Panel ***------------------------------------------------------|
  #____________________________________________________________________________________________________________________|  
  
  
  
  
  nav_panel(
    title = "Master Curve",
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
            selectInput("type_reaction", "Select Test:", choices = NULL),
            selectInput("lot_reaction", "Select Lot:", choices = NULL),
            textOutput("reaction_type_display"),
            div(
              textOutput("assay_min_display"),
              textOutput("assay_max_display"),
              style = "margin-top: 10px; font-size: 14px; color: #555;"
            ),
            hr(),
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
      ),
      card(
        full_screen = TRUE,
        style = "min-height: 800px;",
        card_header("Types of Reactions", class = "bg-primary text-white"),
        card_body(
          DTOutput("type_of_reaction_table")
        )
      )
    )
  ),
  
  
  
#---------------------------------------------------------------------------------------------------------------------------------------|
#--------------------------------------------*** 2. Results Data Upload and Analyze ***--------------------------------------------|
#_______________________________________________________________________________________________________________________________________|

  nav_panel(
    title = 'Results DATA',
    page_sidebar(
      sidebar = sidebar(
        width = 480,
        card(
          card_header(
            "Data Import", 
            class = "bg-primary text-white fw-bold"
          ),
          style = "background-color: #808080;",

#----------------------------------------------* upload results - sidebar

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
                actionButton("reset_results_upload", "Reset", class = "btn-warning btn-sm"),
                downloadButton("export_results_data", "Export", class = "btn-sm"),
                actionButton("upload_to_sql", "Upload to SQL", class = "btn-success btn-sm")  
              ),
              textOutput("results_load_status") %>% 
                tagAppendAttributes(class = "text-info small mt-2"),
              textOutput("upload_status") %>% 
                tagAppendAttributes(class = "text-success small mt-2"),  # Статус загрузки в SQL
              verbatimTextOutput("results_summary") %>%
                tagAppendAttributes(class = "small mt-3")
            ),
  
#------------------------------------------------* Result Data Filters Panel - sidebar

            accordion_panel(
              "Result Data Filters",
              div(
                style = "max-width: 300px;",
                div(
                  class = "mb-3",
                  selectInput(
                    "test_select", 
                    "Select Test:", 
                    choices = NULL, 
                    multiple = FALSE,
                    width = "100%"
                  )
                ),
                div(
                  class = "mb-3",
                  selectInput(
                    "lab_select", 
                    "Select Laboratory:", 
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
                    "date_filter",
                    "Select Date Range:",
                    start = Sys.Date() - 30, # Начало диапазона (30 дней назад)
                    end = Sys.Date(),       # Конец диапазона (сегодня)
                    format = "yyyy-mm-dd",  # Формат отображения даты
                    separator = " to "
                  )
                )
              )
            )
          )
        )
      ),

#---------------------------------------------------* Results Data -> Main Content 

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
                full_screen = TRUE,
                card_body(
                  DTOutput("data_table") %>% withSpinner(type = 4, color = "#0d6efd")
                )
              )
            ),
            nav_panel(
              "Daily Tests Plot",
              card(
                full_screen = TRUE,
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
                full_screen = TRUE,
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
        )
      ) 
    )
  ),
  





#--------------------------------------------------------------------------------------------------------------------|  
#------------------------------------*** 3. Calibration Data (upload and analyze) ***-----------------------------------|
#____________________________________________________________________________________________________________________|

  nav_panel(
    title = "Adjustments DATA",
    page_sidebar(
      sidebar = sidebar(
        width = 500,
        card(
          card_header(
            "Data Import", 
            class = "bg-primary text-white fw-bold"
          ),
          style = "background-color: #808080;",

#----------------------------------------------* upload calibration results - sidebar

          accordion(
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
                actionButton("reset_upload", "Reset", class = "btn-warning btn-sm"),
                downloadButton("export_results", "Export", class = "btn-sm"),
                actionButton("upload_adjustments_to_sql", "Upload to SQL", class = "btn-success btn-sm")
              ),
              textOutput("calib_load_status") %>%
                tagAppendAttributes(class = "text-info small mt-2"),
              verbatimTextOutput("validation_summary") %>%
                tagAppendAttributes(class = "small mt-3")
            ),
  
#------------------------------------------------* Calibration Data Filters Panel - sidebar

            accordion(
              # Calibration Filters Panel
              accordion_panel(
                "Calibration Data Filters",
                div(
                  style = "max-width: 300px;",
                  div(
                    class = "mb-3",
                    # Выпадающий список для выбора Test Name
                    selectInput(
                      inputId = "calib_test_select", 
                      label = "Select Test Name:", 
                      choices = NULL,  # Заполняется динамически
                      multiple = FALSE,
                      width = "100%"
                    )
                  ),
                  div(
                    class = "mb-3",
                    # Выпадающий список для выбора Kit Lot
                    selectInput(
                      inputId = "calib_kit_lot_select", 
                      label = "Select Kit Lot:", 
                      choices = NULL,  # Заполняется динамически
                      multiple = TRUE,
                      width = "100%"
                    )
                  )
                )
              )
            )
          )
        )
      ),

#---------------------------------------------------* Calibration Data -> Main Content 

    layout_column_wrap(
      width = 1,
      
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
      )
    )
  )
),




#--------------------------------------------------------------------------------------------------------------------|  
#------------------------------------*** 4. Kits Data (upload and analyze) ***------------------------------------------|
#____________________________________________________________________________________________________________________|

nav_panel(
  title = "KITS DATA",
  page_sidebar(
    sidebar = sidebar(
      width = 500,
      card(
        card_header(
          "Data Import & Filters", 
          class = "bg-primary text-white fw-bold"
        ),
        style = "background-color: #808080;",
        
        # Kits Data Accordion
        accordion(
          # Kits Data Panel
          accordion_panel(
            "Kits Data",
            fileInput(
              "kits_files",
              span(
                "Upload Kits Files",
                span(class = "text-muted d-block small", "Max size: 20MB")
              ),
              multiple = TRUE,
              accept = c(".xlsx"),
              buttonLabel = "Browse...",
              width = "100%"
            ),
            div(
              class = "d-flex gap-2 my-3",
              actionButton("reset_kits_upload", "Reset", class = "btn-warning btn-sm"),
              downloadButton("export_kits_data", "Export", class = "btn-sm"),
              actionButton("upload_kits_to_sql", "Upload to SQL", class = "btn-success btn-sm")
            ),
            textOutput("kits_load_status") %>%
              tagAppendAttributes(class = "text-info small mt-2"),
            verbatimTextOutput("kits_summary") %>%
              tagAppendAttributes(class = "small mt-3")
          ),
          

# Data Filters Panel
accordion_panel(
  "Data Filters",
  div(
    class = "my-4",
    h5("Filters", class = "text-muted"),
    div(
      style = "max-width: 300px;",
      div(
        class = "mb-3",
        selectInput(
          "kits_test_select",
          "Select Test:", 
          choices = NULL,
          multiple = FALSE,
          width = "100%"
        )
      ),
      div(
        class = "mb-3",
        selectInput(
          "lab_select",
          "Select Laboratory:", 
          choices = NULL,
          multiple = TRUE,
          width = "100%"
        )
      ),
      div(
        class = "mb-3",
        selectInput(
          "kits_lot_select",
          "Select Kit Lot:", 
          choices = NULL,
          multiple = TRUE,
          width = "100%"
        )
      ),
      div(
        class = "mb-3",
        dateRangeInput(
          "kits_date_filter",
          "Select Date Range:",
          start = NULL,
          end = NULL,
          format = "yyyy-mm-dd",
          separator = " to "
        )
      ),
      div(
        class = "mb-3",
        actionButton(
          "reset_filters",
          "Reset Filters",
          class = "btn btn-secondary w-100"
        )
      )
    )
  )
)
        )
      )
    ),

#---------------------------------------------------* Kits Data -> Main Content

      # Main content area
      layout_column_wrap(
        width = 1,
        
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
                  height = "100%",
                  layout_column_wrap(
                    width = 1,
                    heights_equal = "row",
                    style = css(grid_template_rows = "600px 600px"),  # Фиксированная высота для каждого элемента
                    plotlyOutput("kits_usage_plot", height = "550px", width = "100%") %>%
                      withSpinner(type = 4, color = "#0d6efd"),
                    DTOutput("kits_usage_table", height = "550px") %>% 
                      withSpinner(type = 4, color = "#0d6efd")
                  )
                )
              )
            ),
            
            nav_panel(
              "Performance Metrics",
              card(
                full_screen = TRUE,
                card_body(
                  height = "100%",
                  layout_column_wrap(
                    width = 1,
                    heights_equal = "row",
                    style = css(grid_template_rows = "600px 600px"),
                    plotlyOutput("kits_performance_plot", height = "550px", width = "100%") %>%
                      withSpinner(type = 4, color = "#0d6efd"),
                    DTOutput("kits_performance_table", height = "550px") %>% 
                      withSpinner(type = 4, color = "#0d6efd")
                  )
                )
              )
            ),
            
            nav_panel(
              "Kits Validation Details",
              card(
                full_screen = TRUE,
                card_body(
                  layout_column_wrap(
                    width = 1,
                    # Заменяем plotlyOutput на verbatimTextOutput
                    verbatimTextOutput("kits_validation_details") %>%
                      withSpinner(type = 4, color = "#0d6efd"),
                    DTOutput("kits_validation_table") %>% withSpinner(type = 4, color = "#0d6efd")
                  )
                )
              )
            ),
            nav_panel(
              "Calibration Parameters Analysis",
              card(
                full_screen = TRUE,
                card_body(
                  height = "100%",
                  layout_column_wrap(
                    width = 1,
                    heights_equal = "row",
                    style = css(grid_template_rows = "600px 600px"),
                    plotlyOutput("calibration_trend_plot", height = "550px", width = "100%") %>%
                      withSpinner(type = 4, color = "#0d6efd")
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
    









#---------------------------------------------------* Analyzing Data -> SIDEBAR----------------------|




# Sidebar с загрузкой данных и фильтрами
nav_panel(
  title = "Analyzing Data from SQL",
  page_sidebar(
    sidebar = sidebar(
      width = 400,
      card(
        card_header(
          "Data Access & Filters", 
          class = "bg-primary text-white fw-bold"
        ),
        style = "background-color: #2C2C2C; color: white;",
        accordion(
          # Панель загрузки данных
          accordion_panel(
            title = "Upload SQL Data",
            value = "upload_data",
            div(
              class = "d-flex flex-column gap-3 p-3",
              div(
                actionButton(
                  "load_sql_data", 
                  "Load SQL Data", 
                  class = "btn btn-primary w-100"
                )
              ),
              div(
                textOutput("sql_data_status"),  # Статус загрузки данных
                class = "text-muted"
              )
            )
          ),
          hr(),
          # Панель фильтров
          accordion_panel(
            title = "Data Filters",
            div(
              class = "my-3 p-3",
              div(
                style = "max-width: 300px;",
                # Выбор теста
                div(
                  class = "mb-3",
                  selectInput(
                    inputId = "sql_test_select",
                    label = "Select Test:",
                    choices = NULL,
                    selected = "",
                    multiple = FALSE,
                    width = "100%"
                  )
                ),
                # Выбор лаборатории
                div(
                  class = "mb-3",
                  selectInput(
                    inputId = "sql_lab_select",
                    label = "Select Laboratory:",
                    choices = NULL,
                    multiple = TRUE,
                    width = "100%"
                  )
                ),
                # Выбор лота
                div(
                  class = "mb-3",
                  selectInput(
                    inputId = "sql_lot_select",
                    label = "Select Kit Lot:",
                    choices = NULL,
                    multiple = TRUE,
                    width = "100%"
                  )
                ),
                # Диапазон дат
                div(
                  class = "mb-3",
                  dateRangeInput(
                    inputId = "sql_date_filter",
                    label = "Select Date Range:",
                    start = "2005-01-01",  # Устанавливаем начальную дату
                    end = Sys.Date(),    # Устанавливаем конечную дату
                    min = NULL,
                    max = NULL,
                    format = "yyyy-mm-dd",
                    separator = " to "
                  )
                  
                ),
                # Сброс фильтров
                div(
                  class = "mb-3",
                  actionButton(
                    inputId = "sql_reset_filters",
                    label = "Reset Filters",
                    class = "btn btn-secondary w-100"
                  )
                )
              )
            )
          ),
          
          hr(),
          
          verbatimTextOutput("results_summary"),  # Суммарная информация для Results
          verbatimTextOutput("kits_summary"),     # Суммарная информация для Kits
          verbatimTextOutput("adjustments_summary")  # Суммарная информация для Adjustments
        )
      )
    ),
    # Main content area
    layout_column_wrap(
      width = 1,
      card(
        full_screen = TRUE,
        card_header("SQL Data Analysis", class = "bg-primary text-white"),
        navset_card_tab(
          nav_panel(
            "Analysis of Kits Data",
            card(
              full_screen = TRUE,
              card_body(
                
                plotlyOutput("kits_curve_params_plot") %>% 
                  withSpinner(type = 4, color = "#0d6efd"),
                DTOutput("kits_all_data") %>% 
                  withSpinner(type = 4, color = "#0d6efd"),
                hr(),
                hr(),
                div(
                  style = "margin-bottom: 10px;", # Добавление отступа между текстом и графиком
                  p(
                    "The plot below shows the 4PL model for each lot. Each curve represents the CPS values calculated based on the provided parameters.",
                    style = "font-size: 1rem; color: #6c757d;" # Настройка стиля текста
                  )
                ),
                plotlyOutput("mcd_kit_plot") %>%
                  withSpinner(type = 4, color = "#0d6efd"),
                div(
                  class = "d-flex justify-content-between align-items-center",
                  style = "gap: 20px; margin-bottom: 10px;",
                  div(
                    style = "flex: 1;",
                    numericInput("conc_min", "Min Concentration:", value = 0.1, step = 0.1)
                  ),
                  div(
                    style = "flex: 1;",
                    numericInput("conc_max", "Max Concentration:", value = 500000, step = 1000)
                  )
                )
              )
            )
          ),
          nav_panel(
            "Analysis of Adjustments Data",
            card(
              full_screen = TRUE,
              card_body(
                # Таблицы данных
                div(
                  class = "mb-4",
                  DTOutput("adjustments_all_data") %>% 
                    withSpinner(type = 4, color = "#0d6efd"),
                  DTOutput("adjustments_grouped_data") %>% 
                    withSpinner(type = 4, color = "#0d6efd")
                ),
                
                # Разделительная линия
                hr(),
                
                # Заголовок и график
                div(
                  class = "mb-4",
                  plotlyOutput("calibration_plot") %>% 
                    withSpinner(type = 4, color = "#0d6efd")
                ),
                
                # Таблица с данными для графика
                div(
                  class = "mb-4",
                  DTOutput("calibration_data_table") %>% 
                    withSpinner(type = 4, color = "#0d6efd")
                ),
                
                # Описание графика
                div(
                  style = "margin-bottom: 10px;", 
                  p(
                    "The plot below represents the calibration curve. The blue line and points correspond to the Low Adjustor CPS, while the red line and points correspond to the High Adjustor CPS.",
                    style = "font-size: 1rem; color: #6c757d;"
                  )
                ),
              )
            )
          ),
          nav_panel(
            "Analysis of Results Data",
            card(
              full_screen = TRUE,
              card_body(
                DTOutput("results_all_data") %>% 
                  withSpinner(type = 4, color = "#0d6efd"),
                DTOutput("results_grouped_data") %>% 
                  withSpinner(type = 4, color = "#0d6efd")
              )
            )
          ),
          nav_panel(
            "Combined Analysis",
            card(
              full_screen = TRUE,
              card_body(
                DTOutput("sql_detailed_combined_stats") %>% 
                  withSpinner(type = 4, color = "#0d6efd")
              )
            )
          )
        )
      )
    )
  )
)






#---------------------------------------------------* Analyzing Data -> Main Content

)