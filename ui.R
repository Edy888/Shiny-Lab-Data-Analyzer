ui <- page_navbar(
  theme = my_theme,
  title = div(
    style = "padding: 0 15px;",
    span(
      "Master Curve Analysis Tool version 1.0",
      style = "color: #F5F5F5; font-size: 24px;"
    )
  ),
  
  tags$head(
    tags$style(HTML("
    .form-control, .control-label { 
      font-size: 0.85rem !important;
    }
    .small-text .form-control,
    .small-text .control-label {
      font-size: 0.85rem !important;
    }
  "))
  ),
  
  nav_panel(
    title = "Master Curve",
    page_sidebar(
      sidebar = sidebar(
        card(
          card_header(
            "Parameters",
            class = "bg-primary text-white"
          ),
          div(
            style = "padding: 10px;",
            # Оборачиваем все поля ввода в div с уменьшенным отступом
            div(
              style = "display: flex; flex-direction: column; gap: 3px;", # уменьшаем промежуток до 5px
              numericInput("P1", "P1:", value = 3.404367e+19),
              numericInput("P2", "P2:", value = 61440),
              numericInput("P3", "P3:", value = 1.280082e+14),
              numericInput("P4", "P4:", value = -0.9975617),
              hr(),
              numericInput("conc_min", "Min Conc:", value = 0.1, min = 0.001),
              numericInput("conc_max", "Max Conc:", value = 50000, min = 1),
              hr(),
              numericInput("adg_low_sh" ,"Adjustor_Low_MCD, CPS", value = 711703),
              numericInput("adg_high_sh", "Adjustor_High_MCD, CPS", value = 10993840),
              numericInput("adg_low_lab" ,"Adjustor_Low_Lab, CPS", value = 777867),
              numericInput("adg_high_lab" ,"Adjustor_High_Lab, CPS", value = 12597374),
              numericInput(
                "intercept_guide", 
                "Guide Intercept", 
                value = 213510.891)
              
            )
          )
        ),
        width = "300px",
        class = "shadow-sm"
      ),
      
      # Main content
      div(
        style = "display: flex; flex-direction: column; gap: 1rem;",
        
        card(
          height = "auto",
          card_header(
            "Master Curve Analysis Tool",
            style = paste0("background-color:", sh_colors$dark_gray, "; color:", sh_colors$petrol),
            class = "fw-bold fs-4 text-center"
          ),
          card_body(
            tags$h5(
              "Features:",
              style = paste0("color:", sh_colors$light_gray, ";")
            ),
            tags$ul(
              style = paste0("color:", sh_colors$light_gray, ";"),
              tags$li("Visualize and calculate the master curve based on input parameters"),
              tags$li("Calculate SLOPE and INTERCEPT"),
              tags$li("Calculate CPS based on Master Curve Definition"),
              tags$li("Calculate Concentration using CPS")
            )
          ),
          full_screen = TRUE
        ),
        
        card(
          height = "auto",
          full_screen = TRUE,
          card_header(
            "SECTION 1: Generation of MCD using parameters",
            style = paste0("background-color:", sh_colors$dark_gray, "; color:", sh_colors$petrol),
            class = "fw-bold fs-4 text-center"
          ),
          card_body(
            tags$h5("Master Curve:", style = paste0("color:", sh_colors$light_gray, ";")),
            tags$ol(
              style = paste0(
                "color:", sh_colors$light_gray, ";",
                "list-style-type: none;",
                "counter-reset: item;",
                "padding-left: 0;"
              ),
              lapply(list(
                "В боковой панели вводим параметры заводской модели: P1, P2, P3, P4",
                "По параметрам строится график заводской MCD (x - концентрация, y - CPS",
                "Под графиком есть возможность менять ось x: original (концентрация) или log10(концентрация). Log позволяет визуализировать классическую S-образную модель 4PL",
                "На боковой панели есть возможность менять концентрацию для построения MCD. В зависимости от коэффициента P3 (50% conc) для некоторых моделей нужно увеличить макимальную концентрацию для визуализации S- образного графика."
              ), function(text) {
                tags$li(
                  style = paste0(
                    "counter-increment: item;",
                    "margin-bottom: 0.8em;",
                    "padding-left: 2.5em;",
                    "position: relative;"
                  ),
                  # Создаем отдельный span для номера
                  tags$span(
                    style = paste0(
                      "position: absolute;",
                      "left: 0;",
                      "width: 1.8em;",
                      "height: 1.8em;",
                      "background: linear-gradient(135deg, ", sh_colors$petrol, " 0%, ", sh_colors$dark_gray, " 100%);",
                      "color: white;",
                      "text-align: center;",
                      "line-height: 1.8em;",
                      "font-weight: bold;"
                    ),
                    `data-content` = sprintf("%d", length(text))  # Добавляем номер как атрибут
                  ),
                  text
                )
              })
            )
          )
        ),
        
        # Layout для графика
        layout_column_wrap(
          width = 1,
          style = "gap: 1rem;",
          
          # Контейнер для управления шириной карточки
          div(
            style = "max-width: 1200px; width: 100%;", # Задаем максимальную ширину и растягивание на всю доступную ширину
            # Создаем карточку для графика
            card(
              min_height = "400px", # Минимальная высота карточки
              # Заголовок карточки
              card_header(
                "Master Curve", # Текст заголовка
                class = "bg-primary text-white" # Стили заголовка: цвет фона и текста
              ),
              # Основной контейнер содержимого карточки
              div(
                style = "padding: 15px; display: flex; flex-direction: column; height: calc(100% - 56px);", # Отступы и flexbox настройки
                # Контейнер для графика
                div(
                  style = "flex-grow: 1;", # Растягивание на доступное пространство
                  plotlyOutput("mcd_plot", height = "100%") # Вывод графика plotly на всю высоту
                ),
                # Контейнер для радиокнопок
                div(
                  style = "margin-top: 1rem;", # Отступ сверху
                  # Группа радиокнопок для выбора масштаба
                  radioButtons("x_scale", "Scale:", # Создание радиокнопок с меткой "Scale:"
                               choices = c("Original" = "original", "Log" = "log"), # Варианты выбора
                               selected = "log", # Значение по умолчанию
                               inline = TRUE) # Размещение кнопок в одну строку
                )
              )
            )
          )
        ),
        
        card(
          height = "auto",
          full_screen = TRUE,
          card_header(
            "Adjustors Calculation Results",
            style = paste0("background-color:", sh_colors$dark_gray, "; color:", sh_colors$petrol),
            class = "fw-bold fs-4 text-center"
          ),
          card_body(
            tags$h5(
              "Formula:",
              style = paste0("color:", sh_colors$light_gray, ";")
            ),
            # Формулы с использованием математической нотации
            tags$div(
              style = paste0(
                "color:", sh_colors$light_gray, ";",
                "font-family: monospace;",
                "background: rgba(0,0,0,0.1);",
                "padding: 10px;",
                "border-radius: 4px;",
                "margin-bottom: 15px;"
              ),
              tags$pre(
                "slope = (adj_siemens_high - adj_siemens_low) / (adj_lab_high - adj_lab_low)
intercept = adj_siemens_low - (slope * adj_lab_low)"
              )
            ),
            
            # Результаты расчетов
            div(
              style = "display: flex; gap: 20px; flex-wrap: wrap;",
              
              # Карточка для Slope
              card(
                height = "100px",
                card_header(
                  "Calculated Slope",
                  style = paste0("background-color:", sh_colors$petrol, "; color: white;"),
                ),
                card_body(
                  style = "display: flex; align-items: center; justify-content: center;",
                  textOutput("calculated_slope")
                )
              ),
              
              # Карточка для Intercept
              card(
                height = "100px",
                card_header(
                  "Calculated Intercept",
                  style = paste0("background-color:", sh_colors$petrol, "; color: white;"),
                ),
                card_body(
                  style = "display: flex; align-items: center; justify-content: center;",
                  textOutput("calculated_intercept")
                )
              )
            )
          )
        )
      )
    )
  ),  
  
  # Replace the standalone ard( with this properly nested section
  nav_panel(
    title = "Calculators",
    card(
      height = "auto",
      full_screen = TRUE,
      card_header(
        "Calculators",
        style = paste0("background-color:", sh_colors$dark_gray, "; color:", sh_colors$petrol),
        class = "fw-bold fs-4 text-center"
      ),
      card_body(
        div(
          style = "display: flex; gap: 20px; flex-wrap: wrap;",
          
          # Карточка для перевода из лабораторного CPS в концентрацию
          card(
            width = "48%",
            card_header(
              "Laboratory CPS → Concentration",
              style = paste0("background-color:", sh_colors$petrol, "; color: white;")
            ),
            card_body(
              div(
                style = "display: flex; flex-direction: column; gap: 10px;",
                numericInput("input_lab_cps", "Enter Laboratory CPS value:", value = NA),
                div(
                  style = paste0(
                    "background: ", sh_colors$dark_gray, ";",
                    "padding: 10px;",
                    "border-radius: 4px;",
                    "color: ", sh_colors$light_gray, ";"
                  ),
                  "Calculated MCD CPS:",
                  textOutput("calculated_mcd_cps")
                ),
                div(
                  style = paste0(
                    "background: ", sh_colors$dark_gray, ";",
                    "padding: 10px;",
                    "border-radius: 4px;",
                    "color: ", sh_colors$light_gray, ";"
                  ),
                  "Calculated Concentration:",
                  textOutput("calculated_concentration_from_lab")
                )
              )
            )
          ),
          
          # Карточка для расчета CPS по концентрации
          card(
            width = "48%",
            card_header(
              "Concentration → CPS",
              style = paste0("background-color:", sh_colors$petrol, "; color: white;")
            ),
            card_body(
              div(
                style = "display: flex; flex-direction: column; gap: 10px;",
                numericInput("input_concentration", "Enter Concentration value:", value = 10),
                div(
                  style = paste0(
                    "background: ", sh_colors$dark_gray, ";",
                    "padding: 10px;",
                    "border-radius: 4px;",
                    "color: ", sh_colors$light_gray, ";"
                  ),
                  "Calculated MCD CPS:",
                  textOutput("calculated_mcd_cps_direct")
                ),
                div(
                  style = paste0(
                    "background: ", sh_colors$dark_gray, ";",
                    "padding: 10px;",
                    "border-radius: 4px;",
                    "color: ", sh_colors$light_gray, ";"
                  ),
                  "Calculated Laboratory CPS:",
                  textOutput("calculated_lab_cps")
                )
              )
            )
          )
        )
      )
    )
  ),
  
  
  
                                                                                        # Page 2 
  nav_panel(
    title = "Laboratory Data",
    # Создаем страницу с боковой панелью
    page_sidebar(
      # Настройка боковой панели с элементами управления
      sidebar = sidebar(
        # Карточка для элементов управления
        card(
          # Заголовок карточки с контролами
          card_header(
            "Controls",
            class = "bg-primary text-white" # Стилизация заголовка: белый текст на синем фоне
          ),
          # Контейнер для элементов управления с отступами
          div(
            style = "padding: 15px;",
            # Загрузка файла Excel с результатами
            fileInput("results_file", "File with results data:", 
                      accept = c(".xlsx"),
                      buttonLabel = "Browse...",
                      placeholder = "No file selected"),
            
            # Выпадающий список для выбора теста с возможностью поиска
            selectizeInput("test_select", "Test:", 
                           choices = tests,
                           options = list(
                             placeholder = 'Select test...',
                             dropdownParent = 'body' # Размещение выпадающего списка относительно body
                           )),
            
            # Выбор типа анализа данных
            selectInput("analysis_type", "Analysis:",
                        choices = c("Daily" = "daily", 
                                    "Monthly" = "monthly", 
                                    "Lot" = "lot")),
            
            # Переключатель для отображения тренда
            checkboxInput("show_trend", "Show Trend", value = TRUE)
          )
        ),
        width = "300px", # Фиксированная ширина боковой панели
        class = "shadow-sm" # Легкая тень для визуального отделения
      ),
      
      # Основная область с графиками и таблицей
      layout_column_wrap(
        width = 1/2, # Разделение на две колонки
        height = "800px",
        style = "gap: 10px;", # Отступ между карточками
        
        # Первая карточка - График ежедневных тестов
        card(
          height = "380px",
          card_header(
            "Daily Tests",
            class = "bg-primary text-white"
          ),
          div(
            style = "padding: 10px;",
            plotlyOutput("daily_tests_plot", height = "300px") # Интерактивный график
          )
        ),
        
        # Вторая карточка - График скользящего среднего
        card(
          height = "380px",
          card_header(
            "Rolling Mean Analysis",
            class = "bg-primary text-white"
          ),
          div(
            style = "padding: 10px;",
            plotlyOutput("rolling_mean_plot", height = "300px") # Интерактивный график
          )
        ),
        
        # Третья карточка - График результатов тестов
        card(
          height = "380px",
          card_header(
            "Test Results",
            class = "bg-primary text-white"
          ),
          div(
            style = "padding: 10px;",
            plotlyOutput("test_summary_plot", height = "300px") # Интерактивный график
          )
        ),
        
        # Четвертая карточка - Таблица статистики
        card(
          height = "380px",
          card_header(
            "Statistics",
            class = "bg-primary text-white"
          ),
          div(
            style = "padding: 10px; height: 400px; overflow-y: auto;", # Добавление прокрутки при необходимости
            tableOutput("statistics_table") # Вывод статистической таблицы
          )
        )
      )
    )
  ),
  nav_spacer(),
  nav_item(
    tags$a(
      "GitHub",
      href = "https://github.com/yourusername/your-repo",
      target = "_blank",
      class = "nav-link"
    )
  ),
  bg = "#1A1A1A",
  inverse = TRUE,
  fluid = TRUE
)