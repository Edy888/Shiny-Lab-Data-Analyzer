# 1. Обновление выбора тестов для QC страницы
observe({
  req(results_data())
  
  # Получаем уникальные значения тестов из данных
  test_choices <- results_data() %>%
    mutate(Reagent_Name = as.character(Reagent_Name)) %>%
    pull(Reagent_Name) %>%
    unique() %>%
    sort()
  
  # Обновляем choices в selectInput
  updateSelectInput(
    inputId = "test_select_qc",
    choices = c("Выберите тест" = "", test_choices),
    selected = ""
  )
})

# Обновление выбора лотов при выборе теста
observeEvent(input$test_select_qc, {
  req(filtered_qc_data())
  
  # Получаем уникальные значения лотов из отфильтрованных данных
  lot_choices <- filtered_qc_data() %>%
    pull(Kit_Lot) %>%
    unique() %>%
    sort()
  
  # Обновляем выбор лота в selectInput
  updateSelectInput(
    inputId = "lot_select_qc",
    choices = lot_choices,
    selected = NULL  # multiple не требуется здесь
  )
})

# 3. Создаем реактивный датасет для QC с правильными типами данных
qc_data <- reactive({
  req(results_data())
  
  results_data() %>%
    mutate(
      Reagent_Name = as.character(Reagent_Name),
      Kit_Lot = as.character(Kit_Lot),
      id_lab = as.character(id_lab),
      Result = as.numeric(Result)
    ) |> 
    filter(Result >= 0) |> 
    filter(Identifier == 2)
})

# 4. Фильтрованный датасет для QC с учетом множественного выбора лотов
filtered_qc_data <- reactive({
  req(qc_data())
  
  data <- qc_data()
  
  if (input$test_select_qc != "") {
    data <- data %>%
      filter(Reagent_Name == input$test_select_qc)
  }
  
  if (!is.null(input$lot_select_qc) && length(input$lot_select_qc) > 0) {
    data <- data %>%
      filter(Kit_Lot %in% input$lot_select_qc)
  }
  
  data
})

# Визуализация QC графика
output$qc_plot <- renderPlotly({
  req(filtered_qc_data())
  req(calib_data()$raw_data)
  
  # Получаем отфильтрованные данные для графика
  data <- filtered_qc_data()
  
  # Преобразуем `DateTimeOfAdjustment` в POSIXct и проверяем уникальные даты калибровок
  calib_dates <- calib_data()$raw_data %>%
    filter(
      Test_Name == input$test_select_qc,
      Kit_Lot %in% input$lot_select_qc
    ) %>%
    mutate(DateTimeOfAdjustment = as.POSIXct(DateTimeOfAdjustment, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")) %>%
    pull(DateTimeOfAdjustment) %>%
    unique()
  
  # Печатаем даты калибровок для отладки
  print("Calibration Dates:")
  print(calib_dates)
  
  # Построение графика с использованием ggplot
  plot <- ggplot(data, aes(x = Date_And_Time_Started, y = Result, color = id_lab)) +
    # Добавляем вертикальные линии калибровок
    geom_vline(xintercept = calib_dates, linetype = "dashed", color = "gray50", alpha = 0.5) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    labs(
      title = "График контроля качества",
      x = "Дата",
      y = "Концентрация"
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "#2C2C2C", color = NA),    # Фон области графика
      plot.background = element_rect(fill = "#2C2C2C", color = NA),     # Фон всей области графика
      panel.grid.major = element_line(color = "#1A1A1A"),               # Основные линии сетки
      panel.grid.minor = element_line(color = "#1A1A1A", size = 0.5),   # Второстепенные линии сетки
      axis.text = element_text(color = "#F5F5F5"),                      # Цвет текста осей
      axis.title = element_text(color = "#F5F5F5"),                     # Цвет заголовков осей
      plot.title = element_text(color = "#F5F5F5", size = 14, face = "bold"),  # Цвет и стиль заголовка графика
      legend.background = element_rect(fill = "#2C2C2C", color = NA),    # Фон легенды
      legend.text = element_text(color = "#F5F5F5"),                    # Цвет текста в легенде
      legend.title = element_text(color = "#F5F5F5"),                   # Цвет заголовка легенды
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5)    # Настройка подписи по оси X
    )
  
  ggplotly(plot)
})

# Визуализация CPS графика
output$cps_plot <- renderPlotly({
  req(filtered_qc_data())
  req(calib_data()$raw_data)
  
  # Преобразуем `DateTimeOfAdjustment` в POSIXct и получаем уникальные даты калибровок
  calib_dates <- calib_data()$raw_data %>%
    filter(
      Test_Name == input$test_select_qc,
      Kit_Lot %in% input$lot_select_qc
    ) %>%
    mutate(DateTimeOfAdjustment = as.POSIXct(DateTimeOfAdjustment, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")) %>%
    pull(DateTimeOfAdjustment) %>%
    unique()
  
  # Печатаем даты калибровок для отладки
  print("Calibration Dates (CPS):")
  print(calib_dates)
  
  plot <- ggplot(filtered_qc_data(), aes(x = Date_And_Time_Started, y = CPS, color = id_lab)) +
    # Добавляем вертикальные линии калибровок
    geom_vline(xintercept = calib_dates, linetype = "dashed", color = "gray50", alpha = 0.5) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    labs(
      title = "График CPS",
      x = "Дата",
      y = "CPS"
    ) +
    theme_minimal()+
    theme(
      panel.background = element_rect(fill = "#2C2C2C", color = NA),    # Фон области графика
      plot.background = element_rect(fill = "#2C2C2C", color = NA),     # Фон всей области графика
      panel.grid.major = element_line(color = "#1A1A1A"),               # Основные линии сетки
      panel.grid.minor = element_line(color = "#1A1A1A", size = 0.5),   # Второстепенные линии сетки
      axis.text = element_text(color = "#F5F5F5"),                      # Цвет текста осей
      axis.title = element_text(color = "#F5F5F5"),                     # Цвет заголовков осей
      plot.title = element_text(color = "#F5F5F5", size = 14, face = "bold"),  # Цвет и стиль заголовка графика
      legend.background = element_rect(fill = "#2C2C2C", color = NA),    # Фон легенды
      legend.text = element_text(color = "#F5F5F5"),                    # Цвет текста в легенде
      legend.title = element_text(color = "#F5F5F5"),                   # Цвет заголовка легенды
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5)    # Настройка подписи по оси X
    )
  
  ggplotly(plot)
})







