server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30*1024^2)
  
                                                                                            # Цвета Siemens Healthineers
  sh_colors <- list(
    petrol = "#009999",    
    magenta = "#E31C79",   
    orange = "#FF8C00",    
    gray = "#808080",      
    light_gray = "#F5F5F5", 
    dark = "#1A1A1A",      
    dark_gray = "#2C2C2C"  
  )
 
 
   
                                                                                            # Чтение данных
  results_data <- reactive({
    req(input$results_file)
    read_excel(input$results_file$datapath, col_types = "text") %>%
      rename_with(tolower) %>%
      select(unique_record_id_num, identifier, test_type, kit_lot, 
             date_and_time_started, cps, result) %>%
      mutate(
        date_and_time_started = ymd_hms(date_and_time_started),
        date = as.Date(date_and_time_started),
        cps = as.numeric(cps),
        result = as.numeric(result)
      )
  })

                                                                                             # График ежедневных тестов
  output$daily_tests_plot <- renderPlotly({
    req(results_data(), input$test_select)
    
    daily_data <- results_data() %>%
      filter(test_type == input$test_select) %>%
      count(date, kit_lot, name = "test_count")
    
    plot_ly(daily_data, x = ~date, y = ~test_count, color = ~kit_lot,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = paste("Daily Tests:", input$test_select),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Number of Tests"),
        paper_bgcolor = sh_colors$dark,
        plot_bgcolor = sh_colors$dark,
        font = list(color = sh_colors$light_gray)
      )
  })
  
                                                                                             # График скользящего среднего
  output$rolling_mean_plot <- renderPlotly({
    req(results_data(), input$test_select)
    
    rolling_data <- results_data() %>%
      filter(test_type == input$test_select) %>%
      arrange(result) %>%
      group_by(kit_lot) %>%
      mutate(
        rolling_mean = rollmean(result, k = 10, fill = NA, align = "right"),
        index = row_number()
      ) %>%
      filter(!is.na(rolling_mean))
    
    plot_ly(rolling_data, x = ~index, y = ~rolling_mean, color = ~kit_lot,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(
        title = "Rolling Mean (10 concentrations)",
        xaxis = list(title = "Index"),
        yaxis = list(title = "Mean Concentration"),
        paper_bgcolor = sh_colors$dark,
        plot_bgcolor = sh_colors$dark,
        font = list(color = sh_colors$light_gray)
      )
  })
  
                                                                                             # График результатов тестов
  output$test_summary_plot <- renderPlotly({
    req(results_data(), input$test_select)
    
    plot_data <- results_data() %>%
      filter(test_type == input$test_select)
    
    # Создаем график с помощью ggplot2
    p <- ggplot(plot_data, aes(x = date, y = result), color = kit_lot) +
      geom_point() +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = sh_colors$dark),
        plot.background = element_rect(fill = sh_colors$dark),
        text = element_text(color = sh_colors$light_gray),
        axis.text = element_text(color = sh_colors$light_gray),
        panel.grid = element_line(color = sh_colors$light_gray)
      ) +
      geom_smooth(method = 'loess', color = 'darkgray')+
      labs(
        title = "Test Results Over Time",
        x = "Date",
        y = "Result"
      )
    
    # Преобразуем ggplot в plotly
    ggplotly(p) %>%
      layout(
        paper_bgcolor = sh_colors$dark,
        plot_bgcolor = sh_colors$dark
      )
  })
  
                                                                                                      # Таблица статистики
  output$statistics_table <- renderTable({
    req(results_data(), input$test_select)
    
    results_data() %>%
      filter(test_type == input$test_select) %>%
      group_by(kit_lot) %>%
      summarise(
        Tests = n(),
        Avg_CPS = round(mean(cps, na.rm = TRUE), 2),
        Avg_Result = round(mean(result, na.rm = TRUE), 2),
        SD = round(sd(result, na.rm = TRUE), 2),
        CV = round(sd(result, na.rm = TRUE) / mean(result, na.rm = TRUE) * 100, 2)
      )
  })

  # Функция для расчета 4PL модели (сигмоидная кривая)
  model_mcd_cps <- function(conc, P1, P2, P3, P4) {
    # P1 - верхняя асимптота (максимальный сигнал)
    # P2 - EC50 (точка перегиба)
    # P3 - нижняя асимптота (минимальный сигнал)
    # P4 - наклон кривой (крутизна)
    
    # Правильная формула 4PL модели
    P2 + (P1 - P2) / (1 + (conc/P3)^P4)
  }
  
  # Реактивные данные для Master Curve
  plot_data <- reactive({
    # Простая линейная генерация точек
    conc_sim <- seq(from = input$conc_min, to = input$conc_max, length.out = 1000)
    
    # Calculate CPS values using the 4PL model
    cps <- model_mcd_cps(
      conc = conc_sim,
      P1 = input$P1,
      P2 = input$P2,
      P3 = input$P3,
      P4 = input$P4
    )
    
    # Return data frame
    data.frame(
      concentration = conc_sim,
      cps = cps
    )
  })
  
  output$mcd_plot <- renderPlotly({
    # Генерируем последовательность концентраций
    # Используем логарифмическую последовательность для более равномерного распределения точек
    if(input$x_scale == "log") {
      conc_seq <- 10^(seq(log10(input$conc_min), log10(input$conc_max), length.out = 1000))
    } else {
      conc_seq <- seq(from = input$conc_min, to = input$conc_max, length.out = 1000)
    }
    
    # Рассчитываем MCD CPS для каждой концентрации
    mcd_cps <- sapply(conc_seq, function(x) model_mcd_cps(x, input$P1, input$P2, input$P3, input$P4))
    
    # Рассчитываем лабораторные CPS используя slope и intercept
    lab_cps <- (mcd_cps - intercept_value()) / slope_value()
    
    # Рассчитываем границы диапазона используя slope = 0.5 и 1.8 с guide intercept
    lower_bound <- (mcd_cps - input$intercept_guide) / 0.5
    upper_bound <- (mcd_cps - input$intercept_guide) / 1.8
    
    # Создаем data frame для построения
    plot_data <- data.frame(
      Concentration = conc_seq,
      MCD_CPS = mcd_cps,
      Lab_CPS = lab_cps,
      Lower = lower_bound,
      Upper = upper_bound
    )
    
    # Создаем график
    p <- plot_ly() %>%
      # Добавляем диапазон
      add_ribbons(
        data = plot_data,
        x = ~Concentration,
        ymin = ~Lower,
        ymax = ~Upper,
        fillcolor = 'rgba(200, 200, 200, 0.2)',
        line = list(width = 0),
        showlegend = FALSE,
        name = 'Range'
      ) %>%
      # Добавляем основные кривые
      add_trace(
        data = plot_data,
        x = ~Concentration,
        y = ~MCD_CPS,
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#00FF00', width = 2),
        name = 'MCD'
      ) %>%
      add_trace(
        data = plot_data,
        x = ~Concentration,
        y = ~Lab_CPS,
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#FF8C00', width = 2),
        name = 'Laboratory'
      ) %>%
      layout(
        xaxis = list(
          title = "Concentration (ng/mL)",
          type = if(input$x_scale == "log") "log" else "linear",
          gridcolor = sh_colors$dark_gray,
          zerolinecolor = sh_colors$dark_gray,
          # Добавляем форматирование меток оси
          tickformat = if(input$x_scale == "log") ".1e" else "",
          exponentformat = "power"
        ),
        yaxis = list(
          title = "CPS",
          gridcolor = sh_colors$dark_gray,
          zerolinecolor = sh_colors$dark_gray
        ),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        showlegend = TRUE,
        legend = list(
          x = 0.1,
          y = 0.9,
          bgcolor = 'rgba(0,0,0,0)',
          font = list(color = sh_colors$light_gray)
        ),
        font = list(color = sh_colors$light_gray)
      )
    
    p
  })
                                                                                    # Calculators
  
  # Calculators
  
  # Функция для расчета MCD CPS по концентрации
  model_mcd_cps <- function(conc, P1, P2, P3, P4) {
    # P1 - CPS max
    # P2 - CPS min
    # P3 - EC50 (концентрация при 50% ответа)
    # P4 - наклон
    P2 + (P1 - P2) / (1 + (conc/P3)^P4)
  }
  
  # Функция для расчета концентрации по MCD CPS
  inverse_model_cps <- function(cps, P1, P2, P3, P4) {
    # P1 - CPS max
    # P2 - CPS min
    # P3 - EC50
    # P4 - наклон
    P3 * ((P1 - P2)/(cps - P2) - 1)^(1/P4)
  }
  
                                                                                                             # Калькуляторы
  
  # Создаем реактивные значения для slope и intercept
  slope_value <- reactive({
    (input$adg_high_sh - input$adg_low_sh) / (input$adg_high_lab - input$adg_low_lab)
  })
  
  intercept_value <- reactive({
    input$adg_low_sh - (slope_value() * input$adg_low_lab)
  })
  
  # Перевод лабораторного CPS в MCD CPS
  output$calculated_mcd_cps <- renderText({
    req(input$input_lab_cps)
    lab_cps <- as.numeric(input$input_lab_cps)
    validate(need(!is.na(lab_cps), "Please enter a valid number"))
    
    # Используем реактивные значения slope и intercept
    mcd_cps <- lab_cps * slope_value() + intercept_value()
    sprintf("%.0f CPS", mcd_cps)
  })
  
  # Расчет концентрации из лабораторного CPS
  output$calculated_concentration_from_lab <- renderText({
    req(input$input_lab_cps)
    lab_cps <- as.numeric(input$input_lab_cps)
    validate(need(!is.na(lab_cps), "Please enter a valid number"))
    
    # Используем реактивные значения slope и intercept
    mcd_cps <- lab_cps * slope_value() + intercept_value()
    conc <- inverse_model_cps(mcd_cps, input$P1, input$P2, input$P3, input$P4)
    sprintf("%.6f ng/mL", conc)
  })
  
  # Расчет MCD CPS по концентрации
  output$calculated_mcd_cps_direct <- renderText({
    req(input$input_concentration)
    conc <- as.numeric(input$input_concentration)
    validate(need(!is.na(conc), "Please enter a valid number"))
    
    mcd_cps <- model_mcd_cps(conc, input$P1, input$P2, input$P3, input$P4)
    sprintf("%.0f CPS", mcd_cps)
  })
  
  # Расчет лабораторного CPS
  output$calculated_lab_cps <- renderText({
    req(input$input_concentration)
    conc <- as.numeric(input$input_concentration)
    validate(need(!is.na(conc), "Please enter a valid number"))
    
    mcd_cps <- model_mcd_cps(conc, input$P1, input$P2, input$P3, input$P4)
    # Используем реактивные значения slope и intercept
    lab_cps <- (mcd_cps - intercept_value()) / slope_value()
    sprintf("%.0f CPS", lab_cps)
  })
  
  # Выводим значения slope и intercept (для проверки)
  output$calculated_slope <- renderText({
    sprintf("%.6f", slope_value())
  })
  
  output$calculated_intercept <- renderText({
    sprintf("%.2f", intercept_value())
  })
}