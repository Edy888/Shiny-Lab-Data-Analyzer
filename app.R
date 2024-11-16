server <- function(input, output, session) {
  
  # Расчет данных для графика Master Curve
  plot_data <- reactive({
    validate(
      need(input$conc_max > input$conc_min, "Максимальная концентрация должна быть больше минимальной")
    )
    
    # Генерация последовательности концентраций
    if(input$x_scale == "log") {
      conc_sim <- 10^seq(log10(input$conc_min), log10(input$conc_max), length.out = 500)
    } else {
      conc_sim <- seq(input$conc_min, input$conc_max, length.out = 500)
    }
    
    # Расчет значений по модели
    rlu_sim <- input$P3 + (input$P1 - input$P3) / (1 + (conc_sim / input$P2)^input$P4)
    rlu_lab <- (rlu_sim - input$intercept) / input$slope
    rlu_low <- 0.5 * rlu_sim + input$intercept
    rlu_high <- 1.8 * rlu_sim + input$intercept
    
    data.frame(conc_sim, rlu_sim, rlu_lab, rlu_low, rlu_high)
  })
  
  # Расчет диапазона значений по X
  x_range_values <- reactive({
    if(input$x_scale == "log") {
      c(floor(log10(input$conc_min)), ceiling(log10(input$conc_max)))
    } else {
      c(0, ceiling(input$conc_max))
    }
  })
  
  # UI для диапазона X
  output$x_range_ui <- renderUI({
    range_vals <- x_range_values()
    sliderInput("x_range", "Диапазон оси X:",
                min = range_vals[1], max = range_vals[2],
                value = range_vals,
                step = if(input$x_scale == "log") 0.1 else 100)
  })
  
  # Расчет диапазона значений по Y
  y_range_limits <- reactive({
    req(plot_data())
    df <- plot_data()
    
    y_min <- min(df$rlu_low, na.rm = TRUE)
    y_max <- max(df$rlu_high, na.rm = TRUE)
    
    padding <- (y_max - y_min) * 0.1
    c(max(0, floor(y_min - padding)), ceiling(y_max + padding))
  })
  
  # UI для диапазона Y
  output$y_range_ui <- renderUI({
    limits <- y_range_limits()
    sliderInput("y_range", "Диапазон оси Y:",
                min = limits[1], max = limits[2],
                value = limits,
                step = (limits[2] - limits[1]) / 100)
  })
  
  # Рендеринг основного графика MCD
  output$mcd_plot <- renderPlot({
    req(input$x_range, input$y_range, plot_data())
    df_sim <- plot_data()
    
    # Определение переменной X в зависимости от выбранной шкалы
    x_var <- if(input$x_scale == "log") log10(df_sim$conc_sim) else df_sim$conc_sim
    x_lab <- if(input$x_scale == "log") "log10 Concentration" else "Concentration"
    
    # Создание графика
    p <- ggplot(df_sim, aes(x = x_var)) +
      geom_ribbon(aes(ymin = rlu_low, ymax = rlu_high),
                  fill = sh_colors$magenta, alpha = 0.1) +
      geom_line(aes(y = rlu_sim), linewidth = 1, color = sh_colors$petrol) +
      geom_line(aes(y = rlu_lab), linewidth = 1, color = sh_colors$orange) +
      labs(x = x_lab, y = "CPS") +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = sh_colors$dark_gray, color = NA),
        plot.background = element_rect(fill = sh_colors$dark_gray, color = NA),
        panel.grid = element_line(color = sh_colors$gray),
        text = element_text(color = sh_colors$light_gray),
        axis.text = element_text(color = sh_colors$light_gray)
      ) +
      coord_cartesian(xlim = input$x_range, ylim = input$y_range) +
      geom_vline(xintercept = if(input$x_scale == "log") log10(200) else 200,
                 color = sh_colors$orange, linetype = "dashed") +
      geom_vline(xintercept = if(input$x_scale == "log") log10(1) else 1,
                 color = sh_colors$orange, linetype = "dashed")
    
    print(p)  # Явный вывод графика
  })
  
  # Обработка загруженных данных для второй вкладки
  results_data <- reactive({
    req(input$results_file)
    results <- read_excel(input$results_file$datapath, col_types = "text") %>%
      rename_with(tolower) %>%
      select(unique_record_id_num, identifier, test_type, kit_lot, 
             date_and_time_started, cps, result) %>%
      mutate(
        date_and_time_started = ymd_hms(date_and_time_started),
        date = as.Date(date_and_time_started),
        cps = as.numeric(cps),
        result = as.numeric(result)
      )
    return(results)
  })
  
  # График ежедневных тестов
  output$daily_tests_plot <- renderPlotly({
    req(results_data(), input$test_select)
    
    daily_data <- results_data() %>%
      filter(test_type == input$test_select) %>%
      group_by(date, kit_lot) %>%
      summarise(
        test_count = n(),
        .groups = 'drop'
      )
    
    p <- ggplot(daily_data, aes(x = date, y = test_count, color = kit_lot)) +
      geom_point(size = 3) +
      geom_line() +
      theme_minimal() +
      labs(
        title = paste("Daily Test Count for", input$test_select),
        x = "Date",
        y = "Number of Tests",
        color = "Kit Lot"
      ) +
      theme(
        panel.background = element_rect(fill = sh_colors$dark_gray),
        plot.background = element_rect(fill = sh_colors$dark_gray),
        text = element_text(color = sh_colors$light_gray),
        axis.text = element_text(color = sh_colors$light_gray),
        panel.grid = element_line(color = sh_colors$gray)
      )
    
    ggplotly(p) %>% 
      layout(
        plot_bgcolor = sh_colors$dark_gray,
        paper_bgcolor = sh_colors$dark_gray,
        font = list(color = sh_colors$light_gray)
      )
  })
  
  # Сводная таблица
  output$test_summary <- renderTable({
    req(results_data(), input$test_select)
    
    results_data() %>%
      filter(test_type == input$test_select) %>%
      group_by(kit_lot) %>%
      summarise(
        Total_Tests = n(),
        Avg_CPS = mean(cps, na.rm = TRUE),
        Avg_Result = mean(result, na.rm = TRUE)
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}