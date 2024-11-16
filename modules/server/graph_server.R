graph_server <- function(id, parameters) {
  moduleServer(id, function(input, output, session) {
    output$mcd_plot <- renderPlotly({
      req(parameters())
      
      # Пример генерации данных для графика
      data <- data.frame(
        Concentration = seq(parameters()$conc_min, parameters()$conc_max, length.out = 100),
        CPS = parameters()$P1 / (1 + parameters()$P2 * exp(-parameters()$P3 * parameters()$P4))
      )
      
      gg <- ggplot(data, aes(x = Concentration, y = CPS)) +
        geom_line() +
        labs(title = "Master Curve")
      
      ggplotly(gg)
    })
  })
}
