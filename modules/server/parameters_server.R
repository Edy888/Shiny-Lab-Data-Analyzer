parameters_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        P1 = input$P1,
        P2 = input$P2,
        P3 = input$P3,
        P4 = input$P4,
        conc_min = input$conc_min,
        conc_max = input$conc_max,
        adg_low_sh = input$adg_low_sh,
        adg_high_sh = input$adg_high_sh,
        adg_low_lab = input$adg_low_lab,
        adg_high_lab = input$adg_high_lab,
        intercept_guide = input$intercept_guide
      )
    })
  })
}
