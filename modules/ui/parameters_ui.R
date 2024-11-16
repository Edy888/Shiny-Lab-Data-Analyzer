parameters_ui <- function(id) {
  ns <- NS(id) # Создаем функцию для namespace
  sidebar(
    card(
      card_header(
        "Parameters",
        class = "bg-primary text-white"
      ),
      div(
        style = "padding: 10px;",
        div(
          style = "display: flex; flex-direction: column; gap: 3px;",
          numericInput(ns("P1"), "P1:", value = 3.404367e+19),
          numericInput(ns("P2"), "P2:", value = 61440),
          numericInput(ns("P3"), "P3:", value = 1.280082e+14),
          numericInput(ns("P4"), "P4:", value = -0.9975617),
          hr(),
          numericInput(ns("conc_min"), "Min Conc:", value = 0.1, min = 0.001),
          numericInput(ns("conc_max"), "Max Conc:", value = 50000, min = 1),
          hr(),
          numericInput(ns("adg_low_sh"), "Adjustor_Low_MCD, CPS", value = 711703),
          numericInput(ns("adg_high_sh"), "Adjustor_High_MCD, CPS", value = 10993840),
          numericInput(ns("adg_low_lab"), "Adjustor_Low_Lab, CPS", value = 777867),
          numericInput(ns("adg_high_lab"), "Adjustor_High_Lab, CPS", value = 12597374),
          numericInput(ns("intercept_guide"), "Guide Intercept", value = 213510.891)
        )
      )
    ),
    width = "300px",
    class = "shadow-sm"
  )
}
