library(shiny)
library(plotly)

# Подключаем модули
source("modules/ui/parameters_ui.R")
source("modules/ui/graph_ui.R")
source("modules/server/parameters_server.R")
source("modules/server/graph_server.R")

ui <- page_navbar(
  title = "Master Curve Analysis Tool version 1.0",
  nav_panel(
    title = "Master Curve",
    page_sidebar(
      sidebar = sidebar(
        parameters_ui("parameters")  # Подключаем UI для параметров
      ),
      layout_column_wrap(
        width = 1,
        graph_ui("graph")  # Подключаем UI для графика
      )
    )
  )
)

server <- function(input, output, session) {
  parameters <- parameters_server("parameters")  # Server для параметров
  graph_server("graph", parameters)  # Server для графика
}

shinyApp(ui, server)
