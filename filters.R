# Dynamic Test Selection
dynamic_test_select <- function(input, rv_results, rv_calib) {
  renderUI({
    req(rv_results$raw_data, rv_calib$raw_data)
    
    # Определяем пересекающиеся тесты
    common_tests <- intersect(
      unique(rv_results$raw_data$Reagent_Name),
      unique(rv_calib$raw_data$Test_Name)
    )
    
    # Проверяем, есть ли общие тесты
    if (length(common_tests) == 0) {
      common_tests <- c("No common tests available")
    }
    
    selectInput(
      "selected_test",
      "Select Test:",
      choices = common_tests,
      selected = NULL,
      multiple = FALSE
    )
  })
}

# Dynamic Laboratory Selection
dynamic_lab_select <- function(input, rv_results, rv_calib) {
  renderUI({
    req(rv_results$raw_data, rv_calib$raw_data)
    
    # Определяем пересекающиеся лаборатории
    common_labs <- intersect(
      unique(rv_results$raw_data$id_lab),
      unique(rv_calib$raw_data$Lab_ID)
    )
    
    if (length(common_labs) == 0) {
      common_labs <- c("No common laboratories available")
    }
    
    selectInput(
      "selected_lab",
      "Select Laboratory:",
      choices = common_labs,
      selected = NULL,
      multiple = FALSE
    )
  })
}

# Dynamic Kit Lot Selection
dynamic_kitlot_select <- function(input, rv_results, rv_calib) {
  renderUI({
    req(rv_results$raw_data, rv_calib$raw_data)
    
    # Определяем пересекающиеся лоты
    common_kitlots <- intersect(
      unique(rv_results$raw_data$Kit_Lot),
      unique(rv_calib$raw_data$Kit_Lot)
    )
    
    if (length(common_kitlots) == 0) {
      common_kitlots <- c("No common kit lots available")
    }
    
    selectInput(
      "selected_kitlot",
      "Select Kit Lot:",
      choices = common_kitlots,
      selected = NULL,
      multiple = FALSE
    )
  })
}

# Reactive filtered results
filtered_results <- function(input, rv_results) {
  reactive({
    req(rv_results$raw_data, input$selected_test, input$selected_lab, input$selected_kitlot)
    
    rv_results$raw_data %>%
      filter(
        Reagent_Name == input$selected_test,
        id_lab == input$selected_lab,
        Kit_Lot == input$selected_kitlot
      )
  })
}

# Reactive filtered calibrations
filtered_calibrations <- function(input, rv_calib) {
  reactive({
    req(rv_calib$raw_data, input$selected_test, input$selected_lab, input$selected_kitlot)
    
    rv_calib$raw_data %>%
      filter(
        Test_Name == input$selected_test,
        Lab_ID == input$selected_lab,
        Kit_Lot == input$selected_kitlot
      )
  })
}
