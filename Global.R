# Основные библиотеки для работы с Shiny
library(shiny)
library(bslib)

# Визуализация данных
library(ggplot2)
library(plotly)

# Обработка данных
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(tidyverse)

# Работа с временными рядами
library(zoo)
library(xts)

# Работа с таблицами в интерфейсе
library(DT)

# Работа с файлами Excel
library(readxl)
library(openxlsx)

# Удобство работы со строками
library(stringr)

# Форматирование чисел и осей графиков
library(scales)

# Полезные утилиты для очистки данных
library(janitor)

# Управление конфликтами функций
library(conflicted)

# Дополнения для интерфейса Shiny
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(shinyFeedback)

# Обработка математических операций и форматирование
library(glue)
library(magrittr)

library(pool)

conflicted::conflicts_prefer(dplyr::filter)


library(colorspace)


# Цвета Siemens Healthineers
magenta <- "#E31C79"  # Исходный цвет

magenta_rgb <- col2rgb(magenta)

# Цвета Siemens Healthineers
sh_colors <- list(
  petrol = "#009999",    
  magenta = magenta,  
  less_bright_magenta <- lighten(darken(magenta, amount = 0.2), amount = 0.1),
  transparent_magenta <- scales::alpha(magenta, 0.7),
  less_bright_rgb <- magenta_rgb * 0.8,
  orange = "#FF8C00",    
  gray = "#808080",      
  light_gray = "#F5F5F5", 
  dark = "#1A1A1A",      
  dark_gray = "#2C2C2C"  
)

# Обновленная тема с настройками размера шрифта
my_theme <- bs_theme(
  bg = sh_colors$dark,
  fg = sh_colors$light_gray,
  primary = sh_colors$petrol,
  secondary = sh_colors$magenta,
  success = sh_colors$orange,
  "input-border-color" = sh_colors$petrol,
  "input-bg" = sh_colors$dark_gray,
  "input-color" = sh_colors$light_gray,
  "card-bg" = sh_colors$dark_gray,
  
  # Добавляем настройки размера шрифта
  base_font_size = "0.875rem",        # Базовый размер шрифта
  "input-font-size" = "0.8rem",       # Размер шрифта для input элементов
  "btn-font-size" = "1rem",         # Размер шрифта для кнопок
  "card-title-font-size" = "1.4rem",    # Размер шрифта заголовков карточек
  "navbar-font-size" = "1.4rem",        # Размер шрифта в навигационной панели
  "table-font-size" = "0.8rem"        # Размер шрифта в таблицах
)

# Группировка тестов
tests <- list(
  "Thyroid" = c("TSH", "TSI", "T3", "FT3", "TT4", "TBG"),
  "Autoimmune" = c("ATA", "ATG", "TG"),
  "Cardiac" = c("ECM", "TXP", "cTL"),
  "Viral" = c("VCG", "CVG", "HVG", "RUB", "RUM"),
  "Other" = c("RTH", "TU", "F4", "VF4", "TK9", "ECP", "SPE", "TIE", "TOP", "SP4",
              "SPG", "FER", "FOL", "EPN", "VB", "INS", "PEP", "GAS", "BMG", "ALB")
)




# Глобальная таблица type_of_reaction
type_of_reaction_global <- reactiveVal(data.frame(
  Test = character(),
  `Formula Number` = integer(),
  Type = character(),
  stringsAsFactors = FALSE
))

