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

# Обработка математических операций и форматирование
library(glue)
library(magrittr)

conflicted::conflicts_prefer(dplyr::filter)

library(DBI)
library(odbc)
library(rsconnect)

MAX_FILE_SIZE <- options(shiny.maxRequestSize = 200 * 1024^2)  # 200 MB

options(shiny.maxRequestSize = 200 * 1024^2)  # 200 MB

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
  "btn-font-size" = "0.8rem",         # Размер шрифта для кнопок
  "card-title-font-size" = "1rem",    # Размер шрифта заголовков карточек
  "navbar-font-size" = "1rem",        # Размер шрифта в навигационной панели
  "table-font-size" = "0.8rem"        # Размер шрифта в таблицах
)
