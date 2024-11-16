library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(readxl)
library(lubridate)
library(tidyverse)
library(zoo)

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

# Группировка тестов
tests <- list(
  "Thyroid" = c("TSH", "TSI", "T3", "FT3", "TT4", "TBG"),
  "Autoimmune" = c("ATA", "ATG", "TG"),
  "Cardiac" = c("ECM", "TXP", "cTL"),
  "Viral" = c("VCG", "CVG", "HVG", "RUB", "RUM"),
  "Other" = c("RTH", "TU", "F4", "VF4", "TK9", "ECP", "SPE", "TIE", "TOP", "SP4",
              "SPG", "FER", "FOL", "EPN", "VB", "INS", "PEP", "GAS", "BMG", "ALB")
)