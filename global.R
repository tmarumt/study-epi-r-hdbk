# global.R スクリプト

library(tidyverse)
library(lubridate)
library(shiny)

# データの読み込み
malaria_data <- rio::import(here::here("data", "malaria_facility_count_data.rds")) %>%
  as_tibble()

# データを前処理し縦持ちデータへ変換する
malaria_data <- malaria_data %>%
  select(-newid) %>%
  pivot_longer(cols = starts_with("malaria_"), names_to = "age_group", values_to = "cases_reported")

source(here::here("funcs", "plot_epicurve.R"), local = TRUE)
