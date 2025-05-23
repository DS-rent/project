library(shiny)
library(ggplot2)
library(dplyr)

source("utils.R")

preprocess <- function(df) {
  df <- df %>%
    rename(
      district = 鄉鎮市區,
      land_area_m2 = 土地面積平方公尺,
      rent_date = 租賃年月日,
      floor = 租賃層次,
      building_type = 建物型態,
      price_per_m2 = 單價元平方公尺
    )

  df$rent_date <- as.Date(as.character(df$rent_date), format = "%Y%m%d")
  df$land_area_m2 <- as.numeric(gsub("[^0-9.]", "", df$land_area_m2))
  df$price_per_m2 <- as.numeric(gsub("[^0-9.]", "", df$price_per_m2))

  return(df)
}