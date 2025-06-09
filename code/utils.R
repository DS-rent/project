# Conversion factor: 1 坪 = 3.3058 m²
M2_TO_PING <- 1 / 3.3058
PING_TO_M2 <- 3.3058

MAX_PRICE_PER_PING <- 10000 * PING_TO_M2 # ~33,058 元/坪
MAX_LAND_AREA_PING <- 2000 * M2_TO_PING # ~605 坪

install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat("Installing", pkg, "...\n")
      install.packages(pkg, repos = "https://cran.rstudio.com/", dependencies = TRUE)
      if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        cat("Failed to install", pkg, ". Skipping...\n")
      }
    }
  }
}

# Core packages
core_packages <- c("shiny", "shinydashboard", "ggplot2", "dplyr", "plotly", "DT", "caretEnsemble", "shinyjs", "leaflet", "lubridate")
install_and_load(core_packages)

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
  
  # Proactively filter for valid date formats to prevent coercion warnings
  df <- df %>%
    filter(grepl("^[0-9]{7}$", rent_date))

  # 處理日期格式
  df <- df %>%
    mutate(
      converted_date = as.Date(
        paste0(
          as.numeric(substr(rent_date, 1, 3)) + 1911, # 年份 + 1911
          substr(rent_date, 4, 7)
        ), # 月日部分
        format = "%Y%m%d"
      )
    ) %>%
    # Filter out rows where date conversion resulted in NA
    filter(!is.na(converted_date))

  # df$converted_date <- tryCatch({
  #   as.Date(as.character(df$converted_date), format = "%Y%m%d")
  # }, error = function(e) {
  #   as.Date(NA)
  # })


  # 清理數值欄位
  df$land_area_m2 <- as.numeric(gsub("[^0-9.]", "", as.character(df$land_area_m2)))
  df$price_per_m2 <- as.numeric(gsub("[^0-9.]", "", as.character(df$price_per_m2)))

  # 移除異常值 (使用原始m²單位進行篩選以保持與原始資料的兼容性)
  df <- df %>%
    filter(
      !is.na(price_per_m2),
      !is.na(land_area_m2),
      price_per_m2 > 0,
      land_area_m2 > 0,
      price_per_m2 < 10000, # 原始閾值
      land_area_m2 < 2000 # 原始閾值
    )

  # 轉換為坪單位
  df <- df %>%
    mutate(
      land_area_ping = land_area_m2 * M2_TO_PING,
      price_per_ping = price_per_m2 * PING_TO_M2
    )

  # 標準化文字欄位
  df$district <- trimws(as.character(df$district))
  df$building_type <- trimws(as.character(df$building_type))
  df$floor <- trimws(as.character(df$floor))

  # 移除空值行
  df <- df %>%
    filter(
      !is.na(district),
      !is.na(building_type),
      !is.na(floor),
      district != "",
      building_type != "",
      floor != ""
    )

  return(df)
}
