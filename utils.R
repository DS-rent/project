# Install and load required packages
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
core_packages <- c("shiny", "shinydashboard", "ggplot2", "dplyr", "plotly", "DT")
install_and_load(core_packages)

install_optional_package <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    tryCatch({
      suppressWarnings(install.packages(pkg, repos = "https://cran.rstudio.com/", dependencies = TRUE, quiet = TRUE))
      suppressMessages(require(pkg, character.only = TRUE, quietly = TRUE))
    }, error = function(e) {
      
    })
  }
}

install_optional_package("leaflet")
install_optional_package("lubridate")

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

  # 處理日期格式
  df$rent_date <- tryCatch({
    as.Date(as.character(df$rent_date), format = "%Y%m%d")
  }, error = function(e) {
    as.Date(NA)
  })
  
  # 清理數值欄位
  df$land_area_m2 <- as.numeric(gsub("[^0-9.]", "", as.character(df$land_area_m2)))
  df$price_per_m2 <- as.numeric(gsub("[^0-9.]", "", as.character(df$price_per_m2)))
  
  # 移除異常值
  df <- df %>%
    filter(
      !is.na(price_per_m2),
      !is.na(land_area_m2),
      price_per_m2 > 0,
      land_area_m2 > 0,
      price_per_m2 < 10000,  # 排除異常高價
      land_area_m2 < 2000    # 排除異常大面積
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
