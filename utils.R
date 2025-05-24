library(dplyr)
library(lubridate)

preprocess <- function(df) {
    df <- df %>%
        rename(
            district = 鄉鎮市區,
            land_area_m2 = 土地面積平方公尺,
            rent_date = 租賃年月日,
            floor = 租賃層次,
            building_type = 建物型態,
            price_per_m2 = 單價元平方公尺
        ) %>%
        select(district, land_area_m2, rent_date, floor, building_type, price_per_m2)

    # 處理民國年格式，例如 1120504 → 20230504
    df$rent_date <- as.character(df$rent_date)
    df$rent_date <- ifelse(
        grepl("^1\\d{6}$", df$rent_date),
        paste0(as.numeric(substr(df$rent_date, 1, 3)) + 1911, substr(df$rent_date, 4, 7)),
        NA
    )
    df$rent_date <- ymd(df$rent_date)

    df$land_area_m2 <- as.numeric(gsub("[^0-9.]", "", df$land_area_m2))
    df$price_per_m2 <- as.numeric(gsub("[^0-9.]", "", df$price_per_m2))

    # print(str(df))
    return(df)
}
