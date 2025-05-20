library(dplyr)
library(ggplot2)

prepocess <- function(input_file) {
    # rename columns
    df <- input_file %>%
        rename(
            district = 鄉鎮市區,
            land_area_m2 = 土地面積平方公尺,
            rent_date = 租賃年月日,
            floor = 租賃層次,
            building_type = 建物型態,
            price_per_m2 = 單價元平方公尺
        )
    df$rent_date <- as.Date(as.character(df$rent_date), format = "%Y%m%d")

    df$land_area_m2 <- as.numeric(df$land_area_m2)
    df$price_per_m2 <- as.numeric(df$price_per_m2)

}

main <- function(input_file) {
    df <- read.csv(input_file)
    df <- prepocess(df)
    print(head(df))
}
