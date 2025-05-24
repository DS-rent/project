### here is shinyApp server, please put your render fuction here ###

library(shiny)
library(ggplot2)
library(dplyr)

source("utils.R")

server <- function(input, output) {
    # data prepocessing
    data <- reactive({
        df <- read.csv("data/MOI_rent.csv", fileEncoding = "UTF-8-BOM")
        preprocess(df)
    })

    # plot total avg price
    output$histPlot <- renderPlot({
        df <- data()
        ggplot(df %>% filter(!is.na(price_per_m2)), aes(x = price_per_m2)) +
            geom_histogram(binwidth = 50, fill = "skyblue", color = "white") +
            labs(title = "租金單價分佈", x = "元 / 平方公尺", y = "數量")
    })

    # plot each district price
    output$avgPlot <- renderPlot({
        df <- data()
        df %>%
            filter(!is.na(price_per_m2)) %>%
            group_by(district) %>%
            summarise(avg_price = mean(price_per_m2)) %>%
            ggplot(aes(x = reorder(district, avg_price), y = avg_price)) +
            geom_col(fill = "lightgreen") +
            coord_flip() +
            labs(title = "各行政區平均租金單價", x = "行政區", y = "元 / 平方公尺")
    })

    # model argu selected
    observe({
        df <- data()
        updateSelectInput(
            inputId = "sel_district",
            choices = unique(df$district),
            selected = unique(df$district)[1]
        )
        updateSelectInput(
            inputId = "sel_type",
            choices = unique(df$building_type),
            selected = unique(df$building_type)[1]
        )
    })

    # print predict model text
    output$modelPredictText <- renderText({
        df <- data()

        model <- lm(price_per_m2 ~ land_area_m2 + district + building_type, data = df)

        newdata <- data.frame(
            land_area_m2 = input$input_area,
            district = input$sel_district,
            building_type = input$sel_type
        )

        pred <- predict(model, newdata = newdata)

        paste0(
            "在 ", input$sel_district,
            input$input_area, " 平方公尺的",
            "，約為", round((input$input_area) / 3.3058, 0), "坪的",
            input$sel_type,
            "，預測平均租金單價約為 ", round(pred * 3.3058, 1), " 元 / 坪。" # 平方公尺轉成坪
        )
    })

    output$modelPredictPlot <- renderPlot({
        df <- data()

        df$district <- as.factor(df$district)
        df$building_type <- as.factor(df$building_type)

        model <- lm(price_per_m2 ~ land_area_m2 + district + building_type, data = df)

        all_combos <- expand.grid(
            district = levels(df$district),
            building_type = levels(df$building_type),
            stringsAsFactors = FALSE
        )
        all_combos$land_area_m2 <- 50

        all_combos$district <- factor(all_combos$district, levels = levels(df$district))
        all_combos$building_type <- factor(all_combos$building_type, levels = levels(df$building_type))

        all_combos$predicted_price <- predict(model, newdata = all_combos)
        # sort with value
        building_order <- all_combos %>%
            group_by(building_type) %>%
            summarise(avg_pred = mean(predicted_price)) %>%
            arrange(desc(avg_pred)) %>%
            pull(building_type)

        all_combos$building_type <- factor(all_combos$building_type, levels = building_order)
        ggplot(all_combos, aes(x = building_type, y = district, fill = predicted_price * 3.3058)) +
            geom_tile(color = "white") +
            scale_fill_gradient(low = "lightyellow", high = "red") +
            labs(
                title = "預測租金單價熱力圖（面積 50㎡）",
                x = "建物型態",
                y = "行政區",
                fill = "預測租金\n元/ 坪"
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    # View dataset
    output$dataSummary <- renderText({
        df <- data()

        # district
        district_count <- df %>%
            group_by(district) %>%
            summarise(count = n()) %>%
            arrange(desc(count))

        district_text <- paste0(
            "行政區資料數：\n",
            paste0(district_count$district, "資料數: ", district_count$count, " 筆", collapse = "\n")
        )

        # type
        type_count <- df %>%
            group_by(building_type) %>%
            summarise(count = n()) %>%
            arrange(desc(count))

        type_text <- paste0(
            "\n\n房型資料數：\n",
            paste0(type_count$building_type, "資料數: ", type_count$count, " 筆", collapse = "\n")
        )

        paste0(district_text, type_text)
    })
}
