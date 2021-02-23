# ANATOMY OF A GGPLOT2 OBJECT ----
# Author : Ralph D. Tasing ----


library(tidyverse)
library(lubridate)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Anatomy of a ggplot ----

# 1.1 How ggplot works ----

# Step 1: Format data ----
revenue_by_year_tbl <- bike_orderlines_tbl %>%
    select(order_date, total_price) %>%
    mutate(year = year(order_date)) %>%
    
    group_by(year) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup()

revenue_by_year_tbl

# Step 2: Plot ----

g <- revenue_by_year_tbl %>%
    
    # Canvas = fix to the context of the graph ----

    #   __aes() => aesthetic, you can put that everywhere globally in the ggplot / or localy in the geom_point() ----

    ggplot(aes(x = year, y = revenue, color = revenue)) +
    
    # Geometries 
    # you can use aes for the input of the geom_function (geom_line(aes(size=3))) ----
    geom_line(size = 1) +
    geom_point(size = 5) +
    geom_smooth(method = "lm", se = FALSE) +
    
    # Formatting
    #   __expand_limits(x = c(2011, 2015)) / y = c(0, 2e22) ----
    #   __scale_color() => custom the color aesthetic ----
    expand_limits(y = 0) +
    scale_color_continuous(low = "red", high = "black",
                           labels = scales::dollar_format(scale = 1/1e6, suffix = "M")) +
    scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, suffix = "M")) +
    labs(
        title = "Revenue",
        subtitle = "Sales are trending up and to the right!",
        x = "",
        y = "Sales (Millions)",
        color = "Rev ($M)",
        caption = "What's happening?\nSales numbers showing year-over-year growth."
    ) +
    #   __theme_bw() => theme black white, theme_bw() + theme() ----
    theme_bw() +
    theme(legend.position = "right", legend.direction = "horizontal")

# 1.2 What is a ggplot? ----

g

View(g)
