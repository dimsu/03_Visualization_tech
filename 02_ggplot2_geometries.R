# Types of Graphs: ggplot2 Geometries ----
# Author : Ralph D. Tasing ----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Point / Scatter Plots ----
# - Great for Continuous vs Continuous
# - Also good for Lollipop Charts (more on this in advanced plots)

# Goal: Explain relationship between order value and quantity of bikes sold

# Data Manipulation
order_value_tbl <- bike_orderlines_tbl %>%
    select(order_id, order_line, total_price, quantity) %>%
    group_by(order_id) %>%
    summarise(
        total_quantity = sum(quantity), 
        total_price    = sum(total_price)) %>%
    ungroup()

# Scatter Plot

#   __aes() => aesthetics only use when you map on a column ----
order_value_tbl %>% 
    ggplot(aes(x = total_quantity, y = total_price)) +
    
    geom_point(alpha = 0.5, size = 2) + 
    geom_smooth(method = "lm", se = FALSE)

# 2.0 Line Plots ----
# - Great for time series

# Goal: Describe revenue by Month, expose cyclic nature

# Data Manipulation
revenue_by_month_tbl <-  bike_orderlines_tbl %>% 
    select(order_date, total_price) %>%
    
    mutate(year_month = floor_date(order_date, "month") %>% ymd()) %>%
    
    group_by(year_month) %>%
    
    summarise(revenue = sum(total_price)) %>% ungroup()
    
# Line Plot
#   __geom_line() => draw a line 
revenue_by_month_tbl %>% ggplot(aes(year_month, revenue)) + 
    geom_line(size = 0.5, linetype = 1) + 
    geom_smooth(method =  "loess", span = 0.2)


# 3.0 Bar / Column Plots ----
# - Great for categories

# Goal: Sales by Descriptive Category

# Data Manipulation
revenue_by_category_2_tbl <- bike_orderlines_tbl %>% select(category_2, total_price) %>%
    group_by(category_2) %>%
    summarise(revenue = sum(total_price)) %>%
    ungroup()


# Bar Plot
revenue_by_category_2_tbl %>%
    mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(revenue)) %>%
    ggplot(aes(category_2, revenue)) + 
    geom_col(fill = "2c3e50")+
    coord_flip ()


# 4.0 Histogram / Density Plots ----
# - Great for inspecting the distribution of a variable


# Goal: Unit price of bicycles
# Histogram

bike_orderlines_tbl %>%
    
    distinct(model, price) %>%
    
    #  __ggplot() + __geom_histogram() => traçage histogramme univariable
    ggplot(aes(price)) +
    
    geom_histogram(bins = 34, fill = "magenta", color = "white")


# Goal: Unit price of bicylce, segmenting by frame material
# Histogram

bike_orderlines_tbl %>% 
    distinct(price, model, frame_material) %>%
    ggplot(aes(price, fill = frame_material)) +
    geom_histogram() +
    facet_wrap(~ frame_material, ncol =1) + 
    #   __tydiquant() package, scale_fill_tq, theme_tq 
    scale_fill_tq() +
    theme_tq()

# Density
bike_orderlines_tbl %>%
    distinct(price, model, frame_material) %>%
    ggplot(aes(price, fill = frame_material)) +
    
    geom_density(alpha = 0.5) +
    facet_wrap(~ frame_material, ncol = 1) +
    scale_fill_tq() + 
    theme_tq() + 
    theme(legend.position = "bottom")

# 5.0 Box Plot / Violin Plot ----
# - Great for comparing distributions


# Goal: Unit price of models, segmenting by category 2

# Data Manipulation
unit_price_by_cat_2_tbl <- bike_orderlines_tbl %>%
    select(category_2, model, price) %>%
    distinct() %>% 
    
    mutate(category_2 = as_factor(category_2) %>% fct_reorder(price))

# Box Plot ----
unit_price_by_cat_2_tbl %>% 
    ggplot(aes(category_2, price)) + 
    geom_boxplot() +
    
    #   __coord_flip() => change the the axis variable x <-> y ----
    coord_flip ()

# Violin Plot & Jitter Plot ----
unit_price_by_cat_2_tbl %>%
    ggplot(aes(category_2, price)) + 
    
    #   __geom_jitter() => Add a small amount of random variation to the location of each point ----
    geom_jitter(width = 0.19, color = "#2c3e50") +
    geom_violin(alpha = 0.5) +
    coord_flip() +
    theme_tq()

# 6.0 Adding Text & Labels ----

# Goal: Exposing sales over time, highlighting outlier

# Data Manipulation
revenue_by_year_tbl <- bike_orderlines_tbl %>%
    
    select(order_date, total_price) %>%
    mutate(year = year(order_date)) %>%
    group_by(year) %>%
    summarise(revenue = sum(total_price)) %>%
    ungroup()


# Adding text to bar chart

#   __geom_text() => 
revenue_by_year_tbl %>%
    ggplot(aes(year, revenue)) +
    geom_col(fill = "#2c3e50") + 
    geom_smooth(method = "lm", se = FALSE) + 
   
    geom_text(aes(label = scales::dollar(revenue, scale = 1e-6, suffix = "M")),
              vjust = 1.5, color = "white") +
    
    geom_label(label = "Major Demand this year", 
               vjust = -0.5, 
               size = 5,
               fill = "#1f78b4", 
               color = "white", 
               frontface = "italic", 
    #   __data => argument use to specify where we want to put the label)
               data = revenue_by_year_tbl %>% 
        filter(year %in% c(2013))) +
   
    theme_tq() +
    expand_limits(y = 2E7)

# Filtering labels to highlight a point





