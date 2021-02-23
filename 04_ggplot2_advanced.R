# ADVANCED BUSINESS PLOTS ----
# Author : Ralph D. Tasing ----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Lollipop Chart: Top N Customers ----
# - Great for showing order

# Question: How much purchasing power is in top 5 customers?
# Goal: Visualize top N customers in terms of Revenue, include cumulative percentage

n <- 10

# Data Manipulation 

top_customers_tbl <- bike_orderlines_tbl %>%
    select(bikeshop_name, total_price) %>%
    mutate(bikeshop_name = as_factor(bikeshop_name) %>% fct_lump(n = n, w = total_price)) %>%
    group_by(bikeshop_name) %>%
    summarise(revenue = sum(total_price)) %>%
    ungroup() %>%
    
    mutate(bikeshop_name = bikeshop_name %>% fct_reorder(revenue)) %>%
    mutate(bikeshop_name = bikeshop_name %>% fct_relevel("Other", after = 0)) %>%
    arrange(desc(bikeshop_name)) %>%
    
    # Revenue Text
    mutate(revenue_text = scales::dollar(revenue)) %>%
    
    # CUmulative Percent 
    mutate(cum_pct = cumsum(revenue)  / sum(revenue)) %>%
    # put the symbol percent (%) and in the strg form ----
    mutate(cum_pct_text = scales::percent(cum_pct)) %>%
    
    # Rank 
    #   __row_number() => rank the raw, give a raw number 
    mutate(rank = row_number()) %>%
    
    #   __case_when() to filter the 11th rank (other) ----
    mutate(rank = case_when (
        rank == max(rank) ~ NA_integer_, 
        TRUE ~ rank)) %>%
    
    # Label text (\n) => new line ----
    mutate(label_text = str_glue("Rank: {rank}\nRev : {revenue_text}\nCumPt: {cum_pct_text}"))


# Data Visualization ----
top_customers_tbl %>%
    ggplot(aes(revenue, bikeshop_name)) + 
    
    #   __geom_segment() => draw a segment starting at point A to a point B ----

geom_segment(aes(xend = 0, yend = bikeshop_name), color = palette_light()[1]) + 
    
    geom_point(aes(size = revenue), 
               color = palette_light()[1]) + 
    
    #   __palette_light() => color of the tidyquant package 
    geom_label(aes(label = label_text), 
               hjust = "inward", 
               size = 3, 
               color = palette_light()[1]) +
    
    # Formatting
    scale_x_continuous (labels = scales::dollar_format(scale = 1/1E6, suffix = "M")) + 
    labs(
        title = str_glue("Top {n} Customers"), 
        
        subtitle = str_glue("Start: {year(min(bike_orderlines_tbl$order_date))}
                            End : {year(max(bike_orderlines_tbl$order_date))}
                            Author : Ralph D. Tasing"
                            ), 
        x = "Revenue ($M)", 
        y = "Customer", 
        caption = str_glue("Top 6 customers contribute \n51% of purchasing power.
                           ")) + 
    theme_tq() +
    theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 18), 
        plot.caption = element_text(face = "bold.italic", size = 10))


# 2.0 Heatmaps ----
# - Great for showing details in 3 dimensions

# Question: Do specific customers have a purchasing preference?
# Goal: Visualize heatmap of proportion of sales by Secondary Product Category

# Data Manipulation

#   __fct_rev() => use with factor/categorical data    
pct_sales_by_customer_tbl <-   bike_orderlines_tbl %>%
        select(bikeshop_name, category_1, category_2, quantity) %>%
        group_by(bikeshop_name, category_1, category_2) %>%
        summarise(total_qty = sum(quantity)) %>%
        ungroup() %>%
            
        group_by(bikeshop_name) %>%
        mutate(pct = total_qty / sum(total_qty)) %>%
        ungroup() %>%
        
        #   __as_factor() => get factor by alphabetic order ----
        mutate(bikeshop_name = as.factor(bikeshop_name) %>% fct_rev()) %>% 
        
        #   __as.numeric() => check if the numeric order is good (start with "a" is good)
        
        mutate(bikeshop_name_levels = as.numeric(bikeshop_name))
    
# Data visualization 
    pct_sales_by_customer_tbl %>%
        ggplot(aes(category_2, bikeshop_name)) + 
        
    # Geometries
        #   __geom_tile() => heatmap 
        #   __scales::percent(XX, accuracy = .1) to fix the number after the comma ----
        geom_tile(aes(fill = pct)) + 
        geom_text(aes  (label = scales::percent(pct, accuracy = .1)), 
                  size = 3) + 
        facet_wrap(~ category_1, scales = "free_x" ) + 
        
    # Formatting 
        
        #  __labs(caption, title, subtitle) ----
   # scale_fill_viridis_c(option = "E") +
        scale_fill_gradient(low = "white", high = palette_light()[1]) + 
        labs(
            title = "Heatmap of Purchasing Habits", 
            subtitle =  "Author : Ralph D. Tasing",
            x = "Bike Type (Category 2)", 
            y = "Customer", 
            caption = str_glue ("Customers that prefer Road : Ann Arbor Speed, Austin Cruisers,&
            Indianapolis Velocipedes
                               
                               Customers that prefer Mountain : 
                               Ithica Mountain Climbers, Pittsburgh Mountain Machines, & Tampa 29ers"))+ 
        theme_tq() +
    
    # Theme
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
        legend.position = "none", 
        plot.title = element_text(face = "bold", size = 18), 
        plot.caption = element_text(face = "bold.italic", size = 10))
    

# Data Visualization




