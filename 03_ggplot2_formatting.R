# FORMATTING GGPLOTS ----
# Author : Ralph D. Tasing ----

# Libraries & Data ----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)

# Data Manipulation

sales_by_year_category_2_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_2, total_price) %>%
    
    mutate(order_date = ymd(order_date)) %>%
    mutate(year = year(order_date)) %>%
    
    group_by(category_2, year) %>%
    summarize(revenue = sum(total_price)) %>%
    ungroup() %>%
    
    # class by year and revenue -- fct_reorder2() ----
    
    mutate(category_2 = fct_reorder2(category_2, year, revenue))

sales_by_year_category_2_tbl

sales_by_year_category_2_tbl %>%
    mutate(category_2_num = as.numeric(category_2)) %>% arrange(category_2_num)



# 1.0 Working with Colors ----

# 1.1 Color Conversion ----

# Named Colors
colors()

sales_by_year_category_2_tbl %>%
    ggplot(aes(year, revenue)) + 
    geom_col(fill = "yellow2")

# To RGB
col2rgb("yellow2")
col2rgb("#2C3E50")
# To HEX
 rgb(44, 62, 80, maxColorValue = 255)

# 1.2 Color Palettes (group of color) : Viridis, RcolorBrewer ----

# tidyquant
tidyquant::palette_light()
 
 #  __palette_right => research a color in your palette
palette_light()[2] %>% col2rgb()

# Brewer
#   __RcolorBrewer::display.brewer.all() => dsplay the color 
RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal(n=8, name = "Blues")

# Viridis
# select a color in the viridis palette ----
viridisLite::viridis(n = 20)


# 2.0 Aesthetic Mappings ----

# 2.1 Color  -----
# - Used with line and points, Outlines of rectangular objects ----

# __plot_group_mapping (ex = factor type) ----
# we can put the color argument globally in ggplot or localy in geom_line() ----

sales_by_year_category_2_tbl %>%
    ggplot(aes(year, revenue, color = category_2)) + 
    geom_line() +
    
    # We dont need to use aes here coz we are not mapping on a column
    geom_point(color = "grey", size=5)



# 2.2 Fill  -----
# - Used with fill of rectangular objects (geom_col) ----

sales_by_year_category_2_tbl %>% 
    ggplot(aes(year, revenue)) + 
    geom_col(aes(fill = category_2))

# 2.3 Size ----
# - Used with points

sales_by_year_category_2_tbl %>%
    ggplot(aes(year, revenue, color = category_2)) + 
    geom_line( size = 1) + 
    geom_point(aes(size = revenue))


# 3.0 Faceting ----
# - Great way to tease out variation by category

# Goal: Sales annual sales by category 2

#   __facet_wrap() => graph with to show variation by category ----
#   __geom_smooth() => to show the trend in the data, SE = standard deviation band ----
sales_by_year_category_2_tbl %>%
    ggplot(aes(year, revenue, color = category_2)) + 
    geom_line(color = "black") + 
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ category_2, ncol = 3, scales = "free_y") + 
    expand_limits( y = 0 )

# 4.0 Position Adjustments (Stack & Dodge) ----

# Stacked Bars & Side-By-Side Bars ----

#   __geom_col(position = "dodge")/_geom_col(position = position_dodge) = BARRE ----

sales_by_year_category_2_tbl %>%
    
    ggplot(aes(year, revenue, fill = category_2)) + 
   # geom_col(position = "stack") + 
    #geom_col(position = "dodge") 
geom_col(position = position_dodge(width = 0.9), color = "white")
    
    
# Stacked Area = diagramme dents de scie ----
sales_by_year_category_2_tbl %>% 
    ggplot(aes(year, revenue, fill = category_2)) + 
               geom_area(color = "black")

# 5.0 Scales (Colors, Fills, Axis) ----
 

# 5.1 Plot Starting Points ----
# - Continuous (e.g. Revenue): Changes color via gradient palette
# - Categorical (e.g. ): Changes color via discrete palette

# Plot 1: Faceted Plot, Color = Continuous Scale (échelle continue dégradé) ----
#   __color =  ==> with line and point + color = numerical value (make a continous scale)
g_facet_continous <- sales_by_year_category_2_tbl %>%
    ggplot(aes(year, revenue, color = revenue)) + 
    geom_line(size = 1) + 
    geom_point(size = 3) + 
    facet_wrap(~ category_2, scales = "free_y") + 
    #   __ expand_limits(y = 0) => bring the start oh the y_axis at 0  ----
    theme_minimal()

# Plot 2: Faceted Plot, Color = Discrete Scale
g_facet_discrete <- sales_by_year_category_2_tbl %>%
    ggplot(aes(year, revenue, color = category_2)) + 
    geom_line(size = 1) + 
    geom_point(size = 3) + 
    facet_wrap(~~ category_2, scales = "free_y") + 
    #   __ expand_limits(y = 0) => bring the start oh the y_axis at 0  ----
theme_minimal()


# Plot 3: Stacked Area Plot

g_area_discrete <- sales_by_year_category_2_tbl %>% ggplot(aes(year, revenue, fill = category_2)) + 
    geom_area(color = "black") + 
    theme_minimal()


# 5.2 Scale Colors & Fills ----
# - Awesome way to show variation by groups (discrete) and by values (continuous)

# Color by Revenue (Continuous Scale)
#   __scale_color_continous() VS scale_color_viridis_c() 
#   __scale_color_viridis_c() => viridis palette on continous data ("_c") ----

g_facet_continous + 
    #   __customize() the scale color
   # scale_color_continuous(
   #   low  = "black", 
   #     high = "cornflowerblue") +
    scale_color_viridis_c(option = "A", direction = -1)


# Color by Category 2 (Discrete Scale)
RColorBrewer::display.brewer.all()
# (n = 11, name = / le groupe name doit avoir le nombre n minimal de couleur) ----
RColorBrewer::brewer.pal(n = 11, name = "Set3") 

g_facet_discrete + 
    scale_color_brewer(palette = "Set3") + 
    theme_dark()

g_facet_discrete + 
    scale_color_tq(theme = "dark") + 
    #theme_bw() +
    theme_light() 

#   __scale_color_viridis_d => viridis palette on discrete val ("_d")

g_facet_discrete + 
    scale_color_viridis_d(option = "E") + theme_dark()
    

# Fill by Category 2 
#   __scale_color_brewer  VS  __scale_fill_brewer(), color Vs fill ----
g_area_discrete + 
    scale_fill_brewer(palette = "Set3")

g_area_discrete + 
    scale_fill_tq(theme = "dark")

g_area_discrete + 
    scale_fill_viridis_d()


# 5.3 Axis Scales ----

# 6.0 Labels ----

#   __scale_x_date() => Only work with the date type data ----
#   __ if it's a double type, you need to use the scale_x_continuous() ----
#   __scale_x_continuous(breaks = seq(date1, date2, by = 2) => put the x axis in a range ----

g_facet_continous + 
    scale_x_continuous(breaks = seq(2011, 2015, by=2)) + 
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M" )) + 
    geom_smooth(method = "lm", se = FALSE) + 
    
    scale_color_viridis_c()+ 
    theme_dark() + 
    
    labs(
        title = "Bike Sales", 
        subtitle = "Sales are trending up", 
        caption = "5-year sales trends\n comes from our ERP Database", 
        x = "Year", 
        y = "Revenue ($M)", 
        color = "Revenue"
    ) + 
    theme(plot.title = element_text(hjust = 0), 
          plot.subtitle = element_text(hjust = 0))+ 
    scale_color_viridis_c(labels = scales::dollar_format(scale = 1e-6, suffix = "M"))


# 7.0 Themes  ----
#   __theme() => You can modify all the elements contains in a ggplot() ----
#   element_text() ==> generic function used to modify text inside the theme() function ----
#   element_rect() ==> generic func to modify a rectangle in the theme() function ----
#   hjust() ==> peut prendre 3 valeurs (0,  0.5,  1)

g_facet_continous + 
    theme_light() + 
    theme(
        axis.text.x = element_text(
            angle = 45, 
            hjust = 1
        ), 
        #   __change element in the background box + change the text inside the box ----
        strip.background = element_rect(
            color = "green", 
            fill = "pink", 
            size = 1
        ), 
        strip.text = element_text(
            face = "bold", 
            color= "black"
        )
    )



# 8.0 Putting It All Together ----

sales_by_year_category_2_tbl %>%
    
    ggplot(aes(year, revenue, fill = category_2)) + 
    geom_area(color = "black") + 
    
    # Scales
#
#RColorBrewer::display.brewer.all()
#RColorBrewer::brewer.pal(n=8, name = "Blues")
    
#   direction = -1 => reverse the order of the color ----
    scale_fill_brewer(palette = "Blues", direction = -1) + 
    scale_y_continuous(labels = scales::dollar_format()) +
    


    # Labels 
    labs(
        title = "Sales Over Year by Category 2", 
        subtitle = "Sales Trending Upward", 
        x = "",
        y = "Revenue ($M)", 
        fill = "2nd Category", 
        caption = "*Bike sales trends look strong heading into 2016"
    ) + 

    # Themes 
    theme_light() + 
    theme(
        title = element_text(face = "bold", color = "#08306B")
    )




