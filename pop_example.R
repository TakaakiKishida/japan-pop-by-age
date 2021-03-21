library(tidyverse)
library(tidyquant)
library(timetk)
library(sweep)
library(forecast)

bike_sales

bike_sales_monthly <- bike_sales %>%
  mutate(month = month(order.date, label = TRUE),
         year  = year(order.date)) %>%
  group_by(year, month) %>%
  summarise(total.qty = sum(quantity)) 

bike_sales_monthly %>%
  ggplot(aes(x = month, y = total.qty, group = year)) +
  geom_area(aes(fill = year), position = "stack") +
  labs(title = "Quantity Sold: Month Plot", x = "", y = "Sales",
       subtitle = "March through July tend to be most active") +
  scale_y_continuous() +
  theme_tq()
