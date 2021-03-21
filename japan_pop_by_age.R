setwd("~/Documents/GitHub/japan-pop-by-age/data/")

if (!require("pacman")) install.packages("pacman")
library(pacman) 

pacman::p_load(tidyverse, ggrepel)



# ------------------------------------
# import data

pop0_14 <- read_csv("2019-ages-0-14.csv")
pop15_64 <- read_csv("2019-ages-15-64.csv")
pop65_above <- read_csv("2019-ages-65-above.csv")
poptotal <- read_csv("2019-ages-total.csv")



# ------------------------------------
# processing

# pop <- bind_rows(pop0_14, pop15_64, pop65_above, poptotal)
# category <- c("age-0-14", "age-15-64", "age-65-above", "age-total")
# 
# pop <- pop %>% 
#   dplyr::rename(cname = "Country Name") %>% 
#   dplyr::filter(cname == "Japan") %>% 
#   dplyr::mutate(category = category) %>% 
#   dplyr::relocate(cname, category) %>% 
#   dplyr::select(-"Indicator Name", -"Indicator Code", -"Country Code", -"2020")

# convert to long tables

pop_1 <- pop0_14 %>%
  dplyr::rename(cname = "Country Name") %>%
  dplyr::filter(cname == "Japan") %>%
  dplyr::mutate(category = "age-0-14") %>% 
  dplyr::relocate(category) %>% 
  dplyr::select(-"cname", -"Indicator Name", -"Indicator Code", -"Country Code", -"2020") %>% 
  pivot_longer(starts_with(c("19", "20")),
               names_to = "year",
               values_to = "population")

pop_2 <- pop15_64 %>%
  dplyr::rename(cname = "Country Name") %>%
  dplyr::filter(cname == "Japan") %>%
  dplyr::mutate(category = "age-15-64") %>% 
  dplyr::relocate(category) %>% 
  dplyr::select(-"cname", -"Indicator Name", -"Indicator Code", -"Country Code", -"2020") %>% 
  pivot_longer(starts_with(c("19", "20")),
               names_to = "year",
               values_to = "population")

pop_3 <- pop65_above %>%
  dplyr::rename(cname = "Country Name") %>%
  dplyr::filter(cname == "Japan") %>%
  dplyr::mutate(category = "age-65-above") %>% 
  dplyr::relocate(category) %>% 
  dplyr::select(-"cname", -"Indicator Name", -"Indicator Code", -"Country Code", -"2020") %>% 
  pivot_longer(starts_with(c("19", "20")),
               names_to = "year",
               values_to = "population")

pop_4 <- poptotal %>%
  dplyr::rename(cname = "Country Name") %>%
  dplyr::filter(cname == "Japan") %>%
  dplyr::mutate(category = "age-total") %>% 
  dplyr::relocate(category) %>% 
  dplyr::select(-"cname", -"Indicator Name", -"Indicator Code", -"Country Code", -"2020") %>% 
  pivot_longer(starts_with(c("19", "20")),
               names_to = "year",
               values_to = "population")

# as the result below suggests, total population may be incorrect
(pop_1$population + pop_2$population + pop_3$population) == pop_4$population


# append long data
popage <- bind_rows(pop_1, pop_2, pop_3)

is.numeric(popage$year)
is.numeric(popage$population)

popage$year <- as.numeric(popage$year)



# ------------------------------------
# visualizing
install.packages("gcookbook")
library(gcookbook)
uspopage
pop <- uspopage

ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + 
  geom_area(alpha=0.8) +
  scale_x_continuous(expand=c(0,0)) 


popfig1 <- ggplot(data = popage,
                  aes(x = year, y = population,
                      # fill = category,
                      fill = forcats::fct_rev(category)),
                  color = "black") + 
  geom_area(alpha=0.8) 



options(scipen = 999)

popfig1 <- ggplot() + 
  geom_area(data = popage, 
            aes(x = year, y = population / 1000,
                # fill = category,
                fill = forcats::fct_rev(category)),
                color = "white",
            alpha=0.8, size=0.5) +
  labs(title = "Age Distribution of Population in Japan, 1960-2019",
       # subtitle = "",
       caption = "Source: World Bank Open Data",
       x = "Year",
       y = "Population in Thousand") +
  # geom_text_repel(aes(label = population), 
  #                 data = popage,
  #                 color = "black", size = 3) +
  scale_fill_manual(values=c('azure3','lightsteelblue2', 'lightsteelblue3'))+
  # scale_y_continuous(scale_y_continuous(breaks = seq(0, 100000, 25000)),
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(expand=c(0,0)) +
  theme(
    plot.title = element_text(vjust = -2, hjust = 0.1),
    plot.subtitle = element_text(vjust = -2, hjust = 0.1),
    # plot.caption = element_text(vjust = 3, hjust = 0.8),
    text = element_text(family = "Optima"),
    plot.background = element_rect(fill = "#f5f5f2"), 
    legend.position = c(0.13, 0.18)) 

popfig1

setwd("~/Documents/GitHub/japan-pop-by-age/")
ggsave(popfig1, filename = "popfig1.png", 
       width = 8, height = 6)




ggplot(uspopage, aes(x = Year,
                     y = Thousands/1000, 
                     fill = forcats::fct_rev(AgeGroup))) +
  geom_area(color = "black") +
  labs(title = "US Population by age",
       subtitle = "1900 to 2002",
       caption = "source: U.S. Census Bureau, 2003, HS-3",
       x = "Year",
       y = "Population in Millions",
       fill = "Age Group") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

pop_map2 <- ggplot() + 
  geom_point(data= kansai_pop, 
             aes(x = long, y = lat,
                 size = Population, color = Population),
             alpha = 0.3) + 
  scale_color_gradient(low = "#edd6d4", high = "#7A0018") + 
  # Show a legend for a colour
  guides(size = FALSE) + 
  scale_size_area(max_size = 15) + 
  labs(x = NULL, 
       y = NULL, 
       title = "Kansai's Regional Demographics", 
       subtitle = "Population in Kansai Municipalities, 2015", 
       caption = "Source: National Spatial Planning and Regional Policy Bureau") +
  theme_void() +
  theme(
    plot.title = element_text(vjust = -2, hjust = 0.1),
    plot.subtitle = element_text(vjust = -2, hjust = 0.1),
    plot.caption = element_text(vjust = 3, hjust = 0.8),
    text = element_text(family = "Optima"),
    plot.background = element_rect(fill = "#f5f5f2"), 
    legend.position = c(0.13, 0.18))  



# ------------------------------------





# save as images
ggsave(file = "forest_cover_2000.jpeg", plot = cover, 
       width = 30, height = 12.5)

# ggsave(file = "forest_cover_2000.pdf", plot = cover, 
#        width = 30, height = 12.5)

