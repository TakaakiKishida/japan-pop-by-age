setwd("~/Documents/GitHub/japan-pop-by-age/data/")

if (!require("pacman")) install.packages("pacman")
library(pacman) 

pacman::p_load(tidyverse)



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
pop <- bind_rows(pop_1, pop_2, pop_3)

# ------------------------------------
# visualizing




head(pop)

# pop %>% 
#   select(starts_with(c("19", "20")))

pop_1 <- pop %>% 
  pivot_longer(1960:2019, 
               names_to = c(".value", "item"), 
               names_sep = "_")

pop_1 <- pop %>% 
  pivot_longer(starts_with(c("19", "20")), 
               names_to = c(".value", "item"), 
               names_sep = "_")




pop_1 <- pop %>% 
  pivot_longer(starts_with(c("19", "20")), 
               names_to = "category",
               names_prefix = "age-",
               values_to = "category")

pop <- pop %>% 
  pivot_longer(starts_with(c("19", "20")), 
               names_to = "age-",
               values_to = "category")

df4_L <- df4 %>%
  pivot_longer(cols         = starts_with("Confirmed"), 
               names_to     = "Country",
               names_prefix = "Confirmed_",
               values_to    = "Confirmed")

iris %>% select(starts_with(c("Petal", "Sepal")))


# ---------------------

library(gapminder)
gpm <- data(gapminder)
gapminder
gapminder_long <- gapminder %>%
  pivot_longer(
    lifeExp:gdpPercap,
    names_to = "measure",
    values_to = "value"
  )

# ------------------








files <- list.files(path = "~/Documents/GitHub/japan-pop-by-age/data",
                    pattern = ".csv",
                    full.names = T)

data <- lapply(files,read.csv)

for(i in 1:4){
  i <- formatC(i, width = 2, flag = "0") 
  df <- paste("pop", i, ".csv", sep = "") 
  eval(parse(text=paste("data", i, "<- read.csv('", df, "')", sep=""))) 
}

province_shp <- read_sf("gadm36_IDN_1.shp")
district_shp <- read_sf("gadm36_IDN_2.shp")
subdist_shp <- read_sf("gadm36_IDN_3.shp")
forest_subdist <- read.csv("forest_subdist.csv")

# ------------------------------------
# set up data

subdist_shp <- subdist_shp %>%
  # rename 
  dplyr::rename(province = NAME_1, 
                subdistrict = NAME_3,
                subdistrict_id = CC_3) %>% 
  # drop unnecessary variables
  dplyr::select(-GID_0:-GID_1, -NL_NAME_1, -GID_2,
                -VARNAME_3:-ENGTYPE_3, -HASC_3) %>% 
  # sort 
  dplyr::arrange(subdistrict_id)

nrow(subdist_shp)  

# merge "shape file of subdistrict" with 
# "csv of forest cover & loss at the subdistrict level"
subdist_shp <- merge(subdist_shp, forest_subdist, by = "subdistrict")
class(subdist_shp)
names(subdist_shp)

# ------------------------------------

# The unit of the forest cover variable is square kilometer
subdist_shp <- subdist_shp %>% 
  mutate(treecover2000_km = treecover2000 * 0.0009) 
  # mutate(treecover2000_1000meter = treecover2000_meter/1000)

head(subdist_shp)
subdist_shp[c("area_grid_count", "treecover2000",
              "treecover2000_km", "province.x",
              "subdistrict")]

forest_subdist[c("area_grid_count", "treecover2000")] %>% 
  mutate(treecover2000_km = treecover2000 * 0.0009) %>% 
  mutate(area_km = area_grid_count * 0.0009) %>% 
  summarise_each(dplyr::funs(sum))

subdist_shp[c("area_grid_count", "treecover2000_km")] %>% 
  filter(subdistrict == "Simpang Tiga Sakti")

subdist_shp[c("area_grid_count", "treecover2000",
              "treecover2000_km", "province.x",
              "subdistrict")] %>% 
  filter(treecover2000_km > 1000 & treecover2000_km < 2000 )

summary(subdist_shp$treecover2000)
summary(subdist_shp$treecover2000_km)

g <- ggplot() + 
  geom_histogram(data = subdist_shp, aes(x = treecover2000_km))
plot(g)

quantile(subdist_shp$treecover2000_km, 
         probs = seq(0, 1, 1/10))

# Setting forest loss density classes --- choropleth-grid-map
subdist_shp$treecover2000_class <- 
  cut(subdist_shp$treecover2000_km, 
      breaks = c(-Inf, 250, 500, 750, 
                 1000, 1250, 1500, Inf), 
      labels = c("< 250", "250-500", "500-750", 
                 "750-1,000", "1,000-1,250",
                 "1,250-1,500",  "> 1,500")) # 3


# ------------------------------------
# Plot 2000 Forest Cover Map 
cover <- ggplot() + 
  geom_sf(data = subdist_shp, 
          aes(fill = treecover2000_class), 
          color = "grey", size = 0.1) +
  geom_sf(data = district_shp, color = "black", 
          fill = "transparent", size = 0.3) +
  theme_void() + 
  # ggtitle("(A) Forest Cover in 2000") +
  labs(title = "(A) Forest Cover in 2000", 
       fill = expression(Forest~Cover~(km^{"2"}))) + 
       # fill = "Forest Cover (km^{2})") + 
  scale_fill_brewer(palette = "YlGn") + 
  # theme(plot.title = element_text(size = 40, margin = margin(50, 0, 0, 0)),
  theme(plot.title = element_text(size = 50, hjust = 0.05),
        legend.title = element_text(size = 35), 
        legend.text = element_text(size = 30),
        legend.position = c(0.12, 0.22)) + 
  # put islands names
  annotate(geom = "label", x = 101, y = -0.8,
           label = "Sumatra", 
           size = 17,
           fill = "gray",
           alpha = 0.7) +
  annotate(geom = "label", x = 108, y = -7.5,
           label = "Java", 
           size = 17,
           fill = "gray",
           alpha = 0.7) +
  annotate(geom = "label", x = 121, y = -9.5,
           label = "Lesser Sunda Islands", 
           size = 17,
           fill = "gray",
           alpha = 0.7) +
  annotate(geom = "label", x = 113.5, y = -4,
           label = "Kalimantan", 
           size = 17,
           fill = "gray",
           alpha = 0.7) +
  annotate(geom = "label", x = 121, y = -3.5,
           label = "Sulawesi", 
           size = 17,
           fill = "gray",
           alpha = 0.7) +
  annotate(geom = "label", x = 129, y = -4.5,
           label = "Malku Islands", 
           size = 17,
           fill = "gray",
           alpha = 0.7) +
  annotate(geom = "label", x = 135, y = -2,
           label = "Papua", 
           size = 17,
           fill = "gray",
           alpha = 0.7) +
  # scale bar
  annotation_scale(pad_x = unit(21.0, "in"), pad_y = unit(0.8, "in"),
                   height = unit(0.55, "cm"), # how thick it should be
                   width_hint = 0.25, # how long relative to the data
                   text_cex = 1.6) +
  # compass arrow
  annotation_north_arrow(pad_x = unit(26.0, "in"), 
                         pad_y = unit(7.75, "in"),
                         which_north = "true",
                         height = unit(4.0, "cm"), # how tall the arrow should be
                         width= unit(4.0, "cm"), # how wide the arrow should be
                         style = north_arrow_fancy_orienteering(text_size = 20)) 

# save as images
ggsave(file = "forest_cover_2000.jpeg", plot = cover, 
       width = 30, height = 12.5)

# ggsave(file = "forest_cover_2000.pdf", plot = cover, 
#        width = 30, height = 12.5)

