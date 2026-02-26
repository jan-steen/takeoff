### load packages

library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(tmap)
library(readxl)
library(patchwork)
library(viridis)

################################################################################

### directly read the excel file and select rows for metadata (only once per DOI) and other data: 
excel <- read_excel("data/data_extraction/data_extraction_final.xlsx", skip = 1, na = c("","NA"))
meta <- excel[,c(2:10,18:20,39:41)]
data <- excel[,c(2,11:17,21:37)]


### delete now empty rows to receive a "true" metadata table for our 272 articles
meta <- filter(meta, !(is.na(Title)))

### make variables that should be factors into factors
str(meta)
meta[,c(2,6,8:14)] <- meta[,c(2,6,8:14)] |>
  mutate_if(sapply(meta[,c(2,6,8:14)], is.character), as.factor)


str(data)
data[,c(2:7,9:13,15:25)] <- data[,c(2:7,9:13,15:25)] |>
  mutate_if(sapply(data[,c(2:7,9:13,15:25)], is.character), as.factor)



################################################################################

### Number of published articles per year

pub <- ggplot(meta[meta$Year!=2025,],aes(x = Year) )+
  geom_bar(width = 0.8)+
  scale_x_continuous(breaks = c(2012,2015,2018,2021,2024)) +
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  labs(x = "Year of publication", y = "Number of articles")
pub 


#ggsave("figures/publications.pdf", width = 8, height = 6, units = "cm")

################################################################################

### make a sf based map of the world with ggplot

world <- ne_countries(scale = "small", returnclass = "sf")
table(data$Country_study_area)

### make a table with the number of articles per country to merge it to the world sf object
country_counts <- data |>
  filter(Country_study_area!= "") |>
  group_by(Country_study_area) |>
  summarise(count = n()) |>
  arrange(desc(count))

### quick visualisation
ggplot(country_counts, aes(x = reorder(Country_study_area, count), y = count)) + 
  geom_col() + 
  labs(title = "Länder", x = "Land", y = "Anzahl") +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme_minimal()

### merge the sf dataset to our observations
world <- left_join(world, country_counts,   by = join_by(sovereignt == Country_study_area))

### plot it with ggplot and geom_sf
map <- ggplot(data = world) +
  geom_sf(aes(fill = count))+
 # scale_fill_gradientn(
 #   colors = c("#ADD8E6", "#03055B"), # light blue to dark blue
 #   na.value = "white",
 #   name = "Number of Articles")+
  scale_fill_viridis(discrete = F, option = "G", direction = -1, 
                     na.value = "white",
                     name = "Number of \nArticles")+
  coord_sf(crs = "ESRI:54030")+
  theme_bw()+
  theme(legend.position = "right",
        legend.margin = margin(0),
        legend.key.width = unit(0.75, "cm"),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

map

#ggsave("figures/map.pdf", width = 16, height = 8, units = "cm")

################################################################################


################################################################################

