### load packages

library(tidyverse)
library(ggplot2)
library(readxl)
library(treemap)
library(patchwork)

################################################################################

### directly read the excel file and select rows for metadata (only once per DOI) and other data: 
excel <- read_excel("data/data_extraction/data_extraction_final.xlsx", skip = 1, na = c("","NA"))
meta <- excel[,c(2:10,18:20,39:41)]
data <- excel[,c(2,11:17,21:37)]


### delete now empty rows to receive a "true" metadata table for our 272 articles
meta <- filter(meta, !(is.na(Title)))

### make variables into factors
str(meta)
meta[,c(2,6,8:14)] <- meta[,c(2,6,8:14)] |>
  mutate_if(sapply(meta[,c(2,6,8:14)], is.character), as.factor)


str(data)
data[,c(2:7,9:13,15:25)] <- data[,c(2:7,9:13,15:25)] |>
  mutate_if(sapply(data[,c(2:7,9:13,15:25)], is.character), as.factor)



################################################################################
### What kind of habitat quality is analysed in which habitat?


### remove rows where both (%) or one (|) parameteres are NA
### group articles by DOI and fill NAs by the variable that sits above 

habitat <- data |>
  select(DOI, Habitat_type, Habitat_quality_grouped) |>
  filter(!(is.na(Habitat_type) & is.na(Habitat_quality_grouped))) |>
  group_by(DOI) |>
  fill(Habitat_type,.direction = "down") |>
  fill(Habitat_quality_grouped,.direction = "down") |>
  ungroup()


### simplify and sort the habitats
habitat
habitat$Habitat_type <- case_when(
  habitat$Habitat_type %in% c("grassland", "desert") ~ "Grassland",
  habitat$Habitat_type == "heathland" ~ "Heathland",
  habitat$Habitat_type %in% c("peatland", "wetland") ~ "Wetland",
  TRUE ~ "Other"
)

## sort factors for both
habitat$Habitat_type <- factor(habitat$Habitat_type, 
                               levels = c("Grassland", "Wetland", "Heathland", "Other"))
habitat$Habitat_quality_grouped <- factor(habitat$Habitat_quality_grouped,
                                          levels = c("Biomass", "Land Cover", "Species detection","Biogeochemical", "Other","missing"))


################################################################################

### how many studys are in NATURA 2000 areas?

### remove rows where both (%) or one (|) parameteres are NA
### group articles by DOI and fill NAs by the variable that sits above 
### remove observations where all three variables are duplicated
### remove rows where both (%) or one (|) parameteres are NA
### filter countries that are part of the European Union, UK only until 2020

euro <- data |>
  select(DOI, Habitat_type, Country_study_area, NATURA_2000, Habitat_quality_grouped, Habitat_quality_lvl2) |>
  filter(!(is.na(Habitat_type)  & is.na(Country_study_area) & is.na(NATURA_2000) & is.na(Habitat_quality_grouped) & is.na(Habitat_quality_lvl2))) |>
  group_by(DOI) |> 
  fill(Habitat_type,.direction = "down") |>
  fill(NATURA_2000,.direction = "down") |>
  fill(Country_study_area,.direction = "down") |>
  fill(Habitat_quality_grouped,.direction = "down") |>
  fill(Habitat_quality_lvl2,.direction = "down") |>
  ungroup() |>
  left_join(meta, by = "DOI") |>
  filter(Country_study_area %in% c("Belgium","Bulgaria","Czechia","Denmark","Estonia",
                                   "Finland","France", "Germany", "Hungary", "Ireland",
                                   "Italy", "Netherlands", "Portugal", "Republic of Serbia",
                                   "Romania", "Serbia", "Spain", "Sweden")
         | Country_study_area %in% "United Kingdom" & Year < 2020 ) |>
  filter(NATURA_2000 == "yes")

### simplify and sort the habitats
euro
euro$Habitat_type <- case_when(
  euro$Habitat_type %in% c("grassland", "desert") ~ "Grassland",
  euro$Habitat_type == "heathland" ~ "Heathland",
  euro$Habitat_type %in% c("peatland", "wetland") ~ "Wetland",
  TRUE ~ "Other"
)

euro$Habitat_type <- factor(euro$Habitat_type, 
                            levels = c("Grassland", "Wetland", "Heathland", "Other"))
euro$Habitat_quality_grouped <- factor(euro$Habitat_quality_grouped,
                                       levels = c("Biomass", "Land Cover", "Species detection","Biogeochemical", "Other","missing"))


### combine the habitat and euro data frames


combi <- habitat |>
  filter(Habitat_quality_grouped != "missing" & Habitat_type != "Other") |>
  left_join(euro, by = "DOI")

### make a combined ggplot plot
ggplot(combi,aes(x = Habitat_quality_grouped.x, fill = NATURA_2000))+
  geom_bar(position = position_stack(reverse = T), width = 0.8)+
  facet_wrap(combi$Habitat_type.x)+
  scale_y_continuous(expand = c(0,0), limits = c(0,110))+
  theme_classic() +
  labs(x = "", y = "Number of observations", fill = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(size = 10,face = "bold"))+ ### layout of facet titles
  theme(panel.spacing = unit(1, "lines"),
        legend.position = c(0.85,0.85), 
        legend.key.size = unit(0.7,units = "cm"),
        legend.text = element_text(size=10.5),
        panel.background = element_rect(fill = "#fafafa"),
        legend.background = element_rect(fill = "#fafafa"))+
  scale_fill_manual(
    values = "indianred1",
    breaks = c('yes'),
    labels = c("Natura 2000\nhabitat"))

## save the figure as a pdf
#ggsave("figures/habitats.pdf", width = 16, height = 10, units = "cm")


################################################################################
### figure for graphical abstract


ggplot(combi,aes(x = Habitat_quality_grouped.x, fill = NATURA_2000))+
  geom_bar(position = position_stack(reverse = T), width = 0.8)+
  scale_y_continuous(expand = c(0,0), limits = c(0,140))+
  theme_classic() +
  labs(x = "", y = "Number of observations", fill = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(size = 10,face = "bold"))+ ### layout of facet titles
  theme(panel.spacing = unit(1, "lines"),
        legend.position = c(0.69,0.85), 
        legend.key.size = unit(0.7,units = "cm"),
        legend.text = element_text(size=14),
        panel.background = element_rect(fill = "#fafafa"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14))+
  scale_fill_manual(
    values = "indianred1",
    breaks = c('yes'),
    labels = c("Natura 2000\nhabitat"))


#ggsave("figures/extra_figures/abstract.pdf", width = 7.5, height = 10, units = "cm", bg = "transparent" )
#ggsave("figures/extra_figures/abstract.png", width = 7.5, height = 10, units = "cm" , bg = "transparent" )





################################################################################

### poster figures

ggplot(combi,aes(x = Habitat_quality_grouped.x, fill = NATURA_2000))+
  geom_bar(position = position_stack(reverse = T), width = 0.8)+
  facet_wrap(combi$Habitat_type.x)+
  scale_y_continuous(expand = c(0,0), limits = c(0,110))+
  theme_classic() +
  labs(x = "", y = "Number of observations", fill = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        strip.background = element_rect(fill = "#fafafa", linewidth = 0.5, colour = "grey90"),
        strip.text = element_text(size = 11,face = "bold"))+ ### layout of facet titles
  theme(panel.spacing = unit(1, "lines"),
        legend.position = c(0.85,0.85), 
        legend.key.size = unit(0.7,units = "cm"),
        legend.text = element_text(size=11),
        panel.background = element_rect(fill = "#fafafa"),
        panel.border = element_rect(linewidth = 0.5, colour = "grey90"),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill= "transparent", color=NA))+
  scale_fill_manual(
    values = "indianred1",
    breaks = c('yes'),
    labels = c("Natura 2000\nhabitat"))

ggsave("figures/extra_figures/poster_analysis.png", width = 16, height = 10, units = "cm" , bg = "transparent", dpi = 2500)










################################################################################
### make a treemap figure to get a better overview of lvl 2 habitat traits that
### were assessed in the reviewed studies


tree <- data |>
  count(Habitat_quality_grouped, Habitat_quality_lvl2) |>
  filter(Habitat_quality_grouped %in% c("Biomass", "Land Cover", "Biogeochemical", "Species detection", "Other"))

## remove NAs
#tree <- tree[c(1:24),]

### bring the factors in the right order for plotting the colours
tree$Habitat_quality_grouped <- factor(tree$Habitat_quality_grouped, 
                               levels = c("Biomass", "Land Cover", "Species detection", "Biogeochemical","Other"))

### coloured or greyscale palettes
pal2 =c(
  "Biomass" = "#fb8072",
  "Land Cover" = "#80b1d3",
  "Species detection" = "#8dd3a2",
  "Biogeochemical" = "#ccbada",
  "Other" = "#eaeaea"
)

## does not work as intended???
pal2 =c(
  "Biomass" = "#ffffff",
  "Land Cover" = "#eeeeee",
  "Species detection" = "#eeeeee",
  "Biogeochemical" = "#ffffff",
  "Other" = "#ffffff",
  "missing" = "#ffffff"
)

## plot a treemap figure
treemap(tree,
        index = c("Habitat_quality_grouped", "Habitat_quality_lvl2"),
        vSize = "n",
        type = "index",
     #   title = "Habitat quality traits assessed with UASs",
        title = "",
        align.labels = list(c("left","top"),c("center","center")),
        algorithm = "squarified",
        fontcolor.labels = "black", 
        palette = pal2,
        bg.labels = "transparent",
        position.legend = "none"
)

## no ggplot plot object so we have to manually export the image
# 5.92 x 3.92 inches für export

################################################################################

### treemap of only NATURA 2000 plots (not used in final publication)

eurotree <- euro |>
  count(Habitat_quality_grouped, Habitat_quality_lvl2) |>
  filter(Habitat_quality_grouped %in% c("Biomass", "Land Cover", "Biogeochemical", "Species detection", "Other"))

## remove NAs
#tree <- tree[c(1:24),]

### bring the factors in the right order for plotting the colours
eurotree$Habitat_quality_grouped <- factor(eurotree$Habitat_quality_grouped, 
                                       levels = c("Biomass", "Land Cover", "Species detection", "Biogeochemical","Other"))

pal2 =c(
  "Biomass" = "#fb8072",
  "Land Cover" = "#80b1d3",
  "Species detection" = "#8dd3a2",
  "Biogeochemical" = "#ccbada",
  "Other" = "#eaeaea"
)



treemap(eurotree,
        index = c("Habitat_quality_grouped", "Habitat_quality_lvl2"),
        vSize = "n",
        type = "index",
        #   title = "Habitat quality traits assessed with UASs",
        title = "",
        align.labels = list(c("left","top"),c("center","center")),
        algorithm = "squarified",
        fontcolor.labels = "black", 
        palette = pal2
        
)















