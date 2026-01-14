### load packages

library(tidyverse)
library(ggplot2)
library(crosstable)
library(readxl)
library(treemap)
library(treemapify)
library(patchwork)

################################################################################

### directly read the excel file. no need for two files: 
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

### Welche Habitatqualität wird in welchen Habitaten untersucht 



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

habitat$Habitat_type <- factor(habitat$Habitat_type, 
                               levels = c("Grassland", "Wetland", "Heathland", "Other"))
habitat$Habitat_quality_grouped <- factor(habitat$Habitat_quality_grouped,
                                          levels = c("Biomass", "Land Cover", "Species detection","Biogeochemical", "Other","missing"))

##define a color palette
pal <- c(
  "Biomass" = "#e41a1c",
  "Land Cover" = "#377eb8",
  "Species detection" = "#4daf4a",
  "Biogeochemical" = "#984ea3",
  "Other" = "#a9a9a9"
)


### plot which habitat traits were assessed in which habitat type
hab <- ggplot(habitat[habitat$Habitat_quality_grouped != "missing",],aes(x = Habitat_type, fill = Habitat_quality_grouped))+
  geom_bar(position = position_dodge2(preserve = "single"))+
  theme_minimal()+
  labs(fill = "analysed trait", x = "Habitat type", y = "Number of observations")+
  scale_fill_manual(values = pal, guide = guide_legend(ncol = 2))+
  theme(legend.position = "bottom")
hab


#ggsave("figures/habitat.pdfncol = #ggsave("figures/habitat.pdf", width = 14, height = 6, units = "cm")



################################################################################

### how many studys are in NATURA 2000 areas?

### remove rows where both (%) or one (|) parameteres are NA
### group articles by DOI and fill NAs by the variable that sits above 
### remove observations where all three variables are duplicated
### remove rows where both (%) or one (|) parameteres are NA
### filter countries that are part of the European Union, UK only until 2020

euro <- data |>
  select(DOI, Habitat_type, Country_study_area, NATURA_2000) |>
  filter(!(is.na(Habitat_type)  & is.na(Country_study_area) & is.na(NATURA_2000))) |>
  group_by(DOI) |> 
  fill(Habitat_type,.direction = "down") |>
  fill(NATURA_2000,.direction = "down") |>
  fill(Country_study_area,.direction = "down") |>
  ungroup() |>
  left_join(meta, by = "DOI") |>
  filter(Country_study_area %in% c("Belgium","Bulgaria","Czechia","Denmark","Estonia",
                                   "Finland","France", "Germany", "Hungary", "Ireland",
                                   "Italy", "Netherlands", "Portugal", "Republic of Serbia",
                                   "Romania", "Serbia", "Spain", "Sweden")
         | Country_study_area %in% "United Kingdom" & Year < 2020 )


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

euro$NATURA_2000 <- factor(euro$NATURA_2000,
                                levels = c("unknown","no", "yes"))

euro$NATURA_2000 <- euro$NATURA_2000 |> replace_na("unknown")


pal3 <- c(
  "no" = "#e41a1c",
  "yes" = "#377eb8",
  "unknown" = "#a9a9a9"
)

###  plot studies in NATURA 2000 habitats
eur <- ggplot(euro, aes(x = Habitat_type, fill = NATURA_2000))+
  geom_bar(width = 0.7)+
  labs(x = "Habitat type", y = "", fill = "NATURA 2000 area")+
  theme_minimal()+
  scale_fill_manual(values = pal3, guide = guide_legend(ncol = 2, reverse = TRUE))+
  theme(legend.position = "bottom")
eur

#ggsave("figures/NATURA2000.pdf", width = 12, height = 6, units = "cm")

################################################################################
### trying out treemap figures


tree <- data |>
  count(Habitat_quality_grouped, Habitat_quality_lvl2) |>
  filter(Habitat_quality_grouped %in% c("Biomass", "Land Cover", "Biogeochemical", "Species detection", "Other"))

## remove NAs
#tree <- tree[c(1:24),]

pal
### bring the factors in the right order for plotting the colours
tree$Habitat_quality_grouped <- factor(tree$Habitat_quality_grouped, 
                               levels = c("Biomass", "Land Cover", "Species detection", "Biogeochemical","Other"))

pal2 =c(
  "Biomass" = "#fb8072",
  "Land Cover" = "#80b1d3",
  "Species detection" = "#8dd3a2",
  "Biogeochemical" = "#ccbada",
  "Other" = "#eaeaea"
)

## treemap package
treemap(tree,
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

# 5.92 x 3.92 inches für export



################################################################################

### die treemap funktioniert nicht als plot objekt...
hab | eur

#ggsave("figures/habitats.pdf", width = 18, height = 10, units = "cm")



