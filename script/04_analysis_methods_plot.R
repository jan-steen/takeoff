### load packages

library(tidyverse)
library(ggplot2)
library(readxl)
library(gridExtra)
library(grid)
library(RColorBrewer)
library(viridis)

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

table(data$algorithm_grouped)

################################################################################

### heatmaps of algorithm x sensor for all analysed traits (Biomass, land cover etc)

### Als Datensatz dafür brauchen wir:DOI, habitat quality, algorithm grouped und Sensor

combi <- data |>
  select(DOI, Sensor, Habitat_quality_grouped, algorithm_grouped) |>
  filter(!(is.na(algorithm_grouped) & is.na(Habitat_quality_grouped) & is.na(Sensor))) |> 
  group_by(DOI) |>                                      ### group articles by DOI 
  fill(algorithm_grouped,.direction = "down") |>        ### fill NAs by the variable that sits above 
  fill(Habitat_quality_grouped,.direction = "down") |>
  fill(Sensor, .direction = "down") |>
  ungroup()
           
table(combi$Sensor)

### combine custom / other sensor types
combi$Sensor <- case_when(
  combi$Sensor %in% c("hyperspectral", "hyperspectral (panchromatic)") ~ "hyperspectral",
  combi$Sensor == "multispectral" ~ "multispectral",
  combi$Sensor == "LiDAR" ~ "LiDAR",
  combi$Sensor == "RGB" ~ "RGB",
  combi$Sensor == "missing" ~ "missing",
  TRUE ~ "other"
)
table(combi$Sensor)

### define levels for algorithm and sensor
combi$Sensor <- factor(combi$Sensor,
                       levels = c("RGB", "multispectral", "hyperspectral", "LiDAR", "other", "missing"))



### combine algorithm types:
## ddnn ist tippfehler
## dlnn zu cnn um es von den "normalen" nn abzugrenzen
##
##
table(combi$algorithm_grouped)

## zweite version mit etwas gröberer Gruppierung
## vor zeile 77 gleich


### rename algorithms
combi$algorithm_grouped <- case_when(
  combi$algorithm_grouped %in% c("linear", "Non-linear") ~ "parametric statistical analysis",
  combi$algorithm_grouped == "tree" ~ "tree based machine learning",
  combi$algorithm_grouped == "svm" ~ "support vector machine",
  combi$algorithm_grouped %in% c("nn", "dlnn") ~ "deep neural network",
  combi$algorithm_grouped == "unsupervised" ~ "unsupervised machine learning",
  combi$algorithm_grouped == "missing" ~ "missing",
  TRUE ~ "other"
)


combi$algorithm_grouped <- factor(combi$algorithm_grouped,
                                  levels = c("missing","other",
                                             "unsupervised machine learning",
                                             "deep neural network",
                                             "support vector machine",
                                             "tree based machine learning",
                                             "parametric statistical analysis"))

combi$Habitat_quality_grouped <- factor(combi$Habitat_quality_grouped,
                                        levels = c("Biomass", "Biogeochemical", "Land Cover", "Species detection", "Other"))


table(combi$Sensor)
table(combi$algorithm_grouped)

######################################################

test <- combi |>
  select(Habitat_quality_grouped, Sensor, algorithm_grouped) |>
  count(Sensor, algorithm_grouped, Habitat_quality_grouped) |>
  filter(Habitat_quality_grouped %in% c("Biogeochemical", "Biomass", "Land Cover", "Species detection"))  |>
  filter(Sensor %in% c("RGB", "multispectral", "hyperspectral", "LiDAR", "other")) |>
  filter(algorithm_grouped %in% c("deep neural network",
                                  "support vector machine",
                                  "unsupervised machine learning",
                                  "tree based machine learning",
                                  "parametric statistical analysis")) |>
  mutate(n = cut(n, breaks = c(1,5,10,15,20,50), right = F))

test$n <- case_when(
  test$n == "[1,5)" ~ "1-4",
  test$n == "[5,10)" ~ "5-9",
  test$n == "[10,15)" ~ "10-14",
  test$n == "[15,20)" ~ "15-19",
  test$n == "[20,50)" ~ "> 20",
  TRUE ~ "other"
)
test
test$n <- factor(test$n,
                 levels = c("1-4","5-9","10-14","15-19","> 20"))

### define labels with number of observations for the habitat facets
habitats <- c(
  "Biomass" = "Biomass\nn = 138",
  "Biogeochemical" = "Biogeochemical\nn = 76",
  "Land Cover" = "Land Cover\nn = 123",
  "Species detection" = "Species detection\nn = 86"
)

table(combi$Habitat_quality_grouped)

ggplot(data = test, aes(x = Sensor, y = algorithm_grouped, fill = n)) + 
  geom_tile() +
  facet_wrap(vars(Habitat_quality_grouped), 
             labeller = labeller(Habitat_quality_grouped = habitats))+
  # scale_fill_gradient(low = "#fef0d9", high = "#cb181d") + ### if gradient scale should be used
  scale_fill_viridis(discrete = T, option = "G", direction = -1) +
  labs(x = "", y = "", fill = "occurences")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major = element_blank())



#ggsave("figures/analysis.pdf", width = 16, height = 12, units = "cm")








