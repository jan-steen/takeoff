### load packages

library(tidyverse)
library(ggplot2)
library(readxl)
library(RColorBrewer)
library(viridis)

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

table(data$algorithm_grouped)

################################################################################

### heatmaps of algorithm x sensor for all analysed traits (Biomass, land cover etc)

### we need following data:DOI, habitat quality, algorithm grouped and Sensor

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

table(combi$algorithm_grouped)

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

## sort factors for algorithms and habitat qualities
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

### create a new dataframe with observations counts for the use of algorithms per sensor
obs <- combi |>
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

## rename number of obersations to prettier names
obs$n <- case_when(
  obs$n == "[1,5)" ~ "1 - 4",
  obs$n == "[5,10)" ~ "5 - 9",
  obs$n == "[10,15)" ~ "10 - 14",
  obs$n == "[15,20)" ~ "15 - 19",
  obs$n == "[20,50)" ~ "≥ 20",
  TRUE ~ "other"
)
obs
obs$n <- factor(obs$n,
                 levels = c("1 - 4","5 - 9","10 - 14","15 - 19","≥ 20"))

### define labels with number of observations for the habitat facets
table(combi$Habitat_quality_grouped)
habitats <- c(
  "Biomass" = "Biomass\nn = 138",
  "Biogeochemical" = "Biogeochemical\nn = 76",
  "Land Cover" = "Land Cover\nn = 123",
  "Species detection" = "Species detection\nn = 86"
)


### make a ggplot plot
ggplot(data = obs, aes(x = Sensor, y = algorithm_grouped, fill = n)) + 
  geom_tile() +
  facet_wrap(vars(Habitat_quality_grouped), 
             labeller = labeller(Habitat_quality_grouped = habitats))+
  # scale_fill_gradient(low = "#fef0d9", high = "#cb181d") + ### if gradient scale should be used
  scale_fill_viridis(discrete = T, option = "G", direction = -1) +
  labs(x = "", y = "", fill = "Occurences")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major = element_blank(),
        strip.text = element_text(size = 10,face = "bold"),
        panel.spacing = unit(1, "lines"))
        



#ggsave("figures/analysis.pdf", width = 16, height = 12, units = "cm")


################################################################################
### additional plot for presentationa

### count observations

### create a new dataframe with observations counts for the use of algorithms per sensor
obs <- combi |>
  select(Habitat_quality_grouped, Sensor, algorithm_grouped) |>
  count(Sensor, algorithm_grouped) |>
  filter(Sensor %in% c("RGB", "multispectral", "hyperspectral", "LiDAR", "other")) |>
  filter(algorithm_grouped %in% c("deep neural network",
                                  "support vector machine",
                                  "unsupervised machine learning",
                                  "tree based machine learning",
                                  "parametric statistical analysis")) |>
  mutate(n = cut(n, breaks = c(1,5,10,20,50,100), right = F))

## rename number of obersations to prettier names
obs$n <- case_when(
  obs$n == "[1,5)" ~ "1 - 4",
  obs$n == "[5,10)" ~ "5 - 9",
  obs$n == "[10,20)" ~ "10 - 19",
  obs$n == "[20,50)" ~ "20 - 49",
  obs$n == "[50,100)" ~ "≥ 50",
  TRUE ~ "other"
)
obs
obs$n <- factor(obs$n,
                levels = c("1 - 4","5 - 9","10 - 19","20 - 49","≥ 50"))



obs$title <- "Habitat type"
### make a ggplot plot
ggplot(data = obs, aes(x = Sensor, y = algorithm_grouped, fill = n)) + 
  geom_tile() +
  # scale_fill_gradient(low = "#fef0d9", high = "#cb181d") + ### if gradient scale should be used
  scale_fill_viridis(discrete = T, option = "G", direction = -1) +
  labs(x = "Sensors", y = "Analysis\nmethods", fill = "Occurences")+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        strip.text = element_text(size = 10,face = "bold"),
        panel.spacing = unit(1, "lines"),
        axis.title=element_text(size=14))+
  theme(legend.position="none")+
 #ggtitle("Habitat type")+
  #theme(plot.title = element_text(size = 14, hjust = 0.5))+
  facet_grid(.~ title)+
  theme(strip.background = element_rect(fill = "#fafafa"))+
  theme(strip.text = element_text(size = 14))

ggsave("figures/analysis_simple.pdf", width = 6, height = 6, units = "cm", bg = "transparent")
ggsave("figures/analysis_simple.png", width = 6, height = 6, units = "cm", bg = "transparent")



