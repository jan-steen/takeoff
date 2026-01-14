### load packages

library(tidyverse)
library(ggplot2)
library(readxl)
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


### make some small plots for open source and reproducibility

### first change the levels of the factors 

data$Analysis_open_source <- factor(data$Analysis_open_source,
                                    levels = c("yes", "no", "missing"))

data$Photogrammetry_open_source <- factor(data$Photogrammetry_open_source,
                                          levels = c("yes", "no", "missing"))

meta$Data_available <- factor(meta$Data_available,
                              levels = c("yes", "partial", "on request","no"))

meta$Script_available <- factor(meta$Script_available,
                                levels = c("yes", "partial", "on request","no"))

ana <- ggplot(data[!(is.na(data$Analysis_open_source)),], aes(x= Analysis_open_source))+
  geom_bar(width = 0.6)+
  theme_minimal()+
  labs(x = "Analysis open source", y = "Number of observations")+
  ylim(0,300)

pho <- ggplot(data[!(is.na(data$Photogrammetry_open_source)),], aes(x= Photogrammetry_open_source))+
  geom_bar(width = 0.6)+
  theme_minimal()+
  labs(x = "Photogrammetry open source", y = "")+
  ylim(0,300)

scr <- ggplot(meta[!(is.na(meta$Script_available)),], aes(x= Script_available))+
  geom_bar(width = 0.75)+
  theme_minimal()+
  labs(x = "Script available", y = "")+
  ylim(0,225)

dat <- ggplot(meta[!(is.na(meta$Data_available)),], aes(x= Data_available))+
  geom_bar(width = 0.75)+
  theme_minimal()+
  labs(x = "Data available", y = "Number of observations")+
  ylim(0,225)

### try to make the width the same for all plots despite having 3 or 4 categories

(ana | pho )/( dat | scr)

ggsave("figures/reproducibility.pdf", width = 14, height = 12, units = "cm")

################################################################################

















repro <- excel |>
  select(DOI, Data_available, Script_available) |>
  filter(!(is.na(Script_available)))


repro |> 
  tidyr::pivot_longer(
    cols = -DOI,
    names_to = "data_or_script",
    values_to = "availability"
  ) |> 
  ggplot(aes(x = data_or_script, fill = availability))+
  geom_bar(position = "dodge")+
  theme_minimal()+
  labs(x = "", y = "count")



open <- excel |>
  select(DOI, Photogrammetry_open_source, Analysis_open_source) |>
  filter(!(is.na(Photogrammetry_open_source) & is.na(Analysis_open_source))) |>
  group_by(DOI) |>
  fill(Photogrammetry_open_source,.direction = "down") |>
  fill(Analysis_open_source,.direction = "down")



open |> 
  tidyr::pivot_longer(
    cols = -DOI,
    names_to = "test",
    values_to = "Open_Source"
  ) |> 
  ggplot(aes(x = test, fill = Open_Source))+
  geom_bar(position = "dodge")+
  theme_minimal()+
  labs(x = "", y = "count")

table(data$Photogrammetry_open_source)
table(data$Analysis_open_source)
table(meta$Script_available)
table(meta$Data_available)
