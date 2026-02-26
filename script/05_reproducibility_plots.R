### plots developed in this script were only used as a visualisation and not used 
### in final publication


### load packages

library(tidyverse)
library(ggplot2)
library(readxl)
library(patchwork)

################################################################################

### directly read the excel file and select rows for metadata (only once per DOI) and other data:
excel <- read_excel("data/data_extraction/data_extraction_final.xlsx", skip = 1, na = c("","NA"))
meta <- excel[,c(2:10,18:20,39:42)]
data <- excel[,c(2,11:17,21:37)]


### delete now empty rows to receive a "true" metadata table for our 272 articles
meta <- filter(meta, !(is.na(Title)))

### make variables into factors
str(meta)
meta[,c(2,6,8:16)] <- meta[,c(2,6,8:16)] |>
  mutate_if(sapply(meta[,c(2,6,8:16)], is.character), as.factor)


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

meta$model_available<- factor(meta$model_available,
                              levels = c("yes", "no", "not applicable"))

### produce plots

ana <- ggplot(data[!(is.na(data$Analysis_open_source)),], aes(x= Analysis_open_source))+
  geom_bar(width = 0.6)+
  theme_classic() +
  labs(x = "Analysis open source", y = "Number of observations")+
  scale_y_continuous(expand = c(0,0), limits = c(0,300))

pho <- ggplot(data[!(is.na(data$Photogrammetry_open_source)),], aes(x= Photogrammetry_open_source))+
  geom_bar(width = 0.6)+
  theme_classic() +
  labs(x = "Photogrammetry open source", y = "")+
  scale_y_continuous(expand = c(0,0), limits = c(0,300))

scr <- ggplot(meta[!(is.na(meta$Script_available)),], aes(x= Script_available))+
  geom_bar(width = 0.75)+
  theme_classic() +
  labs(x = "Script available", y = "")+
  scale_y_continuous(expand = c(0,0), limits = c(0,225))

dat <- ggplot(meta[!(is.na(meta$Data_available)),], aes(x= Data_available))+
  geom_bar(width = 0.75)+
  theme_classic() +
  labs(x = "Data available", y = "Number of observations")+
  scale_y_continuous(expand = c(0,0), limits = c(0,225))

mod <- ggplot(meta[!(is.na(meta$model_available)),], aes(x= model_available))+
  geom_bar(width = 0.75)+
  theme_classic() +
  labs(x = "Model available", y = "")+
  scale_y_continuous(expand = c(0,0))

### try to make the width the same for all plots despite having 3 or 4 categories

(ana | pho | plot_spacer())/( dat | scr | mod)

#ggsave("figures/reproducibility.pdf", width = 18, height = 12, units = "cm")

################################################################################


table(data$Photogrammetry_open_source)

table(data$Analysis_open_source)

table(meta$Script_available)

table(meta$Data_available)

table(meta$model_available)






