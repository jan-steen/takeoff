### load packages

library(tidyverse)
library(ggplot2)
library(readxl)
library(patchwork)

################################################################################

### directly read the excel file. no need for two files: 
excel <- read_excel("data/data_extraction/data_extraction_final.xlsx", skip = 1, na = c("","NA"))
meta <- excel[,c(2:10,18:20,40:43)]
### data also with the accuracy values
data <- excel[,c(2,11:17,21:38,39)]


### delete now empty rows to receive a "true" metadata table for our 272 articles
meta <- filter(meta, !(is.na(Title)))

### make variables into factors
str(meta)
meta[,c(2,6,8:15)] <- meta[,c(2,6,8:15)] |>
  mutate_if(sapply(meta[,c(2,6,8:15)], is.character), as.factor)


str(data)
data[,c(2:7,9:13,15:26)] <- data[,c(2:7,9:13,15:26)] |>
  mutate_if(sapply(data[,c(2:7,9:13,15:26)], is.character), as.factor)
data$Accuracy_value <- as.numeric(data$Accuracy_value)


################################################################################

acc<- data |>
  select(DOI, Accuracy_value,Accuracy_assessment , Habitat_quality_grouped, algorithm_grouped) |>
  filter(!(is.na(algorithm_grouped) & is.na(Habitat_quality_grouped) & is.na(Accuracy_value) & is.na(Accuracy_assessment))) |> 
  group_by(DOI) |>                                      ### group articles by DOI 
  fill(algorithm_grouped,.direction = "down") |>        ### fill NAs by the variable that sits above 
  fill(Habitat_quality_grouped,.direction = "down") |>
  fill(Accuracy_assessment, .direction = "down") |>
  filter(Habitat_quality_grouped != "Other") |>
  ungroup()


### rename algorithms
acc$algorithm_grouped <- case_when(
  acc$algorithm_grouped %in% c("linear", "Non-linear") ~ "parametric statistical analysis",
  acc$algorithm_grouped == "tree" ~ "tree based machine learning",
  acc$algorithm_grouped == "svm" ~ "support vector machine",
  acc$algorithm_grouped %in% c("nn", "dlnn") ~ "deep neural network",
  acc$algorithm_grouped == "unsupervised" ~ "unsupervised machine learning",
  acc$algorithm_grouped == "missing" ~ "missing",
  TRUE ~ "other"
)


acc$algorithm_grouped <- factor(acc$algorithm_grouped,
                                  levels = c("missing","other",
                                             "parametric statistical analysis",
                                             "tree based machine learning",
                                             "support vector machine",
                                             "deep neural network",
                                             "unsupervised machine learning"))


### trying out plots for accessing R2 values for different habitat traits
## algorithmen aufräumen wie in anderem Plot
## darstellung von NATURA 2000 plots?


r2 <- acc |>
  filter(Accuracy_assessment == "R²" & algorithm_grouped != "other" & algorithm_grouped != "missing")|>
ggplot(aes(x = Accuracy_value, y = "", fill = algorithm_grouped))+
  geom_dotplot(stackdir = "center", alpha = 0.5, dotsize = 1)+
  facet_wrap(vars(Habitat_quality_grouped),
             nrow = 4)+
  theme_classic()+
  labs(x = "R²", y = "", fill =  "")+
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())+
  theme(strip.background = element_rect(linewidth = 0.5))+
  theme(legend.position = "bottom", 
        legend.key.spacing.y = unit(0, "pt"))+
  guides(fill = guide_legend(direction = "vertical"))
r2

## adding mean and SE (but applies her for all groups)
 # geom_point(stat="summary", fun.y="mean", size = 2.5) + 
 # geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) 


ac <- acc |>
  filter(Accuracy_assessment == "Accuracy" & algorithm_grouped != "other" & algorithm_grouped != "missing")|>
  filter(Habitat_quality_grouped != "Biomass") |> ## biomass without observations but occured in the plot???
ggplot( aes(x = Accuracy_value, y = "", fill = algorithm_grouped))+
  geom_dotplot(stackdir = "center", alpha = 0.5, dotsize = 1)+
  facet_wrap(vars(Habitat_quality_grouped), nrow = 2)+
  theme_classic()+
  labs(x = "Accuracy", y = "", fill =  "")+
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())+
  theme(strip.background = element_rect(linewidth = 0.5))+
  theme(legend.position = "none")
ac

acc |>
  filter(Accuracy_assessment == "Kappa" & algorithm_grouped != "other" & algorithm_grouped != "missing")|>
ggplot( aes(x = Accuracy_value, y = "", fill = algorithm_grouped))+
  geom_dotplot(stackdir = "center", alpha = 0.5, dotsize = 0.8)+
  facet_wrap(vars(Habitat_quality_grouped), nrow=2)+
  theme_classic()+
  labs(x = "Kappa", y = "", fill =  "")+
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())+
  theme(strip.background = element_rect(linewidth = 0.5))


r2 | (plot_spacer() / ac) 

#ggsave("figures/methodsuccess.pdf", width = 16, height = 16, units = "cm")






