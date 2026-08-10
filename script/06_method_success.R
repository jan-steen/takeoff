### load packages

library(tidyverse)
library(ggplot2)
library(readxl)
library(patchwork)

################################################################################

### directly read the excel file and select rows for metadata (only once per DOI) and other data:
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
### create a data frame with accuracy values, habitat traits and algorithms

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

# sort the factors
acc$algorithm_grouped <- factor(acc$algorithm_grouped,
                                  levels = c("missing","other",
                                             "parametric statistical analysis",
                                             "tree based machine learning",
                                             "support vector machine",
                                             "deep neural network",
                                             "unsupervised machine learning"))



### first plot for the distribution of reported R2 values

r2 <- acc |>
  filter(Accuracy_assessment == "R²" & algorithm_grouped != "other" & algorithm_grouped != "missing")|>
ggplot(aes(x = Accuracy_value, y = "", fill = algorithm_grouped))+
  geom_dotplot(stackdir = "center", alpha = 0.5, dotsize = 1)+
  facet_wrap(vars(Habitat_quality_grouped),
             nrow = 4)+
  theme_classic()+
  labs(x = "R²", y = "", fill =  "")+
  scale_x_continuous(breaks = c(0.25,0.5,0.75,1),
                     limits = c(0,1.1),
                     expand = c(0,0))+
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())+
  theme(strip.background = element_rect(linewidth = 0.5))+
  theme(legend.position = "bottom", 
        legend.key.spacing.y = unit(0, "pt"),
        panel.background = element_rect(fill = "#fafafa"),
        strip.background = element_blank(),
        strip.text = element_text(size = 10,face = "bold"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_legend(direction = "vertical"))
r2




### second plot for the distribution of reported accuracy values
ac <- acc |>
  filter(Accuracy_assessment == "Accuracy" & algorithm_grouped != "other" & algorithm_grouped != "missing")|>
  filter(Habitat_quality_grouped != "Biomass") |> ## biomass without observations but occured in the plot???
ggplot( aes(x = Accuracy_value, y = "", fill = algorithm_grouped))+
  geom_dotplot(stackdir = "center", alpha = 0.5, dotsize = 1)+
  facet_wrap(vars(Habitat_quality_grouped), nrow = 2)+
  theme_classic()+
  labs(x = "Accuracy", y = "", fill =  "")+
  scale_x_continuous(breaks = c(0.25,0.5,0.75,1),
                     limits = c(0,1.1),
                     expand = c(0,0)) +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())+
  theme(strip.background = element_rect(linewidth = 0.5))+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#fafafa"),
        strip.background = element_blank(),
        strip.text = element_text(size = 10,face = "bold"))
ac


################################################################################
## put plots together in a single plot
design <- c(
  area(1,1,2),
  area(1,2,1),
  area(2,2)
)
## did the design work?
plot(design)

r2 + guide_area() + ac + plot_layout(design = design, 
                                     guides = "collect", 
                                     heights = c(1,1.045))

#ggsave("figures/methodsuccess.pdf", width = 16, height = 12, units = "cm")

