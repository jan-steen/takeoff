# load packages 

library(tidyverse)
library(ggplot2)
library(ggstats)
library(readxl)
library(patchwork)
library(ggrepel)

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

# Erstellung eines ersten Plots für die Verteilung der Jahre und den verwendeten Sensoren

sensors <- data |>
  select(DOI, Sensor) |>
  filter(!(is.na(Sensor))) |>
  left_join(meta, by = "DOI")

### combine other sensor types
table(data$Sensor)
sensors$Sensor <- case_when(
  sensors$Sensor %in% c("hyperspectral", "hyperspectral (panchromatic)") ~ "hyperspectral",
  sensors$Sensor == "multispectral" ~ "multispectral",
  sensors$Sensor == "LiDAR" ~ "LiDAR",
  sensors$Sensor == "RGB" ~ "RGB",
  sensors$Sensor == "missing" ~ "missing",
  TRUE ~ "Other"
)

### sort factors for the plots
sensors$Sensor <- factor(sensors$Sensor,
                         levels = c("RGB", "multispectral", "hyperspectral", "LiDAR", "Other", "missing"))

##define a color palette
pal <- c(
  RGB = "#e41a1c",
  multispectral = "#377eb8",
  hyperspectral = "#4daf4a",
  LiDAR = "#984ea3",
  Other = "#a9a9a9"
)


################################################################################
### barplot with percentages written inside

datbar <- sensors |>
  filter(Sensor %in% c("RGB", "multispectral", "hyperspectral", "LiDAR", "Other")) |>
  filter(Year != "2025")
datbar$Sensor <- factor(datbar$Sensor,
                      levels = c( "Other", "LiDAR", "hyperspectral", "multispectral", "RGB"))
bar <- ggplot(datbar, aes(x = "", fill = Sensor)) +
  geom_bar(position = "fill", width = 0.6, color = "white") +
  geom_text(
    stat = "count",
    aes(label = after_stat(ifelse(count / sum(count) >= 0.13,
                                  scales::percent(count / sum(count), 0.1), ""))),
    position = position_fill(vjust = 0.5),
    size = 3, col = "white"
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = pal) +
  theme_void() +
  theme(legend.position = "none")+
  labs (subtitle = "    Overall usage of sensor types")

bar
################################################################################

datlin <- sensors |>
  filter(Sensor %in% c("RGB", "multispectral", "hyperspectral", "LiDAR", "Other")) |>
  filter(Sensor %in% names(pal), Year != 2025) |>
  mutate(Year = as.integer(Year)) |>
  count(Year, Sensor, name = "n") |>
  complete(Year = full_seq(Year, 1), Sensor, fill = list(n = 0))

end_labels <- datlin |>
  group_by(Sensor) |>
  filter(Year == max(Year[n > 0], default = max(Year))) |>
  slice_tail(n = 1) |>
  ungroup()

datlin$Sensor <- factor(datlin$Sensor,
                           levels = c("RGB", "multispectral", "hyperspectral", "LiDAR", "Other"))

sen <- ggplot(datlin[datlin$Sensor != "missing",], aes(Year, n, color = Sensor)) +
  geom_line(linewidth = 1.5) +
  geom_text(
    data = end_labels[end_labels$Sensor != "missing",],
    aes(label = Sensor),
    hjust = 0, nudge_x = 0.3, size = 3.5
  ) +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = c(2012,2015,2018,2021,2024),expand = expansion(mult = c(0.01, 0.4))) +
  scale_y_continuous(expand = c(0,0))+
  theme_classic() +
  theme(legend.position = "none") +
  labs(
  #  title = "Sensors used in UAS studies",
   # subtitle = "data missing = 3",
    y = "Number of observations",
    x = "Year of publication"
  )

sen


sen / bar + plot_layout(heights = c(4,1))

################################################################################


################################################################################

### new drone type plots

##define a color palette
pal2 <- c(
  Rotor = "#e41a1c",
  "Fixed Wing" = "#377eb8",
  Helicopter = "#4daf4a",
  VTOL = "#984ea3"
)

################################################################################
### barplot with percentages written inside

drone <- data |>
  select(DOI, Sensor, drone_grouped, Drone_type) |>
  filter(!(is.na(Sensor) & is.na(drone_grouped))) |>
  group_by(DOI) |>
  fill(Sensor,.direction = "down") |> 
  fill(drone_grouped,.direction = "down") |>
  fill(Drone_type,.direction = "down") |>
  left_join(meta, by = "DOI") |>
  ungroup()

datbar2 <- drone |>
  filter(Drone_type%in% c("Rotor", "Fixed Wing", "Helicopter", "VTOL")) |>
  filter(Year != "2025")
datbar2$Drone_type <- factor(datbar2$Drone_type,
                    levels = c( "VTOL", "Helicopter", "Fixed Wing", "Rotor"))
UASbar <- ggplot(datbar2, aes(x = "", fill = Drone_type)) +
  geom_bar(position = "fill", width = 0.6, color = "white") +
  geom_text(
    stat = "count",
    aes(label = after_stat(ifelse(count / sum(count) >= 0.06,
                                  scales::percent(count / sum(count), 0.1), ""))),
    position = position_fill(vjust = 0.5),
    size = 3, col = "white"
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = pal2) +
  theme_void() +
  theme(legend.position = "none")+
  labs (subtitle = "    Overall usage of UAS types")
UASbar
######

datlin2 <- drone |>
  filter(Drone_type %in% c("Rotor", "Fixed Wing", "Helicopter", "VTOL")) |>
  filter(Drone_type %in% names(pal2), Year != 2025) |>
  mutate(Year = as.integer(Year)) |>
  count(Year, Drone_type, name = "n") |>
  complete(Year = full_seq(Year, 1), Drone_type, fill = list(n = 0))

end_labels <- datlin2 |>
  group_by(Drone_type) |>
  filter(Year == max(Year[n > 0], default = max(Year))) |>
  slice_tail(n = 1) |>
  ungroup()
## manually change the text label positions to better fit because text_repel does not work that good
end_labels$Year <- c(2024, 2018, 2022, 2024, 2022)
end_labels$n <- c(4,3.5,1,55.5,3)

#table(data$Drone_type)
dro <- ggplot(datlin2[datlin2$Drone_type != "missing" & datlin2$n != "0",], aes(Year, n, color = Drone_type)) +
  geom_line(linewidth = 1.5) +
  geom_text(
    data = end_labels[end_labels$Drone_type != "missing",],
    aes(label = Drone_type),
    hjust = 0, nudge_x = 0.3, size = 3.5
  ) +
  scale_color_manual(values = pal2) +
  scale_x_continuous(breaks = c(2012,2015,2018,2021,2024),expand = expansion(mult = c(0.01, 0.35))) +
  scale_y_continuous(expand = c(0,0))+
  theme_classic() +
  theme(legend.position = "none") +
  labs(
  #  title = "UAS type used",
  #  subtitle = "data missing = 15",
    y = "",
    x = "Year of publication"
  )

dro


dro / UASbar + plot_layout(heights = c(4,1))

#
(sen / bar) + plot_layout(heights = c(4,1)) | 
  (dro / UASbar) + plot_layout(heights = c(4,1))#| 
 # (man / plot_spacer())+ plot_layout(heights = c(4,1))

#ggsave("figures/tech.pdf", width = 18, height = 10, units = "cm")

################################################################################

################################################################################


### plots about used drone platforms and then combined with used sensots
table(data$drone_grouped)
### sort drone factors and combine infrequent groups
data$drone_grouped <- fct_lump_n(fct_infreq(data$drone_grouped), n = 9) 
table(data$drone_grouped)


### group the DJI Models together
### combine other sensor types
table(data$DJI_model)
data$DJI_model <- case_when(
  data$DJI_model == "Matrice" ~ "Matrice",
  data$DJI_model == "Mavic" ~ "Mavic",
  data$DJI_model == "Phantom" ~ "Phantom",
  data$DJI_model == "Inspire" ~ "Inspire",
  data$DJI_model %in% c("Air", "Genie", "S1000", "Spark", "Spirit", "Wind", "Wizard", "Zenmuse") ~ "Other"
)
data$DJI_model <- factor(data$DJI_model,
                         levels = c("Other", "Inspire", "Mavic", "Matrice", "Phantom"))

##define a color palette
pal3 <- c(
  Phantom = "#e41a1c",
  Matrice = "#377eb8",
  Mavic = "#4daf4a",
  Inspire = "#984ea3",
  Other = "#a9a9a9"
)

man <- ggplot(data[!(is.na(data$drone_grouped)) & data$drone_grouped != "missing",], aes(x= drone_grouped, fill = DJI_model))+
  geom_bar(width = 0.8)+
  scale_y_continuous(expand = c(0,0))+
  theme_classic() +
  #  coord_flip()+
  scale_fill_manual(breaks = c("Phantom", "Matrice", "Mavic", "Inspire", "Other"),
                    values = pal3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(
    fill = guide_legend(
      #position = "inside", 
                             title = "DJI model"))+
 # theme(legend.position.inside = c(0.3,0.68))+
  labs(
    #title = "UAS Manufacturers",
     #  subtitle = "data missing = 18",
       y = "Number of observations", x = "")
man 

#ggsave("figures/manufacturer.pdf", width = 8, height = 8, units = "cm")

#
(sen / bar) + plot_layout(heights = c(4,1)) | 
  (dro / UASbar) + plot_layout(heights = c(4,1))| 
 (man / plot_spacer())+ plot_layout(heights = c(4,1))
























### additional plots

################################################################################
### vielleicht 2025 hier kürzen, da es das Bild ein bisschen verzerrt

sen <- sensors |>
  filter(Sensor %in% c("RGB", "multispectral", "hyperspectral", "LiDAR", "Other")) |>
  filter(Year != "2025") |>
  ggplot(aes(x=Year, color = Sensor))+
  geom_line(stat="count", linewidth = 1.5)+
  scale_color_manual(values = pal)+
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.12))) + # room for labels on right
  theme_minimal() +
  theme(legend.position = "none")+
  labs(title = "Sensors used in UAS studies", 
       subtitle = "data missing = 3",
       y = "number of studies", 
       x = "Year of Publication")
sen 
#ggsave("figures/sensors.pdf", width = 10, height = 6, units = "cm")

################################################################################
### barplot with percentages of usage

df <- sensors |>
  filter(Sensor %in% names(pal), Year != 2025) |>
  count(Sensor, name = "n") |>
  mutate(p = n / sum(n),
         Sensor = factor(Sensor, levels = names(pal))) |>
  arrange(Sensor) |>
  mutate(y = cumsum(p) - p/2, # center of each segment (along the bar)
         off_x = if_else(row_number() %% 2 == 0, -0.12, 0.12)) # up/down offset

df$Sensor <- factor(df$Sensor,
                    levels = c( "Other", "LiDAR", "hyperspectral", "multispectral", "RGB"))

bar <- ggplot(df, aes(x = 1, y = p, fill = Sensor)) +
  geom_col(width = 0.6, color = "white") +
  coord_flip(clip = "off") +
  geom_label(
    aes(x = 1 + off_x, y = y, label = Sensor),
    size = 3.25,
    label.padding = unit(0.25, "lines")
  ) +
  scale_fill_manual(values = pal) +
  theme_void() +
  theme(legend.position = "none")

sen / bar + plot_layout(heights = c(4,1))


################################################################################


### Sensor x drone

### remove rows where both (%) or one (|) parameteres are NA
### group articles by DOI and fill NAs by the variable that sits above 
### remove observations where all three variables are duplicated
###  distinct(DOI, Sensor, Habitat_quality_grouped,.keep_all = TRUE)
### remove rows where both (%) or one (|) parameteres are NA
### combine with metadata

drone <- data |>
  select(DOI, Sensor, drone_grouped, Drone_type) |>
  filter(!(is.na(Sensor) & is.na(drone_grouped))) |>
  group_by(DOI) |>
  fill(Sensor,.direction = "down") |> 
  fill(drone_grouped,.direction = "down") |>
  fill(Drone_type,.direction = "down") |>
  left_join(meta, by = "DOI") |>
  ungroup()

drone$Drone_type <- factor(drone$Drone_type,
                           levels = c("Rotor", "Fixed Wing", "Helicopter", "VTOL", "missing"))


### plot changes in used drone type over time
typ <- ggplot(drone[drone$Year != "2025",],aes(x=Year, color = Drone_type))+
  geom_line(stat="count", linewidth = 1.5)+
  scale_fill_brewer(palette = "Set2")+
  theme_minimal()
typ 

#ggsave("figures/dronetype.pdf", width = 10, height = 6, units = "cm")




sen + typ +  man 

#ggsave("figures/tech.pdf", width = 25, height = 6, units = "cm")










################################################################################
### plot drones and the sensors used with each drone
ggplot(drone,aes(x = drone_grouped, fill = Sensor))+
  geom_bar()+
  theme_minimal()+
  scale_fill_brewer(palette = "Set2")+
  coord_flip()+
  xlab("Drone model grouped")+
  ylab("")

### Tarot and microdrones occured 5 times

### plot drone type and the sensors used
drone$Drone_type <- fct_infreq(drone$Drone_type)
ggplot(drone,aes(x = Drone_type, fill = Sensor))+
  geom_bar()+
  theme_minimal()+
  scale_fill_brewer(palette = "Set2")+
  coord_flip()+
  xlab("Drone model grouped")+
  ylab("")


### Anteil an Rotor drohnen an allen Drohnen
table(data$Drone_type)
266/(35+5+15+266+2)

### Anteil an DJI Drohnen an allen
table(data$drone_grouped)
(12+14+67+26+96)/(4+1+5+12+14+67+26+96+5+4+5+2+35+3+3+19+5+2)
