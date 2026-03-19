# load packages 

library(tidyverse)
library(ggplot2)
library(ggstats)
library(readxl)
library(patchwork)
library(ggrepel)

################################################################################

### directly read the excel file and select rows for metadata (once per DOI) and other data: 
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

# Creation of an initial plot for the distribution of years and the sensors used
# new table with only sensor data and merge it through DOI with the metadata
sensors <- data |>
  select(DOI, Sensor) |>
  filter(!(is.na(Sensor))) |>
  left_join(meta, by = "DOI")

### combine other sensor types
table(data$Sensor)
sensors$Sensor <- case_when(
  sensors$Sensor %in% c("hyperspectral", "hyperspectral (panchromatic)") ~ "Hyperspectral",
  sensors$Sensor == "multispectral" ~ "Multispectral",
  sensors$Sensor == "LiDAR" ~ "LiDAR",
  sensors$Sensor == "RGB" ~ "RGB",
  sensors$Sensor == "missing" ~ "missing",
  TRUE ~ "Other"
)

### sort factors for the plots
sensors$Sensor <- factor(sensors$Sensor,
                         levels = c("RGB", "Multispectral", "Hyperspectral", "LiDAR", "Other", "missing"))

##define a color palette (by with cols4all)
pal <- c(
  RGB = "#E16A86",
  Multispectral = "#909800",
  Hyperspectral = "#00AD9A",
  LiDAR = "#9183E6",
  Other = "#a9a9a9"
)


################################################################################
### barplot with percentages written inside
# omit missing data for this visualisation
datbar <- sensors |>
  filter(Sensor %in% c("RGB", "Multispectral", "Hyperspectral", "LiDAR", "Other")) |>
  filter(Year != "2025")
# sort factors so they appear in the right order when plotted
datbar$Sensor <- factor(datbar$Sensor,
                      levels = c( "Other", "LiDAR", "Hyperspectral", "Multispectral", "RGB"))

bar <- ggplot(datbar, aes(x = "", fill = Sensor)) +
  geom_bar(position = "fill", width = 0.6, color = "white") +
  geom_text(
    stat = "count",
    aes(label = after_stat(ifelse(count / sum(count) >= 0.08,
                                  scales::percent(count / sum(count), 1), ""))),
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
### line plot of the sensor use over time (excluding 2025)
# omit missing data and data from 2025 and count observations per year
datlin <- sensors |>
  filter(Sensor %in% c("RGB", "Multispectral", "Hyperspectral", "LiDAR", "Other")) |>
  filter(Sensor %in% names(pal), Year != 2025) |>
  mutate(Year = as.integer(Year)) |>
  count(Year, Sensor, name = "n") |>
  complete(Year = full_seq(Year, 1), Sensor, fill = list(n = 0))

# define where labels should be plotted next to the end of each line
end_labels <- datlin |>
  group_by(Sensor) |>
  filter(Year == max(Year[n > 0], default = max(Year))) |>
  slice_tail(n = 1) |>
  ungroup()

# sort the factors
datlin$Sensor <- factor(datlin$Sensor,
                           levels = c("RGB", "Multispectral", "Hyperspectral", "LiDAR", "Other"))

# make a ggplot object
sen <- ggplot(datlin[datlin$Sensor != "missing",], aes(Year, n, color = Sensor)) +
  geom_line(linewidth = 1) +
  geom_text(
    data = end_labels[end_labels$Sensor != "missing",],
    aes(label = Sensor),
    hjust = 0, nudge_x = 0.3, size = 3.5
  ) +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = c(2012,2015,2018,2021,2024),expand = expansion(mult = c(0.01, 0.42))) +
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

## first combined visualitation to check for errors
sen / bar + plot_layout(heights = c(4,1))

################################################################################


################################################################################

### now start the plots for drone type in the same way as the sensor plots

drone <- data |>
  select(DOI, Drone_type) |>
  filter(!(is.na(Drone_type))) |>
  left_join(meta, by = "DOI")

##define a color palette
pal2 <- c(
  Rotor = "#E16A86",
  "Fixed Wing" = "#909800",
  Helicopter = "#00AD9A",
  VTOL = "#9183E6"
)

################################################################################
### barplot with percentages written inside for the use of UAS systems

# omit missing data and data from 2025
datbar2 <- drone |>
  filter(Drone_type%in% c("Rotor", "Fixed Wing", "Helicopter", "VTOL")) |>
  filter(Year != "2025")

# sort the factos
datbar2$Drone_type <- factor(datbar2$Drone_type,
                    levels = c( "VTOL", "Helicopter", "Fixed Wing", "Rotor"))

# make a ggplot plot object
UASbar <- ggplot(datbar2, aes(x = "", fill = Drone_type)) +
  geom_bar(position = "fill", width = 0.6, color = "white") +
  geom_text(
    stat = "count",
    aes(label = after_stat(ifelse(count / sum(count) >= 0.06,
                                  scales::percent(count / sum(count), 1), ""))),
    position = position_fill(vjust = 0.5),
    size = 3, col = "white"
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = pal2) +
  theme_void() +
  theme(legend.position = "none")+
  labs (subtitle = "    Overall usage of UAS types")

UASbar



################################################################################
### now start the line plot for the drone system use during time

# count drone type use per year

datlin2 <- drone |>
  filter(Drone_type %in% c("Rotor", "Fixed Wing", "Helicopter", "VTOL")) |>
  filter(Drone_type %in% names(pal2), Year != 2025) |>
  mutate(Year = as.integer(Year)) |>
  count(Year, Drone_type, name = "n") |>
  complete(Year = full_seq(Year, 1), Drone_type, fill = list(n = 0))

# define label positions for annotating the lines
end_labels <- datlin2 |>
  group_by(Drone_type) |>
  filter(Year == max(Year[n > 0], default = max(Year))) |>
  slice_tail(n = 1) |>
  ungroup()

## manually change the text label positions to better fit because text_repel does not work that good
end_labels$Year <- c(2025,2025,2025,2024,2025)
end_labels$n <- c(13,8,0,48,3)

# Define coordinates for lines connecting end labels to plotted lines
end_labels$x_start <- c(2024, 2021, 2023.8, 2023.5, 2023)
end_labels$y_start <- c(2, 1, 0, 56, 1)

# create a ggplot plot object
dro <- ggplot(datlin2[datlin2$Drone_type != "missing" ,], aes(Year, n, color = Drone_type)) +
  geom_line(linewidth = 1) +
  geom_text(
    data = end_labels[end_labels$Drone_type != "missing",],
    aes(label = Drone_type),
    hjust = 0, nudge_x = 0.3, size = 3.5
  ) +
  geom_segment(
    data = end_labels[end_labels$Drone_type!= c("missing", "Rotor"),],
    aes(x = x_start, y = y_start, xend = Year + 0.3, yend = n),
    linetype = "solid"
  ) +
  scale_color_manual(values = pal2) +
  scale_x_continuous(breaks = c(2012,2015,2018,2021,2024),expand = expansion(mult = c(0.01, 0.35))) +
  scale_y_continuous(breaks = c(0,20,40,60) , limits = c(0,55), expand = c(0,0))+
  theme_classic() +
  theme(legend.position = "none") +
  labs(
  #  title = "UAS type used",
  #  subtitle = "data missing = 15",
    y = "",
    x = "Year of publication"
  )

dro

## first visualisation to check for errors
dro / UASbar + plot_layout(heights = c(4,1))

# make the final plot layout and write a pdf file
plots <- (sen / bar) + plot_layout(heights = c(4.5,1)) | 
  (dro / UASbar) + plot_layout(heights = c(4.5,1)) 

plots + plot_annotation(tag_levels = list(c("A","","B","")))

#ggsave("figures/tech.pdf", width = 18, height = 10, units = "cm")






################################################################################

################################################################################

### plots about used drone platforms and then combined with used sensors


table(data$drone_grouped)

### sort drone factors and combine infrequent groups
data$drone_grouped <- fct_lump_n(fct_infreq(data$drone_grouped), n = 9) 
table(data$drone_grouped)


### group the DJI Models together
table(data$DJI_model)
data$DJI_model <- case_when(
  data$DJI_model == "Matrice" ~ "Matrice",
  data$DJI_model == "Mavic" ~ "Mavic",
  data$DJI_model == "Phantom" ~ "Phantom",
  data$DJI_model == "Inspire" ~ "Inspire",
  data$DJI_model %in% c("Air", "Genie", "S1000", "Spark", "Spirit", "Wind", "Wizard", "Zenmuse") ~ "Other"
)

# sort the factors
data$DJI_model <- factor(data$DJI_model,
                         levels = c("Other", "Inspire", "Mavic", "Matrice", "Phantom"))

##define a color palette
pal3 <- c(
  Phantom = "#E16A86",
  Matrice = "#909800",
  Mavic = "#00AD9A",
  Inspire = "#9183E6",
  Other = "#a9a9a9"
)

## make a ggplot plot
man <- ggplot(data[!(is.na(data$drone_grouped)) & data$drone_grouped != "missing",], aes(x= drone_grouped, fill = DJI_model))+
  geom_bar(width = 0.8)+
  scale_y_continuous(expand = c(0,0), limits = c(0,240))+
  theme_classic() +
  scale_fill_manual(breaks = c("Phantom", "Matrice", "Mavic", "Inspire", "Other"),
                    values = pal3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+
  guides(
    fill = guide_legend(title = "DJI model"))+
  labs(#title = "UAS Manufacturers",
       #subtitle = "data missing = 18",
       y = "Number of observations", x = "")+
  annotate("text", x = 2.7, y = 85, label = "Phantom", size = 3.5)+
  annotate("text", x = 2.5, y = 155, label = "Matrice", size = 3.5)+
  annotate("text", x = 2.3, y = 178, label = "Mavic", size = 3.5)+
  annotate("text", x = 2.4, y = 198, label = "Inspire", size = 3.5)
 # annotate("text", x = 2.8, y = 230, label = "DJI Model", fontface = "bold", size = 3.5)
man 

#ggsave("figures/manufacturer.pdf", width = 7, height = 8, units = "cm")




