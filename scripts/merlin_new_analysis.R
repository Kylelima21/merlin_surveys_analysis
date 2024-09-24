### Merlin Sound ID in point counts ms analysis
### Schoodic Institute at Acadia National Park, 2023 - 2024


#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

### Call necessary packages
library(tidyverse)
library(rgdal)
library(sf)
library(leaflet)
library(htmlwidgets)
library(webshot2)
library(lme4)
library(ggtext)
library(emmeans)


### Source the script with custom functions for use in analysis
source("scripts/analysis_functions.R")



#------------------------------------------------#
####        Data Import and Cleaning          ####
#------------------------------------------------#

### Read in, format as tibble, and clean
## We are excluding data from 2023-07-13 and 2023-10-06 because those surveys
## were conducted by experienced observers.
merl <- read.csv("data/merlin_survey_data_20231110.csv") %>% 
  tibble() %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y"),
         device = str_replace(device, "phone", "Phone"),
         device = str_replace(device, "pro", "Pro"),
         device = str_replace(device, "mini", "Mini"),
         device = str_replace(device, "samsung_galaxy_tab_A", 
                              "Samsung_Galaxy_Tab_A7 Lite"),
         device = str_replace_all(device, "\\_", " "),
         device = ifelse(is.na(device) & obs.method == "hearing", "human", device),
         species.code = str_replace(species.code, "AMGO", "AGOL"),
         point.number = factor(point.number, levels = c("RC1", "RC2", "RC3",
                                                        "RC4", "RC5", "RC6",
                                                        "RC7", "RC8", "RC9", 
                                                        "RC10")),
         temp = ifelse(date == "2023-08-07", 67, temp)) %>% 
  filter(date != "2023-07-13" & date != "2023-10-06")




#------------------------------------------------#
####             Study Area Maps              ####
#------------------------------------------------#

#### Figure 1 sub figures, edited to final version in PowerPoint

### Maine sub figure
## Create map bounds
maxLong = -71.096425 + 0.7
maxLat = 47.444973 - 0.7
minLong = -66.996655 - 0.7
minLat = 43.063393 + 0.7


## Put label info into a data frame
loc <- c("Schoodic Forest", "Maine, USA")
long <- c(-68.063206, -69.1)
lat <- c(44.410149, 45.32)
data <- data.frame(loc, lat, long)
maine <- tibble(data) %>% filter(loc == "Maine, USA")
sfor <- tibble(data) %>% filter(loc == "Schoodic Forest")


## Read in state polygons
state <- readOGR("data/st_us.kml")
state_polygon <- spTransform(state, CRS("+init=epsg:4326"))


## Format for mapping
lyr <- ogrListLayers("data/st_us.kml")
mykml <- lapply(lyr, function(i) readOGR("data/st_us.kml", i))
names(mykml) <- lyr


## Plot Maine map with leaflet
me_map <- leaflet::leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                    options = providerTileOptions(maxZoom = 9)) %>% 
  leaflet::addPolygons(data = mykml$MAINE, weight = 1, opacity = 1,
                       color = "white", fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$`NEW HAMPSHIRE`, weight = 1, opacity = 1, 
                       color = "white", fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$VERMONT, weight = 1, opacity = 1, 
                       color = "white", fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$MASSACHUSETTS, weight = 1, opacity = 1, 
                       color = "white", fillOpacity = 0) %>% 
  leaflet::addPolygons(data = mykml$`NEW YORK`, weight = 1, opacity = 1, 
                       color = "white", fillOpacity = 0) %>% 
  leaflet::addLabelOnlyMarkers(sfor$long, sfor$lat, label = "TE",
                               labelOptions = labelOptions(
                                 noHide = T,
                                 direction = "center",
                                 textOnly = T,
                                 textsize = "25px",
                                 style = list(
                                   "color" = "rgb(0, 0, 0, 0)",
                                   "box-sizing" = "border-box",
                                   "border-radius" = "0",
                                   "background-color" = "transparent",
                                   "border" = "black solid 4px",
                                   "font-weight" = "600",
                                   "text-align" = "center",
                                   "padding" = "1px 4px",
                                   "box-shadow" = "3px 3px 1rem rgba(0, 0, 0, 0.4)"
                                 ))) %>%
  leaflet::fitBounds(minLong, minLat, maxLong, maxLat)


## View map
me_map


## Save map (uncomment to save)
# saveWidget(me_map, "outputs/temp.html", selfcontained = FALSE)
# webshot("outputs/temp.html", 
#         file = "outputs/forpub/extras/maine_panel.png", zoom = 5)



#------------------------------------------------#

### Schoodic Forest sub figure
## Read in shapefile
sforest <- read_sf("data/schoodic_forest/11168_Survey North of 186 2021.shp") %>% 
  st_transform(crs = '+proj=longlat +datum=WGS84')


## Plot Cadillac map with leaflet
sf_map <- leaflet::leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                    options = providerTileOptions(maxZoom = 11)) %>%
  leaflet::addPolygons(data = sforest, color = "white", opacity = 100, 
                       fillOpacity = 0, weight = 1.5)


## View map
sf_map


## Save map (uncomment to save)
# saveWidget(sf_map, "outputs/temp.html", selfcontained = FALSE)
# webshot("outputs/temp.html", 
#         file = "outputs/forpub/extras/sfor_panel_empty.png", zoom = 4)



#------------------------------------------------#

#### Figure 2 (point count sites) created in QGIS




#------------------------------------------------#
####             Summary Stats                ####
#------------------------------------------------#

### Number of individual road counts
merl %>% 
  dplyr::select(date, point.number) %>%
  arrange(date) %>% 
  distinct()
# 144 surveys



### Number of days surveyed
merl %>% 
  dplyr::select(date) %>% 
  distinct()
# 15 days



### Percent of human IDs from audio
nrow(merl %>% filter(obs.method == "hearing" & type.a.v == "a")) / 
  nrow(merl %>% filter(obs.method == "hearing" & !is.na(type.a.v))) * 100  
# 98.06763



### Total number of species
## Make list of species
spp <- merl %>% 
  mutate(correct.id = ifelse(is.na(correct.id), 
                             "yes", correct.id)) %>% 
  filter(correct.id != "no") %>% 
  filter(species.code != "none" & species.code != "warbler sp." & 
           species.code != "bird sp." & species.code != "sparrow sp." & 
           species.code != "Empidonax sp." & species.code != "UNK") %>% 
  distinct(species.code) %>% 
  arrange(species.code)


## Calculate total species
length(spp$species.code) # 50 species


## Write out species list (uncomment to save)
# write.csv(spp, "outputs/2023_summary/merlin_survey_species_list.csv", 
#           row.names = F)



### Number of species identified from the recordings post-hoc
doc.spp <- merl %>% 
  filter(obs.method == "playback") %>% 
  filter(species.code != "none" & species.code != "warbler sp." &
           species.code != "bird sp." & species.code != "sparrow sp." &
           species.code != "Empidonax sp." & species.code != "UNK") %>%
  dplyr::select(species.code) %>% 
  distinct() %>% 
  arrange(species.code)

length(doc.spp$species.code) # 45 species



#------------------------------------------------#
####            Detection Heatmap             ####
#------------------------------------------------#

merl %>% 
  select(date, point.number) %>%
  distinct() %>% 
  group_by(date) %>% 
  summarise(num.surveys = length(point.number))


heatdat <- merl %>%
  filter(date != "2023-09-18" & date != "2023-10-11")


splists <- heatdat %>% 
  # filter(correct.id == "yes" | correct.id == "unverifiable" |
  #          is.na(correct.id)) %>% 
  filter(species.code != "UNK" & species.code != "none" & 
           species.code != "bird sp." & species.code != "warbler sp." & 
           species.code != "sparrow sp.") %>% 
  select(species.code) %>% 
  distinct() %>% 
  arrange(species.code)


spdfhm <- bind_rows(splists, splists) %>% 
  bind_rows(., splists) %>% 
  bind_rows(., splists) %>% 
  bind_rows(., splists) %>% 
  bind_rows(., splists) %>% 
  bind_rows(., splists) %>% 
  bind_rows(., splists) %>% 
  bind_rows(., splists) %>% 
  bind_rows(., splists) %>% 
  bind_cols(., data.frame(point.number = rep(paste0("RC", seq(10)), 59)) %>% 
              mutate(point.number = factor(point.number, 
                                           levels = c("RC1", "RC2", "RC3",
                                                      "RC4", "RC5", "RC6",
                                                      "RC7", "RC8", "RC9", 
                                                      "RC10"))) %>% 
              arrange(point.number))


nmsurv <- heatdat %>% 
  select(date, point.number) %>%
  distinct() %>% 
  group_by(point.number) %>% 
  summarise(num.surveys = length(point.number))


test_mat <- matrix(0, 59, 10)
colnames(test_mat) <- paste0("RC", seq(ncol(test_mat)))


bind_cols(test_mat, splists) %>% 
  select(species.code, everything())


hearhm <- heatdat %>% 
  filter(obs.method == "hearing") %>%
  mutate(point.id = paste0(date, "-", point.number), pres = 1) %>% 
  group_by(point.number, species.code) %>% 
  summarise(count = sum(pres)) %>% 
  filter(species.code != "UNK" & species.code != "none") %>% 
  left_join(., nmsurv, by =  "point.number") %>% 
  mutate(count = count) %>% 
  select(point.number, species.code, count) %>% 
  # pivot_wider(names_from = point.number,
  #             values_from = perc)
  full_join(., spdfhm, by = c("point.number", "species.code"))


hearhm.correct <- heatdat %>% 
  filter(obs.method == "hearing") %>%
  mutate(point.id = paste0(date, "-", point.number), pres = 1) %>% 
  filter(correct.id == "yes" | correct.id == "unverifiable") %>% 
  group_by(point.number, species.code) %>% 
  summarise(count = sum(pres)) %>% 
  filter(species.code != "UNK" & species.code != "none") %>% 
  left_join(., nmsurv, by =  "point.number") %>% 
  mutate(count = count) %>% 
  select(point.number, species.code, count) %>% 
  full_join(., spdfhm, by = c("point.number", "species.code"))


merlhm <- heatdat %>% 
  filter(obs.method == "merlin") %>%
  select(date, point.number, species.code) %>% 
  distinct() %>% 
  mutate(pres = 1) %>% 
  group_by(point.number, species.code) %>% 
  summarise(count = sum(pres)) %>% 
  filter(species.code != "UNK" & species.code != "none" & 
           species.code != "sparrow sp.") %>% 
  left_join(., nmsurv, by = "point.number") %>% 
  mutate(count = count) %>% 
  select(point.number, species.code, count) %>% 
  # pivot_wider(names_from = point.number,
  #             values_from = perc) %>% 
  full_join(., spdfhm, by = c("point.number", "species.code"))


merlhm.correct <- heatdat %>% 
  filter(obs.method == "merlin") %>%
  filter(correct.id == "yes") %>% 
  select(date, point.number, species.code) %>% 
  distinct() %>% 
  mutate(pres = 1) %>% 
  group_by(point.number, species.code) %>% 
  summarise(count = sum(pres)) %>% 
  filter(species.code != "UNK" & species.code != "none" & 
           species.code != "sparrow sp.") %>% 
  left_join(., nmsurv, by = "point.number") %>% 
  mutate(count = count) %>% 
  select(point.number, species.code, count) %>% 
  full_join(., spdfhm, by = c("point.number", "species.code"))


pbhm <- heatdat %>% 
  filter(obs.method == "playback") %>%
  mutate(point.id = paste0(date, "-", point.number), pres = 1) %>% 
  #filter(correct.id == "yes" | correct.id == "unverifiable") %>% 
  group_by(point.number, species.code) %>% 
  summarise(count = sum(pres)) %>% 
  filter(species.code != "UNK" & species.code != "none" &
           species.code != "bird sp." & species.code != "warbler sp.") %>% 
  left_join(., nmsurv, by = "point.number") %>% 
  mutate(count = count) %>% 
  select(point.number, species.code, count) %>% 
  full_join(., spdfhm, by = c("point.number", "species.code"))


# pbhm %>% ungroup() %>% select(species.code) %>% distinct(species.code)


bind_rows(pbhm %>% mutate(obs = "playback"), merlhm %>% mutate(obs = "merlin"), 
          hearhm %>% mutate(obs = "human")) %>%  
  ggplot() +
  geom_tile(aes(y = species.code, x = point.number,
                fill = perc)) +
  facet_wrap("obs") +
  labs(x = "Point number", y = "Species") +
  scale_fill_continuous(na.value = "gray92") +
  scale_y_discrete(limits = rev) +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5))


ggsave("outputs/detection_heatmap_alldata.png", width = 6.0, height = 7.8, dpi = 700)



####
count.clean <- bind_rows(pbhm %>% mutate(obs = "playback"), merlhm %>% mutate(obs = "merlin"), 
          hearhm %>% mutate(obs = "human")) %>% 
  arrange(obs, point.number, species.code) %>% 
  mutate(count = ifelse(is.na(count), 0, count))


count.clean %>% 
  pivot_wider(., names_from = "point.number", values_from = c("count")) %>%
  mutate(across(RC1:RC10, ~replace(., . > 1, 1)),
         sum = rowSums(across(where(is.numeric)))) %>% 
  write.csv(., "outputs/allthree_detection_data.csv", row.names = F)



####
count.clean.cor <- bind_rows(pbhm %>% mutate(obs = "playback"), merlhm.correct %>% mutate(obs = "merlin"), 
                         hearhm.correct %>% mutate(obs = "human")) %>% 
  arrange(obs, point.number, species.code) %>% 
  mutate(count = ifelse(is.na(count), 0, count))


count.clean.cor %>% 
  pivot_wider(., names_from = "point.number", values_from = c("count")) %>%
  mutate(across(RC1:RC10, ~replace(., . > 1, 1)),
         sum = rowSums(across(where(is.numeric)))) %>% 
  write.csv(., "outputs/correct_detection_data.csv", row.names = F)


