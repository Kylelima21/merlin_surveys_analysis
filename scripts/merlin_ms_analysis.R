### Merlin Sound ID in point counts ms analysis
### Schoodic Institute at Acadia National Park, 2023


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
# library(ggeffects)
library(ggtext)


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
####          Correct IDs by method           ####
#------------------------------------------------#

### Begin to clean up the 'merl' data frame for use in modeling
## Remove non species-level obs
## Collect only correct identifications (for human observers unverifiable too)
## Get unique species per observation method and point count
## Calculate number of species for each method and point count
divdat <- merl %>% 
  filter(species.code != "none" & species.code != "warbler sp." &
           species.code != "bird sp." & species.code != "sparrow sp." &
           species.code != "Empidonax sp." & species.code != "UNK") %>% 
  filter(obs.method == "hearing" | obs.method == "merlin") %>%
  filter(correct.id != "no") %>% 
  select(date, obs.method, device, point.number, species.code, sky, wind, noise, 
         temp, time) %>% 
  distinct() %>% 
  group_by(date, obs.method, device, point.number, sky, wind, noise, 
           temp, time) %>% 
  summarise(num.spp = length(species.code))


### Create list of all point counts on each day for adding zeros
## Filter to only the Merlin and human data
## Return the list of all unique combinations of day and point count site
mis.rows <- merl %>% 
  filter(obs.method == "hearing" | obs.method == "merlin") %>% 
  select(date, obs.method, device, point.number, sky, wind, noise, 
         temp, time) %>% 
  group_by(date, obs.method, device, point.number, sky, wind, noise, 
           temp, time) %>% 
  distinct()


### Format data for poisson regression model
## Join the two data sets so that pc's with zero observations are included
## Rename covariate columns and add in formatted time since midnight column
divdatfull <- full_join(divdat, mis.rows, by = c("date", "obs.method", "device",
                                                 "point.number")) %>% 
  mutate(sky = coalesce(sky.x, sky.y),
         wind = coalesce(wind.x, wind.y),
         noise = coalesce(noise.x, noise.y),
         temp = coalesce(temp.x, temp.y),
         juldate = as.integer(format(date, "%j")),
         month = as.integer(month(date)),
         time = coalesce(time.x, time.y),
         time = strptime(time, format = "%H:%M"),
         midn = strptime("00:00:00", format = "%H:%M"),
         tsm = round(as.double(str_extract(difftime(midn, time), 
                                           "\\d*\\.\\d*")), digits = 2),
         num.spp = ifelse(is.na(num.spp), 0, num.spp)) %>% 
  select(date, month, obs.method, device, point.number, num.spp, sky, wind,
         noise, temp, time, tsm, juldate) %>% 
  arrange(date, obs.method, device, point.number) %>% 
  ungroup()


### Run a poisson regression for number of species by method (human pc/Merlin)
## Fixed effects: month
## Random effect: device, point count site (point.number)
modeldiv <- glmer(num.spp ~ obs.method + month + (1 | device) + (1 | point.number), 
                  data = divdatfull, family = poisson)


### Explore model fit and results
explore_model(modeldiv)


### Test model for overdispersion using B. Bolker's suggested test
## Model is slightly underdispersed (ratio = 0.85)
overdisp_fun(modeldiv)


### Use ggpredict function to predict estimates for number of species on 
### a point-count for human observers and Merlin
# divmodspp <- ggpredict(modeldiv, terms = c("obs.method"))


### Calculate estimated marginal means
emcorl <- emmeans(modeldiv, "obs.method", type = "response")
emcorl


### Pairwise comparison between human observers and Merlin
pairs(emcorl)



#------------------------------------------------#

### Verifiable only (species that could be identified from recording)
## Remove non species-level obs
## Collect only correct identifications (not including unverified human obs)
## Get unique species per observation method and point-count
## Calculate number of species for each method and point-count
divdatc <- merl %>% 
  filter(species.code != "none" & species.code != "warbler sp." &
           species.code != "bird sp." & species.code != "sparrow sp." &
           species.code != "Empidonax sp." & species.code != "UNK") %>% 
  filter(obs.method == "hearing" | obs.method == "merlin") %>%
  filter(correct.id == "yes") %>% 
  select(date, obs.method, device, point.number, species.code, sky, wind, noise, 
         temp, time) %>% 
  distinct() %>% 
  group_by(date, obs.method, device, point.number, sky, wind, noise, 
           temp, time) %>% 
  summarise(num.spp = length(species.code))


### Format data for poisson regression model
## Join the two data sets so that pc's with zero observations are included
## Rename covariate columns and add in formatted time since midnight column
divdatfullc <- full_join(divdatc, mis.rows, by = c("date", "obs.method", "device",
                                                 "point.number")) %>% 
  mutate(sky = coalesce(sky.x, sky.y),
         wind = coalesce(wind.x, wind.y),
         noise = coalesce(noise.x, noise.y),
         temp = coalesce(temp.x, temp.y),
         juldate = as.integer(format(date, "%j")),
         month = as.integer(month(date)),
         time = coalesce(time.x, time.y),
         time = strptime(time, format = "%H:%M"),
         midn = strptime("00:00:00", format = "%H:%M"),
         tsm = round(as.double(str_extract(difftime(midn, time), 
                                           "\\d*\\.\\d*")), digits = 2),
         num.spp = ifelse(is.na(num.spp), 0, num.spp)) %>% 
  select(date, month, obs.method, device, point.number, num.spp, sky, wind,
         noise, temp, time, tsm, juldate) %>% 
  arrange(date, obs.method, device, point.number) %>% 
  ungroup()


### Run a poisson regression for number of species by method (human pc/Merlin)
## Fixed effects: month
## Random effect: device, point count site (point.number)
modeldivc <- glmer(num.spp ~ obs.method + month + (1 | device) + (1 | point.number), 
                  data = divdatfullc, family = poisson)


### Explore model fit and results
explore_model(modeldivc)


### Test model for overdispersion using B. Bolker's suggested test
## Model is not overdispersed (ratio = 0.96)
overdisp_fun(modeldivc)


### Use ggpredict function to predict estimates for number of species on 
### a point-count for human observers and Merlin
# divmodsppc <- ggpredict(modeldivc, terms = c("obs.method"))


### Calculate estimated marginal means
emcor <- emmeans(modeldivc, "obs.method", type = "response")
emcor


### Pairwise comparison between human observers and Merlin
pairs(emcor)




#------------------------------------------------#
####        Incorrect IDs by method           ####
#------------------------------------------------#

### Format data for model
## Filter data and calculate number of of incorrect IDs per point count
incdat <- merl %>% 
  filter(obs.method == "hearing" | obs.method == "merlin") %>% 
  filter(correct.id == "no") %>% 
  group_by(date, obs.method, device, point.number) %>% 
  summarise(num.inc = length(correct.id))

## Join with the mis.rows data frame to add zeros
inccomb <- full_join(incdat, mis.rows) %>% 
  ungroup() %>% 
  mutate(num.inc = ifelse(is.na(num.inc), 0, num.inc),
         juldate = format(date, "%j"),
         month = as.integer(month(date)),
         pointdev = paste(point.number, device)) %>% 
  arrange(date, obs.method, device, point.number)


### Run a poisson model for number of incorrect IDs by method (human pc/Merlin)
## Fixed effects: month
## Random effect: device, point count site (point.number)
modinc <- glmer(num.inc ~ obs.method + month + (1 | device) + (1 | point.number), 
                data = inccomb, family = poisson)


### Explore model fit and results
explore_model(modinc)


### Test model for overdispersion using B. Bolker's suggested test
## Model is slightly underdispersed (ratio = 0.89)
overdisp_fun(modinc)


### Use ggpredict function to predict estimates for number of incorrect 
### species identifications
# divmodin <- ggpredict(modinc, terms = c("obs.method"))


### Calculate estimated marginal means
eminc <- emmeans(modinc, "obs.method", type = "response")
eminc


### Pairwise comparison between human observers and Merlin
pairs(eminc)




#------------------------------------------------#
####           Omissions by method            ####
#------------------------------------------------#

### Format Human observer data for modeling
## Get full list of sites and observers for human observer data
main.obs <- merl %>% 
  filter(obs.method == "hearing") %>% 
  select(date, point.number, observer.initials) %>% 
  group_by(date, point.number) %>% 
  distinct() %>% 
  arrange(date, point.number)


## Create the data for all human obs joining with the main.obs to get zeros
human.all <- merl %>% 
  filter(obs.method == "hearing" | obs.method == "playback") %>% 
  group_by(date, point.number, species.code) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(correct.id = ifelse(is.na(correct.id) & obs.method == "playback", 
                             "missed", correct.id)) %>% 
  left_join(main.obs, by = c("date", "point.number")) %>% 
  select(date, observer.initials = observer.initials.y, obs.method:correct.id) %>% 
  filter(species.code != "none" & species.code != "warbler sp." &
           species.code != "bird sp." & species.code != "sparrow sp." &
           species.code != "Empidonax sp." & species.code != "UNK")


## Finalize the human data
human.cm <- human.all %>% 
  mutate(device = "human",
         obs.method = "hearing") %>% 
  filter(correct.id == "missed") %>% 
  group_by(date, device, point.number) %>% 
  summarise(num.mis = length(species.code)) %>% 
  full_join(., mis.rows) %>% 
  ungroup() %>% 
  filter(obs.method == "hearing") %>% 
  mutate(num.mis = ifelse(is.na(num.mis), 0, num.mis)) %>% 
  arrange(date, point.number)


### Format Merlin data for modeling
## Get list of point counts and devices for Merlin data
device_list <- merl %>% 
  distinct(device) %>% 
  filter(!is.na(device) & device != "human") %>% 
  unlist()


## Map over the custom merldat_calc function for each device
merlin.cm <- map_dfr(device_list, merldat_calc) %>% 
  arrange(date, device, point.number)


### Combine the human observer data and the Merlin data for the model
merhum.cm <- bind_rows(human.cm, merlin.cm) %>% 
  mutate(juldate = format(date, "%j"),
         month = as.integer(month(date)))


### Run a poisson regression for number of omitted species by method
## Fixed effects: month
## Random effect: device, point count site (point.number)
modelomis <- glmer(num.mis ~ obs.method + month + (1 | device) + (1 | point.number), 
                   data = merhum.cm, family = poisson)


### Explore model fit and results
explore_model(modelomis)


### Test model for overdispersion using B. Bolker's suggested test
## Model is slightly underdispersed (ratio = 0.93)
overdisp_fun(modelomis)


### Use ggpredict function to predict estimates for number of species
### omitted for human observers and Merlin
# divmodomis <- ggpredict(modelomis, terms = c("obs.method"))


### Calculate estimated marginal means
emomis <- emmeans(modelomis, "obs.method", type = "response")
emomis


### Pairwise comparison between human observers and Merlin
pairs(emomis)




#------------------------------------------------#
####     Merlin-Human Comparison Figure       ####
#------------------------------------------------#

### Make figure for the 'accuracy' metrics calculated in the previous sections
## Bind all the emmeans for each metric
compfigdat <- bind_rows(as_tibble(emcorl) %>% mutate(metric = "All correct"),
          as_tibble(emcor) %>% mutate(metric = "Verified correct"),
          as_tibble(eminc) %>% mutate(metric = "Incorrect"),
          as_tibble(emomis) %>% mutate(metric = "Omitted")) %>% 
  mutate(metric = factor(metric, levels = c("All correct",
                                            "Verified correct",
                                            "Incorrect",
                                            "Omitted")))


## Create data frame for significance labels
barlabs <- data.frame(metric = c("All correct", "Omitted"),
           rate = c(3, 1.4),
           denot = c("*", "*"))


## Plot with some quick html formatting
compfigdat %>% 
  mutate(obs.method = ifelse(obs.method == "hearing", "Human observers", 
                             "<i>Merlin</i>"),
         obs.method = factor(obs.method, levels = c("Human observers", 
                                                    "<i>Merlin</i>"))) %>% 
  ggplot(aes(x = metric, y = rate, fill = obs.method)) +
  # geom_bar(stat = "identity", position = position_dodge()) +  # bar graph
  geom_errorbar(aes(ymin = rate - SE, ymax = rate + SE), 
                width = 0.12, position = position_dodge(0.2)) +
  geom_point(aes(color = obs.method, shape = obs.method), 
             position = position_dodge(width = 0.2), size = 2.2) +
  geom_text(aes(x = metric, y = rate, label = denot, fill = NULL), 
            data = barlabs, size = 6, fontface = "bold") +
  labs(x = "Metric", y = "Mean rate") +
  ylim(ymin = 0, ymax = 3.2) +
  scale_color_manual(values = c("Human observers" = "black",
                       "<i>Merlin</i>" = "gray60")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(linewidth = 1),
        axis.text = element_text(color = "black", size = "11"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        axis.ticks.length = unit(0.13, "cm"),
        legend.position = "inside",
        legend.position.inside = c(0.83,0.9),
        legend.title = element_blank(),
        legend.text = element_markdown(size = "11"))


## Save (uncomment to save)
ggsave(paste0("outputs/forpub/figure_accuracy_", str_replace_all(today(), "-", ""), ".png"),
       height = 5, width = 7, units = "in", dpi = 700)




#------------------------------------------------#
####        Merlin v Humans v Both            ####
#------------------------------------------------#

### Format data for binomial model
## Create data frame to merge later and get zeros
joiner <- merl %>% 
  select(date, obs.method, point.number, sky, wind, noise, temp) %>% 
  group_by(date, obs.method, point.number, sky, wind, noise, temp) %>% 
  distinct()


## Manipulate data to get human only, Merlin only, and combined detections 
thedat <- merl %>%
  mutate(correct.id = ifelse(obs.method == "playback", "yes", correct.id),
         device = ifelse(obs.method == "playback", "playback", device)) %>%
  filter(correct.id == "yes") %>%
  select(date, obs.method, device, point.number, species.code, sky, wind, noise, temp) %>%
  full_join(., joiner) %>%
  group_by(date, obs.method, point.number, species.code) %>%
  slice_sample(n = 1) %>%
  ungroup() %>%
  mutate(playback.tp = ifelse(obs.method == "playback" & !is.na(device), 1, 0),
         human.tp = ifelse(obs.method == "hearing" & !is.na(device), 1, 0),
         merlin.tp = ifelse(obs.method == "merlin" & !is.na(device), 1, 0)) %>%
  group_by(date, point.number, species.code, sky, wind, noise, temp) %>%
  summarise(playback.tp = sum(playback.tp),
            human.tp = sum(human.tp),
            merlin.tp = sum(merlin.tp)) %>%
  mutate(code = paste0(playback.tp, human.tp, merlin.tp)) %>%
  filter(code != "000") %>%
  mutate(code.chr = ifelse(code == "111", "both_tp",
                           ifelse(code == "110", "merlin_fn",
                                  ifelse(code == "101", "human_fn",
                                         ifelse(code == "100", "both_fn", "error"))))) %>%
  arrange(date, point.number)


## Calculate total number of species per point count from expert 
## review of recordings
Ns <- thedat %>% 
  ungroup() %>% 
  group_by(date, point.number) %>% 
  summarise(N = sum(playback.tp))


## Get number of species detected by each (human, Merlin, both) per point count
ybytype <- thedat %>% 
  ungroup() %>% 
  select(-code.chr) %>% 
  mutate(either = human.tp + merlin.tp,
         either = ifelse(either > 0, 1, either)) %>% 
  group_by(date, point.number) %>% 
  summarise(human = sum(human.tp),
            merlin = sum(merlin.tp),
            combined = sum(either)) %>% 
  pivot_longer(cols = c(human, merlin, combined))


## Join these two so we can calculate the number of undetected species per count
binomdat <- left_join(ybytype, Ns, by = c("date", "point.number")) %>% 
  select(date, point.number, N, obs.type = name, y = value) %>% 
  mutate(undetected = N - y,
         point.count.id = paste(point.number, date),
         obs.type = ifelse(obs.type == "combined", "Combined",
                           ifelse(obs.type == "human", "Human only",
                                  ifelse(obs.type == "merlin", "<i>Merlin</i> only", "ERROR"))),
         obs.type = factor(obs.type, levels = c("Combined", "Human only", "<i>Merlin</i> only")))


### Run a binomial regression for number of detections by method
## Random effect: unique point count ID for each point count
m <- glmer(cbind(y, undetected) ~ obs.type + (1 | point.count.id),
          family = binomial, data = binomdat)


### Explore model fit and results
explore_model(m)


### Calculate estimated marginal means
## Note that type = response puts the results on the probability scale
## which makes results an estimate of detection probability
ems <- emmeans(m, "obs.type", type = "response")
ems


## Get pairwise comparisons among the three treatment groups
pairs(ems)


### Test model for overdispersion using B. Bolker's suggested test 
## See analysis_functions.R script for this test specifics
## Model is quite underdispersed
overdisp_fun(m)



#------------------------------------------------#

### Create detection probability figure
## Plot with ggplot
as_tibble(ems) %>% 
  mutate(siglab = c("a", "b", "c")) %>% 
  ggplot() +
  geom_point(aes(x = obs.type, y = prob), size = 1.8) +
  geom_errorbar(aes(x = obs.type, ymin = prob - SE, ymax = prob + SE), 
                width = 0.05) +
  geom_text(aes(x = obs.type, y = prob, label = siglab),
            nudge_y = 0.1, nudge_x = 0) +
  ylim(0, 1) +
  labs(x = "Observation type", y = "Detection probability") +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black", size = "11"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        axis.ticks.length = unit(0.13, "cm"),
        axis.text.x = element_markdown())


## Save (uncomment to save)
# ggsave(paste0("outputs/forpub/figure_detectionprob_", str_replace_all(today(), "-", ""), ".png"),
#        height = 5, width = 7, units = "in", dpi = 700)




#------------------------------------------------#
####       Species-specific analysis          ####
#------------------------------------------------#

sspbn <- function(sp) {

  Nsp <- thedat %>% 
    ungroup() %>% 
    filter(species.code == sp) %>% 
    group_by(date, point.number) %>% 
    summarise(N = sum(playback.tp))
  
  
  ybytypesp <- thedat %>% 
    ungroup() %>% 
    select(-code.chr) %>%
    filter(species.code == sp) %>% 
    mutate(combined = human.tp + merlin.tp,
           combined = ifelse(combined > 0, 1, combined)) %>% 
    group_by(date, point.number) %>% 
    summarise(human = sum(human.tp),
              merlin = sum(merlin.tp)) %>% 
    #combined = sum(combined)) %>% 
    pivot_longer(cols = c(human, merlin))
  
  
  binomdatsp <- left_join(ybytypesp, Nsp, by = c("date", "point.number")) %>% 
    select(date, point.number, N, obs.type = name, y = value) %>% 
    mutate(undetected = N - y,
           point.count.id = paste(point.number, date))
  # obs.type = ifelse(obs.type == "either", "Combined",
  #                   ifelse(obs.type == "human", "Human only",
  #                          ifelse(obs.type == "merlin", "Merlin only", "ERROR"))))
  
  
  # fisher.test(binomdatsp$y, binomdatsp$obs.type)
  
  
  ##analyze the data with a binomial regression
  msp = glmer(y ~ obs.type + (1 | point.count.id), family = binomial, 
              data = binomdatsp)
  
  explore_model(msp)
  
  
  ##calculate estimated marginal means
  ##note that type = response puts the results on the probability scale
  ##which makes results an estimate of detection probability
  emssp = emmeans(msp, "obs.type", type = "response")
  emssp
  
  
  ##get pairwise comparisons among the three treatment groups
  pairssp <- as_tibble(pairs(emssp)) %>% 
    mutate(species = sp) %>% 
    rename(oddsSE = SE) %>% 
    select(species, contrast, odds.ratio, oddsSE, z.ratio, p.value)
  
  
  emmssptab <- as_tibble(emssp) %>% 
    mutate(asymp.LCL = round(asymp.LCL, 3),
           asymp.UCL = round(asymp.UCL, 3),
           CI = paste0(asymp.LCL, "-", asymp.UCL),
           species = sp) %>% 
    rename(probSE = SE) %>% 
    select(species, obs.type, prob, probSE, CI)
  
  
  findat <- bind_cols(emmssptab, pairssp) %>%
    rename(species = species...1) %>%
    select(-species...6)


 return(findat)

}


input


map_dfr(input, ~sspbn(.))








sp <- "BLJA"


# sspbn <- function(sp) {
  
  Nsp <- thedat %>% 
    ungroup() %>% 
    filter(species.code == sp) %>% 
    group_by(date, point.number) %>% 
    summarise(N = sum(playback.tp))
  
  
  ybytypesp <- thedat %>% 
    ungroup() %>% 
    select(-code.chr) %>%
    filter(species.code == sp) %>% 
    mutate(combined = human.tp + merlin.tp,
           combined = ifelse(combined > 0, 1, combined)) %>% 
    group_by(date, point.number) %>% 
    summarise(human = sum(human.tp),
              merlin = sum(merlin.tp), 
              combined = sum(combined)) %>% 
    pivot_longer(cols = c(combined, human, merlin))
  
  
  binomdatsp <- left_join(ybytypesp, Nsp, by = c("date", "point.number")) %>% 
    select(date, point.number, N, obs.type = name, y = value) %>% 
    mutate(undetected = N - y,
           point.count.id = paste(point.number, date),
           obs.type = ifelse(obs.type == "combined", "Combined",
                             ifelse(obs.type == "human", "Human only",
                                    ifelse(obs.type == "merlin", "Merlin only", "ERROR"))))
  
  
  # fisher.test(binomdatsp$y, binomdatsp$obs.type)
  cbind(binomdatsp$y, binomdatsp$undetected)
  
  ##analyze the data with a binomial regression
  msp = glmer(cbind(y, undetected) ~ obs.type + (1 | point.count.id), family = binomial, 
              data = binomdatsp)
  
  explore_model(msp)
  
  
  ##calculate estimated marginal means
  ##note that type = response puts the results on the probability scale
  ##which makes results an estimate of detection probability
  emssp = emmeans(msp, "obs.type", type = "response")
  emssp
  
  
  ##get pairwise comparisons among the three treatment groups
  pairssp <- as_tibble(pairs(emssp)) %>% 
    mutate(species = sp) %>% 
    rename(oddsSE = SE) %>% 
    select(species, contrast, odds.ratio, oddsSE, z.ratio, p.value)
  
  
  emmssptab <- as_tibble(emssp) %>% 
    mutate(asymp.LCL = round(asymp.LCL, 3),
           asymp.UCL = round(asymp.UCL, 3),
           CI = paste0(asymp.LCL, "-", asymp.UCL),
           species = sp) %>% 
    rename(probSE = SE) %>% 
    select(species, obs.type, prob, probSE, CI)
  
  
  findat <- bind_cols(emmssptab, pairssp) %>%
    rename(species = species...1) %>%
    select(-species...6)
  
  
  # return(findat)
  
# }


input


map_dfr(input, ~sspbn(.))












ci <- merl %>% 
  filter(correct.id == "yes" | obs.method == "playback") %>% 
  filter(species.code != "warbler sp.", 
         species.code != "bird sp.", 
         species.code != "none") %>% 
  mutate(count = 1) %>% 
  group_by(date, point.number, species.code, obs.method) %>% 
  summarise(detect = sum(count)) %>% 
  mutate(detect = ifelse(detect > 1, 1, detect)) %>% 
  group_by(species.code, obs.method) %>% 
  summarise(detect = sum(detect)) %>% 
  pivot_wider(values_from = detect, names_from = obs.method) %>% 
  mutate(hearing = ifelse(is.na(hearing), 0, hearing),
         merlin = ifelse(is.na(merlin), 0, merlin),
         hearing.perc = round(hearing/playback, 2),
         merlin.perc = round(merlin/playback, 2),
         diff = abs(hearing.perc - merlin.perc)) %>% 
  filter(playback > 13)


testd <- merl %>% 
  filter(correct.id == "yes" | obs.method == "playback") %>% 
  filter(species.code != "warbler sp.", 
         species.code != "bird sp.", 
         species.code != "none") %>% 
  mutate(count = 1) %>% 
  group_by(date, point.number, species.code, obs.method) %>% 
  summarise(detect = sum(count)) %>% 
  mutate(detect = ifelse(detect > 1, 1, detect))
  
  
  











input <- merl %>% 
  filter(correct.id == "yes") %>% 
  group_by(species.code) %>% 
  summarise(count = length(species.code)) %>% 
  arrange(-count) %>% 
  filter(count >= 20) %>% 
  select(species.code) %>% 
  unlist()


Nsp <- thedat %>% 
  ungroup() %>% 
  filter(species.code %in% input) %>% 
  group_by(date, point.number, species.code) %>% 
  summarise(N = sum(playback.tp))


ybytypesp <- thedat %>% 
  ungroup() %>% 
  select(-code.chr) %>%
  filter(species.code %in% input) %>% 
  mutate(either = human.tp + merlin.tp,
         either = ifelse(either > 0, 1, either)) %>% 
  group_by(date, point.number, species.code) %>% 
  summarise(human = sum(human.tp),
            merlin = sum(merlin.tp),
            either = sum(either)) %>% 
  pivot_longer(cols = c(human, merlin, either))


binomdatsp1 <- left_join(ybytypesp, Nsp, by = c("date", "point.number", "species.code")) %>% 
  select(date, point.number, species = species.code, N, obs.type = name, y = value) %>% 
  mutate(undetected = N - y,
         point.count.id = paste(point.number, date),
         obs.type = ifelse(obs.type == "either", "Either",
                           ifelse(obs.type == "human", "Human only",
                                  ifelse(obs.type == "merlin", "Merlin only", "ERROR"))))


binomdatsp <- binomdatsp1 %>% 
  filter(species == "BLJA")


##analyze the data with a binomial regression
msp = glmer(y ~ obs.type + (1 | point.count.id),
            family = binomial, data = binomdatsp)

explore_model(msp)


##calculate estimated marginal means
##note that type = response puts the results on the probability scale
##which makes results an estimate of detection probability
emssp = emmeans(msp, "obs.type", type = "response")
emssp


##get pairwise comparisons among the three treatment groups
pairssp <- as_tibble(pairs(emssp)) %>% 
  mutate(species = sp) %>% 
  rename(oddsSE = SE) %>% 
  select(species, contrast, odds.ratio, oddsSE, z.ratio, p.value)


emmssptab <- as_tibble(emssp) %>% 
  mutate(asymp.LCL = round(asymp.LCL, 3),
         asymp.UCL = round(asymp.UCL, 3),
         CI = paste0(asymp.LCL, "-", asymp.UCL),
         species = sp) %>% 
  rename(probSE = SE) %>% 
  select(species, obs.type, prob, probSE, CI)






input


sp <- "BLJA"


#sspbn <- function(sp) {
  
  Nsp <- thedat %>% 
    ungroup() %>% 
    filter(species.code == sp) %>% 
    group_by(date, point.number) %>% 
    summarise(N = sum(playback.tp))
  
  
  ybytypesp <- thedat %>% 
    ungroup() %>% 
    select(-code.chr) %>%
    filter(species.code == sp) %>% 
    mutate(combined = human.tp + merlin.tp,
           combined = ifelse(combined > 0, 1, either)) %>% 
    group_by(date, point.number) %>% 
    summarise(human = sum(human.tp),
              merlin = sum(merlin.tp)) %>% 
              #combined = sum(combined)) %>% 
    pivot_longer(cols = c(human, merlin))
  
  
  binomdatsp <- left_join(ybytypesp, Nsp, by = c("date", "point.number")) %>% 
    select(date, point.number, N, obs.type = name, y = value) %>% 
    mutate(undetected = N - y,
           point.count.id = paste(point.number, date))
           # obs.type = ifelse(obs.type == "either", "Combined",
           #                   ifelse(obs.type == "human", "Human only",
           #                          ifelse(obs.type == "merlin", "Merlin only", "ERROR"))))
  
  
  fisher.test(binomdatsp$y, binomdatsp$obs.type)
  
  
  ##analyze the data with a binomial regression
  msp = glmer(y ~ obs.type + (1 | point.count.id),
            family = binomial, data = binomdatsp)
  
  msp = glmer(y ~ obs.type + (1 | date),
              family = binomial, data = binomdatsp)
  
  msp2 = glmer(cbind(y, undetected) ~ obs.type + (1 | date),
              family = binomial, data = binomdatsp)
  
  explore_model(msp)
  
  
  
  ##calculate estimated marginal means
  ##note that type = response puts the results on the probability scale
  ##which makes results an estimate of detection probability
  emssp = emmeans(msp, "obs.type", type = "response")
  emssp
  
  
  ##get pairwise comparisons among the three treatment groups
  pairssp <- as_tibble(pairs(emssp)) %>% 
    mutate(species = sp) %>% 
    rename(oddsSE = SE) %>% 
    select(species, contrast, odds.ratio, oddsSE, z.ratio, p.value)
  
  
  emmssptab <- as_tibble(emssp) %>% 
    mutate(asymp.LCL = round(asymp.LCL, 3),
           asymp.UCL = round(asymp.UCL, 3),
           CI = paste0(asymp.LCL, "-", asymp.UCL),
           species = sp) %>% 
    rename(probSE = SE) %>% 
    select(species, obs.type, prob, probSE, CI)
  
  
  # findat <- bind_cols(emmssptab, pairssp) %>% 
  #   rename(species = species...1) %>% 
  #   select(-species...6)
  
  
#  return(findat)

#}




