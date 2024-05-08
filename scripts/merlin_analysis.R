### Comparison of human observers and merlin ID phone app
### Schoodic Institute at Acadia National Park, 2023


#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)
library(rgdal)
library(sf)
library(leaflet)
library(htmlwidgets)
library(webshot2)
library(lmerTest)
library(ggeffects)
library(mlogit)
library(dfidx)

source("scripts/analysis_functions.R")




#------------------------------------------------#
####        Data Import and Cleaning          ####
#------------------------------------------------#

## Read in, format as tibble, and fix the date column
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

### Maine
## Create map bounds
maxLong = -71.096425 + 0.7
maxLat = 47.444973 - 0.7
minLong = -66.996655 - 0.7
minLat = 43.063393 + 0.7


## Put label data into a data frame
loc <- c("Schoodic Forest", "Maine, USA")
long <- c(-68.063206, -69.1)
lat <- c(44.410149, 45.32)
data <- data.frame(loc, lat, long)
maine <- tibble(data) %>% filter(loc == "Maine, USA")
sfor <- tibble(data) %>% filter(loc == "Schoodic Forest")


## Read in state polygons
state <- readOGR("data/st_us.kml")
state_polygon <- spTransform(state, CRS("+init=epsg:4326"))


## Format for plotting
lyr <- ogrListLayers("data/st_us.kml")
mykml <- lapply(lyr, function(i) readOGR("data/st_us.kml", i))
names(mykml) <- lyr


## Plot Maine map with Leaflet
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


## Save map
# saveWidget(me_map, "outputs/temp.html", selfcontained = FALSE)
# webshot("outputs/temp.html", 
#         file = "outputs/forpub/extras/maine_panel.png", zoom = 5)



#------------------------------------------------#

### Schoodic Forest
## Read in shapefile
sforest <- read_sf("data/schoodic_forest/11168_Survey North of 186 2021.shp") %>% 
  st_transform(crs = '+proj=longlat +datum=WGS84')


## Plot Cadillac map with Leaflet
sf_map <- leaflet::leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                    options = providerTileOptions(maxZoom = 11)) %>%
  leaflet::addPolygons(data = sforest, color = "white", opacity = 100, 
                       fillOpacity = 0, weight = 1.5)


## View map
sf_map


## Save map
# saveWidget(sf_map, "outputs/temp.html", selfcontained = FALSE)
# webshot("outputs/temp.html", 
#         file = "outputs/forpub/extras/sfor_panel_empty.png", zoom = 4)



#------------------------------------------------#

### Survey points
## Read in shapefile
surveypoints <- read_sf("data/survey_points/schoodic_forest_merlin_survey_sites.shp")

## Plot the map with Leaflet
plot_map <- leaflet::leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                    options = providerTileOptions(maxZoom = 19)) %>%
  leaflet::addPolygons(data = sforest, color = "darkorange", opacity = 100,
                       fillOpacity = 0, weight = 2) %>%
  leaflet::addCircleMarkers(data = surveypoints, radius = 6, opacity = 100, 
                            weight = 1.5, fillOpacity = 100, color = "black", 
                            stroke = T, fillColor = "white") %>% 
  leaflet::addScaleBar(position = "bottomleft", 
                       options = scaleBarOptions(maxWidth = 300))


## View map
plot_map


## Save map
# saveWidget(plot_map, "outputs/temp.html", selfcontained = FALSE)
# webshot("outputs/temp.html", 
#         file = "outputs/forpub/extras/survey_points_map2.png", zoom = 6)




#------------------------------------------------#
####             Summary Stats                ####
#------------------------------------------------#

## Number of days surveyed (BDG and EDH only)
merl %>% 
  dplyr::select(date) %>% 
  distinct()                             # 15 days


## Number of individual road counts
merl %>% 
  dplyr::select(date, point.number) %>%
  arrange(date) %>% 
  distinct()                             # 144 surveys


# ## Number of observers
# merl %>% 
#   filter(obs.method == "hearing") %>% 
#   dplyr::select(observer.initials) %>%
#   distinct()                             # 4 human observers


## Percent of human IDs from audio
nrow(merl %>% filter(obs.method == "hearing" & type.a.v == "a")) / 
  nrow(merl %>% filter(obs.method == "hearing" & !is.na(type.a.v))) * 100  
# 98.06763


## Number of species
spp <- merl %>% 
  mutate(correct.id = ifelse(is.na(correct.id), 
                             "yes", correct.id)) %>% 
  filter(correct.id != "no") %>% 
  filter(species.code != "none" & species.code != "warbler sp." & 
         species.code != "bird sp." & species.code != "sparrow sp." & 
         species.code != "Empidonax sp." & species.code != "UNK") %>% 
  distinct(species.code) %>% 
  arrange(species.code)

length(spp$species.code) # 50 species


## Write out species list
# write.csv(spp, "outputs/2023_summary/merlin_survey_species_list.csv", 
#           row.names = F)


## Number of audio documented species
doc.spp <- merl %>% 
  filter(obs.method == "playback") %>% 
  filter(species.code != "none" & species.code != "warbler sp." &
         species.code != "bird sp." & species.code != "sparrow sp." &
         species.code != "Empidonax sp." & species.code != "UNK") %>%
  dplyr::select(species.code) %>% 
  distinct() %>% 
  arrange(species.code)

length(doc.spp$species.code) # 45 species



### Average number of species
## Calculate total number of species per point count
spavg <- merl %>% 
  filter(species.code != "none" & species.code != "warbler sp." &
           species.code != "bird sp." & species.code != "sparrow sp." &
           species.code != "Empidonax sp." & species.code != "UNK") %>% 
  mutate(correct.id = ifelse(obs.method == "playback", "yes", correct.id)) %>% 
  filter(correct.id != "no") %>% 
  select(date, point.number, species.code) %>% 
  distinct() %>% 
  group_by(date, point.number) %>% 
  summarise(num.spp = length(species.code))


## List of all point counts to get the 4 point counts that had 0 spp
nosp <- merl %>% 
  select(date, point.number) %>% 
  group_by(date, point.number) %>% 
  distinct()


## Join to include 0s and then calculate mean and SE
totspavg <- full_join(spavg, nosp, by = c("date", "point.number")) %>% 
  mutate(num.spp = ifelse(is.na(num.spp), 0, num.spp)) %>% 
  ungroup() %>% 
  summarise(mean = mean(num.spp),
            se = sd(num.spp)/sqrt(length(num.spp)))




#------------------------------------------------#
####        Number of correct species         ####
#------------------------------------------------#

### Begin to clean up the 'merl' data frame for use in modeling
## Remove non species-level obs
## Collect only correct identifications (for human observers unverifiable too)
## Get unique species per observation method and point-count
## Calculate number of species for each method and point-count
divdat <- merl %>% 
  filter(species.code != "none" & species.code != "warbler sp." &
           species.code != "bird sp." & species.code != "sparrow sp." &
           species.code != "Empidonax sp." & species.code != "UNK") %>% 
  # mutate(correct.id = ifelse(obs.method == "playback", "yes", correct.id)) %>% 
  filter(obs.method == "hearing" | obs.method == "merlin") %>%
  filter(correct.id != "no") %>% 
  select(date, obs.method, device, point.number, species.code, sky, wind, noise, 
         temp, time) %>% 
  distinct() %>% 
  group_by(date, obs.method, device, point.number, sky, wind, noise, temp, time) %>% 
  summarise(num.spp = length(species.code))


### Create list of all point-counts on each day for adding zeros in to the above
## Filter to only the Merlin and human data
## Return the list of all unique combinations of day and point-count
mis.rows <- merl %>% 
  filter(obs.method == "hearing" | obs.method == "merlin") %>% 
  select(date, obs.method, device, point.number, sky, wind, noise, temp, time) %>% 
  group_by(date, obs.method, device, point.number, sky, wind, noise, temp, time) %>% 
  distinct()


### Format for model input
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
  select(date, month, obs.method, device, point.number, num.spp, sky, wind, noise, temp,
         time, tsm, juldate) %>% 
  arrange(date, obs.method, device, point.number) %>% 
  ungroup()


### Run a poisson model for number of species by method (human pc/Merlin)
## Fixed effects: wind and month
## Random effect: separate effects of device and point number
modeldiv <- glmer(num.spp ~ obs.method + month + (1 | device) + (1 | point.number), 
                  data = divdatfull, family = poisson)


### Explore model fit and results
## QQ plot and pearson residual plot look good
## Significant difference between human observers and Merlin
## Wind also significantly lowers number of species as expected
explore_model(modeldiv)


### Test model for overdispersion using B. Bolker's suggested test
## Model is slightly underdispersed (ratio = 0.85)
overdisp_fun(modeldiv)


### Use ggpredict function to predict estimates for number of species on 
### a point-count for human observers and Merlin
## Merlin recorded about 1 species less than human observers 
## 95% CIs do not overlap
divmodo <- ggpredict(modeldiv, terms = c("obs.method"))

  

#------------------------------------------------#

### Begin to clean up the 'merl' data frame for use in modeling
## Remove non species-level obs
## Collect only correct identifications (for human observers unverifiable too)
## Remove human observations that were estimated to be over 50 m from center
## Get unique species per observation method and point-count
## Calculate number of species for each method and point-count
divdat50 <- merl %>% 
  filter(species.code != "none" & species.code != "warbler sp." &
           species.code != "bird sp." & species.code != "sparrow sp." &
           species.code != "Empidonax sp." & species.code != "UNK") %>% 
  filter(obs.method == "hearing" | obs.method == "merlin") %>%
  filter(correct.id != "no") %>%
  # mutate(distance.50.u.o = ifelse(obs.method == "merlin", "u", 
                                  # distance.50.u.o)) %>% 
  filter(distance.50.u.o == "u" | obs.method == "merlin") %>% 
  select(date, obs.method, device, point.number, species.code, sky, wind, 
         noise, temp) %>% 
  distinct() %>% 
  group_by(date, obs.method, device, point.number, sky, wind, noise, temp) %>% 
  summarise(num.spp = length(species.code))


### Format for model input
## Using the same 'mis.rows' data frame from above, join the two data sets so 
## that point-counts with zero observations are included
## Have to remove July 7 & 13 because over/under 50 m was not recorded
## Rename covaritate columns and clean
divdatfull50 <- full_join(divdat50, mis.rows, by = c("date", "obs.method",
                                                     "device", 
                                                     "point.number")) %>%
  filter(date != "2023-07-07" & date != "2023-07-13") %>% 
  mutate(sky = coalesce(sky.x, sky.y),
         wind = coalesce(wind.x, wind.y),
         noise = coalesce(noise.x, noise.y),
         temp = coalesce(temp.x, temp.y),
         num.spp = ifelse(is.na(num.spp), 0, num.spp),
         juldate = format(date, "%j"),
         month = month(date)) %>% 
  select(date, obs.method, device, point.number, num.spp, month, sky, wind, noise, temp, juldate) %>% 
  arrange(date, obs.method, device, point.number)


### Run the same poisson model for number of species by method (human pc/Merlin)
modeldiv50 <- glmer(num.spp ~ obs.method + month + (1 | device) + (1 | point.number), 
                    data = divdatfull50, family = poisson)


### Explore model fit and results
## QQ plot and pearson residual plot look good
## No significant difference between human observers and Merlin
## Wind also significantly lowers number of species as expected
explore_model(modeldiv50)


### Test model for overdispersion using B. Bolker's suggested test
## Model is slightly underdispersed (ratio = 0.86)
overdisp_fun(modeldiv50)


### Use ggpredict function to predict estimates for number of species on 
### a limited radius (50 m) point-count for human observers and Merlin
## Merlin and human observers recorded about the same number of species
## 95% CIs do not overlap
divmodo50 <- ggpredict(modeldiv50, terms = c("obs.method"))




#------------------------------------------------#
####        Incorrect IDs by method           ####
#------------------------------------------------#

## Format data for models
incdat <- merl %>% 
  filter(obs.method == "hearing" | obs.method == "merlin") %>% 
  filter(correct.id == "no") %>% 
  group_by(date, obs.method, device, point.number) %>% 
  summarise(num.inc = length(correct.id))


inccomb <- full_join(incdat, mis.rows) %>% 
  ungroup() %>% 
  mutate(num.inc = ifelse(is.na(num.inc), 0, num.inc),
         juldate = format(date, "%j"),
         month = as.integer(month(date)),
         pointdev = paste(point.number, device)) %>% 
  arrange(date, obs.method, device, point.number)


### Run a poisson model for number of incorrect IDs by method (human pc/Merlin)
modinc <- glmer(num.inc ~ obs.method + month + (1 | point.number), 
                 data = inccomb, family = poisson)
# modtest <- glmer(num.inc ~ obs.method + (1 | juldate) + (1 | pointdev), 
#                     data = inccomb, family = poisson)


### Explore model fit and results
## QQ plot and pearson residual plot look good
## No significant difference between human observers and Merlin
## Wind also significantly lowers number of species as expected
explore_model(modinc)


### Test model for overdispersion using B. Bolker's suggested test
## Model is slightly underdispersed (ratio = 0.86)
overdisp_fun(modinc)


### Use ggpredict function to predict estimates for number of incorrect 
### species identifications
## Merlin and human observers recorded about the same number of species
## 95% CIs overlap, no statistical difference
divmodin <- ggpredict(modinc, terms = c("obs.method"))




# ## Run ttest to see if the mean percent of erroneous IDs varies by device
# t.methodi <- t.test(num.inc ~ obs.method, data = inccomb)
# t.methodi
# 
# wilcox.test(num.inc ~ obs.method, data = inccomb)
# 
# 
# ## Stats
# inccomb %>% 
#   group_by(obs.method) %>% 
#   summarise(mean = mean(num.inc),
#             se = sd(num.inc)/sqrt(length(num.inc)))




#------------------------------------------------#
####           Omissions by method            ####
#------------------------------------------------#

main.obs <- merl %>% 
  filter(obs.method == "hearing") %>% 
  select(date, point.number, observer.initials) %>% 
  group_by(date, point.number) %>% 
  distinct() %>% 
  arrange(date, point.number)


### 
## The field observer identified a song as REVI, but it was a BHVI for example
## I'm counting that BHVI as a missed bird because I think it becomes
## too subjective when an incorrect ID is given what bird did they 
## actually hear?
human.all1 <- merl %>% 
  filter(obs.method == "hearing" | obs.method == "playback") %>% 
  group_by(date, point.number, species.code) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(correct.id = ifelse(is.na(correct.id) & obs.method == "playback", "missed", correct.id)) %>% 
  left_join(main.obs, by = c("date", "point.number")) %>% 
  select(date, observer.initials = observer.initials.y, obs.method:correct.id) %>% 
  filter(species.code != "none" & species.code != "warbler sp." &
           species.code != "bird sp." & species.code != "sparrow sp." &
           species.code != "Empidonax sp." & species.code != "UNK")


human.cm <- human.all1 %>% 
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


device_list <- merl %>% 
  distinct(device) %>% 
  filter(!is.na(device) & device != "human") %>% 
  unlist()


merlin.cm <- map_dfr(device_list, merldat_calc) %>% 
  arrange(date, device, point.number)


merhum.cm <- bind_rows(human.cm, merlin.cm) %>% 
  mutate(juldate = format(date, "%j"),
         month = as.integer(month(date)))


### Run the same poisson model for number of species by method (human pc/Merlin)
## Fixed effects: month
## Random effect: separate effects of device and point.number
modelomis <- glmer(num.mis ~ obs.method + month + (1 | point.number), 
                    data = merhum.cm, family = poisson)


### Explore model fit and results
## QQ plot and pearson residual plot look good
## No significant difference between human observers and Merlin
## Wind also significantly lowers number of species as expected
explore_model(modelomis)


### Test model for overdispersion using B. Bolker's suggested test
## Model is slightly underdispersed (ratio = 0.93)
overdisp_fun(modelomis)


### Use ggpredict function to predict estimates for number of species on 
### a limited radius (50 m) point-count for human observers and Merlin
## Merlin and human observers recorded about the same number of species
## 95% CIs do not overlap
divmodomis <- ggpredict(modelomis, terms = c("obs.method"))




#------------------------------------------------#
####       Species-specific analysis          ####
#------------------------------------------------#

### All species combined - multinomial regression
joiner <- merl %>% 
  select(date, obs.method, point.number, sky, wind, noise, temp) %>% 
  group_by(date, obs.method, point.number, sky, wind, noise, temp) %>% 
  distinct()


probfiller <- data.frame(choice = c("both_tp", "merlin_fn", "human_fn", "both_fn"),
                         Freq = c(0, 0, 0, 0))


set.seed(1435)


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


reshaped <- dfidx(thedat, shape = "wide", choice = "code.chr")
summary(mnmod <- mlogit(code.chr ~ 1, data = reshaped, reflevel = "both_tp"))


alod <- as.data.frame(exp(cbind("odds.ratio" = coef(mnmod), confint.default(mnmod, level = 0.95)))) %>% 
  bind_cols(as_tibble(coef(mnmod))) %>% 
 mutate_if(is.double, ~round(., digits = 3)) %>% 
  mutate(outcome = rownames(.),
         odds.ratio.CI = paste0(`2.5 %`, "-", `97.5 %`)) %>% 
  as_tibble() %>% 
  select(outcome, b = value, odds.ratio, odds.ratio.CI)


# write.csv(alod, "outputs/forpub/table_allspp_mlogit.csv", row.names = F)


end <- as.data.frame(mnmod$freq / sum(mnmod$freq)) %>% 
  full_join(., probfiller) %>% 
  group_by(choice) %>% 
  summarise(freq = sum(Freq)) %>% 
  select(choice, freq) %>%
  mutate(choice = factor(choice, levels = c("both_tp", "human_fn", 
                                          "merlin_fn", "both_fn")))


end %>% 
  ggplot() +
  geom_point(aes(x = choice, y = freq), size = 2.5) +
  labs(x = "Outcomes", y = "Probability") +
  geom_label(aes(x = choice, y = freq, label = round(freq, 3)), 
             nudge_y = 0.1) +
  lims(y = c(0, 1)) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())



end %>% 
  pivot_wider(values_from = freq, names_from = choice) %>% 
  summarise(human.tp = both_tp + merlin_fn,
            merlin.tp = both_tp + human_fn)

end %>% 
  pivot_wider(values_from = freq, names_from = choice) %>% 
  summarise(both.tp = both_tp + merlin_fn + human_fn)
  



### Species specific multinomial regression

species <- "GCKI"

mnfun <- function(species) {

  joiner2 <- merl %>% 
    select(date, obs.method, point.number, sky, wind, noise, temp) %>% 
    group_by(date, obs.method, point.number, sky, wind, noise, temp) %>% 
    distinct()
  
  
  probfiller2 <- data.frame(choice = c("both_tp", "merlin_fn", "human_fn", "both_fn"),
                           Freq = c(0, 0, 0, 0))
  
  
  set.seed(1435)
  
  
  thedat2 <- merl %>%
    mutate(correct.id = ifelse(obs.method == "playback", "yes", correct.id),
           device = ifelse(obs.method == "playback", "playback", device)) %>%
    filter(species.code == species & correct.id == "yes") %>%
    select(date, obs.method, device, point.number, species.code, sky, wind, noise, temp) %>%
    full_join(., joiner2) %>%
    group_by(date, obs.method, point.number) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    mutate(playback.tp = ifelse(obs.method == "playback" & !is.na(device), 1, 0),
           human.tp = ifelse(obs.method == "hearing" & !is.na(device), 1, 0),
           merlin.tp = ifelse(obs.method == "merlin" & !is.na(device), 1, 0)) %>%
    group_by(date, point.number, sky, wind, noise, temp) %>%
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
  
  
  reshaped2 <- dfidx(thedat2, shape = "wide", choice = "code.chr")
  summary(mnmod2 <- mlogit(code.chr ~ 1, data = reshaped2, reflevel = "both_tp"))
  
  
  alod2 <- as.data.frame(exp(cbind("odds.ratio" = coef(mnmod2), confint.default(mnmod2, level = 0.95)))) %>% 
    bind_cols(as_tibble(coef(mnmod2))) %>% 
    mutate_if(is.double, ~round(., digits = 3)) %>% 
    mutate(outcome = rownames(.),
           odds.ratio.CI = paste0(`2.5 %`, "-", `97.5 %`)) %>% 
    as_tibble() %>% 
    select(outcome, b = value, odds.ratio, odds.ratio.CI)
  
  
  # write.csv(alod2, paste0("outputs/spp_mlogit_tabs/table_", species, "_mlogit.csv"), row.names = F)
  
   
  end2 <- as.data.frame(mnmod2$freq / sum(mnmod2$freq)) %>% 
    # pivot_wider(names_from = choice, values_from = Freq) %>% 
    full_join(., probfiller2) %>% 
    group_by(choice) %>% 
    summarise(prob = sum(Freq)) %>% 
    # replace(is.na(.), 0) %>% 
    mutate(species = species) %>% 
    select(species, choice, prob)
    # select(species, both_tp, merlin_fn, human_fn, both_fn)

  
  return(end2)

}


# mnfun("REVI")


input <- merl %>% 
  filter(correct.id == "yes") %>% 
  group_by(species.code) %>% 
  summarise(count = length(species.code)) %>% 
  arrange(-count) %>% 
  filter(count >= 20) %>% 
  select(species.code) %>% 
  unlist()
  

d <- map_dfr(input, ~mnfun(.)) %>% 
  mutate(choice = factor(choice, levels = c("both_tp", "human_fn", 
                                               "merlin_fn", "both_fn")))


d %>% 
  pivot_wider(values_from = prob, names_from = choice) %>% 
  arrange(-both_tp)


d %>% 
  pivot_wider(values_from = freq, names_from = choice) %>% 
  group_by(species) %>% 
  summarise(human.tp = both_tp + merlin_fn,
            merlin.tp = both_tp + human_fn) %>% 
  arrange(-human.tp)



d %>% 
  filter(species == "AMCR") %>% 
  ggplot() +
  geom_point(aes(x = choice, y = freq), size = 2.5) +
  geom_label(aes(x = choice, y = freq, label = round(freq, 3)), 
             nudge_y = 0.1) +
  labs(x = "Outcomes", y = "Probability") +
  lims(y = c(0, 1)) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = "12"),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())






## Binomial model
Ns <- thedat %>% 
  ungroup() %>% 
  group_by(date, point.number) %>% 
  summarise(N = sum(playback.tp))


ybytype <- thedat %>% 
  ungroup() %>% 
  select(-code.chr) %>% 
  mutate(either = human.tp + merlin.tp,
         either = ifelse(either > 0, 1, either)) %>% 
  group_by(date, point.number) %>% 
  summarise(human = sum(human.tp),
            merlin = sum(merlin.tp),
            either = sum(either)) %>% 
  pivot_longer(cols = c(human, merlin, either))


binomdat <- left_join(ybytype, Ns, by = c("date", "point.number")) %>% 
  select(date, point.number, N, obs.type = name, y = value) %>% 
  mutate(undetected = N - y,
         point.count.id = paste(point.number, date)) 



##analyze the data with a binomial regression
m = glmer(cbind(y, undetected) ~ obs.type + (1 | point.count.id),
          family = binomial,
          data = binomdat)

explore_model(m)

##calculate estimated marginal means
##note that type = response puts the results on the probability scale
##which makes results an estimate of detection probability
ems = emmeans(m, "obs.type", type = "response")
ems


##get pairwise comparisons among the three treatment groups
pairs(ems)


### Test model for overdispersion using B. Bolker's suggested test
## Model is underdispersed
overdisp_fun(m)





as_tibble(ems)

Ns <- thedat %>% 
  ungroup() %>% 
  group_by(date, point.number) %>% 
  summarise(N = sum(playback.tp))


ybytype <- thedat %>% 
  ungroup() %>% 
  select(-code.chr) %>% 
  mutate(either = human.tp + merlin.tp,
         either = ifelse(either > 0, 1, either)) %>% 
  group_by(date, point.number) %>% 
  summarise(human = sum(human.tp),
            merlin = sum(merlin.tp),
            either = sum(either)) %>% 
  pivot_longer(cols = c(human, merlin, either))


binomdat <- left_join(ybytype, Ns, by = c("date", "point.number")) %>% 
  select(date, point.number, N, obs.type = name, y = value) %>% 
  mutate(undetected = N - y,
         point.count.id = paste(point.number, date)) 



##analyze the data with a binomial regression
m = glmer(cbind(y, undetected) ~ obs.type + (1 | point.count.id),
          family = binomial,
          data = binomdat)

explore_model(m)

##calculate estimated marginal means
##note that type = response puts the results on the probability scale
##which makes results an estimate of detection probability
ems = emmeans(m, "obs.type", type = "response")
ems


##get pairwise comparisons among the three treatment groups
pairs(ems)











# library(tidymodels) # for modeling
# 
# thedat2 <- thedat %>% 
#   mutate(code.chr = factor(code.chr, levels = c("both_tp", "human_fn", 
#                                                 "merlin_fn", "both_fn")),
#          noise = ifelse(noise > 1, 1, 0))
# 
# model_fit <- multinom_reg() %>%  
#   fit(code.chr ~ 1, data = thedat2)
# 
# model_fit
# 
# tidyd <- tidy(model_fit, exponentiate = TRUE, conf.int = TRUE) %>%  
#   mutate_if(is.numeric, round, 3) %>% 
#   select(-std.error, -statistic)
# 
# 
# tidyd %>% 
#   #filter(species == "YRWA") %>% 
#   ggplot() +
#   geom_point(aes(x = y.level, y = estimate), size = 2.5) +
#   geom_errorbar(aes(x = y.level, y = estimate, ymin = conf.low, ymax = conf.high),
#                 width = 0.1) +
#   labs(x = "Error types", y = "Odds") +
#   #lims(y = c(0, max(tidyd$estimate) + 0.2)) +
#   theme_classic() +
#   theme(axis.text = element_text(color = "black", size = "12"),
#         axis.title = element_text(color = "black", size = "12"),
#         axis.title.x = element_text(margin = margin(0.6, 0, 0, 0, "cm")),
#         axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
#         panel.grid.minor = element_blank(), 
#         panel.grid.major = element_blank())







# ### Number of obs per species for humans and merlin
# ## Remove extra taxa
# ## Filter to correct merlin and human obs
# ## Calculate the number of obs per species
# sp.nums <- merl %>% 
#   filter(species.code != "none" & species.code != "warbler sp." &
#            species.code != "bird sp." & species.code != "sparrow sp." &
#            species.code != "Empidonax sp." & species.code != "UNK") %>% 
#   filter(obs.method == "merlin" | obs.method == "hearing") %>% 
#   filter(correct.id != "no") %>% 
#   group_by(species.code, obs.method) %>% 
#   summarise(count = length(species.code),
#             perc = round((count/144)*100, digits = 0)) %>% 
#   arrange(obs.method, -count)
# 
# 
# ## Write our csv to make table for ms
# # write.csv(sp.nums, "outputs/table_spp_freq.csv", row.names = F)
# 
# 
# 
# data = merl
# species = "BCCH"
# bootwilcox <- function(species) {
#   
#   
#   options(dplyr.summarise.inform = FALSE)
#   
#   
#   num.points <- merl %>% 
#     distinct(date, point.number) %>% 
#     group_by(date) %>%
#     summarise(num.points = length(point.number))
#   
#   
#   fulldates <- num.points %>% 
#     select(date)
#   
#   
#   hudat <- merl %>% 
#     filter(species.code == species[1]) %>% 
#     filter(obs.method == "hearing") %>% 
#     filter(correct.id != "no") %>% 
#     left_join(., num.points, by = "date", keep = T) %>% 
#     mutate(date = date.x) %>% 
#     group_by(date, num.points) %>% 
#     summarise(count = length(species.code)) %>% 
#     mutate(perc.count = 100*count/num.points) %>% 
#     ungroup() %>% 
#     left_join(num.points, ., keep = T) %>% 
#     select(date = date.x, num.points = num.points.x, count, perc.count) %>% 
#     mutate(count = ifelse(is.na(count), 0, count),
#            perc.count = ifelse(is.na(perc.count), 0, perc.count),
#            obs.method = "hearing")
#   
#   
#   medat <- merl %>% 
#     filter(obs.method == "merlin") %>% 
#     mutate(species.code = ifelse(species.code == species[1] & correct.id == "yes", 
#                                  species[1], "other")) %>% 
#     left_join(., num.points, by = "date", keep = T) %>% 
#     mutate(date = date.x) %>% 
#     select(date, device, point.number, num.points, species.code) %>% 
#     distinct() %>% 
#     group_by(date, point.number, num.points) %>% 
#     slice_sample(n = 1) %>% 
#     filter(species.code == species[1]) %>% 
#     group_by(date, num.points) %>% 
#     summarise(count = length(species.code)) %>% 
#     mutate(perc.count = 100*count/num.points) %>% 
#     ungroup() %>% 
#     left_join(num.points, ., keep = T) %>% 
#     select(date = date.x, num.points = num.points.x, count, perc.count) %>% 
#     mutate(count = ifelse(is.na(count), 0, count),
#            perc.count = ifelse(is.na(perc.count), 0, perc.count),
#            obs.method = "merlin")
#   
#   
#   binddat <- bind_rows(hudat, medat) %>% 
#     ungroup()
#   
#   
#   sumstat <- binddat %>%
#     group_by(obs.method) %>%
#     summarise(mean = mean(perc.count),
#               se = sd(perc.count)/sqrt(length(perc.count)))
#   
# 
#   output <- wilcox.test(perc.count ~ obs.method, data = binddat)
# 
#   
#   return(sumstat$mean)
# 
# }
# 
# 
# boot(data = merl, statistic = bootwilcox, R = 50, species = "AMCR")
# 
# 
# 
# 
# rep(bootwilcox("BCCH"), n = 20)
# 
# reps <- 100
# 
# d <- as.data.frame(replicate(reps, bootwilcox("BCCH"), simplify = FALSE))
# 
# test <- d %>% 
#   pivot_longer(cols = 1:ncol(.)) %>% 
#   mutate(obs.method = rep(c("human", "merlin"), each = reps)) %>% 
#   group_by(obs.method) %>% 
#   summarise(mean = mean(value))
#   
# 
# 
# 
# library(boot)
# #define function to calculate R-squared
# rsq_function <- function(formula, data, indices) {
#   d <- data[indices,] #allows boot to select sample
#   fit <- lm(formula, data=d) #fit regression model
#   return(summary(fit)$r.square) #return R-squared of model
# }
# #perform bootstrapping with 2000 replications
# reps <- boot(data=mtcars, statistic=rsq_function, R=2000, formula=mpg~disp)














# num.points <- data %>% 
#   distinct(date, point.number) %>% 
#   group_by(date) %>%
#   summarise(num.points = length(point.number))
# 
# 
# species <- "AMCR"
# 
# 
# hudat <- merl %>% 
#   filter(species.code == species[1]) %>% 
#   filter(obs.method == "hearing") %>% 
#   filter(correct.id != "no") %>% 
#   left_join(., num.points, by = "date", keep = T) %>% 
#   rename(date = date.x) %>% 
#   mutate(device = ifelse(observer.initials == "BDG", "human1", "human2")) %>% 
#   group_by(date, obs.method, device, num.points) %>% 
#   summarise(count = length(species.code)) %>% 
#   mutate(perc.count = 100*count/num.points) %>% 
#   ungroup() %>% 
#   left_join(num.points, ., keep = T) %>% 
#   select(date = date.x, obs.method, device, num.points = num.points.x, 
#          count, perc.count) %>% 
#   mutate(count = ifelse(is.na(count), 0, count),
#          perc.count = ifelse(is.na(perc.count), 0, perc.count),
#          obs.method = "hearing",
#          device = ifelse(is.na(device), "human1", device), 
#          month = month(date))
# 
# 
# medat <- merl %>% 
#   filter(obs.method == "merlin") %>% 
#   mutate(species.code = ifelse(species.code == species[1] & correct.id == "yes", 
#                                species[1], "other")) %>% 
#   left_join(., num.points, by = "date", keep = T) %>% 
#   rename(date = date.x) %>% 
#   select(date, obs.method, device, point.number, num.points, species.code) %>% 
#   distinct() %>% 
#   # group_by(date, point.number, num.points) %>% 
#   # slice_sample(n = 1) %>% 
#   # filter(species.code == species[1]) %>% 
#   mutate(pres = ifelse(species.code == species[1], 1, 0)) %>% 
#   group_by(date, obs.method, device, num.points) %>% 
#   summarise(count = sum(pres)) %>% 
#   mutate(perc.count = 100*count/num.points) %>% 
#   ungroup()
# 
# 
# binddat <- bind_rows(hudat, medat) %>% 
#   mutate(month = as.integer(month),
#          perc.count = as.numeric(perc.count))
# 
# 
# 
# testmod <- lmer(perc.count ~ obs.method + month + (1 | device),
                   # data = binddat)






