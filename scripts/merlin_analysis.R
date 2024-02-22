### Comparison of human observers and merlin ID phone app
### Schoodic Institute at Acadia National Park, 2023


#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#

library(tidyverse)
library(lmerTest)
library(sf)
library(rgdal)
library(leaflet)
library(htmlwidgets)
library(webshot2)
library(ggeffects)
library(dobserv)

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
                                                        "RC10"))) %>% 
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
# 96.91358


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

nosp <- merl %>% 
  select(date, point.number) %>% 
  group_by(date, point.number) %>% 
  distinct()

totspavg <- full_join(spavg, nosp, by = c("date", "point.number")) %>% 
  mutate(num.spp = ifelse(is.na(num.spp), 0, num.spp)) %>% 
  ungroup() %>% 
  summarise(mean = mean(num.spp),
            se = sd(num.spp)/sqrt(length(num.spp)))




#------------------------------------------------#
####       Number of species by method        ####
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
         time = coalesce(time.x, time.y),
         time = strptime(time, format = "%H:%M"),
         midn = strptime("00:00:00", format = "%H:%M"),
         tsm = round(as.double(str_extract(difftime(midn, time), 
                                           "\\d*\\.\\d*")), digits = 2),
         num.spp = ifelse(is.na(num.spp), 0, num.spp)) %>% 
  select(date, obs.method, device, point.number, num.spp, sky, wind, noise, temp,
         time, tsm, juldate) %>% 
  arrange(date, obs.method, device, point.number)


### Run a poisson model for number of species by method (human pc/Merlin)
## Fixed effects: noise and wind
## Random effect: date
modeldiv <- glmer(num.spp ~ obs.method + noise + wind + (1 | juldate) + (1 | point.number:device), 
                  data = divdatfull, family = poisson)


### Explore model fit and results
## QQ plot and pearson residual plot look good
## Significant difference between human observers and Merlin
## Wind also significantly lowers number of species as expected
explore_model(modeldiv)


### Test model for overdispersion using B. Bolker's suggested test
## Model is slightly underdispersed (ratio = 0.74)
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
  mutate(distance.50.u.o = ifelse(obs.method == "merlin", "u", 
                                  distance.50.u.o)) %>% 
  filter(distance.50.u.o == "u") %>% 
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
         juldate = format(date, "%j")) %>% 
  select(date, obs.method, device, point.number, num.spp, sky, wind, noise, temp, juldate) %>% 
  arrange(date, obs.method, device, point.number)


### Run the same poisson model for number of species by method (human pc/Merlin)
## Fixed effects: noise and wind
## Random effect: date
modeldiv50 <- glmer(num.spp ~ obs.method + noise + wind + (1 | juldate) + (1 | point.number:device), 
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

## Calculate data for models/testing
incdat <- merl %>% 
  filter(obs.method == "hearing" | obs.method == "merlin") %>% 
  filter(correct.id == "no") %>% 
  group_by(date, obs.method, device, point.number) %>% 
  summarise(num.inc = length(correct.id))


inccomb <- full_join(incdat, mis.rows) %>% 
  ungroup() %>% 
  mutate(num.inc = ifelse(is.na(num.inc), 0, num.inc)) %>% 
  arrange(date, obs.method, device, point.number)

  
## Run ttest to see if the mean percent of erroneous IDs varies by device
t.methodi <- t.test(num.inc ~ obs.method, data = inccomb)
t.methodi

wilcox.test(num.inc ~ obs.method, data = inccomb)

## Stats
inccomb %>% 
  group_by(obs.method) %>% 
  summarise(mean = mean(num.inc),
            se = sd(num.inc)/sqrt(length(num.inc)))




#------------------------------------------------#
####          Missed IDs by method            ####
#------------------------------------------------#

main.obs <- merl %>% 
  filter(obs.method == "hearing") %>% 
  select(date, point.number, observer.initials) %>% 
  group_by(date, point.number) %>% 
  distinct() %>% 
  arrange(date, point.number)


### 
## The field observer identified a song as REVI, but it was a BHVI for example
## I'm counting that BHVI ID as a missed bird because I think it becomes
## very subjective as to when an incorrect ID is given, what bird did they 
## actually here?
human.all <- merl %>% 
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


human.cm <- human.all %>% 
  mutate(device = "human") %>% 
  filter(obs.method == "playback") %>% 
  group_by(date, device, point.number) %>% 
  summarise(num.mis = length(species.code)) %>% 
  mutate(obs.method = "hearing") %>% 
  full_join(., mis.rows) %>% 
  ungroup() %>% 
  filter(obs.method == "hearing") %>% 
  filter(date != "2023-07-13" & date != "2023-10-06") %>% 
  mutate(num.mis = ifelse(is.na(num.mis), 0, num.mis)) %>% 
  arrange(date, obs.method, point.number)



merlin.all <- merl %>% 
  filter(obs.method == "merlin" | obs.method == "playback") %>% 
  group_by(date, point.number, species.code) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(correct.id = ifelse(is.na(correct.id) & obs.method == "playback", "missed", correct.id)) %>% 
  left_join(main.obs, by = c("date", "point.number")) %>% 
  select(date, observer.initials = observer.initials.y, obs.method:correct.id) %>% 
  filter(species.code != "none" & species.code != "warbler sp." &
           species.code != "bird sp." & species.code != "sparrow sp." &
           species.code != "Empidonax sp." & species.code != "UNK")













t.method <- t.test(num.inc ~ obs.method, data = inccomb)




#------------------------------------------------#
####      Independent Observer p* - raw       ####
#------------------------------------------------#

mdpdat <- merl %>% 
  filter(obs.method == "merlin") %>% 
  group_by(date, point.number)


set.seed(1)


mdpfilt <- mdpdat %>% 
  distinct(observer.initials, device) %>% 
  sample_n(1)


merldp <- mdpdat %>%
  left_join(mdpfilt, by = c("date", "point.number")) %>% 
  filter(device.x == device.y & observer.initials.x == observer.initials.y) %>% 
  select(-c(device.y, observer.initials.y)) %>% 
  rename(device = device.x, observer.initials = observer.initials.x)


humandp <- merl %>% 
  filter(obs.method == "hearing")


hmdat <- bind_rows(humandp, merldp) %>% 
  arrange(date, point.number, obs.method) %>% 
  filter(species.code != "UNK" & species.code != "none" & species.code != "sparrow sp.") %>% 
  filter(date != "2023-07-13" & date != "2023-10-06") %>% 
  mutate(device = ifelse(obs.method == "hearing", "human", device)) %>% 
  add_count(date, point.number, species.code) %>% 
  mutate(n = ifelse(n == 2, "both", n),
         n = ifelse(n == 1 & obs.method == "hearing", "hum.only", n),
         n = ifelse(n == 1 & obs.method == "merlin", "merl.only", n)) 


sp.to.filt <- hmdat %>% 
  group_by(species.code) %>% 
  summarise(sum = sum(count.number)) %>% 
  filter(sum < 10)


x10 <- hmdat %>% 
  mutate(species.code = ifelse(species.code %in% sp.to.filt$species.code, 
                               "grp1", species.code)) %>% 
  filter(n == "hum.only") %>% 
  group_by(date, point.number, species.code) %>% 
  summarise(X10 = sum(count.number), .groups = "drop") %>% 
  group_by(species.code) %>% 
  summarise(X10 = sum(X10))


x01 <- hmdat %>% 
  mutate(species.code = ifelse(species.code %in% sp.to.filt$species.code, 
                               "grp1", species.code)) %>% 
  filter(n == "merl.only") %>% 
  group_by(date, point.number, species.code) %>% 
  summarise(X01 = sum(count.number), .groups = "drop") %>% 
  group_by(species.code) %>% 
  summarise(X01 = sum(X01))


x11 <- hmdat %>% 
  mutate(species.code = ifelse(species.code %in% sp.to.filt$species.code, 
                               "grp1", species.code)) %>% 
  filter(n == "both") %>% 
  group_by(date, point.number, species.code) %>% 
  summarise(X11 = sum(count.number), .groups = "drop") %>% 
  group_by(species.code) %>% 
  summarise(X11 = sum(X11))


inddpdat <- full_join(x10, x01, by = "species.code") %>% 
  full_join(., x11, by = "species.code") %>% 
  replace(is.na(.), 0) %>% 
  mutate(obs = 1, sum = X10 + X01 + X11) %>% 
  select(obs, everything()) %>% 
  rename(sp = species.code) %>% 
  arrange(sp) %>% 
  as.data.frame()


modz <- dobserv_indep_obs(inddpdat, model = ~sp + obs)
print(round(modz$z, 2))




#------------------------------------------------#
####      Independent Observer p* - clean       ####
#------------------------------------------------#

mdpdat <- merl %>% 
  filter(obs.method == "merlin" & correct.id == "yes") %>% 
  group_by(date, point.number)


set.seed(1)


mdpfilt <- mdpdat %>% 
  distinct(observer.initials, device) %>% 
  sample_n(1)


merldp <- mdpdat %>%
  left_join(mdpfilt, by = c("date", "point.number")) %>% 
  filter(device.x == device.y & observer.initials.x == observer.initials.y) %>% 
  select(-c(device.y, observer.initials.y)) %>% 
  rename(device = device.x, observer.initials = observer.initials.x)


humandp <- merl %>% 
  filter(obs.method == "hearing" & correct.id != "no")


hmdat <- bind_rows(humandp, merldp) %>% 
  arrange(date, point.number, obs.method) %>% 
  filter(species.code != "UNK" & species.code != "none" & species.code != "sparrow sp.") %>% 
  filter(date != "2023-07-13" & date != "2023-10-06") %>% 
  mutate(device = ifelse(obs.method == "hearing", "human", device)) %>% 
  add_count(date, point.number, species.code) %>% 
  mutate(n = ifelse(n == 2, "both", n),
         n = ifelse(n == 1 & obs.method == "hearing", "hum.only", n),
         n = ifelse(n == 1 & obs.method == "merlin", "merl.only", n)) 


sp.to.filt <- hmdat %>% 
  group_by(species.code) %>% 
  summarise(sum = sum(count.number)) %>% 
  filter(sum < 10)


x10 <- hmdat %>% 
  mutate(species.code = ifelse(species.code %in% sp.to.filt$species.code, 
                               "grp1", species.code)) %>% 
  filter(n == "hum.only") %>% 
  group_by(date, point.number, species.code) %>% 
  summarise(X10 = sum(count.number), .groups = "drop") %>% 
  group_by(species.code) %>% 
  summarise(X10 = sum(X10))


x01 <- hmdat %>% 
  mutate(species.code = ifelse(species.code %in% sp.to.filt$species.code, 
                               "grp1", species.code)) %>% 
  filter(n == "merl.only") %>% 
  group_by(date, point.number, species.code) %>% 
  summarise(X01 = sum(count.number), .groups = "drop") %>% 
  group_by(species.code) %>% 
  summarise(X01 = sum(X01))


x11 <- hmdat %>% 
  mutate(species.code = ifelse(species.code %in% sp.to.filt$species.code, 
                               "grp1", species.code)) %>% 
  filter(n == "both") %>% 
  group_by(date, point.number, species.code) %>% 
  summarise(X11 = sum(count.number), .groups = "drop") %>% 
  group_by(species.code) %>% 
  summarise(X11 = sum(X11))


inddpdat <- full_join(x10, x01, by = "species.code") %>% 
  full_join(., x11, by = "species.code") %>% 
  replace(is.na(.), 0) %>% 
  mutate(obs = 1, sum = X10 + X01 + X11) %>% 
  select(obs, everything()) %>% 
  rename(sp = species.code) %>% 
  arrange(sp) %>% 
  as.data.frame()


modz <- dobserv_indep_obs(inddpdat, model = ~sp + obs)
print(round(modz$z, 2))




#------------------------------------------------#
####  Independent Obs p* - clean ~ post hoc   ####
#------------------------------------------------#

humandp <- merl %>% 
  filter(obs.method == "hearing" & correct.id != "no")


posthdp <- merl %>% 
  filter(obs.method == "playback")


hmdat <- bind_rows(humandp, posthdp) %>% 
  arrange(date, point.number, obs.method) %>% 
  filter(species.code != "UNK" & species.code != "none" & species.code != "sparrow sp."
         & species.code != "Empidonax sp." & species.code != "warbler sp." 
         & species.code != "bird sp.") %>% 
  filter(date != "2023-07-13" & date != "2023-10-06") %>% 
  mutate(count.number = ifelse(obs.method == "playback", 1, count.number)) %>% 
  add_count(date, point.number, species.code) %>% 
  mutate(n = ifelse(n == 2, "both", n),
         n = ifelse(n == 1 & obs.method == "playback", "pb.only", n),
         n = ifelse(n == 1 & obs.method == "hearing", "hum.only", n)) 


sp.to.filt <- hmdat %>% 
  group_by(species.code) %>% 
  summarise(sum = sum(count.number)) %>% 
  filter(sum < 10)


x10 <- hmdat %>% 
  mutate(species.code = ifelse(species.code %in% sp.to.filt$species.code, 
                               "grp1", species.code)) %>% 
  filter(n == "hum.only") %>% 
  group_by(date, point.number, species.code) %>% 
  summarise(X10 = sum(count.number), .groups = "drop") %>% 
  group_by(species.code) %>% 
  summarise(X10 = sum(X10))


x01 <- hmdat %>% 
  mutate(species.code = ifelse(species.code %in% sp.to.filt$species.code, 
                               "grp1", species.code)) %>% 
  filter(n == "pb.only") %>% 
  group_by(date, point.number, species.code) %>% 
  summarise(X01 = sum(count.number), .groups = "drop") %>% 
  group_by(species.code) %>% 
  summarise(X01 = sum(X01))


x11 <- hmdat %>%
  mutate(species.code = ifelse(species.code %in% sp.to.filt$species.code, 
                                              "grp1", species.code)) %>% 
  filter(n == "both") %>% 
  group_by(date, point.number, species.code) %>% 
  summarise(X11 = sum(count.number), .groups = "drop") %>% 
  group_by(species.code) %>% 
  summarise(X11 = sum(X11))


inddpdat <- full_join(x10, x01, by = "species.code") %>% 
  full_join(., x11, by = "species.code") %>% 
  replace(is.na(.), 0) %>% 
  mutate(obs = 1) %>% 
  select(obs, everything()) %>% 
  rename(sp = species.code) %>% 
  arrange(sp) %>% 
  as.data.frame()


modz <- dobserv_indep_obs(inddpdat, model = ~sp + obs)
print(round(modz$z, 2))




#------------------------------------------------#
####     Independent Observer p* - < 50 m     ####
#------------------------------------------------#

mdpdat <- merl %>% 
  filter(obs.method == "merlin") %>% 
  group_by(date, point.number)


set.seed(1)


mdpfilt <- mdpdat %>% 
  distinct(observer.initials, device) %>% 
  sample_n(1)


merldp <- mdpdat %>%
  left_join(mdpfilt, by = c("date", "point.number")) %>% 
  filter(device.x == device.y & observer.initials.x == observer.initials.y) %>% 
  select(-c(device.y, observer.initials.y)) %>% 
  rename(device = device.x, observer.initials = observer.initials.x)


## Select only the human obs
## Remove dates where distance was not recorded
humandp <- merl %>% 
  filter(obs.method == "hearing") %>% 
  filter(date != "2023-07-07" & date != "2023-07-20") %>% 
  filter(distance.50.u.o == "u")


## Combine human and Merlin data
## Remove dates that were SB and KL
hmdat <- bind_rows(humandp, merldp) %>% 
  arrange(date, point.number, obs.method) %>% 
  filter(species.code != "UNK" & species.code != "none" & species.code != "sparrow sp.") %>% 
  filter(date != "2023-07-13" & date != "2023-10-06") %>% 
  mutate(device = ifelse(obs.method == "hearing", "human", device)) %>% 
  add_count(date, point.number, species.code) %>% 
  mutate(n = ifelse(n == 2, "both", n),
         n = ifelse(n == 1 & obs.method == "hearing", "hum.only", n),
         n = ifelse(n == 1 & obs.method == "merlin", "merl.only", n)) 


sp.to.filt <- hmdat %>% 
  group_by(species.code) %>% 
  summarise(sum = sum(count.number)) %>% 
  filter(sum < 10)


x10 <- hmdat %>% 
  mutate(species.code = ifelse(species.code %in% sp.to.filt$species.code, 
                               "grp1", species.code)) %>% 
  filter(n == "hum.only") %>% 
  group_by(date, point.number, species.code) %>% 
  summarise(X10 = sum(count.number), .groups = "drop") %>% 
  group_by(species.code) %>% 
  summarise(X10 = sum(X10))


x01 <- hmdat %>% 
  mutate(species.code = ifelse(species.code %in% sp.to.filt$species.code, 
                               "grp1", species.code)) %>% 
  filter(n == "merl.only") %>% 
  group_by(date, point.number, species.code) %>% 
  summarise(X01 = sum(count.number), .groups = "drop") %>% 
  group_by(species.code) %>% 
  summarise(X01 = sum(X01))


x11 <- hmdat %>% 
  mutate(species.code = ifelse(species.code %in% sp.to.filt$species.code, 
                               "grp1", species.code)) %>% 
  filter(n == "both") %>% 
  group_by(date, point.number, species.code) %>% 
  summarise(X11 = sum(count.number), .groups = "drop") %>% 
  group_by(species.code) %>% 
  summarise(X11 = sum(X11))


inddpdat50 <- full_join(x10, x01, by = "species.code") %>% 
  full_join(., x11, by = "species.code") %>% 
  replace(is.na(.), 0) %>% 
  mutate(obs = 1) %>% 
  select(obs, everything()) %>% 
  rename(sp = species.code) %>% 
  arrange(sp) %>% 
  as.data.frame()


mod50 <- dobserv_indep_obs(inddpdat50, model = ~sp + obs)
print(round(mod50$z, 2))



