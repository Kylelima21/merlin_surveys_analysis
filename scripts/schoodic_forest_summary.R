##Quick compilation of Schoodic Forest eBird data

#------------------------------------------------#
####                Function                  ####
#------------------------------------------------#

## Function to filter within Schoodic Forest
filter_SF <- function(dat, lat, long) {
  
  sf::sf_use_s2(FALSE)

  
  bounds <- sf::read_sf("data/schoodic_forest/11168_Survey North of 186 2021.shp") %>%
    st_transform(4326)


  dat2 <- dat %>%
    rename(x = paste(long), y = paste(lat)) %>%
    mutate(longitude.keep = x,
           latitude.keep = y) %>%
    sf::st_as_sf(., coords = c("x","y"), crs = sf::st_crs(bounds))


  output <- sf::st_join(dat2, bounds, left = F) %>%
    st_set_geometry(., NULL) %>%
    select(everything(), latitude = latitude.keep, longitude = longitude.keep)

  
  return(output)
  
}




#------------------------------------------------#
####              Gather Data                 ####
#------------------------------------------------#

sfd <- tibble(read.delim("data/ebd_US-ME-009_relDec-2023.txt", header = T, quote = "")) %>% 
  select(c('COMMON.NAME', 'SCIENTIFIC.NAME', 'CATEGORY', 'OBSERVATION.DATE', 'OBSERVATION.COUNT', 
           'DURATION.MINUTES', 'SAMPLING.EVENT.IDENTIFIER', 'OBSERVER.ID', 'NUMBER.OBSERVERS',
           'PROTOCOL.TYPE', 'ALL.SPECIES.REPORTED', 'EFFORT.DISTANCE.KM', 'LOCALITY', 'COUNTY', 
           'LATITUDE', 'LONGITUDE')) %>% 
  rename('obs.date'='OBSERVATION.DATE', 'common.name'='COMMON.NAME', 
         'scientific.name'='SCIENTIFIC.NAME', 'count'='OBSERVATION.COUNT', 'locality'='LOCALITY', 
         'checklist.id'='SAMPLING.EVENT.IDENTIFIER', 'latitude'='LATITUDE', 'longitude'='LONGITUDE',
         'observer.id'='OBSERVER.ID', 'category'='CATEGORY', 'county'='COUNTY', 
         'protocol'='PROTOCOL.TYPE', 'all.species.reported'='ALL.SPECIES.REPORTED', 
         'duration.min'='DURATION.MINUTES', 'num.observers'='NUMBER.OBSERVERS', 
         'distance.km'='EFFORT.DISTANCE.KM') %>% 
  filter_SF(., "latitude", "longitude") %>% 
  filter(locality != "Schoodic Peninsula" & obs.date > "2022-12-31")


sfd %>% 
  distinct(checklist.id)


sfd %>% 
  distinct(observer.id)


sfd %>% 
  select(checklist.id, duration.min) %>% 
  distinct() %>% 
  summarise(total.hour = sum(duration.min)/60)


list <- sfd %>% 
  distinct(common.name) %>% 
  arrange(common.name)

write.csv(list, "outputs/2023_summary/eBird_species_list.csv", row.names = F)








