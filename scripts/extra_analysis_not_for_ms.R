## Extra analysis stuff not in manuscript


#------------------------------------------------#
####         Difference in observers          ####
#------------------------------------------------#

## Create main observer data to clean up data later
main.obs <- merl %>% 
  filter(obs.method == "hearing") %>% 
  select(date, point.number, observer.initials) %>% 
  group_by(date, point.number) %>% 
  distinct() %>% 
  arrange(date, point.number)


## Cleaned data 
human.all <- merl %>% 
  filter(obs.method == "hearing" | obs.method == "playback") %>% 
  group_by(date, point.number, species.code) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(correct.id = ifelse(is.na(correct.id) & obs.method == "playback", "missed", correct.id)) %>% 
  left_join(main.obs, by = c("date", "point.number")) %>% 
  select(date, observer.initials = observer.initials.y, obs.method:correct.id) %>% 
  filter(species.code != "none" & species.code != "UNK")


## Format and calculate stats for models/testing
obs.dat <- human.all %>% 
  select(date, observer.initials, point.number, correct.id) %>% 
  filter(correct.id != "unverifiable" | is.na(correct.id)) %>% 
  group_by(observer.initials, date, point.number) %>%
  summarise(percent.incorrect = 100 *
              (length(which(correct.id == "no" | correct.id == "missed")) / length(observer.initials)))


### Messing with linear mixed model
model2 <- lmer(percent.incorrect ~ observer.initials + (1 | date), data = obs.dat)
qqnorm(resid(model2))
qqline(resid(model2))
plot(model2)
summary(model2)


### Testing ANOVA method
## Calculate summary stats
obs.dat %>% 
  group_by(observer.initials) %>% 
  summarise(n = n(),
            mean = mean(percent.incorrect),
            sd = sd(percent.incorrect),
            median = median(percent.incorrect))


## Run an ANOVA to see if the mean percent of erroneous IDs varies by device
aov.obs <- aov(percent.incorrect ~ observer.initials, data = obs.dat)


## Residuals v fitted and QQ plots
plot(aov.obs, 1)
plot(aov.obs, 2)


## Extract residuals
resids.obs <- residuals(object = aov.obs)


## Run Shapiro-Wilk normality test
shapiro.test(x = resids.obs)


## Our ANOVA meets the assumptions, so look at model summary
summary(aov.obs)


## There was no difference among devices
## Create boxplot
obs.dat %>% 
  ggplot(aes(x = observer.initials, y = percent.incorrect, fill = observer.initials)) +
  geom_boxplot() +
  labs(x = "Device", y = "Incorrect IDs (%)") +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = "12"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.2, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.border = element_rect(color = 'black', fill = NA)) 


## Save
# ggsave(paste0("outputs/forpub/figure_misIDs_by_device", str_replace_all(today(), "-", ""), ".png"), 
#        height = 5.5, width = 7, units = "in", dpi = 700)




#------------------------------------------------#
####   Difference in observers - Errors Only  ####
#------------------------------------------------#

## Broken down by observer
obs.dat <- merl %>% 
  filter(obs.method == "hearing") %>% 
  select(date, observer.initials, point.number, correct.id) %>% 
  filter(correct.id != "unverifiable") %>% 
  group_by(observer.initials, date) %>%
  summarise(percent.incorrect = 100 *
              (length(which(correct.id == "no")) / length(observer.initials)))


## Calculate summary stats
obs.dat %>% 
  group_by(observer.initials) %>% 
  summarise(n = n(),
            mean = mean(percent.incorrect),
            sd = sd(percent.incorrect),
            median = median(percent.incorrect))


## Run an ANOVA to see if the mean percent of erroneous IDs varies by device
aov.obs <- aov(percent.incorrect ~ observer.initials, data = obs.dat)


## Residuals v fitted and QQ plots
plot(aov.obs, 1)
plot(aov.obs, 2)


## Extract residuals
resids.obs <- residuals(object = aov.obs)


## Run Shapiro-Wilk normality test
shapiro.test(x = resids.obs)


## Our ANOVA meets the assumptions, so look at model summary
summary(aov.obs)


## There was no difference among devices
## Create boxplot
obs.dat %>% 
  ggplot(aes(x = observer.initials, y = percent.incorrect, fill = observer.initials)) +
  geom_boxplot() +
  labs(x = "Device", y = "Incorrect IDs (%)") +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = "12"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.2, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.border = element_rect(color = 'black', fill = NA)) 


## Save
# ggsave(paste0("outputs/forpub/figure_misIDs_by_device", str_replace_all(today(), "-", ""), ".png"), 
#        height = 5.5, width = 7, units = "in", dpi = 700)




#------------------------------------------------#
####          Difference in Devices           ####
#------------------------------------------------#

## Create function to clean data and get incorrect IDs and missed IDs
device.data.fun <- function(phone) {
  
  dat1 <- merl %>% 
    filter(device == paste(phone) | obs.method == "playback") %>% 
    group_by(date, point.number, species.code) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate(correct.id = ifelse(is.na(correct.id) & obs.method == "playback", "missed", correct.id)) %>% 
    filter(species.code != "none" & species.code != "UNK")
  
  date.filt <- dat1 %>% 
    filter(device == paste(phone)) %>% 
    distinct(date)
  
  output <- dat1 %>% 
    filter(date %in% date.filt$date) %>% 
    mutate(device = paste(phone))
  
  
  return(output)
  
}


## Create list for the input to our map function
device.list <- merl %>% 
  distinct(device) %>% 
  filter(!is.na(device)) %>% 
  list() %>% 
  unlist()


## Run map loop and get clean data
merlin.all <- map_dfr(device.list, ~device.data.fun(.)) %>% 
  arrange(date, point.number, device)


## Calculate data for models/testing
device.dat <- merlin.all %>% 
  select(date, observer.initials, species.code, obs.method, device, point.number,
         time, correct.id) %>% 
  group_by(device, date) %>% 
  summarise(percent.incorrect = 100 *
              (length(which(correct.id == "no" | correct.id == "missed")) / length(device)))


### Messing with linear mixed model
model1 <- lmer(percent.incorrect ~ device + (1 | date), data = device.dat)
qqnorm(resid(model1))
qqline(resid(model1))
plot(model1)
summary(model1)


### Trying ANOVA method
## Calculate summary stats
device.dat %>% 
  group_by(device) %>% 
  summarise(n = n(),
            mean = mean(percent.incorrect),
            sd = sd(percent.incorrect),
            median = median(percent.incorrect))


## Run an ANOVA to see if the mean percent of erroneous IDs varies by device
aov.device <- aov(percent.incorrect ~ device, data = device.dat)


## Residuals v fitted and QQ plots
plot(aov.device, 1)
plot(aov.device, 2)


## Extract residuals
resids.device <- residuals(object = aov.device)


## Run Shapiro-Wilk normality test
shapiro.test(x = resids.device)


## Our ANOVA meets the assumptions, so look at model summary
summary(aov.device)


## There was no difference among devices
## Create boxplot
device.dat %>% 
  mutate(device = ifelse(device == "Samsung Galaxy Tab A7 Lite", "Samsung Galaxy\nTab A7 Lite", device)) %>% 
  ggplot(aes(x = device, y = percent.incorrect, fill = device)) +
  geom_boxplot() +
  labs(x = "Device", y = "Incorrect IDs (%)") +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = "12"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.2, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.border = element_rect(color = 'black', fill = NA)) 


## Save
# ggsave(paste0("outputs/forpub/figure_misIDs_by_device", str_replace_all(today(), "-", ""), ".png"), 
# height = 5.5, width = 7, units = "in", dpi = 700)




#------------------------------------------------#
####    Difference in Devices - Errors Only   ####
#------------------------------------------------#

## Format data for ANOVA
device.dat <- merl %>% 
  select(date, observer.initials, species.code, obs.method, device, point.number,
         time, correct.id) %>% 
  filter(obs.method == "merlin") %>% 
  group_by(device, date) %>% 
  summarise(percent.incorrect = 100 *
              (length(which(correct.id == "no")) / length(device)))


## Calculate summary stats
device.dat %>% 
  group_by(device) %>% 
  summarise(n = n(),
            mean = mean(percent.incorrect),
            sd = sd(percent.incorrect),
            median = median(percent.incorrect))


## Run an ANOVA to see if the mean percent of erroneous IDs varies by device
aov.device <- aov(percent.incorrect ~ device, data = device.dat)


## Residuals v fitted and QQ plots
plot(aov.device, 1)
plot(aov.device, 2)


## Extract residuals
resids.device <- residuals(object = aov.device)


## Run Shapiro-Wilk normality test
shapiro.test(x = resids.device)


## Our ANOVA meets the assumptions, so look at model summary
summary(aov.device)


## There was no difference among devices
## Create boxplot
device.dat %>% 
  mutate(device = ifelse(device == "Samsung Galaxy Tab A7 Lite", "Samsung Galaxy\nTab A7 Lite", device)) %>% 
  ggplot(aes(x = device, y = percent.incorrect, fill = device)) +
  geom_boxplot() +
  labs(x = "Device", y = "Incorrect IDs (%)") +
  scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "none",
        axis.text = element_text(color = "black", size = "12"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(color = "black", size = "12"),
        axis.title.x = element_text(margin = margin(0.2, 0, 0, 0, "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, "cm")),
        panel.background = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.border = element_rect(color = 'black', fill = NA)) 



# ggsave(paste0("outputs/forpub/figure_misIDs_by_device", str_replace_all(today(), "-", ""), ".png"), 
# height = 5.5, width = 7, units = "in", dpi = 700)



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






#------------------------------------------------#
####             Dependent obs P              ####
#------------------------------------------------#
# 
# 
# mdpdat <- merl %>% 
#   filter(obs.method == "merlin") %>% 
#   group_by(date, point.number)
# 
# set.seed(1)
# 
# mdpfilt <- mdpdat %>% 
#   distinct(observer.initials, device) %>% 
#   sample_n(1)
# 
# 
# merldp <- mdpdat %>%
#   left_join(mdpfilt, by = c("date", "point.number")) %>% 
#   filter(device.x == device.y & observer.initials.x == observer.initials.y) %>% 
#   select(-c(device.y, observer.initials.y)) %>% 
#   rename(device = device.x, observer.initials = observer.initials.x)
#   
# 
# humandp <- merl %>% 
#   filter(obs.method == "hearing")
#   
# 
# hmdat <- humandp %>% 
#   bind_rows(merldp) %>% 
#   arrange(date, point.number, obs.method) %>% 
#   filter(species.code != "UNK") %>% 
#   filter(date != "2023-07-13" & date != "2023-10-06") %>% 
#   mutate(device = ifelse(obs.method == "hearing", "human", device))
# 
# 
# dpdat <- hmdat %>% 
#   distinct(date) %>% 
#   mutate(pob.method = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1)) %>% 
#   right_join(hmdat, by = "date")
# 
# 
# pob1 <- dpdat %>% 
#   filter(pob.method == 1) %>% 
#   distinct(date, point.number) %>% 
#   mutate(primary.obs = rep(c("human", "merlin"), nrow(.)/2))
# 
# 
# pob2 <- dpdat %>% 
#   filter(pob.method == 2) %>% 
#   distinct(date, point.number) %>% 
#   mutate(primary.obs = rep(c("merlin", "human"), nrow(.)/2))
# 
# 
# detprobdat <- bind_rows(pob1, pob2) %>% 
#   left_join(dpdat, by = c("date", "point.number")) %>% 
#   filter(species.code != "none" & species.code != "sparrow sp.") %>% 
#   add_count(date, point.number, species.code)
# 
# 
# hprim <- detprobdat %>% 
#   filter(primary.obs == "human") %>% 
#   select(date, point.number, primary.obs, species.code, observer.initials:n, pob.method) %>%
#   mutate(obs.method = factor(obs.method, levels = c("hearing", "merlin"))) %>% 
#   arrange(date, point.number, obs.method) %>% 
#   group_by(date, point.number, species.code) %>%
#   slice(1) %>% 
#   arrange(date, point.number, obs.method)
# 
# 
# mprim <- detprobdat %>% 
#   filter(primary.obs == "merlin") %>% 
#   select(date, point.number, primary.obs, species.code, observer.initials:n, pob.method) %>%
#   mutate(obs.method = factor(obs.method, levels = c("merlin", "hearing"))) %>% 
#   arrange(date, point.number, obs.method) %>% 
#   group_by(date, point.number, species.code) %>%
#   slice(1) %>% 
#   arrange(date, point.number, obs.method)
# 
# 
# dpdatfin <- bind_rows(hprim, mprim) %>% 
#   arrange(date, point.number, obs.method) %>% 
#   ungroup() %>% 
#   mutate(obs = as.integer(ifelse(primary.obs == "human", 1, ifelse(primary.obs == "merlin", 2, 'ERROR')))) %>% 
#   rename(sp = species.code)
#   # group_by(date, point.number, obs.method, time, sky, wind, temp) %>% 
#   # summarise(count = length(obs.method))
# 
# 
# pomdat <- dpdatfin %>% 
#   filter(primary.obs == device | primary.obs == obs.method) %>% 
#   group_by(obs, sp) %>% 
#   summarise(X1 = sum(count.number)) %>% 
#   arrange(sp, obs)
# 
# 
# pomdat2 <- dpdatfin %>% 
#   filter(primary.obs == "human" & obs.method == "merlin" | primary.obs == "merlin" & obs.method == "hearing") %>% 
#   group_by(obs, sp) %>% 
#   summarise(X01 = sum(count.number)) %>% 
#   arrange(sp, obs)
# 
# 
# j <- dpdatfin %>% 
#   distinct(sp)
# 
# 
# k <- dpdatfin %>% 
#   distinct(sp)
# 
# 
# cpomdat <- bind_rows(j, k) %>% 
#   arrange(sp) %>% 
#   mutate(obs = rep(1:2, 52)) %>% 
#   left_join(pomdat, by = c("obs", "sp")) %>% 
#   left_join(pomdat2, by = c("obs", "sp")) %>% 
#   select(obs, sp, X1, X01) %>% 
#   replace(is.na(.), 0)
# 
# 
# spfilt <- cpomdat %>% 
#   mutate(sum = X1 + X01) %>%
#   group_by(sp) %>% 
#   summarise(sum = sum(sum)) %>% 
#   filter(sum <= 10) %>% 
#   select(sp)
# 
# 
# dpmoddat <- cpomdat %>% 
#   mutate(sp = ifelse(sp %in%spfilt$sp, "grp1", sp)) %>% 
#   group_by(obs, sp) %>% 
#   summarise(X1 = sum(X1),
#             X01 = sum(X01)) %>% 
#   arrange(sp, obs) %>% 
#   ungroup() %>% 
#   mutate(X1 = as.double(X1),
#          X01 = as.double(X01)) %>% 
#   as.data.frame(.)
# 
# 
# z1 <- dobserv_dep_obs(dpmoddat, model = ~1)
# print(round(z1$z, 2))
# 
# 
# 
# wafname=system.file('extdata','sample.csv', package='dobserv')
# d1=read.csv(fname,as.is=TRUE)
# data=pool_lt10(d1);
# #   pool CAGO,PUMA even though > 10 detections because all detections were by same obs.
# i=which(data$sp=="CAGO" | data$sp=="PUMA"); data$sp[i]='grp1'
# 
# #  summarize by species...
# sp=as.factor(data$sp); usp=levels(sp); nsp=length(usp); d2=matrix(0,2*nsp,2)
# jj=(as.numeric(sp)-1)*2+(data[,1]==data[1,1])+1
# for (i in 1:nrow(data)) d2[jj[i],]=d2[jj[i],]+as.numeric(data[i,3:4])
# d2=data.frame(obs=rep(1:2,nsp),sp=rep(as.character(usp),each=2),X1=d2[,1],X01=d2[,2])
# 
# z1=dobserv_dep_obs(d2,model=~obs)
# print(round(z1$z,4))



#------------------------------------------------#
####             Random old stuff             ####
#------------------------------------------------#
# 
# ### ID errors made by humans observers
# ## Total human data
# merl %>% 
#   filter(obs.method == "hearing") %>% 
#   group_by(obs.method) %>% 
#   summarize(wrong = length(which(correct.id == "no")),
#             total = length(which(correct.id == "yes"  | correct.id == "no")),
#             perc.wrong = (wrong / total) * 100)
# 
# 
# ## Broken down by observer
# merl %>% 
#   filter(obs.method == "hearing") %>% 
#   group_by(observer.initials) %>% 
#   summarize(wrong = length(which(correct.id == "no")),
#             total = length(which(correct.id == "yes"  | correct.id == "no")),
#             perc.wrong = wrong / total * 100)
# 
# 
# merl %>% 
#   filter(obs.method == "hearing") %>% 
#   group_by(obs.method) %>% 
#   summarize(wrong = length(which(correct.id == "no")),
#             #right = length(which(correct.id == "yes")),
#             total = length(which(correct.id == "yes"  | correct.id == "no")),
#             perc.wrong = wrong / total * 100)
# 
# 
# ## Incorrect IDs made by Merlin
# merl %>% 
#   filter(obs.method == "merlin") %>% 
#   group_by(obs.method) %>% 
#   summarize(wrong = length(which(correct.id == "no")),
#             total = length(which(correct.id == "yes"  | correct.id == "no")),
#             perc.wrong = wrong / total * 100)  
# 
# 
# ## Incorrect Merlin IDs by phone model
# merr <- merl %>% 
#   filter(obs.method == "merlin") %>% 
#   group_by(device) %>% 
#   summarize(wrong = length(which(correct.id == "no")),
#             total = length(which(correct.id == "yes" | correct.id == "no")),
#             percent.wrong = wrong / total * 100)
# 
# msurv <- merl %>% 
#   filter(obs.method == "merlin") %>% 
#   dplyr::select(date, device, point.number) %>% 
#   distinct() %>% 
#   mutate(count = 1) %>% 
#   group_by(device) %>% 
#   summarise(num.surveys = sum(count))
# 
# 
# left_join(merr, msurv, by = "device")
# 
# 
# 
# 
# 
# 
# 
# verified <- merl %>% 
#   filter(obs.method == "playback") %>% 
#   dplyr::select(date, observer.initials, point.number, time, species.code) %>% 
#   mutate(verified = TRUE)
# 
# 
# 
# miss.id <- merl %>% 
#   filter(obs.method == "hearing") %>% 
#   dplyr::select(date, observer.initials, point.number, time, species.code, correct.id) %>% 
#   merge(verified, by = c("date", "point.number", "time", "species.code"), all = T) %>% 
#   select(-observer.initials.y) %>% 
#   tibble() %>% 
#   mutate(id.type = ifelse(correct.id == "yes", "correct", ""))
# 
# 
# # write.csv(miss.id, "outputs/miss_id_data.csv", row.names = F)
# 
# 
# 
# miss.data <- tibble(read.csv("outputs/miss_id_data_edited.csv")) %>% 
#   rename(observer = observer.initials.x)
# 
# 
# 
# miss.data %>% 
#   filter(id.type == "correct" | id.type == "incorrect" | id.type == "missed") %>% 
#   group_by(observer) %>% 
#   summarise(incorrect = length(which(id.type == "incorrect")),
#             missed = length(which(id.type == "missed")),
#             total = length(which(id.type == "correct" | id.type == "incorrect" | id.type == "missed")),
#             perc.incorrect = incorrect / total * 100,
#             perc.missed = missed / total * 100)
# 
# 
# unver <- miss.data %>% 
#   filter(id.type == "unverifiable") %>% 
#   mutate(count = 1) %>% 
#   group_by(observer) %>% 
#   summarise(unverified = sum(count))
# 
# 
# 
# ver.survey <- miss.data %>% 
#   filter(id.type == "unverifiable") %>% 
#   dplyr::select(observer, date, point.number) %>% 
#   distinct() %>% 
#   mutate(count = 1) %>% 
#   group_by(observer) %>% 
#   summarise(ver.surveys = sum(count))
# 
# 
# left_join(unver, ver.survey, by = "observer") %>% 
#   mutate(ver.obs.per.survey = unverified / ver.surveys)

