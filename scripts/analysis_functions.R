## Schoodic Institute at Acadia National Park


#------------------------------------------------#
####                Functions                 ####
#------------------------------------------------#

## Make a function that will allow us to quickly visualize the model results
explore_model <- function(model) {
  
  try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
  
  
  qqnorm(resid(model))
  qqline(resid(model))
  
  
  print(plot(model))
  
  
  summary(model)
  
}




## Overdispersion function from B. Bolker
## https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#testing-for-overdispersioncomputing-overdispersion-factor
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}




### Function to get all missed IDs by merlin for each device 
merldat_calc <- function (phone) { 
  
  mdates <- merl %>% 
    filter(device == phone) %>% 
    distinct(date)
  
  merlin.phone <- merl %>% 
    filter(obs.method == "merlin" & device == phone | 
             obs.method == "playback") %>%
    group_by(date, point.number, species.code) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate(correct.id = ifelse(is.na(correct.id) & obs.method == "playback", "missed", correct.id)) %>% 
    left_join(main.obs, by = c("date", "point.number")) %>% 
    select(date, observer.initials = observer.initials.y, obs.method:correct.id) %>% 
    filter(species.code != "none" & species.code != "warbler sp." &
             species.code != "bird sp." & species.code != "sparrow sp." &
             species.code != "Empidonax sp." & species.code != "UNK") %>% 
    filter(date %in% mdates$date) %>%
    mutate(obs.method = "merlin",
           device = phone) %>% 
    filter(correct.id == "missed") %>% 
    group_by(date, device, point.number) %>% 
    summarise(num.mis = length(species.code)) %>% 
    full_join(., mis.rows) %>% 
    ungroup() %>% 
    filter(obs.method == "merlin" & device == phone) %>% 
    mutate(num.mis = ifelse(is.na(num.mis), 0, num.mis)) %>% 
    arrange(date, obs.method, point.number)

}



