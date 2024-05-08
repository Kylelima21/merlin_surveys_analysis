library(lme4)
library(emmeans)

rm(list = ls())

##############################################################################
## Simulate data that has a row for each observation type at each point count
## point.count = identifier for the point count survey
## N = number of birds species truly at the point
## obs.type = human, merlin, or either
## y = number of bird species correctly identified by the obs.type
##############################################################################
  ##set the number of point counts
    n.point.counts = 50

  ##set the true size of the bird community in the study area
  ##this is the maximum number of birds that can be detected at a point
    n.birds = 30
    
  ##set proportion of the bird community at each point count location
    p.birds = 0.3
  
  ##set the true detection probabilities for the simulation
    p.human = 0.8
    p.merlin = 0.7
    p.either = p.human + p.merlin - p.human*p.merlin

  ##create independent variables
    d = data.frame(point.count = rep(1:n.point.counts, 
                                     each = 3), 
                   N = rep(rbinom(n = n.point.counts, 
                                  size = n.birds,
                                  p = p.birds), 
                           each = 3),
                   obs.type = rep(c("human", "merlin", "either"), 
                                  n.point.counts),
                   true.det.prob = rep(c(p.human, p.merlin, p.either),
                                       n.point.counts))

  ##simulate number of correct detections for each observer type
    d$y = rbinom(nrow(d),
                 size = d$N,
                 p = d$true.det.prob) 
   

################################################
## Analyze the data using a binomial regression
################################################
  ##calculate the number of undetected species for the regression
    d$undetected = d$N - d$y
    
  ##analyze the data with a binomial regression
    m = glmer(cbind(y, undetected) ~ obs.type + (1|point.count),
              family = binomial,
              data = d)

  ##calculate estimated marginal means
  ##note that type = response puts the results on the probability scale
  ##which makes results an estimate of detection probability
    ems = emmeans(m, "obs.type", type = "response")
    ems
    
  ##get pairwise comparisons among the three treatment groups
    pairs(ems)
    
    
