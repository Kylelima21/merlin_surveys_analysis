library(lme4)
library(emmeans)
library(reshape2)

rm(list = ls())

###############################################################################################
## Simulate data that is more like the data from the trials
## simulate individual species, and their detection by merlin and humans
## then use the merlin and human detections to calculate the either merlin or human detections
################################################################################################

##set parameters
  n.sp = 30
  n.point.count = 50
  p.true.presence = 0.3
  p.human = 0.5
  p.merlin = 0.5

##simulate bird community at each point
  sp.site = matrix(rbinom(n = n.point.count*n.sp,
                                        size = 1, 
                                        prob = p.true.presence), 
                   nrow = n.point.count,
                   ncol = n.sp)
  
##simulate point counts
  sp.det.human = matrix(rbinom(n = n.point.count*n.sp,
                               size = 1, 
                               prob = p.human), 
                         nrow = n.point.count,
                         ncol = n.sp) * sp.site  
  sp.det.merlin = matrix(rbinom(n = n.point.count*n.sp,
                                size = 1, 
                                prob = p.merlin), 
                         nrow = n.point.count,
                         ncol = n.sp) * sp.site 
  sp.det.either = ifelse(sp.det.human == 1 |
                         sp.det.merlin == 1,
                         1, 
                         0)

##put together into data frame
  sp.det.all = as.data.frame(rbind(sp.det.human,
                                   sp.det.merlin,
                                   sp.det.either))

  sp.det.all$point.count = rep(1:n.point.count, 3)
  sp.det.all$obs.type = rep(c("human", "merlin", "either"),
                            each = n.point.count)
   
  d.all = melt(sp.det.all,
               id.vars = c("point.count", "obs.type"),
               variable.name = "sp",
               value.name = "detected") 
  
##aggregate to number of species for obs.type
  d = aggregate(d.all$detected,
                by = list(point.count = d.all$point.count,
                              obs.type = d.all$obs.type),
                sum)
  names(d)[3] = "y"
  d$N = rep(apply(sp.site, 1, sum),
                3)
 
  
# ##############################################################################
# ## Simulate data that has a row for each observation type at each point count
# ## point.count = identifier for the point count survey
# ## N = number of birds species truly at the point
# ## obs.type = human, merlin, or either
# ## y = number of bird species correctly identified by the obs.type
# ##############################################################################
#   ##set the number of point counts
#     n.point.counts = 50
# 
#   ##set the ture size of the bird community in the study area
#   ##this is the maximum number of birds that can be detected at a point
#     n.birds = 30
#     
#   ##set proportion of the bird community at each point count location
#     p.birds = 0.3
#   
#   ##set the true detection probabilities for the simulation
#     p.human = 0.5
#     p.merlin = 0.5
#     p.either = p.human + p.merlin - p.human*p.merlin
# 
#   ##create independent variables
#     d = data.frame(point.count = rep(1:n.point.counts, 
#                                      each = 3), 
#                    N = rep(rbinom(n = n.point.counts, 
#                                   size = n.birds,
#                                   p = p.birds), 
#                            each = 3),
#                    obs.type = rep(c("human", "merlin", "either"), 
#                                   n.point.counts),
#                    true.det.prob = rep(c(p.human, p.merlin, p.either),
#                                        n.point.counts))
# 
#   ##simulate number of correct detections for each observer type
#     d$y = rbinom(nrow(d),
#                  size = d$N,
#                  p = d$true.det.prob) 
#    

################################################
## Analyze the data using a binomial regression
################################################
  ##calculate the number of undetected species for the regression
    d$undetected = d$N - d$y
    
  ##analyze the data with a binomial regression
    m = glmer(cbind(y, undetected) ~ obs.type + (1|point.count),
              family = binomial,
              data = d)

  ##test for overdispersion
    overdisp_fun <- function(model) {
      rdf <- df.residual(model)
      rp <- residuals(model,type="pearson")
      Pearson.chisq <- sum(rp^2)
      prat <- Pearson.chisq/rdf
      pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
      c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
    }
    
    overdisp_fun(m)
        
  ##calculate estimated marginal means
  ##note that type = response puts the results on the probability scale
  ##which makes results an estimate of detection probability
    ems = emmeans(m, "obs.type", type = "response")
    ems
    
  ##get pairwise comparisons among the three treatment groups
    pairs(ems)
    