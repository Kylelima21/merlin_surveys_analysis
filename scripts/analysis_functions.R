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