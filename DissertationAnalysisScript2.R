## Second stage of analysis exploring heterogeneous effects
rm(list = ls())

library(tidyverse)

#### Merging terror covariates and return data ####
load('TerrorCovariates.Rdata')
load('AnalysisOutput/CAR_Data.RData')

covariates.u <- inner_join(x = CAR.10.unfiltered, y = terror.covariates, by = c('market.date' = 'Date')) %>% as.tibble


all.dummies <- grep('dummies', colnames(covariates.u), value = TRUE)


Formula <- formula(paste("event.car ~ 0 + success + suicide + nkill + nwound + nperps + claimed + " , 
                         paste(all.dummies, collapse=" + ")))

fit1 <- lm(Formula, covariates.u)
summary(fit1)


dummies <- as.formula(paste('event.car', "~",  paste(colnames(covariates.u)[c(61:106)], collapse = "+"), sep = ""))



provinces <- covariates.u$province.dummies.Northern.Ireland + covariates.u$province.dummies.England + covariates.u$province.dummies.Scotland + covariates.u$province.dummies.Wales

library(glmnetUtils)
library(broom)
x.matrix <- 
fit2 <- glmnet(Formula, covariates.u)
test <- tidy(coef(fit2)) %>% as.tibble

