# Exploring determinants of terror responses
rm(list = ls())
library(tidyverse)
library(readxl)
library(car)
library(dummies)

#### Cleaning up terror data
source('DissertationFunctions.R')
dropbox.path <- "C:/Users/ed/Dropbox/Ed/Ed Uni work/EME/Data/Original Data/globalterrorismcsv.csv"

terror.tb <- read_csv(dropbox.path)

terror.UK <- dplyr::filter(terror.tb, country_txt == 'United Kingdom') %>%
  dplyr::filter(iday != 0) %>% 
  subset(select = -c(approxdate,
                     region,
                     region_txt,
                     latitude,
                     longitude,
                     specificity,
                     summary,
                     crit1,
                     crit2,
                     crit3,
                     doubtterr,
                     nkillus,
                     nwoundus,
                     propcomment,
                     scite1,
                     scite2,
                     scite3,
                     dbsource,
                     addnotes,
                     alternative,
                     alternative_txt,
                     related,
                     vicinity,
                     claimmode,
                     claimmode_txt,
                     compclaim,
                     claimmode2,
                     claimmode3,
                     motive,
                     propextent_txt,
                     propextent,
                     propcomment,
                     nhostkidus,
                     divert,
                     kidhijcountry,
                     ransomamtus,
                     ransompaidus,
                     ransomnote,
                     corp1,
                     corp2,
                     corp3,
                     target1,
                     natlty1,
                     country,
                     country_txt,
                     resolution,
                     location,
                     natlty2,
                     natlty3,
                     gsubname,
                     gsubname3,
                     gsubname2,
                     gname2,
                     gname3,
                     guncertain2,
                     guncertain3,
                     claim2,
                     claim3,
                     claimmode2_txt,
                     claimmode3_txt,
                     nhours,
                     ndays,
                     hostkidoutcome,
                     nreleased,
                     propvalue,
                     INT_LOG,
                     INT_ANY)) %>% 
  unite(Date, iyear, imonth, iday, sep = '/', remove = FALSE)


# Recoding some missing values/NA
terror.UK$nperps <- Recode(terror.UK$nperps, "c(-99, NA) = 1") # recoding missing and NA to 1. It seems axiomatic for an attack to occur that there must be at least one perpetrator - this variable now admits an 'at least x' inference
terror.UK$nperpcap <- Recode(terror.UK$nperpcap, "c(-99, NA) = 0") # I find it hard to believe the number of perpetrators captured would be mis-reported if someone was actually captured. Should test with and without this
terror.UK$nkillter <- Recode(terror.UK$nkillter, "NA = 0") # Setting NA to 0
terror.UK$nhostkid <- Recode(terror.UK$nhostkid, "c(NA, -99) = 0") # Where no one was kidnapped or there isn't info, setting to 0
terror.UK$ransomamt <- Recode(terror.UK$ransomamt, "NA = 0") # Where there was no ransom requested, no ransom amount was requsted
terror.UK$ransompaid <- Recode(terror.UK$ransompaid, "NA = 0") # Again, where no ransom was requested nothing was paid


# Recoding factors columns in preparation for creating dummies
factor.df <- subset(terror.UK, select = c(attacktype1_txt,
                                          attacktype2_txt,
                                          attacktype3_txt,
                                          targtype1_txt,
                                          targtype2_txt,
                                          targtype3_txt,
                                          natlty1_txt,
                                          natlty2_txt,
                                          natlty3_txt,
                                          gname,
                                          weapdetail,
                                          weaptype1_txt,
                                          weaptype2_txt,
                                          weaptype3_txt,
                                          weaptype4_txt,
                                          provstate,
                                          city,
                                          weapsubtype1_txt,
                                          weapsubtype2_txt,
                                          weapsubtype3_txt,
                                          weapsubtype4_txt,
                                          targsubtype1_txt,
                                          targsubtype2_txt,
                                          targsubtype3_txt))
factor.df.cleaned <- apply(factor.df, 2, clean.column) %>% 
  as.tibble



attack.dummies <- create.dummies(factor.df.cleaned$attacktype1_txt,
                                 factor.df.cleaned$attacktype2_txt,
                                 factor.df.cleaned$attacktype3_txt)

target.dummies <- create.dummies(factor.df.cleaned$targtype1_txt,
                                 factor.df.cleaned$targtype2_txt,
                                 factor.df.cleaned$targtype3_txt)

target.nationality.dummies <- create.dummies(factor.df.cleaned$natlty1_txt,
                                             factor.df.cleaned$natlty2_txt,
                                             factor.df.cleaned$natlty3_txt)

perpetrator.dummies <- create.dummies(factor.df.cleaned$gname, factor.df.cleaned$gname) # Only one factor here but easier to repeat it as repeated obs dropped anyway and ensures consistent formatting

weaptype.dummies <- create.dummies(factor.df.cleaned$weaptype1_txt,
                                   factor.df.cleaned$weaptype2_txt,
                                   factor.df.cleaned$weaptype3_txt,
                                   factor.df.cleaned$weaptype4_txt)

weapdetail.dummies <- create.dummies(factor.df.cleaned$weapdetail, factor.df.cleaned$weapdetail)

province.dummies <- create.dummies(factor.df.cleaned$provstate, factor.df.cleaned$provstate)

city.dummies <- create.dummies(factor.df.cleaned$city, factor.df.cleaned$city)

weapsubtype.dummies <- create.dummies(factor.df.cleaned$weaptype1_txt,
                                      factor.df.cleaned$weaptype2_txt,
                                      factor.df.cleaned$weaptype3_txt,
                                      factor.df.cleaned$weaptype4_txt)

targetsubtype.dummies <- create.dummies(factor.df.cleaned$targsubtype1_txt,
                                        factor.df.cleaned$targsubtype2_txt,
                                        factor.df.cleaned$targsubtype3_txt)
