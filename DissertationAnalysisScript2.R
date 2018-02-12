# Exploring determinants of terror responses
rm(list = ls())
library(tidyverse)
library(readxl)

#### Cleaning up terror data
dropbox.path <- "C:/Users/nfa/Dropbox/Ed/Ed Uni work/EME/Data/Original Data/globalterrorismcsv.csv"

terror.tb <- read_csv(dropbox.path)

terror.UK <- dplyr::filter(terror.tb, country_txt == 'United Kingdom') %>% 
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
                     corp3)) %>% 
  unite(Date, iyear, imonth, iday, sep = '/', remove = FALSE)


# TODO: Factor everything and then create dummies. then groupby and summate but coerce >1 to 1.??
need.to.factor <- c(attacktype1_txt, attacktype2_text, attacktype3_txt,
                    targtype1_txt, targsubtype1_txt, target1,
                    natlty1_txt, targtype2_txt, targsubtype2_txt.....)
