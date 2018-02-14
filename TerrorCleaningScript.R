# Exploring determinants of terror responses
rm(list = ls())
library(tidyverse)
library(readxl)
library(car)
library(dummies)
source('DissertationFunctions.R')
#### Preparing in depth terror data ####
dropbox.path <- "C:/Users/ed/Dropbox/Ed/Ed Uni work/EME/Data/Original Data/globalterrorismdb_0617dist.xlsx"

terror.tb <- read_xlsx(dropbox.path)

# Removing events where day of event is unknown as it seems reasonable that these are particularly insignificant events and are okay to drop. I think there's ~3 events dropped.
# Removing a long list of variables that I don't really need
terror.UK <- dplyr::filter(terror.tb, country_txt == 'United Kingdom') %>%
  dplyr::filter(iday != 0) %>% 
  unite(Date, iyear, imonth, iday, sep = '/', remove = FALSE) %>% 
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
                     INT_ANY,
                     eventid,
                     iyear,
                     imonth,
                     iday,
                     attacktype1,
                     attacktype2,
                     attacktype3,
                     targtype1,
                     targtype2,
                     targtype3,
                     targsubtype1,
                     targsubtype2,
                     targsubtype3,
                     weaptype1,
                     weaptype2,
                     weaptype3,
                     weaptype4))
  



# Recoding some missing values/NA - A lot of these should be dropped from final specification but including atm for interest.
terror.UK$nperps <- Recode(terror.UK$nperps, "c(-99, NA) = 1") # recoding missing and NA to 1. It seems axiomatic for an attack to occur that there must be at least one perpetrator - this variable now admits an 'at least x' inference
terror.UK$nperpcap <- Recode(terror.UK$nperpcap, "c(-99, NA) = 0") # I find it hard to believe the number of perpetrators captured would be mis-reported if someone was actually captured. Should test with and without this
terror.UK$nkillter <- Recode(terror.UK$nkillter, "NA = 0") # Setting NA to 0
terror.UK$nhostkid <- Recode(terror.UK$nhostkid, "c(NA, -99) = 0") # Where no one was kidnapped or there isn't info, setting to 0
terror.UK$ransomamt <- Recode(terror.UK$ransomamt, "NA = 0") # Where there was no ransom requested, no ransom amount was requsted
terror.UK$ransompaid <- Recode(terror.UK$ransompaid, "NA = 0") # Again, where no ransom was requested nothing was paid
terror.UK$property <- Recode(terror.UK$property, "-9 = 0") # Where no one has recorded any property damage it's set to 0 - this occurs in roughly 70 of 5000 obs
terror.UK$ishostkid <- Recode(terror.UK$ishostkid, "-9 = 0") # There's only one missing observation for hostages so setting to 0
terror.UK$ransom <- Recode(terror.UK$ransom, 'c(-9, NA) = 0') # Only 2 observations where it's unknown and 1000 where the term ransom 'isn't applicanble' according to START so also setting to 0 for this specification
terror.UK$INT_IDEO <- Recode(terror.UK$INT_IDEO, '-9 = 0') # This one's a little less clear cut, only including out of interest. Probably shouldn't include in final specification
terror.UK$Date <- as.Date(terror.UK$Date)
terror.UK$incident <- 1 # When we aggreagate by day will show how many incidents occurred on that day




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



# Creating all the dummies and assigning them unique column names. There's a lot of repetition here, should clean up.

attack.dummies <- create.dummies(factor.df.cleaned$attacktype1_txt,
                                 factor.df.cleaned$attacktype2_txt,
                                 factor.df.cleaned$attacktype3_txt)
attack.dummies <- create.unique.colnames(attack.dummies)

target.dummies <- create.dummies(factor.df.cleaned$targtype1_txt,
                                 factor.df.cleaned$targtype2_txt,
                                 factor.df.cleaned$targtype3_txt)
target.dummies <- create.unique.colnames(target.dummies)

target.nationality.dummies <- create.dummies(factor.df.cleaned$natlty1_txt,
                                             factor.df.cleaned$natlty2_txt,
                                             factor.df.cleaned$natlty3_txt)
target.nationality.dummies <- create.unique.colnames(target.nationality.dummies)

perpetrator.dummies <- create.dummies(factor.df.cleaned$gname, factor.df.cleaned$gname) # Only one factor here but easier to repeat it as repeated obs dropped anyway and ensures consistent formatting
perpetrator.dummies <- create.unique.colnames(perpetrator.dummies)

weaptype.dummies <- create.dummies(factor.df.cleaned$weaptype1_txt,
                                   factor.df.cleaned$weaptype2_txt,
                                   factor.df.cleaned$weaptype3_txt,
                                   factor.df.cleaned$weaptype4_txt)
weaptype.dummies <- create.unique.colnames(weaptype.dummies)

weapdetail.dummies <- create.dummies(factor.df.cleaned$weapdetail, factor.df.cleaned$weapdetail)
weapdetail.dummies <- create.unique.colnames(weapdetail.dummies)
province.dummies <- create.dummies(factor.df.cleaned$provstate, factor.df.cleaned$provstate)
province.dummies <- create.unique.colnames(province.dummies)
city.dummies <- create.dummies(factor.df.cleaned$city, factor.df.cleaned$city)
city.dummies <- create.unique.colnames(city.dummies)
weapsubtype.dummies <- create.dummies(factor.df.cleaned$weaptype1_txt,
                                      factor.df.cleaned$weaptype2_txt,
                                      factor.df.cleaned$weaptype3_txt,
                                      factor.df.cleaned$weaptype4_txt)  
weapsubtype.dummies <-  create.unique.colnames(weapsubtype.dummies)

targetsubtype.dummies <- create.dummies(factor.df.cleaned$targsubtype1_txt,
                                        factor.df.cleaned$targsubtype2_txt,
                                        factor.df.cleaned$targsubtype3_txt) 
targetsubtype.dummies <-  create.unique.colnames(targetsubtype.dummies)





# Combining the terror event dummies with the date they occurred so can group by date. This is easier to do now rather than with the whole dataframe
most.covariates.terror.and.date <- cbind(terror.UK$Date,
                                attack.dummies,
                                target.dummies,
                                weaptype.dummies,
                                province.dummies) %>%
  as.tibble %>% 
  group_by(`terror.UK$Date`) %>% 
  summarise_all(sum)



# Recoding any dummies > 1 to 1. Since multiple gun attacks can happen on the same day, without this our dummy variable will have a a value greater than 1 for some days
most.covariates.terror.and.date.cleaned <- most.covariates.terror.and.date[, -1] %>% 
  map(function(.){Recode(., "0 = 0; else = 1")}) %>%
  as.tibble %>% 
  cbind(most.covariates.terror.and.date$`terror.UK$Date`, .) %>% 
  as.tibble # Not sure why I have to use as.tibble() twice. Something odd going on here.




# # Quickly testing that our summations and recoding have worked
# attack <- most.covariates.terror.and.date.cleaned[, grep(pattern = 'attack', colnames(most.covariates.terror.and.date.cleaned))]
# attack$total <- rowSums(attack)
# summary(attack$total) ## The min value should always be 1 here. A values greater than 1 in the total column means that a gun and knife attack occured on the same day for instance.



# Now I group up the rest of the data without the dummies. This is used extensively as a list of events in AnalysisScript1
terror.UK.grouped <- terror.UK %>% 
  group_by(Date) %>% 
  summarise_all(funs(if(is.numeric(.)) sum(., na.rm = TRUE) else first(.)))
save(terror.UK.grouped, file = 'UKTerrorData.Rdata')

terror.covariates <- cbind(terror.UK.grouped, most.covariates.terror.and.date) %>% as.tibble
# Some of the original dummies in the dataset are now greater than 1 as we've aggregated events at the day level. Recoding dummies > 1 to 1.
terror.covariates <- terror.covariates %>% 
  mutate(success = replace(success, success >= 1, 1),
         multiple = replace(multiple, multiple >=1, 1),
         suicide = replace(suicide, suicide >=1, 1),
         claimed = replace(claimed, claimed >=1, 1),
         property = replace(property, property >=1, 1),
         ishostkid = replace(ishostkid, ishostkid >=1, 1),
         ransom = replace(ransom, ransom >= 1, 1),
         INT_IDEO = replace(INT_IDEO, INT_IDEO >= 1, 1),
         INT_MISC = replace(INT_MISC, INT_MISC >= 1, 1))

tidy.name.vector <- make.names(colnames(terror.covariates), unique=TRUE)
colnames(terror.covariates) <- tidy.name.vector




save(terror.covariates, file= 'TerrorCovariates.Rdata')


