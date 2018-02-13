# Exploring determinants of terror responses
rm(list = ls())
library(tidyverse)
library(readxl)
library(car)
library(dummies)

#### Cleaning up terror data
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
                     natlty1_txt,
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

terror.UK$nperps <- Recode(terror.UK$nperps, "c(-99, NA) = 1") # recoding missing and NA to 1. It seems axiomatic for an attack to occur that there must be at least one perpetrator - this variable now admits an 'at least x' inference
terror.UK$nperpcap <- Recode(terror.UK$nperpcap, "c(-99, NA) = 0") # I find it hard to believe the number of perpetrators captured would be mis-reported if someone was actually captured. Should test with and without this
terror.UK$nkillter <- Recode(terror.UK$nkillter, "NA = 0") # Setting NA to 0
terror.UK$nhostkid <- Recode(terror.UK$nhostkid, "c(NA, -99) = 0") # Where no one was kidnapped or there isn't info, setting to 0
terror.UK$ransomamt <- Recode(terror.UK$ransomamt, "NA = 0") # Where there was no ransom requested, no ransom amount was requsted
terror.UK$ransompaid <- Recode(terror.UK$ransompaid, "NA = 0") # Again, where no ransom was requested nothing was paid


## Creating dummy variables

attack1.dummies <- dummy(terror.UK$attacktype1_txt, verbose = TRUE)
attack2.dummies <- dummy(terror.UK$attacktype2_txt, verbose = TRUE)

attack1.tibble <- as.tibble(attack1.dummies)

attack.dummies.test <- str_detect( colnames(attack1.dummies), 'Armed' )

ed <-attack2.dummies[, grepl('Armed', colnames(attack2.dummies))]
new <- attack1.tibble$`attacktype1_txtArmed Assault` + ed
new



# We have a problem where there are multiple factor columns when I only want one. i.e. we have weapon1/2/3 but these are separate and list the weapon used. If I want a 'used a gun' dummy I 
# need to convert all these factor columns into a dummy column and then aggregate across them.

create.dummy.colnames <- function(...){
  # Creates a list of unique names for use in our dummy columns
  dots.list <- list(...)
  col.names <- dots.list %>% 
    map(unique) %>% 
    unlist %>% 
    as.tibble %>% 
    unique %>% 
    na.omit

  return(col.names)
}

find.a.similar.dummy.column <- function(dummy.col, other.dummy.matrix, regex){
  # Uses regex to find a similar column and return the similar column
  similar.col <- other.dummy.matrix[, grepl(regex, colnames(other.dummy.matrix))]
  return(similar.col)
  
}

add.similar.dummies <- function(dummies.df, dummy.colnames){
  # This function takes a dummies.df object where all the dummies with the same name are. It then adds them all up and uses an indicator function
  # to check if they're greater than 1 in which case they're set to 1
  empty.list <- rep(0, nrow(dummies.df))
  summed.dummies.df <- data.frame(original = empty.list)
  L = nrow(dummy.colnames)
  for (l in 1:L){
    
    sim.dummies <- dummies.df[, grepl(dummy.colnames[l, ], colnames(dummies.df))]
    summed.dummy <- data.frame(rowSums(sim.dummies))
    name.to.set <- dummy.colnames[l,]
    colnames(summed.dummy) <- name.to.set
    summed.dummy <- apply(summed.dummy, 2, function(x)ifelse((x>1),1,0))
    summed.dummies.df<- cbind(summed.dummies.df, summed.dummy)
  }
  
  return(summed.dummies.df)
}





create.similar.dummies <- function(...){
  # Creates a list of dummy variable columns that crucially share the same name. Due to the way the for-loop works I have each dummy column 'pair' ('original' and 'similar') repeated 4 times
  # This is because the first match is the original column with itself and the second match is the dummy with itself. Then the two match each other separately twice.
  dot.list <- list(...)
  dummy.col.names <- create.dummy.colnames(...)
  
  dummy.matrices <- dot.list %>% 
    map(dummy)
  K <- length(dummy.matrices)
  L <- nrow(dummy.col.names)
  dummies.data.frame <- data.frame(matrix('empty', nrow = nrow(dummy.matrices[[1]]), ncol = 1))
  for (j in 1:K){
    for (i in 1:K){
      for (l in 1:L){
        col <- find.a.similar.dummy.column(dummy.matrices[[j]], dummy.matrices[[i]], dummy.col.names[l, ])
        if (length(col) != 0){
          new.col.name <- dummy.col.names[l,]
          col.df <- data.frame(col)
          colnames(col.df) <- new.col.name
          dummies.data.frame <- cbind(dummies.data.frame, col.df)
        }
  }}}
  # Now we have a dataframe of dummies but the repeated observations share the same name so we can add them up
  complete.dummies <- add.similar.dummies(dummies.df = dummies.data.frame, dummy.colnames = dummy.col.names) 
    
  
  return(complete.dummies)
 
}


yest <- create.similar.dummies(terror.UK$attacktype1_txt, terror.UK$attacktype2_txt)

