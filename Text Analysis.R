
## News Reading ##

library(quanteda)

file <- "C:/Users/ed/Dropbox/Ed/Ed Uni work/EME/Data/Original Data/News Data/Headline Specification"

news.text <- VCorpus(DirSource(file, encoding = 'UTF-8'), readerControl = list(language = 'lat'))



file2 <- "C:/Users/ed/Dropbox/Ed/Ed Uni work/EME/Data/Original Data/News Data/Headline Specification/News 1980-2000.txt"
news.text.split <- corpus_segment(file2, pattern = 'DOCUMENTS')

# Dealing with newspaper data for media intensity index
rm(list = ls())
library(quanteda)
library(readtext)
library(tidyverse)
library(lubridate)
library(zoo)

my.text <- readtext('News Data/Headline Specification/*')
myCorpus <- corpus(x = my.text)

# Each text document contains multiple newspaper articles. Splitting via 'DOCUMENTS' gives each document its own corpus
myCorpus.split <- corpus_segment(myCorpus, pattern = 'DOCUMENTS')

#### Functions ####

get.news.date <- function(news.corpus){
# This function reads the first 100 characters of each article looking for a date. Once it's found a date it returns the date for use in the corpus meta data
  news.subset <- substr(news.corpus, 1, 105)
  month.list <- c('January',
                  'February',
                  'March',
                  'April',
                  'May',
                  'June',
                  'July',
                  'August',
                  'September',
                  'October',
                  'November',
                  'December')
  
  news.tokens <- tokens(news.subset)
  month.token <- tokens_select(news.tokens, month.list, selection = 'keep', case_insensitive = FALSE)[[1]]
  token.pos <- regexpr(month.token, news.subset)
  length.month.name <- nchar(as.character(month.token))
  article.date <- substr(news.subset, token.pos, (length.month.name + 8 + token.pos))
  article.days <- as.numeric(str_extract_all(article.date, "[0-9]+")[[1]])[[1]]
  article.years <- as.numeric(str_extract_all(article.date, "[0-9]+")[[1]])[[2]]
  final.article.date <- paste(article.days, month.token, article.years)[[1]]
  return(final.article.date)
}

#### Dates ####
 # Creating a list of documents to find the date for
corpus.to.datesearch <-  myCorpus.split[1:nrow(myCorpus.split$documents)]

# Finding dates and formatting
dates <- corpus.to.datesearch %>% 
  map(possibly(get.news.date, otherwise = 'NA'))

dates.tibble <- as.tibble(dates) %>% 
  gather(key = document, value = Date)

# Adding dates to documents
docvars(myCorpus.split, 'Article Date') <- dates.tibble$Date





#### Filtering for UK only events ####




newsmap_dict <- dictionary(file = 'english.yml')
UK.dict <- dictionary(list(
  in.UK = c(
    'uk',
    'britain',
    'england',
    'wales',
    'scotland',
    'ireland',
    'london'
  ),
  Sept.11 = c('9/11')
))

news.dfm <- dfm(myCorpus.split,
                remove = stopwords('english'),
                stem = TRUE,
                remove_punct = TRUE)

news.dfm.UK <- dfm_lookup(news.dfm, UK.dict, levels = 1)
news.dfm.UK.filtered <- dfm_subset(news.dfm.UK, news.dfm.UK[,'in.UK'] > 3) 
# news.dfm.UK.filtered <- dfm_subset(news.dfm.UK.filtered, news.dfm.UK.filtered[, 'Sept.11'] < 2)
news.selected <- docvars(news.dfm.UK.filtered)$`Article Date`


#### Media Intensity ####
news.dates <- as.tibble(x =news.selected)
colnames(news.dates) <- 'Date'
  
news.dates <- news.dates %>% 
  dplyr::filter(Date != 'NA')




news.dates$n <- 1
 
news.dates$Date.format <- dmy(news.dates$Date)
news.grouped <- news.dates %>% 
  group_by(Date.format) %>% 
  summarise(`number of articles` = sum(n))

dates <- seq.Date(from = min(news.grouped$Date.format), to = max(news.grouped$Date.format), by = 'day')
dates <- data.frame(dates)
news.grouped <- left_join(dates, news.grouped, by = c('dates' = 'Date.format'))
news.grouped$`number of articles`[is.na(news.grouped$`number of articles`)] <- 0
news.grouped$MA4 <- rollmean(news.grouped$`number of articles`, k = 4, align = 'right', fill = NA)
news.grouped$MA10 <- rollmean(news.grouped$`number of articles`, k = 10, align = 'right', fill = NA)
news.grouped <- na.omit(news.grouped)
summary(news.grouped)
  


save(news.grouped, file = 'news.grouped.Rdata')

# doc.feature.matrix <- dfm(myCorpus.split, remove = stopwords('english'), stem = TRUE, remove_punct = TRUE)
# topfeatures(doc.feature.matrix)
# 
# 
# UK.list <- c('UK',
#              'Britain',
#              'England',
#              'Wales',
#              'Scotland',
#              'Ireland',
#              'London')
# 
# 
# filtered.dfm <- dfm_keep(doc.feature.matrix, pattern = UK.list )

