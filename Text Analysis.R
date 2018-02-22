# Dealing with newspaper data for media intensity index
library(quanteda)
library(readtext)

my.text <- readtext('News Data/Headline Specification/*')
myCorpus <- corpus(x = my.text)

# Each text document contains multiple newspaper articles. Splitting via 'DOCUMENTS' gives each document its own corpus
myCorpus.split <- corpus_segment(myCorpus, pattern = 'DOCUMENTS')

#### Functions ####

get.news.date <- function(news.corpus){
# This function reads the first 100 characters of each article looking for a date. Once it's found a date it returns the date for use in the corpus meta data
  news.subset <- substr(news.corpus, 1, 100)
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

myCorpus.split

