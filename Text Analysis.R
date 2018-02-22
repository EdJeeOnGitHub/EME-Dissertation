## News Reading ##
library(tm)
library(quanteda)

file <- "C:/Users/ed/Dropbox/Ed/Ed Uni work/EME/Data/Original Data/News Data/Headline Specification"

news.text <- VCorpus(DirSource(file, encoding = 'UTF-8'), readerControl = list(language = 'lat'))



file2 <- "C:/Users/ed/Dropbox/Ed/Ed Uni work/EME/Data/Original Data/News Data/Headline Specification/News 1980-2000.txt"
news.text.split <- corpus_segment(file2, pattern = 'DOCUMENTS')
