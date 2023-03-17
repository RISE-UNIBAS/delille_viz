# Prepare data for plotly pie chart
# Not necessary to run, unless data frame needs to be changed
library(rjson)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
#setwd("/Users/antheaalberto/Desktop/RISE/Marchal/delille_file_system_layer_20210410/dataViz/data")
# adapt file path!
cite_verse_type <- fromJSON(file="citations-per-verse-per-article-type_count.json") # load data

names(cite_verse_type$results$bindings[[1]])

cite_type_df <- tibble(verse = character(),
                       translationArticle = character(),
                       literaryPedagogicArticle = character(),
                       literaryNewsArticleCount = character(),
                       hDCReviewArticle = character(),
                       hDCTranslationReviewArticle = character(),
                       otherDelilleExpressionReviewArticle = character(),
                       poemOrVersePlayArticle = character(),
                       novelArticle = character(),
                       literaryNonfictionArticle = character(),
                       essayOrPanoramicOrPhilosophicOrHumanitiesArticle = character(),
                       literaryScientificArticle = character(),
                       literaryArticle = character(),
                       scientificOrVulgarizedArticle = character(),
                       allCitingArticle = character())


for (i in 1:length(cite_verse_type$results$bindings)) {
  number <- cite_verse_type$results$bindings[[i]]$verseOrdinalNumeral$value
  citation1 <- cite_verse_type$results$bindings[[i]]$translationArticleCount$value
  citation2 <- cite_verse_type$results$bindings[[i]]$literaryPedagogicArticleCount$value
  citation3 <- cite_verse_type$results$bindings[[i]]$literaryNewsArticleCount$value
  citation4 <- cite_verse_type$results$bindings[[i]]$hDCReviewArticleCount$value
  citation5 <- cite_verse_type$results$bindings[[i]]$hDCTranslationReviewArticleCount$value
  citation6 <- cite_verse_type$results$bindings[[i]]$otherDelilleExpressionReviewArticleCount$value
  citation7 <- cite_verse_type$results$bindings[[i]]$poemOrVersePlayArticleCount$value
  citation8 <- cite_verse_type$results$bindings[[i]]$novelArticleCount$value
  citation9 <- cite_verse_type$results$bindings[[i]]$literaryNonfictionArticleCount$value
  citation10 <- cite_verse_type$results$bindings[[i]]$essayOrPanoramicOrPhilosophicOrHumanitiesArticleCount$value
  citation11 <- cite_verse_type$results$bindings[[i]]$literaryScientificArticleCount$value
  citation12 <- cite_verse_type$results$bindings[[i]]$literaryArticleCount$value
  citation13 <- cite_verse_type$results$bindings[[i]]$scientificOrVulgarizedArticleCount$value
  citation14 <- cite_verse_type$results$bindings[[i]]$allCitingArticle$value
  cite_type_df[i,1] <- number
  cite_type_df[i,2] <- citation1
  cite_type_df[i,3] <- citation2
  cite_type_df[i,4] <- citation3
  cite_type_df[i,5] <- citation4
  cite_type_df[i,6] <- citation5
  cite_type_df[i,7] <- citation6
  cite_type_df[i,8] <- citation7
  cite_type_df[i,9] <- citation8
  cite_type_df[i,10] <- citation9
  cite_type_df[i,11] <- citation10
  cite_type_df[i,12] <- citation11
  cite_type_df[i,13] <- citation12
  cite_type_df[i,14] <- citation13
  cite_type_df[i,15] <- citation14
}

cite_type_df <- sapply(cite_type_df,as.numeric)
cite_type_df <- as.data.frame(cite_type_df)

long_type <- cite_type_df %>% gather(type, citations, -c(verse))
long_type <- long_type[long_type$type!="allCitingArticle",]

save(long_type, file = "long_type.Rda")
