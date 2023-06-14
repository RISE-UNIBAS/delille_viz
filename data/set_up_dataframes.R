## Marchal project ##
#### Setting up all the necessary dataframes ###
###### 0. Preparation ########

list.of.packages <- c("ggplot2", "rjson", "dplyr", "tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
## the three lines above whether the necessary packages are installed; and if they are not, they will be installed
## the four lines below LOAD the necessary packages
library(ggplot2)
library(rjson)
library(dplyr)
library(tidyr)

setwd("/Users/antheaalberto/Documents/GitHub/delille_viz/data") ## Adjust this!

rm(list=ls()) # cleans the environment to make everything a bit less busy

# I generally prefer to work with "standard" data frames instead of lists
# So sections 1-5 (amend that) will read the json in form of a list and then transform them into a data frame
# The logic is essentially the same for every separate data frame

###### 1. citations-per-verse.json ######
cite_verse <- fromJSON(file="citations-per-verse.json") # read data

cite_verse_df <- tibble(verse = character(),
                        citations = character()) # create empty data frame

# Then we have a so-called "for-loop" go through the list and fill the empty data frame 
for (i in 1:length(cite_verse$results$bindings)) {
  number <- cite_verse$results$bindings[[i]]$verseOrdinalNumeral$value
  citation <- cite_verse$results$bindings[[i]]$citationCount$value
  cite_verse_df[i,1] <- number
  cite_verse_df[i,2] <- citation
}

# Right now, verse and citation are stored as character strings, but re-coding them to be numbers makes more sense for our purposes
# Since the first command messes with the structure somewhat, we'll turn everything back into a proper df with the second command
cite_verse_df <- sapply(cite_verse_df,as.numeric)
cite_verse_df <- as.data.frame(cite_verse_df)


###### 2. citations-per-verse-per-year.json ######
cite_verse_year <- fromJSON(file="citations-per-verse-per-year.json")

cite_year_df <- tibble(verse = character(),
                       year = character(),
                       citations = character())


for (i in 1:length(cite_verse_year$results$bindings)) {
  number <- cite_verse_year$results$bindings[[i]]$verseOrdinalNumeral$value
  year <- cite_verse_year$results$bindings[[i]]$citationYear$value
  citation <- cite_verse_year$results$bindings[[i]]$numberOfCitationsPerYear$value
  cite_year_df[i,1] <- number
  cite_year_df[i,2] <- year
  cite_year_df[i,3] <- citation
}

cite_year_df <- sapply(cite_year_df,as.numeric)
cite_year_df <- as.data.frame(cite_year_df)

cite_year_df <-  
  cite_year_df %>% 
  group_by(verse) %>% 
  arrange(year)

#### Make it cumulative
cite_year_cumsum <- cite_year_df %>% 
  group_by(verse) %>% 
  mutate(cumsum_cite = cumsum(citations))

cite_year_cumsum <- cite_year_cumsum %>%   
  complete(year = full_seq(c(1789:1926), 1))

cite_year_cumsum <- cite_year_cumsum %>% 
  group_by(verse) %>% 
  fill(cumsum_cite)

cite_year_cumsum$cumsum_cite <- ifelse(is.na(cite_year_cumsum$cumsum_cite), 0, cite_year_cumsum$cumsum_cite)


##### 3. citations-per-verse-per-period.json #####
cite_verse_period <- fromJSON(file="citations-per-verse-per-period.json")

cite_period_df <- tibble(verse = character(),
                         cite_before1800 = character(),
                         cite_1800_05 = character(),
                         cite_1806_13 = character(),
                         cite_1814_50 = character(),
                         cite_after = character())

for (i in 1:length(cite_verse_period$results$bindings)) {
  number <- cite_verse_period$results$bindings[[i]]$verseOrdinalNumeral$value
  citation1 <- cite_verse_period$results$bindings[[i]]$publicationBefore1800Count$value
  citation2 <- cite_verse_period$results$bindings[[i]]$publication1800_05Count$value
  citation3 <- cite_verse_period$results$bindings[[i]]$publication1806_13Count$value
  citation4 <- cite_verse_period$results$bindings[[i]]$publication1814_50Count$value
  citation5 <- cite_verse_period$results$bindings[[i]]$publicationAfter1850Count$value
  cite_period_df[i,1] <- number
  cite_period_df[i,2] <- citation1
  cite_period_df[i,3] <- citation2
  cite_period_df[i,4] <- citation3
  cite_period_df[i,5] <- citation4
  cite_period_df[i,6] <- citation5
}



cite_period_df <- sapply(cite_period_df,as.numeric)
cite_period_df <- as.data.frame(cite_period_df)

# to make the visualization easier to create, we'll transform the df from wide to long
long_period <- cite_period_df %>% gather(period, citations, -c(verse))

long_period$period <- factor(long_period$period, levels = c("cite_before1800", "cite_1800_05", "cite_1806_13", "cite_1814_50", "cite_after"))

##### 4. citations-per-verse-per-citer-type_count.json #####
citers <- fromJSON(file="distinct-citers-per-verse-per-citer-type-per-year_count.json")
names(citers$results$bindings[[1]])

citer_df <- tibble(verse = character(),
                   year = character(),
                   man_of_letters = character(),
                   vulgarizer = character(),
                   artist = character(),
                   others = character(),
                   all = character())
for (i in 1:length(citers$results$bindings)) {
  number <- citers$results$bindings[[i]]$verseOrdinalNumeral$value
  year <- citers$results$bindings[[i]]$citationYear$value
  man_of_letters <- citers$results$bindings[[i]]$manOfLettersCount$value
  vulgarizer <- citers$results$bindings[[i]]$vulgarizerCount$value
  artist <- citers$results$bindings[[i]]$artistCount$value
  others <- citers$results$bindings[[i]]$otherRoleCount$value
  all <- citers$results$bindings[[i]]$allRoles$value
  citer_df[i,1] <- number
  citer_df[i,2] <- year
  citer_df[i,3] <- man_of_letters
  citer_df[i,4] <- vulgarizer
  citer_df[i,5] <- artist
  citer_df[i,6] <- others
  citer_df[i,7] <- all
}

citer_df <- sapply(citer_df,as.numeric)
citer_df <- as.data.frame(citer_df)

long_citer <- citer_df %>% gather(citer, citations, -c(verse, year))

long_citer <- long_citer[long_citer$citer!="all",]

long_citer$citer <- as.factor(long_citer$citer)
levels(long_citer$citer)
long_citer$citer <- factor(long_citer$citer, levels=c("others", "artist", "vulgarizer", "man_of_letters"))

#### 5. citations-per-article-type-per-year_count ######

cite_art_yr <- fromJSON(file="citations-per-article-type-per-year_count.json")

names(cite_art_yr$results$bindings[[1]])

cite_art_yr_df <- tibble(year = character(),
                         translationArticle = character(),
                         literaryPedagogicArticle = character(),
                         literaryNewsArticle = character(),
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


for (i in 1:length(cite_art_yr$results$bindings)) {
  number <- cite_art_yr$results$bindings[[i]]$citationYear$value
  citation1 <- cite_art_yr$results$bindings[[i]]$translationArticleCount$value
  citation2 <- cite_art_yr$results$bindings[[i]]$literaryPedagogicArticleCount$value
  citation3 <- cite_art_yr$results$bindings[[i]]$literaryNewsArticleCount$value
  citation4 <- cite_art_yr$results$bindings[[i]]$hDCReviewArticleCount$value
  citation5 <- cite_art_yr$results$bindings[[i]]$hDCTranslationReviewArticleCount$value
  citation6 <- cite_art_yr$results$bindings[[i]]$otherDelilleExpressionReviewArticleCount$value
  citation7 <- cite_art_yr$results$bindings[[i]]$poemOrVersePlayArticleCount$value
  citation8 <- cite_art_yr$results$bindings[[i]]$novelArticleCount$value
  citation9 <- cite_art_yr$results$bindings[[i]]$literaryNonfictionArticleCount$value
  citation10 <- cite_art_yr$results$bindings[[i]]$essayOrPanoramicOrPhilosophicOrHumanitiesArticleCount$value
  citation11 <- cite_art_yr$results$bindings[[i]]$literaryScientificArticleCount$value
  citation12 <- cite_art_yr$results$bindings[[i]]$literaryArticleCount$value
  citation13 <- cite_art_yr$results$bindings[[i]]$scientificOrVulgarizedArticleCount$value
  citation14 <- cite_art_yr$results$bindings[[i]]$allCitingArticle$value
  cite_art_yr_df[i,1] <- number
  cite_art_yr_df[i,2] <- citation1
  cite_art_yr_df[i,3] <- citation2
  cite_art_yr_df[i,4] <- citation3
  cite_art_yr_df[i,5] <- citation4
  cite_art_yr_df[i,6] <- citation5
  cite_art_yr_df[i,7] <- citation6
  cite_art_yr_df[i,8] <- citation7
  cite_art_yr_df[i,9] <- citation8
  cite_art_yr_df[i,10] <- citation9
  cite_art_yr_df[i,11] <- citation10
  cite_art_yr_df[i,12] <- citation11
  cite_art_yr_df[i,13] <- citation12
  cite_art_yr_df[i,14] <- citation13
  cite_art_yr_df[i,15] <- citation14
}

cite_art_yr_df <- sapply(cite_art_yr_df,as.numeric)
cite_art_yr_df <- as.data.frame(cite_art_yr_df)

long_art_yr <- cite_art_yr_df %>% gather(article, citations, -c(year))
long_art_yr <- long_art_yr[long_art_yr$article!="allCitingArticle",]
long_art_yr <- long_art_yr[long_art_yr$article!="literaryArticle",] # I am not sure about this? But the numbers don't add up otherwise


long_art_yr$article <- as.factor(long_art_yr$article)

### Make it cumulative
long_cumsum <- long_art_yr %>% 
  group_by(article) %>% 
  mutate(cumsum_cite = cumsum(citations))

long_cumsum <- long_cumsum %>% 
  complete(year = full_seq(c(1789:1904), 1))

long_cumsum <- long_cumsum %>% 
  group_by(article) %>% 
  fill(cumsum_cite)

####### 6. citations-per-book-type-per-year-count ########

books <- fromJSON(file="citations-per-book-type-per-year-count.json")

names(books$results$bindings[[1]])

books_df <- tibble(year = character(),
                   period = character(),
                   Dictionnaires = character(),
                   Anthologies_et_manuels = character(),
                   Science_et_vulgarisation = character(),
                   Autres = character(),
                   Textes_en_vers = character(),
                   Roman_récit_fictionnel = character(),
                   Esthétique_poétique_histoire_littéraire = character(),
                   Mémoires_biographies_histoire_etc = character(),
                   Autres_essais = character(),
                   Ouvrages_littéraires = character())

for (i in 1:length(books$results$bindings)) {
  year <- books$results$bindings[[i]]$publicationYear$value
  period <- books$results$bindings[[i]]$publicationPeriod$value
  dictionaries <- books$results$bindings[[i]]$Dictionnaires$value
  anthologies <- books$results$bindings[[i]]$Anthologies_et_manuels$value
  science_vulg <- books$results$bindings[[i]]$Science_et_vulgarisation$value
  others <- books$results$bindings[[i]]$Autres$value
  text_vers <- books$results$bindings[[i]]$Textes_en_vers$value
  novels <- books$results$bindings[[i]]$Roman___récit_fictionnel$value
  aesthetic <- books$results$bindings[[i]]$Esthétique___poétique___histoire_littéraire$value
  memoirs <- books$results$bindings[[i]]$Mémoires___biographies___histoire___etc$value
  other_essays <- books$results$bindings[[i]]$Autres_essais$value
  literary <- books$results$bindings[[i]]$Ouvrages_littéraires$value
  books_df[i,1] <- year
  books_df[i,2] <- period
  books_df[i,3] <- dictionaries
  books_df[i,4] <- anthologies
  books_df[i,5] <- science_vulg
  books_df[i,6] <- others
  books_df[i,7] <- text_vers
  books_df[i,8] <- novels
  books_df[i,9] <- aesthetic
  books_df[i,10] <- memoirs
  books_df[i,11] <- other_essays
  books_df[i,12] <- literary # this is a catch all category!!
  print(i)
}

books_df[,c(1,3:12)] <- sapply(books_df[,c(1,3:12)],as.numeric)
books_df <- as.data.frame(books_df)

books_df <- books_df[,-2] # deleting period for now

long_books <- books_df %>% gather(books, citations, -c(year))

long_books_agg <- long_books %>% 
  group_by(year, books) %>% 
  summarise(citations = sum(citations))

### Make it cumulative
books_cumsum <- long_books_agg %>% 
  group_by(books) %>% 
  mutate(cumsum_cite = cumsum(citations))

books_cumsum <- books_cumsum %>% 
  complete(year = full_seq(c(1798:1926), 1))

books_cumsum <- books_cumsum %>% 
  group_by(books) %>% 
  fill(cumsum_cite)


##### Save data frames #####
# either adjust or comment out, this was for getting the proper data into the app folder

save(cite_art_yr_df, file = "/Users/antheaalberto/Documents/GitHub/delille_viz/scripts/article_year_pie/cite_art_yr_df.Rda")
save(cite_verse_df, file = "/Users/antheaalberto/Documents/GitHub/delille_viz/scripts/delille_verses_only/cite_verse_df.Rda")
save(citer_df, file = "/Users/antheaalberto/Documents/GitHub/delille_viz/scripts/stacked_barplot_and_piechart/citer_df.Rda")
save(long_citer, file = "/Users/antheaalberto/Documents/GitHub/delille_viz/scripts/stacked_barplot_and_piechart/long_citer.Rda")
save(long_period, file = "/Users/antheaalberto/Documents/GitHub/delille_viz/scripts/verse_and_period/long_period.Rda")
save(cite_year_df, file = "/Users/antheaalberto/Documents/GitHub/delille_viz/scripts/year_and_verse/cite_year_df.Rda")
save(cite_year_cumsum, file = "/Users/antheaalberto/Documents/GitHub/delille_viz/scripts/year_and_verse_cumulative/cite_year_cumsum.Rda")
save(long_cumsum, file = "/Users/antheaalberto/Documents/GitHub/delille_viz/scripts/pie_cumulative/long_cumsum.Rda")
save(books_cumsum, file ="/Users/antheaalberto/Documents/GitHub/delille_viz/scripts/pie_books/books_cumsum.Rda")
save(long_books_agg, file = "/Users/antheaalberto/Documents/GitHub/delille_viz/scripts/books_range_plotly/long_books_agg.Rda")
