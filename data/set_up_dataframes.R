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

rm(list=ls()) # cleans the environment to make everything a bit less busy

# I generally prefer to work with "standard" data frames instead of lists
# So chapters 1-4 (amend that) will read the json in form of a list and then transform them into a data frame
# The logic is essentially the same for every separate data frame

###### 1. citations-per-verse.json ######
cite_verse <- fromJSON(file="citations-per-verse.json") # read data

cite_verse_df <- tibble(verse = character(),
                        citations = character()) # create empty data frame

# Then we have a so-called "for-loop" go through the list annd fill the empty data frame 
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
citers <- fromJSON(file="citations-per-verse-per-citer-type_count.json")
names(citers$results$bindings[[1]])

citer_df <- tibble(verse = character(),
                   man_of_letters = character(),
                   vulgarizer = character(),
                   artist = character(),
                   others = character(),
                   all = character())
for (i in 1:length(citers$results$bindings)) {
  number <- citers$results$bindings[[i]]$verseOrdinalNumeral$value
  man_of_letters <- citers$results$bindings[[i]]$manOfLettersCount$value
  vulgarizer <- citers$results$bindings[[i]]$vulgarizerCount$value
  artist <- citers$results$bindings[[i]]$artistCount$value
  others <- citers$results$bindings[[i]]$otherRoleCount$value
  all <- citers$results$bindings[[i]]$allRoles$value
  citer_df[i,1] <- number
  citer_df[i,2] <- man_of_letters
  citer_df[i,3] <- vulgarizer
  citer_df[i,4] <- artist
  citer_df[i,5] <- others
  citer_df[i,6] <- all
}

citer_df <- sapply(citer_df,as.numeric)
citer_df <- as.data.frame(citer_df)

long_citer <- citer_df %>% gather(citer, citations, -c(verse))

long_citer <- long_citer[long_citer$citer!="all",]

long_citer$citer <- as.factor(long_citer$citer)
levels(long_citer$citer)
long_citer$citer <- factor(long_citer$citer, levels=c("others", "artist", "vulgarizer", "man_of_letters"))

##### Save data frames #####

