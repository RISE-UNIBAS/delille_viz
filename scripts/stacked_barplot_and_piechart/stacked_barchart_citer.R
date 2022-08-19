# a static version of another plot I created with shiny
library(rjson)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)

setwd("/Users/antheaalberto/Desktop/RISE/Marchal/delille_file_system_layer_20210410/dataViz/data")


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

stacked_cite <- ggplot(long_citer, aes(x=verse, y=citations, fill = citer)) + 
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_manual(values = c("green4", "olivedrab2", "royalblue3", "lightsteelblue2"), 
                    labels = c("Autres", "Artistes", "Savants ou vulgarisateurs", "Gens de lettres"),
                    name = "") + scale_x_continuous(labels = scales::number_format(accuracy=1)) + 
  labs(x = "Verse", y = "Citations") +
   theme_classic()

stacked_cite


stacked_cite_col <- ggplot(long_citer, aes(x=verse, y=citations, fill = citer)) + 
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_manual(values = c("#3777FF", "#FFB5C2", "#FFBE86", "#FFE9CE"), 
                    labels = c("Autres", "Artistes", "Savants ou vulgarisateurs", "Gens de lettres"),
                    name = "") + scale_x_continuous(labels = scales::number_format(accuracy=1)) + 
  labs(x = "Verse", y = "Citations") +
  theme_classic()


stacked_cite_col
# save plot

