# Creating various pie charts with plotly of articles types that cited Delille's verses
library(plotly)
library(tidyverse)
#setwd("/Users/antheaalberto/Documents/GitHub/delille_viz/scripts/pie_chart_plotly")
# adapt file path!

load("long_type.Rda")


long_type$type <- as.factor(long_type$type)


long_agg <- long_type %>% 
  group_by(type) %>% 
  summarise(total_citations=sum(citations))

colors <- c("#83B692", "#F9ADA0", "#F9627D", "#C65B7C", "#5B3758",
            "#413C58", "#A3C4BC", "#BFD7B5", "#E7EFC5", "#F2E7C9",
            "#4A4063", "#BFACC8", "#C8C6D7")
labels = c("Essay", "Review", "Translation Review", "Literary", "Literary News",
           "Literary Nonfiction", "Literary Pedagogic", "Literary Scientific", "Novel",
           "Other Expression", "Poem or Verse", "Scientific or Vulgarized", "Translation")
long_agg$type_new <- labels

fig <- plot_ly(long_agg, labels = ~type_new, values = ~total_citations, type = 'pie',
               marker = list(colors = colors, 
                             line = list(color = '#FFFFFF', width = 1)))
fig <- fig %>% layout(title = 'Article Type',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig

# reference: https://plotly.com/r/pie-charts/

## According to the PPP on page 11, novels was left out (I think) as was the largest category, literary

colors_sub <- c("#B24C63", "#5438DC", "#357DED", "#56EEF4", "#32E875",
                "#DDFFF7", "#FFD2FC", "#E980FC", "#B96AC9", "#231B1B",
                "#297373")
long_agg_sub <- long_agg[(long_agg$type_new!="Literary" & long_agg$type_new!="Novel"),]

fig2 <- plot_ly(long_agg_sub, labels = ~type_new, values = ~total_citations, type = 'pie',
               marker = list(colors = colors_sub, 
                             line = list(color = '#FFFFFF', width = 1)))
fig2 <- fig2 %>% layout(title = 'Article Type',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig2
# can be saved as an html


