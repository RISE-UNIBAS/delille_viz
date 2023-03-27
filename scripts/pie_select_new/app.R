## test test
## this version works! 
## needed to change input values
## but now the colors update properly
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(RColorBrewer)
load("long_cumsum.Rda")

# make a color scheme so it updates consistently if a category is deselected
myColors <- brewer.pal(12,"Set3") # pre-defined colors
names(myColors) <- levels(long_cumsum$article)
colScale <- scale_fill_manual(name = "article",values = myColors, 
                              labels = c("Essay", "Review", "Translation Review", "Literary News",
                                         "Literary Nonfiction", "Literary Pedagogic", "Literary Scientific", "Novel",
                                         "Other Expression", "Poem or Verse", "Scientific or Vulgarized", "Translation"))

ui <- fluidPage(
  
  # App title ----
  titlePanel("Type of citations by year"),
  
  # Make slider for years
  sidebarLayout(
    
    sidebarPanel(
      
      
      
      sliderTextInput(inputId = "year", "Year:",
                      choices = unique(long_cumsum$year), 
                      selected = min(long_cumsum$year),
                      grid = T,
                      width = "100%",
                      animate = animationOptions(interval=1000)),
      checkboxGroupInput("article", "Article Type:",
                         choices = c("Essay"= "essayOrPanoramicOrPhilosophicOrHumanitiesArticle",
                                     "Review" = "hDCReviewArticle",
                                     "Translation Review" = "hDCTranslationReviewArticle",
                                     "Literary News" = "literaryNewsArticle",
                                     "Literary Nonfiction" = "literaryNonfictionArticle",
                                     "Literary Pedagogic" = "literaryPedagogicArticle",
                                     "Literary Scientific" = "literaryScientificArticle",
                                     "Novel" = "novelArticle",
                                     "Other" = "otherDelilleExpressionReviewArticle",
                                     "Poem/Verse/Play" = "poemOrVersePlayArticle",
                                     "Scientific or Vulgarized" = "scientificOrVulgarizedArticle",
                                     "Translation" = "translationArticle"),
                         selected = c("Essay"= "essayOrPanoramicOrPhilosophicOrHumanitiesArticle",
                                      "Review" = "hDCReviewArticle",
                                      "Translation Review" = "hDCTranslationReviewArticle",
                                      "Literary News" = "literaryNewsArticle",
                                      "Literary Nonfiction" = "literaryNonfictionArticle",
                                      "Literary Pedagogic" = "literaryPedagogicArticle",
                                      "Literary Scientific" = "literaryScientificArticle",
                                      "Novel" = "novelArticle",
                                      "Other" = "otherDelilleExpressionReviewArticle",
                                      "Poem/Verse/Play" = "poemOrVersePlayArticle",
                                      "Scientific or Vulgarized" = "scientificOrVulgarizedArticle",
                                      "Translation" = "translationArticle"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output:
      plotOutput("artPlot")
      
    )
  )
)

# Define server logic required to display pie chart
server <- function(input, output, session) {
  
  
  
  s <- reactive({
    long_cumsum %>%
      filter(
        year == input$year, # make it reactive
        article %in% input$article 
      )
  })
  
  output$artPlot <- renderPlot({
    
    ggplot(s(), aes(x="", y=cumsum_cite, fill=article)) + geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) + colScale +
      theme_bw() + labs(x="", y="", title = "Total citations by article type")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
