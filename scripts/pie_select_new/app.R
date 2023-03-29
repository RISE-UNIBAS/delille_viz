## Fix for incorrect updating of labels when selecting is due to article_new not being a factor and if factorized, the order changes
## So the fix is to manually specify labels (this does mean they cannot easily be changed, however)
## Alternative with long_cumsum (instead of long_cumsum_new): not adding labels = in graph, everything will update properly but labels will be hDCReviewArticle etc. (not shorter names)

## Maybe try: assigning labels within ggplot; with the manual reordering that might still worl

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(RColorBrewer)
load("long_cumsum_new.Rda")

# make a color scheme so it updates consistently if a category is deselected
myColors <- brewer.pal(12,"Set3") # pre-defined colors
long_cumsum_new$article_new <- factor(long_cumsum_new$article_new, levels = c("Essay", "Review", "Translation Review",
                                                                              "Literary News", "Literary Nonfiction", "Literary Pedagogic",
                                                                              "Literary Scientific", "Novel", "Other Expression",
                                                                              "Poem or Verse", "Scientific or Vulgarized", "Translation"))
## assigning factor levels by hand, otherwise the order changes and labels do not update correctly
names(myColors) <- levels(long_cumsum_new$article_new)


ui <- fluidPage(
  
  # App title ----
  titlePanel("Type of citations by year"),
  
  # Make slider for years
  sidebarLayout(
    
    sidebarPanel(
      
      
      
      sliderTextInput(inputId = "year", "Year:",
                      choices = unique(long_cumsum_new$year), 
                      selected = min(long_cumsum_new$year),
                      grid = T,
                      width = "100%",
                      animate = animationOptions(interval=1000)),
      checkboxGroupInput("article_new", "Article Type:",
                         choices = unique(long_cumsum_new$article_new),
                         selected = unique(long_cumsum_new$article_new))
      
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
    long_cumsum_new %>%
      filter(
        year == input$year, # make it reactive
        article_new %in% input$article_new 
      )
  })
  
  output$artPlot <- renderPlot({
    
    ggplot(s(), aes(x="", y=cumsum_cite, fill=article_new)) + geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) + scale_fill_manual(name = "article", values = myColors, labels = input$article_new) +
      theme_bw() + labs(x="", y="", title = "Total citations by article type")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
