## Book data, with category (de-)selection
## works, but category names still have underscores in them

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(RColorBrewer)
load("books_cumsum.Rda")

books_cumsum <- books_cumsum %>% 
  filter(!(books == "Ouvrages_littéraires"))


# make a color scheme so it updates consistently if a category is deselected
myColors <- brewer.pal(9,"Set1") # pre-defined colors
books_cumsum$books <- factor(books_cumsum$books, levels = c("Anthologies_et_manuels", "Autres", "Autres_essais", "Dictionnaires", 
                                                            "Esthétique_poétique_histoire_littéraire", 
                                                            "Mémoires_biographies_histoire_etc", "Roman_récit_fictionnel", 
                                                            "Science_et_vulgarisation", "Textes_en_vers"))
## assigning factor levels by hand, otherwise the order changes and labels do not update correctly
names(myColors) <- levels(books_cumsum$books)


ui <- fluidPage(
  
  # App title ----
  titlePanel("Type of citations by year"),
  
  # Make slider for years
  sidebarLayout(
    
    sidebarPanel(
      
      
      
      sliderTextInput(inputId = "year", "Year:",
                      choices = unique(books_cumsum$year), 
                      selected = min(books_cumsum$year),
                      grid = T,
                      width = "100%",
                      animate = animationOptions(interval=1000)),
      checkboxGroupInput("books", "Article Type:",
                         choices = unique(books_cumsum$books),
                         selected = unique(books_cumsum$books))
      
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
    books_cumsum %>%
      filter(
        year == input$year, # make it reactive
        books %in% input$books 
      )
  })
  
  output$artPlot <- renderPlot({
    
    ggplot(s(), aes(x="", y=cumsum_cite, fill=books)) + geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) + scale_fill_manual(name = "article", values = myColors, labels = input$books) +
      theme_bw() + labs(x="", y="", title = "Total citations by article type")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
