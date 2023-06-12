## Book data, with category (de-)selection
## checked against other visualization, updating when deselecting should work properly

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(RColorBrewer)
load("books_cumsum.Rda")

books_cumsum <- books_cumsum %>% 
  filter(!(books == "Ouvrages_littéraires"))

books_cumsum[books_cumsum$books == "Anthologies_et_manuels",1] <- "Anthologies et manuels"
books_cumsum[books_cumsum$books == "Autres_essais",1] <- "Autres essais"
books_cumsum[books_cumsum$books == "Esthétique_poétique_histoire_littéraire",1] <- "Esthétique, poétique, histoire littéraire"
books_cumsum[books_cumsum$books == "Mémoires_biographies_histoire_etc",1] <- "Mémoires, biographies, histoire etc"
books_cumsum[books_cumsum$books == "Roman_récit_fictionnel",1] <- "Roman, récit fictionnel"
books_cumsum[books_cumsum$books == "Science_et_vulgarisation",1] <- "Science et vulgarisation"
books_cumsum[books_cumsum$books == "Textes_en_vers",1] <- "Textes en vers"

books_cumsum$books <- factor(books_cumsum$books, levels = c("Anthologies et manuels", "Autres", "Autres essais", "Dictionnaires", 
                                                            "Esthétique, poétique, histoire littéraire", 
                                                            "Mémoires, biographies, histoire etc", "Roman, récit fictionnel", 
                                                            "Science et vulgarisation", "Textes en vers"))

# make a color scheme so it updates consistently if a category is deselected
myColors <- brewer.pal(9,"Set1") # pre-defined colors

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
