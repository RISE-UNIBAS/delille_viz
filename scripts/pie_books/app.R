#### Book data
#### Barebones visualization, no category selection

library(shiny)
library(tidyverse)
library(shinyWidgets)
load("books_cumsum.Rda")

books_cumsum <- books_cumsum %>% 
  filter(!(books == "Ouvrages_littéraires"))

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
                      animate = animationOptions(interval=1000))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output:
      plotOutput("artPlot")
      
    )
  )
)

# Define server logic required to display pie chart
server <- function(input, output) {
  s <- reactive({
    books_cumsum %>%
      filter(
        year == input$year # make it reactive
      )
  })
  
  output$artPlot <- renderPlot({
    
    ggplot(s(), aes(x="", y=cumsum_cite, fill=books)) + geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      scale_fill_manual(values = c("#83B692", "#F9ADA0", "#F9627D", "#5B3758",
                                   "#413C58", "#A3C4BC", "#BFD7B5", "#E7EFC5", "#F2E7C9"),
                        labels = c("Anthologies et manuels", "Autres", "Autres essais", "Dictionnaires", "Esthétique, poétique, histoire, littéraire", 
                                   "Mémoires, biographies, histoire etc", "Roman, récit fictionnel", 
                                   "Science et vulgarisation", "Textes en vers"),
                        name = "Book Type") + 
      theme_bw() + labs(x="", y="", title = "Citations livres") # change title
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
