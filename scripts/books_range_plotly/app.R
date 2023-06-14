## BOOKS
## custom range slider done and with plotly

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
load("long_books_agg.Rda")

long_books_agg <- long_books_agg %>% 
  filter(!(books == "Ouvrages_littéraires"))

long_books_agg[long_books_agg$books == "Anthologies_et_manuels",2] <- "Anthologies et manuels"
long_books_agg[long_books_agg$books == "Autres_essais",2] <- "Autres essais"
long_books_agg[long_books_agg$books == "Esthétique_poétique_histoire_littéraire",2] <- "Esthétique, poétique, histoire littéraire"
long_books_agg[long_books_agg$books == "Mémoires_biographies_histoire_etc",2] <- "Mémoires, biographies, histoire etc"
long_books_agg[long_books_agg$books == "Roman_récit_fictionnel",2] <- "Roman, récit fictionnel"
long_books_agg[long_books_agg$books == "Science_et_vulgarisation",2] <- "Science et vulgarisation"
long_books_agg[long_books_agg$books == "Textes_en_vers",2] <- "Textes en vers"

long_books_agg$color <- leaflet::colorFactor(
  palette = "Set1", domain = long_books_agg$books
)(long_books_agg$books)

ui <- fluidPage(
  
  # App title ----
  titlePanel(div("Citations par type des livres",  style = "font-size: 15px")),
  
  # Make slider for years
  sidebarLayout(
    
    sidebarPanel(
      
      
      
      sliderInput(inputId = "year", "Anées:",
                  min = min(long_books_agg$year), max = max(long_books_agg$year),
                  value = c(1798,1808),
                  width = "100%",
                  step = 1,
                  sep = ""),
      checkboxGroupInput("books", "Book Type:",
                         choices = unique(long_books_agg$books),
                         selected = unique(long_books_agg$books))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output:
      plotlyOutput(outputId = "artPlot")
      
    )
  )
)

# Define server logic required to display pie chart
server <- function(input, output, session) {
  s <- reactive({
    long_books_agg %>%
      filter(
        year >= input$year[1],
        year <= input$year[2],
        books %in% input$books # make it reactive
      )
  })
  
  output$artPlot <- renderPlotly({
    
    plot_ly(s(), labels = ~books, values = ~citations, type = 'pie', 
            marker = list(colors = ~color, 
                          line = list(color = '#FFFFFF', width = 1))) %>% 
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
