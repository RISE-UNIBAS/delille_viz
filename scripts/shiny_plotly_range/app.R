## trying shiny and plotly (WIP) WITH A RANGE SLIDER

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
load("long_art_yr.Rda")
load("long_art_yr_new.Rda")

long_art_yr_new$color <- leaflet::colorFactor(
  palette = "Set3", domain = long_art_yr_new$article_new
)(long_art_yr_new$article_new)

ui <- fluidPage(
  
  # App title ----
  titlePanel(div("Citations par type d'articles",  style = "font-size: 15px")),
  
  # Make slider for years
  sidebarLayout(
    
    sidebarPanel(
      
      
      
      sliderInput(inputId = "year", "Anées:",
                  min = min(long_art_yr_new$year), max = max(long_art_yr_new$year),
                  value = c(1789,1800),
                  width = "100%",
                  step = 1,
                  sep = ""),
      checkboxGroupInput("article_new", "Article Type:",
                         choices = unique(long_art_yr_new$article_new),
                         selected = unique(long_art_yr_new$article_new))
      
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
    long_art_yr_new %>%
      filter(
        year >= input$year[1],
        year <= input$year[2],
        article_new %in% input$article_new # make it reactive
      )
  })
  
  output$artPlot <- renderPlotly({
    
    plot_ly(s(), labels = ~article_new, values = ~citations, type = 'pie', 
            marker = list(colors = ~color, 
                          line = list(color = '#FFFFFF', width = 1))) %>% 
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
