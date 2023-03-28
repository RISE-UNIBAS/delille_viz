## trying shiny and plotly (WIP)
## with category (de-)selection
## I used the "Paired" and "Set3" (more pastel colors) palette 
## but it's possible to create a vector with the desired colors

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
load("long_cumsum_new.Rda")
                    
long_cumsum_new$color <- leaflet::colorFactor(
  palette = "Set3", domain = long_cumsum_new$article_new
)(long_cumsum_new$article_new)

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
      plotlyOutput(outputId = "artPlot")
      
    )
  )
)

# Define server logic required to display pie chart
server <- function(input, output, session) {
  s <- reactive({
    long_cumsum_new %>%
      filter(
        year == input$year,
        article_new %in% input$article_new # make it reactive
      )
  })
  
  output$artPlot <- renderPlotly({
    
    plot_ly(s(), labels = ~article_new, values = ~cumsum_cite, type = 'pie', 
            marker = list(colors = ~color, 
                          line = list(color = '#FFFFFF', width = 1))) %>% 
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
