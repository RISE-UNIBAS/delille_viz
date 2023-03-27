## Shiny app that has two sliders, one for year, and a range for verses that is CUMULATIVE
## it also has a play button for years
library(shiny)
library(shinyWidgets)
library(tidyverse)
load("cite_year_cumsum.Rda")
cite_year_cumsum <- cite_year_cumsum[,-3]

ui <- fluidPage(
  
  # App title ----
  titlePanel(div("Évolution chronologique des citations",  style = "font-size: 15px")),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      
      
      sliderTextInput(inputId = "year", "Années:",
                      choices = unique(cite_year_cumsum$year), 
                      selected = min(cite_year_cumsum$year),
                      grid = T,
                      width = "100%",
                      animate = animationOptions(interval=500)), # the lower the interval, the faster the animation
      sliderInput(inputId = "verse", "Vers:",
                  min = min(cite_year_cumsum$verse), max = max(cite_year_cumsum$verse),
                  value = c(1,10),
                  step = 1,
                  width = "100%")
      
    ),
    
    mainPanel(
      
      plotOutput("citePlot")
      
    )
  )
)

server <- function(input, output) {
  s <- reactive(cite_year_cumsum %>% 
                  filter(year==input$year,
                         verse >= input$verse[1],
                         verse <= input$verse[2]))
  
  output$citePlot <- renderPlot({
    
    ggplot(s()) + geom_col(aes(x=verse, y = cumsum_cite), position = "dodge", fill = "#087F8C") + theme_bw() +
      labs(x="Vers", y="Citations") + scale_x_continuous(labels = scales::number_format(accuracy=1)) + 
      ylim(0,40)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
