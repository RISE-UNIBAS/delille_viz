## Shiny app that has two sliders, one for year, and a range for verses that is CUMULATIVE
## it also has a play button for years
library(shiny)
library(shinyWidgets)
library(tidyverse)
load("cite_year_cumsum.Rda")

ui <- fluidPage(
  
  # App title ----
  titlePanel("Citations by year and verse (cumulative)"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      
      
      sliderTextInput(inputId = "year", "Year:",
                      choices = unique(cite_year_cumsum$year), 
                      selected = min(cite_year_cumsum$year),
                      grid = T,
                      width = "100%",
                      animate = animationOptions(interval=1000)),
      sliderInput(inputId = "verse", "Verse:",
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
    
    ggplot(s(), aes(x=verse, y = citations)) + geom_col(aes(x=verse, y = cumsum_cite), position = "dodge", fill = "#087F8C") + theme_bw() +
      labs(x="Verse", y="Citations") + scale_x_continuous(labels = scales::number_format(accuracy=1), limits = c(min(input$verse), max(input$verse))) + 
      ylim(0,40)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
