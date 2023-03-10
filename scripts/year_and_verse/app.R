## Shiny app that has two sliders, one for year, and a range for verses
## it also has a play button for years
library(shiny)
library(shinyWidgets)
library(tidyverse)
load("cite_year_df.Rda")

ui <- fluidPage(
  
  # App title ----
  titlePanel("Citations by year and verse"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      
      
      sliderInput(inputId = "year", "Year:",
                      min = min(cite_year_df$year), max = max(cite_year_df$year),
                      value = c(1789,1800),
                      step = 1,
                      width = "100%",
                      sep = ""),
      sliderInput(inputId = "verse", "Verse:",
                  min = min(cite_year_df$verse), max = max(cite_year_df$verse),
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
  s <- reactive(cite_year_df %>% 
                  filter(year >= input$year[1],
                         year <= input$year[2],
                         verse >= input$verse[1],
                         verse <= input$verse[2]))
  
  output$citePlot <- renderPlot({
    
    ggplot(s(), aes(x=verse, y = citations)) + geom_col(aes(x=verse, y = citations), position = "dodge", fill = "#087F8C") + theme_bw() +
      labs(x="Verse", y="Citations") + scale_x_continuous(labels = scales::number_format(accuracy=1), limits = c(min(input$verse), max(input$verse))) + ylim(0,7)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
