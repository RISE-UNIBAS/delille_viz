## Shiny app that has two sliders, one for year, and a range for verses
## it also has a play button for years
library(shiny)
library(shinyWidgets)
library(tidyverse)
load("cite_year_df.Rda")

ui <- fluidPage(
  
  # App title ----
  titlePanel("Subset by year and verse"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      
      
      sliderTextInput(inputId = "year", "Year:",
                      choices = unique(cite_year_df$year), 
                      selected = min(cite_year_df$year),
                      grid = T,
                      width = "100%",
                      animate = animationOptions(interval=1000)),
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
                  filter(year==input$year,
                         verse >= input$verse[1],
                         verse <= input$verse[2]))
  
  output$citePlot <- renderPlot({
    
    ggplot(s(), aes(x=verse, y = citations)) + geom_col(aes_string(x="verse", y = "citations"), position = "dodge", fill = "#087F8C") + theme_bw() +
      labs(x="Verse", y="Citations") + scale_x_continuous(labels = scales::number_format(accuracy=1), limits = c(min(input$verse), max(input$verse))) + ylim(0,7)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
