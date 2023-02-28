## This app creates a dynamic viz of the period when a certain verse was cited

library(shiny)
library(tidyverse)
load("long_period.Rda")

ui <- fluidPage(
  
  # App title ----
  titlePanel("Sliders"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      
      

      sliderInput(inputId = "verse", "Verse:",
                  min = min(long_period$verse), max = max(long_period$verse),
                  value = c(1,10),
                  step = 1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      plotOutput("citePlot")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  s <- reactive({
    long_period %>%
      filter(
        verse >= input$verse[1],
        verse <= input$verse[2] 
      )
  })
  
  output$citePlot <- renderPlot({
    
    ggplot(s(), aes(x=verse, y = citations, fill = period)) + geom_bar(position = "dodge", stat = "identity") + theme_classic() +
      labs(fill = "Time Period") + scale_fill_manual(breaks = c("cite_before1800", "cite_1800_05", "cite_1806_13", "cite_1814_50", "cite_after"), #technically unnecessary, since I reordered before
                                                     labels = c("Before 1800", "1800-1805", "1806-1813", "1814-1850", "After 1850"),
                                                     values = c("skyblue2", "royalblue3", "palegreen2", "green4", "pink1")) +
      labs(x ="Verse", y = "Citations") + scale_x_continuous(labels = scales::number_format(accuracy=1))
  })
}

shinyApp(ui = ui, server = server)
