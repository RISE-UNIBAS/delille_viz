## Shiny app that has one slider, namely a range for verses
## citations-per-verse.json

## should I add a ylim?


library(shiny)
library(shinyWidgets)



ui <- fluidPage(
  
  # App title ----
  titlePanel("Subset by verse"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      
      
      # Input: Specification of range within an interval ----
      sliderInput(inputId = "verse", "Verse:",
                  min = min(cite_verse_df$verse), max = max(cite_verse_df$verse),
                  value = c(1,10),
                  step = 1,
                  width = "100%")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      plotOutput("citePlot")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  s <- reactive(cite_verse_df %>% 
                  filter(verse >= input$verse[1],
                         verse <= input$verse[2]))
  
  output$citePlot <- renderPlot({
    
    ggplot(s(), aes_string(x="verse", y = "citations")) + geom_col(aes_string(x="verse", y = "citations"), position = "dodge", fill = "#83B692") + theme_bw() +
      labs(x="Verse", y="Citations") + scale_x_continuous(labels = scales::number_format(accuracy=1))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
