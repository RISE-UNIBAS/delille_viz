## This app creates a dynamic viz of when a certain verse was cited
## should I do an input that is not a slider?



library(shiny)

# Define UI for application that draws a histogram
# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sliders"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      
      

      sliderInput(inputId = "verse", "Verse:",
                  min = min(long_period$verse), max = max(long_period$verse),
                  value = c(1,10),
                  step = 1)
      
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
  s <- reactive({
    long_period %>%
      filter(
        verse >= input$verse[1],
        verse <= input$verse[2] # update 16.6.22: it works, but I still don't know what this does
      )
  })
  
  output$citePlot <- renderPlot({
    
    ggplot(s(), aes_string(x="verse", y = "citations", fill = "period")) + geom_bar(position = "dodge", stat = "identity") + theme_classic() +
      labs(fill = "Time Period") + scale_fill_manual(breaks = c("cite_before1800", "cite_1800_05", "cite_1806_13", "cite_1814_50", "cite_after"), #technically unnecessary, since I reordered before
                                                     labels = c("Before 1800", "1800-1805", "1806-1813", "1814-1850", "After 1850"),
                                                     values = c("skyblue2", "royalblue3", "palegreen2", "green4", "pink1")) +
      labs(x ="Verse", y = "Citations") + scale_x_continuous(labels = scales::number_format(accuracy=1))
  })
}
## update: changed y = "citation" and deleted selectInput
## before it was input$y and selectInput included (i.e. not commented out)
# Run the application 
shinyApp(ui = ui, server = server)
