## Shiny app that has one slider, namely a range for verses
## the structure of each app is very similar, si I will put the explanations here

library(shiny)
library(shinyWidgets)
# load the necessary packages
# data should've been loaded beforehand, so no need to do it again

ui <- fluidPage( # first, we create the user interface (the front end)
  
  # App title ----
  titlePanel("Subset by verse"),
  
  
  sidebarLayout( 
    
    
    sidebarPanel( # first, the slider in the sidebar panel
      
      
      
      # This is the slider
      sliderInput(inputId = "verse", "Verse:",
                  min = min(cite_verse_df$verse), max = max(cite_verse_df$verse),
                  value = c(1,10), # default value displayed
                  step = 1,
                  width = "100%")
      
    ),
    
    # Main panel for displaying outputs, i.e. the visualization
    mainPanel(
      
      # Output: bar chart of how often a verse was cited
      plotOutput("citePlot")
      
    )
  )
)

# Define server logic required to draw a bar chart (this is the back end)
server <- function(input, output) {
  s <- reactive(cite_verse_df %>% 
                  filter(verse >= input$verse[1],
                         verse <= input$verse[2])) # make it reactive so plot reacts to slider
  
  output$citePlot <- renderPlot({
    
    ggplot(s(), aes_string(x="verse", y = "citations")) + geom_col(aes_string(x="verse", y = "citations"), position = "dodge", fill = "#83B692") + theme_bw() +
      labs(x="Verse", y="Citations") + scale_x_continuous(labels = scales::number_format(accuracy=1))
    
  }) # this generates the plot
}

# Run the application 
shinyApp(ui = ui, server = server)
