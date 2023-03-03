## NEW

library(shiny)
library(tidyverse)
load("long_cumsum.Rda")

ui <- fluidPage(
  
  # App title ----
  titlePanel("Type of citations by year"),
  
  # Make slider for years
  sidebarLayout(
    
    sidebarPanel(
      
      
      
      sliderTextInput(inputId = "year", "Year:",
                      choices = unique(long_cumsum$year), 
                      selected = min(long_cumsum$year),
                      grid = T,
                      width = "100%",
                      animate = animationOptions(interval=1000))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output:
      plotOutput("artPlot")
      
    )
  )
)

# Define server logic required to display pie chart
server <- function(input, output) {
  s <- reactive({
    long_cumsum %>%
      filter(
        year == input$year # make it reactive
      )
  })
  
  output$artPlot <- renderPlot({
    
    ggplot(s(), aes(x="", y=cumsum_cite, fill=article)) + geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      scale_fill_manual(values = c("#83B692", "#F9ADA0", "#F9627D", "#5B3758",
                                            "#413C58", "#A3C4BC", "#BFD7B5", "#E7EFC5", "#F2E7C9",
                                            "#3AB795", "#A0E8AF", "#86BAA1"),
                                            labels = c("Essay", "Review", "Translation Review", "Literary News",
                                                       "Literary Nonfiction", "Literary Pedagogic", "Literary Scientific", "Novel",
                                                       "Other Expression", "Poem or Verse", "Scientific or Vulgarized", "Translation"),
                        name = "Article Type") + 
      theme_bw() + labs(x="", y="", title = "Total citations by article type")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
