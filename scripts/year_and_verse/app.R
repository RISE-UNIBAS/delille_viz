## Shiny app that has two sliders, one for year, and a range for verses

## SOLVED:
## time range is confusing, because the more citations there are, the more a year appears in the df, which is reflected in the slider (many points for 1800 for example)
## this does not falsify the viz, but it's not exactly ideal
## another grouping mechanism? -> unique() in the slider did the trick!

## Problem (cosmetic): sometimes it does not really update the x-axis "properly"
## i.e. when there are only citations between verses 340-350 for a certain year, then I can adjust the slider almost as much as I want, it will keep on showing me the range with citations only
## fix: adding limits = c(min(input$verse), max(input$verse)) to scale_x_continuous did the trick!
## depending on what one wants, this can be removed again

library(shiny)
library(shinyWidgets)




ui <- fluidPage(
  
  # App title ----
  titlePanel("Subset by year and verse"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      
      
      # Input: Specification of range within an interval ----
      sliderTextInput(inputId = "year", "Year:",
                      choices = unique(cite_year_df$year), # well, would you look at that
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
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      plotOutput("citePlot")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  s <- reactive(cite_year_df %>% 
                  filter(year==input$year,
                         verse >= input$verse[1],
                         verse <= input$verse[2]))
  
  output$citePlot <- renderPlot({
    
    ggplot(s(), aes_string(x="verse", y = "citations")) + geom_col(aes_string(x="verse", y = "citations"), position = "dodge", fill = "#087F8C") + theme_bw() +
      labs(x="Verse", y="Citations") + scale_x_continuous(labels = scales::number_format(accuracy=1), limits = c(min(input$verse), max(input$verse))) + ylim(0,7)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
