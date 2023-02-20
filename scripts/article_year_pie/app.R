## This app creates a dynamic viz (pie chart) of article type with citations per year
## RECHECK LABELS ETC.

library(shiny)
library(tidyverse)
load("cite_art_yr_df.Rda")
load("long_art_yr.Rda")

ui <- fluidPage(
  
  # App title ----
  titlePanel("Type of citations per year"),
  
  # Make slider for years
  sidebarLayout(
    
    sidebarPanel(
      
      
      
      sliderInput(inputId = "year", "Year:",
                  min = min(cite_art_yr_df$year), max = max(cite_art_yr_df$year),
                  value = c(1789,1800),
                  width = "100%",
                  step = 1,
                  sep = "")
      
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
    long_art_yr %>%
      filter(
        year >= input$year[1],
        year <= input$year[2] # make it reactive
      )
  })
  
  output$artPlot <- renderPlot({
    
    ggplot(s(), aes(x="", y=citations, fill=article)) + geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      scale_fill_manual(values = c("#83B692", "#F9ADA0", "#F9627D", "#5B3758",
                                            "#413C58", "#A3C4BC", "#BFD7B5", "#E7EFC5", "#F2E7C9",
                                            "#3AB795", "#A0E8AF", "#86BAA1"),
                        labels = c("Essay", "Review", "Translation Review", "Literary News",
                                  "Literary Nonfiction", "Literary Pedagogic", "Literary Scientific", "Novel",
                                  "Other Expression", "Poem or Verse", "Scientific or Vulgarized", "Translation"),
                        name = "Article Type") + theme_bw() + labs(x="", y="", title = "Total citations by article type")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
