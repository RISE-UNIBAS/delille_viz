## This app creates a dynamic viz (bar chart) of who cited which verse
## PIE CHART (should slider be range or single year?)
## RECHECK VALUES
## citations-per-verse-per-citer-type_count.json

library(shiny)
library(tidyverse)
load("citer_df.Rda")
load("long_citer.Rda")
### help found here: https://stackoverflow.com/questions/62782423/r-shiny-how-to-filter-by-time-range-on-the-x-axis-and-simultaneously-have-two

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sliders"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    sidebarPanel(
      
      
      
      sliderInput(inputId = "year", "Year:",
                      min = min(citer_df$year), max = max(citer_df$year),
                      value = c(1789,1800),
                      width = "100%",
                      step = 1,
                      sep = ""),
      sliderInput(inputId = "verse", "Verse:",
                  min = min(citer_df$verse), max = max(citer_df$verse),
                  value = c(1,10),
                  step = 1,
                  width = "100%")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output:
      plotOutput("citePlot")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  s <- reactive({
    long_citer %>%
      filter(
        year >= input$year[1],
        year <= input$year[2],
        verse >= input$verse[1],
        verse <= input$verse[2] # make it reactive
      )
  })
  
  output$citePlot <- renderPlot({
    
    ggplot(s(), aes(x="", y=citations, fill = citer)) + 
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) + 
      scale_fill_manual(values = c("green4", "olivedrab2", "royalblue3", "lightsteelblue2"), 
                        labels = c("Autres", "Artistes", "Savants ou vulgarisateurs", "Gens de lettres"),
                        name = "") + theme_bw() + labs(x="", y="", title = "Total citations by citer")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
#
