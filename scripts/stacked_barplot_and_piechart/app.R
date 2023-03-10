## This app creates a dynamic viz (bar chart) of who cited which verse
## citations-per-verse-per-citer-type_count.json

library(shiny)
library(tidyverse)
load("citer_df.Rda")
load("long_citer.Rda")
### help found here: https://stackoverflow.com/questions/62782423/r-shiny-how-to-filter-by-time-range-on-the-x-axis-and-simultaneously-have-two

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # App title ----
  titlePanel("Citations by citer type (barchart)"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    sidebarPanel(
      
      
      
      sliderInput(inputId = "verse", "Verse:",
                  min = min(citer_df$verse), max = max(citer_df$verse),
                  value = c(1,10),
                  step = 1)
      
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
        verse >= input$verse[1],
        verse <= input$verse[2] # make it reactive
      )
  })
  
  output$citePlot <- renderPlot({
    
    ggplot(s(), aes(x=verse, y=citations, fill = citer, color=citer)) + 
      geom_bar(position = "stack", stat = "identity") + 
      scale_fill_manual(values = c("green4", "olivedrab2", "royalblue3", "lightsteelblue2"), 
                        labels = c("Autres", "Artistes", "Savants ou vulgarisateurs", "Gens de lettres"),
                        name = "") + 
      scale_color_manual(values = c("green4", "olivedrab2", "royalblue3", "lightsteelblue2"), 
                        labels = c("Autres", "Artistes", "Savants ou vulgarisateurs", "Gens de lettres"),
                        name = "", guide="none") +
      scale_x_continuous(labels = scales::number_format(accuracy=1)) +
      labs(x = "Verse", y = "Citations") +
      theme_classic()
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
