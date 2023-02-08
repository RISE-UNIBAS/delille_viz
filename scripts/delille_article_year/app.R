## This app creates a dynamic viz (bar chart) of article type with citations per year
## maybe another type of visualization would be better, though?
## citations-per-article-type-per-year_count.json
## add ylim?

library(shiny)
### help found here: https://stackoverflow.com/questions/62782423/r-shiny-how-to-filter-by-time-range-on-the-x-axis-and-simultaneously-have-two

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sliders"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    sidebarPanel(
      
      
      
      sliderInput(inputId = "year", "Year:",
                  min = min(cite_art_yr_df$year), max = max(cite_art_yr_df$year),
                  value = c(1,10),
                  step = 1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output:
      plotOutput("artPlot")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  s <- reactive({
    long_art_yr %>%
      filter(
        year >= input$year[1],
        year <= input$year[2] # make it reactive
      )
  })
  
  output$artPlot <- renderPlot({
    
    ggplot(s(), aes_string(x="year", y="citations", fill = "article")) + 
      geom_bar(position = "stack", stat = "identity") + 
      scale_fill_manual(values = c("#83B692", "#F9ADA0", "#F9627D", "#C65B7C", "#5B3758",
                                            "#413C58", "#A3C4BC", "#BFD7B5", "#E7EFC5", "#F2E7C9",
                                            "#3AB795", "#A0E8AF", "#86BAA1"),
                        labels = c("Essay", "Review", "Translation Review", "Literary", "Literary News",
                                    "Literary Nonfiction", "Literary Pedagogic", "Literary Scientific", "Novel",
                                    "Other Expression", "Poem or Verse", "Scientific or Vulgarized", "Translation"),
                        name = "") + scale_x_continuous(labels = scales::number_format(accuracy=1)) +
      labs(x = "Year", y = "Citations") + 
      theme_classic()
  })
}
# Run the application 
shinyApp(ui = ui, server = server)