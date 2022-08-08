## This app creates a dynamic viz of who cited which verse
## citations-per-verse-per-citer-type_count.json

library(shiny)
### help found here: https://stackoverflow.com/questions/62782423/r-shiny-how-to-filter-by-time-range-on-the-x-axis-and-simultaneously-have-two
### atm I only get it to work by having a choice option between citations and citations2 - both are the same variable
### cite_strophe_df$citations2 <- cite_strophe_df$citations
### how do I get it to work with one variable alone??

# Define UI for application that draws a histogram
# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sliders"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      
      
      # Input: Specification of range within an interval ----
      #      selectInput(inputId = "y", label = "Y-Axis:", 
      #                  choices=c("citations"), 
      #                  selected = "citations"),
      sliderInput(inputId = "verse", "Verse:",
                  min = min(citer_df$verse), max = max(citer_df$verse),
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
    long_citer %>%
      filter(
        verse >= input$verse[1],
        verse <= input$verse[2] # update 16.6.22: it works, but I still don't know what this does
      )
  })
  
  output$citePlot <- renderPlot({
    
    ggplot(s(), aes_string(x="verse", y="citations", fill = "citer")) + 
      geom_bar(position = "stack", stat = "identity") + 
      scale_fill_manual(values = c("green4", "olivedrab2", "royalblue3", "lightsteelblue2"), 
                        labels = c("Autres", "Artistes", "Savants ou vulgarisateurs", "Gens de lettres"),
                        name = "") + scale_x_continuous(labels = scales::number_format(accuracy=1)) +
      labs(x = "Verse", y = "Citations") +
      theme_classic()
  })
}
## update: changed y = "citation" and deleted selectInput
## before it was input$y and selectInput included (i.e. not commented out)
# Run the application 
shinyApp(ui = ui, server = server)
