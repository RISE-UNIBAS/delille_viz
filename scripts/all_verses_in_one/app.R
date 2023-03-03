## New app
## Trying to put all verses in one

library(shiny)
library(shinyWidgets)
library(tidyverse)
load("cite_verse_df.Rda")
load("long_period.Rda")
load("cite_year_df.Rda")
load("citer_df.Rda")
load("long_citer.Rda")
# load the necessary packages & data


ui <- fluidPage( # first, we create the user interface (the front end)
  
  # App title ----
  titlePanel("Citations by verse"),
  
  
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
      
      tabsetPanel(type = "tabs",
                  tabPanel("Verses only", plotOutput("citePlot")),
                  tabPanel("Verses by Period", plotOutput("citePlot2")),
                  tabPanel("Verses by Citer", plotOutput("citePlot3")))
      
    )
  )
)

# Define server logic required to draw a bar chart (this is the back end)
server <- function(input, output) {
  s <- reactive(cite_verse_df %>% 
                  filter(verse >= input$verse[1],
                         verse <= input$verse[2]))
  r <- reactive({
    long_period %>%
      filter(
        verse >= input$verse[1],
        verse <= input$verse[2] 
      )
  })# make it reactive so plot reacts to slider
  t <- reactive({
    long_citer %>%
      filter(
        verse >= input$verse[1],
        verse <= input$verse[2] # make it reactive
      )
  })
  
  output$citePlot <- renderPlot({
    
    ggplot(s(), aes(x=verse, y = citations)) + geom_col(aes(x=verse, y = citations), position = "dodge", fill = "#83B692") + theme_bw() +
      labs(x="Verse", y="Citations") + scale_x_continuous(labels = scales::number_format(accuracy=1))
    
  }) # this generates the plot
  output$citePlot2 <- renderPlot({
    
    ggplot(r(), aes(x=verse, y = citations, fill = period)) + geom_bar(position = "dodge", stat = "identity") + theme_classic() +
      labs(fill = "Time Period") + scale_fill_manual(breaks = c("cite_before1800", "cite_1800_05", "cite_1806_13", "cite_1814_50", "cite_after"), #technically unnecessary, since I reordered before
                                                     labels = c("Before 1800", "1800-1805", "1806-1813", "1814-1850", "After 1850"),
                                                     values = c("skyblue2", "royalblue3", "palegreen2", "green4", "pink1")) +
      labs(x ="Verse", y = "Citations") + scale_x_continuous(labels = scales::number_format(accuracy=1))
  })
  output$citePlot3 <- renderPlot({
    
    ggplot(t(), aes(x=verse, y=citations, fill = citer)) + 
      geom_bar(position = "stack", stat = "identity") + 
      scale_fill_manual(values = c("green4", "olivedrab2", "royalblue3", "lightsteelblue2"), 
                        labels = c("Autres", "Artistes", "Savants ou vulgarisateurs", "Gens de lettres"),
                        name = "") + scale_x_continuous(labels = scales::number_format(accuracy=1)) +
      labs(x = "Verse", y = "Citations") +
      theme_classic()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
