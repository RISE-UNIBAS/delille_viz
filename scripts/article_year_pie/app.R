## This app creates a dynamic viz (pie chart) of article type with citations per year
## RECHECK LABELS ETC.

library(shiny)
library(tidyverse)
load("cite_art_yr_df.Rda")
load("long_art_yr.Rda")

ui <- fluidPage(
  
  # App title ----
  titlePanel(div("Citations par type d'articles",  style = "font-size: 15px")),
  
  # Make slider for years
  sidebarLayout(
    
    sidebarPanel(
      
      
      
      sliderInput(inputId = "year", "Anées:",
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
      scale_fill_manual(values = c("red3", "mediumorchid", "slateblue", "yellowgreen",
                                         "pink", "dodgerblue2", "lightsalmon1", "khaki2", "grey43",
                                         "chartreuse4", "darkorange1", "skyblue1"),
                                         labels = c("Autres essais, littérature panoramique, etc.", "Comptes rendus du poème", "Comptes rendus de traductions", "Vie littéraire",
                                                    "Mémoires, biographies, anecdotes, etc.", "Pédagogie des sciences ou des lettres", "Esthétique, poétique, histoire littéraire", "Roman, fiction en prose",
                                                    "Comptes rendus d'autres œuvres de Delille", "Texte en vers", "Science et vulgarisation", "Traductions"),
                        name = "Catégories") + 
      theme_bw() + labs(x="", y="", title = "Total des citations par catégories")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
