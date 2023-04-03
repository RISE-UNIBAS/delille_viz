## same as pie_select, but with a range for years
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(RColorBrewer)
load("long_art_yr.Rda")
load("long_art_yr_new.Rda")

# make a color scheme so it updates consistently if a category is deselected
myColors <- brewer.pal(12,"Set3") # pre-defined colors
long_art_yr_new$article_new <- factor(long_art_yr_new$article_new, levels = c( "Traduction", "Pédagogie des sciences ou des lettres", "Vie littéraire", 
                                                                               "Comptes rendus du poème", "Comptes rendus de traductions", 
                                                                               "Comptes rendus d'autres œuvres de Delille", "Texte en vers", "Roman, fiction en prose", 
                                                                               "Mémoires, biographies, anecdotes, etc.", "Autres essais, littérature panoramique, etc.", 
                                                                               "Esthétique, poétique, histoire littéraire", "Science et vulgarisation"))
## assigning factor levels by hand, otherwise the order changes (to alphabetical) and labels do not update correctly/mapping is wrong
names(myColors) <- levels(long_art_yr_new$article_new)


ui <- fluidPage(
  
  # App title ----
  titlePanel(div("Citations par type d'articles",  style = "font-size: 15px")),
  
  # Make slider for years
  sidebarLayout(
    
    sidebarPanel(
      
      
      
      sliderInput(inputId = "year", "Anées:",
                  min = min(long_art_yr_new$year), max = max(long_art_yr_new$year),
                  value = c(1789,1800),
                  width = "100%",
                  step = 1,
                  sep = ""),
      checkboxGroupInput("article_new", "Article Type:",
                         choices = unique(long_art_yr_new$article_new),
                         selected = unique(long_art_yr_new$article_new)) ## the order here is decisive in how to assign levels!!
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output:
      plotOutput("artPlot")
      
    )
  )
)

# Define server logic required to display pie chart
server <- function(input, output, session) {
  
  
  
  s <- reactive({
    long_art_yr_new %>%
      filter(
        year >= input$year[1],
        year <= input$year[2], # make it reactive
        article_new %in% input$article_new 
      )
  })
  
  output$artPlot <- renderPlot({
    
    ggplot(s(), aes(x="", y=citations, fill=article_new)) + geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) + scale_fill_manual(name = "article", values = myColors, labels = input$article_new) +
      theme_bw() + labs(x="", y="", title = "Total citations by article type")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
