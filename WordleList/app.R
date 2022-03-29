#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("5 Letter Word Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("LetterPosition", "Letter Position", c("L1", "L2", "L3", "L4", "L5")),
            selectInput("letter", "Letter", toupper(letters))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Create the wordlist
  wordlist <- read_delim("https://raw.githubusercontent.com/theglucru/ShinyApps/main/WordleList/wordlist.txt", delim = ",")
  wordlist <- tibble("word" = colnames(wordlist))
  wordlist$word <- toupper(wordlist$word)
  
  wordlist <- wordlist %>% mutate(L1 = str_sub(wordlist[[1]], 1, 1),
                                  L2 = str_sub(wordlist[[1]], 2, 2),
                                  L3 = str_sub(wordlist[[1]], 3, 3),
                                  L4 = str_sub(wordlist[[1]], 4, 4),
                                  L5 = str_sub(wordlist[[1]], 5, 5),)
  
  workingList <- reactive(wordlist %>% 
                            select(contains("L")) %>% 
                            filter(.data[[input$LetterPosition]] == input$letter) %>% 
                            pivot_longer(everything(), names_to = "pos", values_to = "letter") %>% 
                            group_by(pos) %>% 
                            count(letter) %>% 
                            arrange(desc(n), .by_group = TRUE) %>% 
                            slice_head(n = 5) %>% 
                            filter(pos != input$LetterPosition))
  
  output$Table <- renderTable(workingList()) # used for Reference
  
  plot <- reactive(ggplot(data = workingList())+
                     geom_col(aes(x = fct_reorder(letter, -n), y = n, fill = pos), 
                              position = position_dodge()))
  
  output$Plot <- renderPlot(plot()+
                              labs(x = "",
                                   y = "",
                                   title = "Letter position appearances"))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
