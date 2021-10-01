library(shiny)
library(tidyverse)

ui <- fluidPage(
  fluidRow(
    column(4, selectInput("statedex", "Select state:", state.name))
  ),
  #fluidRow(column(4, tableOutput("table")))
  plotOutput("plot", width = "800px")
)

server <- function(input, output, session) {
  covid_index_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  state_index <- reactive(covid_index_states %>% filter(state == input$statedex))
  
  # output$table <- renderTable(state_index())
  
    
  output$plot <- renderPlot({
    ggplot(state_index(),
           aes(x = date,
               y = cases))+
      geom_line()+
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%m/%y")+
      scale_y_continuous(n.breaks = 15)+
      labs(x = "Date",
           y = "Cases",
           title = "Total covid Cases per state")
  }, res = 96)
}

shinyApp(ui, server)