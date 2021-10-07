library(shiny)
library(tidyverse)
library(lubridate)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("statedex", "Select state:", state.name),
      selectInput("plottype", "Select plot:", c("cases", "delta_cases", "deaths", "delta_deaths")), width = 3
    ),
    mainPanel(plotOutput("plot", width = "800px"),
              tableOutput("table"))
  )
)

server <- function(input, output, session) {
  covid_index_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  covid_index_states$date <- as.character(covid_index_states$date)
  
  state_index <- reactive(covid_index_states %>% filter(state == input$statedex) %>% 
                            mutate(delta_cases = 
                                     coalesce(cases - lag(cases, order_by = state), 1)) %>% 
                            mutate(delta_deaths = 
                                     coalesce(deaths - lag(deaths, order_by = state), 1)) %>% 
                            filter(delta_cases >= 1) # Delta > 0 to account for holidays where nothing is reported and random negative deltas
                          )
  
  
  output$plot <- renderPlot({
    ggplot(state_index(),
           aes(x = as.Date(date),
               y = eval(parse(text = input$plottype))))+
      geom_line()+
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%m/%y")+
      #scale_y_continuous(n.breaks = 15)+
      labs(x = "Date",
           y = input$plottype,
           title = paste0(input$plottype, " per state"))
  }, res = 96)
  
  output$table <- renderTable(state_index() %>% tail(5))
  
}

shinyApp(ui, server) 