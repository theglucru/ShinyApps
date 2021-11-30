library(shiny) 
library(tidyverse)
library(lubridate)

covid_index_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
covid_index_states$date <- as.character(covid_index_states$date)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("statedex", "Select state:", state.name),
      selectInput("plottype", "Select plot:", c("cases", "delta_cases", "deaths", "delta_deaths")), width = 3,
      dateRangeInput("daterange", "Date Range", min = "2020-01-01", start = "2020-03-01")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot", width = "800px")),
        tabPanel("Table", dataTableOutput("table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  state_index <- reactive({covid_index_states %>% 
                            filter(date >= input$daterange[1] & date <= input$daterange[2]) %>% 
                            filter(state == input$statedex) %>% 
                            mutate(delta_cases = 
                                     coalesce(cases - lag(cases, order_by = state), 1)) %>% 
                            mutate(delta_deaths = 
                                     coalesce(deaths - lag(deaths, order_by = state), 1)) %>% 
                            filter(delta_cases >= 1) # Delta > 0 to account for holidays where nothing is reported and random negative deltas
                          })
  
  
  output$plot <- renderPlot({
    ggplot(state_index(),
           aes(x = as.Date(date),
               y = eval(parse(text = input$plottype))))+
      geom_line()+
      theme_bw()+
      scale_x_date(date_breaks = "1 month",
                   date_labels = "%m/%y")+
      scale_y_continuous(n.breaks = 10)+
      labs(x = "Date",
           y = input$plottype,
           title = paste0(input$plottype, " per state"))
  }, res = 96)
  
  output$table <- renderDataTable(state_index(), options = list(pageLength = 10,
                                                                ordering = NULL))
  
}

shinyApp(ui, server) 
