library(shiny) 
library(tidyverse)
library(lubridate)
library(rsconnect)
library(maps)

covid_index_states <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
covid_index_counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
covid_index_states$date <- as.character(covid_index_states$date)
options(scipen = 999)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("statedex", "Select state:", state.name),
      selectInput("plottype", "Select plot:", c("cases", "delta_cases", "deaths", "delta_deaths")), width = 3,
      dateRangeInput("daterange", "Date Range", min = "2020-01-01", start = "2020-03-01")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 plotOutput("plot", 
                            width = "800px")),
        tabPanel("Table", 
                 dataTableOutput("table")),
        tabPanel("Map", 
                 plotOutput("fill")
                 )
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
      scale_x_date(date_breaks = "2 months",
                   date_labels = "%m/%y")+
      scale_y_continuous(n.breaks = 10)+
      labs(x = "Date",
           y = input$plottype,
           title = paste0(input$plottype, " per state"))
  }, res = 96)
  
  output$table <- renderDataTable(state_index(), options = list(pageLength = 10,
                                                                ordering = NULL))
  
  output$map <- renderPlot(plot())
  output$fill <- renderPlot(fill())
    
  cur_state <- reactive({
    covid_index_counties %>% 
      filter(state == input$statedex & date == Sys.Date() - 1) %>% 
      mutate_all(., tolower)
  })

  
  state_map <- reactive({
    map_data("county", input$statedex)
  })
  
  joined_county <- reactive({
      inner_join(cur_state(), state_map(), by = c("county" = "subregion"))
  })
  
  # Base map
  plot <- reactive({
    ggplot(data = joined_county(), aes(x = long, y = lat, group = group))+
    geom_polygon(fill = NA, color = "black")+
    coord_quickmap()+
    theme(
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank()
    )
  })
  
  # Filling in map
  fill <-reactive({ 
    plot()+
      geom_polygon(data = joined_county(), aes(fill = as.double(cases)), color = "black")+
      scale_fill_distiller(palette = 8, direction = 1)+
      labs(fill = "Total cases")
  })
    
}

shinyApp(ui, server) 
