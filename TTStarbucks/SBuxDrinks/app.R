library(shiny)
library(tidyverse)
library(tidytuesdayR)

#tuesdata <- tidytuesdayR::tt_load('2021-12-21')
starbucks <- tuesdata$starbucks
starbucks %>% mutate(across(.cols = 6:13, .fns = as.double))
starbucks$milk <- as.character(starbucks$milk)
starbucks$milk[starbucks$milk == 0] <- "none"
starbucks$milk[starbucks$milk == 1] <- "nonfat"
starbucks$milk[starbucks$milk == 2] <- "2%"
starbucks$milk[starbucks$milk == 3] <- "Soy"
starbucks$milk[starbucks$milk == 4] <- "Coconut"
starbucks$milk[starbucks$milk == 5] <- "Whole"
starbucks$whip[starbucks$whip == 0] <- "No"
starbucks$whip[starbucks$whip == 1] <- "Yes"

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Sidebar with a selector of drinks
    sidebarLayout(
        sidebarPanel(
            selectInput("drink1", "Drink 1", choices = sort(unique(starbucks$product_name))),
            selectInput("size1", "Size", choices = NULL),
            selectInput("milk1", "Milk Type", choices = NULL),
            selectInput("whip1", "Whipped Cream", choices = NULL),
            
            selectInput("drink2", "Drink 2", choices = sort(unique(starbucks$product_name))),
            selectInput("size2", "Size", choices = NULL),
            selectInput("milk2", "Milk Type", choices = NULL),
            selectInput("whip2", "Whipped Cream", choices = NULL)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("table1"),
           tableOutput("table2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    drink1 <- reactive({
      starbucks %>% filter(product_name == input$drink1)
    })
    
  observeEvent(drink1(), {
    choices <- unique(drink1()$size)
    updateSelectInput(inputId = "size1", choices = choices)
  })
  
  observeEvent(drink1(), {
    choices <- unique(drink1()$milk)
    updateSelectInput(inputId = "milk1", choices = choices)
  })
  
  observeEvent(drink1(), {
    choices <- unique(drink1()$whip)
    updateSelectInput(inputId = "whip1", choices = choices)
  })
  
  output$table1 <- renderTable({
    req(input$size1, input$milk1, input$whip1)
    drink1() %>% 
      filter(size == input$size1 & milk == input$milk1 & whip == input$whip1) %>% 
      select(-size, -milk, -whip, -serv_size_m_l, -product_name) %>% 
      mutate(across(.cols = everything(), .fns = as.double)) %>%
      pivot_longer(cols = 1:10,
                   names_to = "nutrition",
                   values_to = "amount")
  })
  
  drink2 <- reactive({
    starbucks %>% filter(product_name == input$drink2)
  })
  
  observeEvent(drink2(), {
    choices <- unique(drink2()$size)
    updateSelectInput(inputId = "size2", choices = choices)
  })
  
  observeEvent(drink2(), {
    choices <- unique(drink2()$milk)
    updateSelectInput(inputId = "milk2", choices = choices)
  })
  
  observeEvent(drink2(), {
    choices <- unique(drink2()$whip)
    updateSelectInput(inputId = "whip2", choices = choices)
  })
  
  output$table2 <- renderTable({
    req(input$size2, input$milk2, input$whip2)
    drink2() %>% 
      filter(size == input$size2 & milk == input$milk2 & whip == input$whip2) %>% 
      select(-size, -milk, -whip, -serv_size_m_l, -product_name) %>% 
      mutate(across(.cols = everything(), .fns = as.double)) %>%
      pivot_longer(cols = 1:10,
                   names_to = "nutrition",
                   values_to = "amount")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
