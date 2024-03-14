library(shiny)
library(shinyWidgets)
library(dplyr)
library(DT)
library(shinythemes)


European_Ski_Resorts <- read.csv("European_Ski_Resorts.csv")

ui <- fluidPage( theme = shinytheme("united"),
  
  titlePanel("Cost calculator"),
  
  sidebarLayout(
    sidebarPanel(
      #Date range 
      {
        dateRangeInput("dates", "When?",
                       start = Sys.Date(), 
                       min = Sys.Date())   
      },
      
      #Traveler count slider
      {
        sliderInput("travelers", "How many travelers?",
                    min = 1, max = 20, value = 10)
      },
      
      #Choosing how prices are shown
      {
        selectInput("show_price", "Show price",
                    choices = list("Per person" = 1, "For everyone" = 2),
                    selected = 1)
      },
      
      #Price range
      
      uiOutput("priceSlider"),
      
      {
        fluidRow(
          column(width = 6,
                 numericInput("min_cost",
                              "Min cost",
                              value = 0)
          ),
          column(width = 6,
                 numericInput("max_cost",
                              "Max cost",
                              value = 100)
          )
        )
      },
      
      #More filters
      actionButton("more_filters", "Filters")
    ),
    
    mainPanel(
      DT::dataTableOutput("result_table")
    )
  ),
  
  #Panel with additional options
  {fluidRow(
    column(6,
           uiOutput("additional_panel_1")
    ),
    column(2,
           uiOutput("additional_panel_2")
    ),
    column(2,
           uiOutput("additional_panel_3")
    ),
    column(2,
           uiOutput("additional_panel_4")
    )
  )}
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  # Updating the minimum value for dates
  observe({
    updateDateRangeInput(session, "dates",
                         min = if (!is.null(input$dates[1])){
                           input$dates[1]
                         })
  })
  
  # Modification of the European_Ski_Resorts data frame
  result_data <- reactive({
    European_Ski_Resorts %>% 
      mutate(ShowPrice = ifelse(input$show_price == 1, 1, 2),
             Price = (input$dates[2] - input$dates[1] + 1) * DayPassPriceAdult,
             PriceForEntirety = ifelse(ShowPrice == 2,
                                       Price * input$travelers,
                                       Price)) %>%
      filter(PriceForEntirety > 0 & TotalSlope > 0) 
  })
  
  # Generating the slider based on the PriceForEntirety column
  output$priceSlider <- renderUI({
    sliderInput("travel_cost", "Travel cost (in euros)",
                min = min(result_data()$PriceForEntirety),
                max = max(result_data()$PriceForEntirety),
                value = c(min(result_data()$PriceForEntirety), 
                          max(result_data()$PriceForEntirety)))
  })
  
  # Updating the value of max_cost and min_cost based on the selected range of the slider
  observe({
    updateNumericInput(session, "min_cost",
                       value = input$travel_cost[1],
                       max = input$travel_cost[2],
                       min = min(result_data()$PriceForEntirety))
    updateNumericInput(session, "max_cost",
                       value = input$travel_cost[2],
                       min = input$travel_cost[1],
                       max = max(result_data()$PriceForEntirety))
  })
  
  #Updating the value of the slider based on changes in numericInput
  observe({
    updateSliderInput(session, "travel_cost",
                      value = c(input$min_cost, input$max_cost))
  })
  
  # Support for a button to show/hide the panel with additional options
  options_visible <- reactiveVal(FALSE)
  
  observeEvent(input$more_filters, {
    options_visible(!options_visible())
  })
  
  output$additional_panel_1 <- renderUI({
    
    unique_country <- sort(unique(result_data()$Country))
    
    if (options_visible()) {
      # Additional selection of country
      pickerInput("country", "Preferred country",
                  choices = unique_country,
                  multiple = TRUE,
                  selected = unique_country,
                  options = list(`actions-box` = TRUE),
                  width = "85%")
    }
  })
  
  
  output$additional_panel_2 <- renderUI({
    if (options_visible()) {
      # Additional selection of ski lift type
      checkboxGroupInput("ski_lift",
                         "Type of ski lift",
                         choices = list("Surface lift" = 1,
                                        "Chairlift" = 2,
                                        "Gondola Lift" = 3),
                         selected = c(1,2,3),
                         width = "100%")
    }
  })
  
  output$additional_panel_3 <- renderUI({
    
    if (options_visible()) {
      # Additional choice of night skiing
      checkboxGroupInput("night_skiing",
                         "Night skiing",
                         choices = list("Yes" = 1),
                         selected = FALSE,
                         width = "100%")
    }
  })
  
  output$additional_panel_4 <- renderUI({
    
    if (options_visible()) {
      # Additional choice of snowparks
      checkboxGroupInput("snowparks",
                         "Snowparks",
                         choices = list("Yes" = 1),
                         selected = FALSE,
                         width = "100%")
    }
  })
  
  update_result_data <- reactive({
    
    if(options_visible()){
      
      result_data() %>%
        filter(Country %in% input$country,
               between(PriceForEntirety, input$min_cost, input$max_cost)) %>%
        
        # Selecting the type of ski lift
        {if (1 %in% input$ski_lift && length(input$ski_lift) > 0) filter(.,SurfaceLifts >= 1) 
          else filter(.,SurfaceLifts == 0)} %>%
        {if (2 %in% input$ski_lift && length(input$ski_lift) > 0) filter(.,ChairLifts >= 1) 
          else filter(.,ChairLifts == 0)} %>%
        {if (3 %in% input$ski_lift && length(input$ski_lift) > 0 ) filter(.,GondolaLifts >= 1) 
          else filter(.,GondolaLifts == 0)} %>%
        
        # Selecting the nightskiing option
        {if (input$night_skiing == 1 && length(input$night_skiing) > 0) filter(.,NightSki == "Yes") 
          else filter(.,NightSki %in% c("No","Yes"))} %>%
        
        # Selecting the snowparks option
        {if (input$snowparks == 1 && length(input$snowparks) > 0) filter(.,Snowparks == "Yes") 
          else filter(.,Snowparks %in% c("No","Yes"))} 
    } else {
      result_data()
    } 
  })
  
  result_data_rprice <- reactive({
    update_result_data() %>%
      filter(between(PriceForEntirety, input$min_cost, input$max_cost)) %>%
      select(Country, Resort, TotalSlope, PriceForEntirety) %>%
      rename("Total length of slope (in km)" = TotalSlope) %>%
      rename_with(~ ifelse( input$show_price == 2,
                            "The total cost of ski passes",
                            "The total cost of ski passes per person"),
                  "PriceForEntirety")
  })
  
  output$result_table = DT::renderDataTable({
    datatable(result_data_rprice(), rownames = FALSE)
    
  })
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)
