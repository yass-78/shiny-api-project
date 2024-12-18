library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(readr)
library(httr)  # Load httr for making HTTP requests

# Define the API URL
api_url <- "http://my-api-container:8000/predict"

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Título principal
  titlePanel(
    div(
      style = "text-align: center; color: #2C3E50; font-family: Arial; margin-bottom: 20px;",
      tags$h1("Breast Cancer Prediction with BRCA Model")
    )
  ),
  
  # Diseño general con colores de fondo para separar secciones
  fluidRow(
    column(
      4,
      div(
        style = "background-color: #F8F9FA; padding: 20px; border-radius: 10px; box-shadow: 0px 2px 5px rgba(0,0,0,0.1);",
        tags$h4("Patient Information", style = "color: #2C3E50;"),
        numericInput("age", "Age:", value = 50, min = 20, max = 100),
        selectInput("menopause", "Menopause:", 
                    choices = list("Premenopausia" = 0, "Postmenopausia" = 1)),
        selectInput("hormone", "Hormone Therapy:", 
                    choices = list("No" = 0, "Yes" = 1)),
        selectInput("brca", "BRCA Mutated:", 
                    choices = list("No" = 0, "Yes" = 1)),
        actionButton("predict", "Predict", 
                     icon = icon("play"), 
                     style = "background-color: #3498DB; color: white; border: none; 
                              width: 100%; height: 40px; font-size: 16px; 
                              border-radius: 5px; margin-top: 20px;")
      )
    ),
    
    column(
      8,
      div(
        style = "background-color: #FFFFFF; padding: 20px; border-radius: 10px; 
                 box-shadow: 0px 2px 5px rgba(0,0,0,0.1);",
        tags$h4("Prediction Results", style = "color: #34495E; text-align: center;"),
        wellPanel(
          style = "background-color: #ECF0F1; padding: 15px; border-radius: 10px;",
          tags$h4("Probability of Breast Cancer:", style = "color: #2C3E50; text-align: center;"),
          verbatimTextOutput("result", placeholder = TRUE)
        ),
        div(
          style = "margin-top: 30px;",
          plotOutput("cancerPlot", height = "400px")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Prediction on user input
  observeEvent(input$predict, {
    
    # Prepare data for the API request
    user_data <- list(
      BRCA_GENE = as.numeric(input$brca),
      HORMONE_THERAPY = as.numeric(input$hormone),
      INFERRED_MENOPAUSAL_STATE = as.numeric(input$menopause),
      AGE = as.numeric(input$age)
    )
    
    # Send POST request to the API
    response <- POST(
      url = api_url,
      body = user_data,
      encode = "json"
    )
    
    # Parse the response
    result <- content(response)
    
    # Output prediction
    output$result <- renderText({
      paste0("The probability of getting breast cancer is: ", result$cancer_probability, "%")
    })
    
    # Generate and display the plot
    output$cancerPlot <- renderPlot({
      # Generate age range for the plot
      ages <- seq(20, 100, by = 5)
      
      # Create data frame for prediction
      prediction_data <- data.frame(
        AGE = ages,
        BRCA_GENE = as.numeric(input$brca),
        INFERRED_MENOPAUSAL_STATE = as.numeric(input$menopause),
        HORMONE_THERAPY = as.numeric(input$hormone)
      )
      
      # Predict cancer probabilities
      prediction_data$CANCER_PROBABILITY <- result$cancer_probability  # Use the value from the API response
      
      # Create the plot
      ggplot(prediction_data, aes(x = AGE, y = CANCER_PROBABILITY)) +
        geom_line(color = "#3498DB", size = 1.2) +
        geom_point(color = "#3498DB", size = 3) +
        labs(
          title = "Probability of Breast Cancer by Age",
          x = "Age",
          y = "Cancer Probability"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#34495E"),
          axis.title.x = element_text(size = 14, color = "#2C3E50"),
          axis.title.y = element_text(size = 14, color = "#2C3E50"),
          axis.text = element_text(size = 12, color = "#34495E")
        ) +
        scale_y_continuous(limits = c(0, 1), labels = scales::percent)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)