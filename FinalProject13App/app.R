library(shiny)
#Load the package
library(devtools)
install_github("AU-R-Programming/FinalProject13")
library(FinalProject13)

# Load dataset
data_path <- system.file("extdata", "expenses.csv", package = "FinalProject13")
expenses <- read.csv(data_path)

# Add derived columns
median_charges <- 9382
expenses$charges_above_median <- ifelse(expenses$charges > median_charges, 1, 0)
expenses$years_older_than_min <- expenses$age - 18
expenses$bmi_greater_than_min <- expenses$bmi - 15.96
expenses$smoker_binary <- ifelse(expenses$smoker == "yes", 1, 0)
expenses$sex_binary <- ifelse(expenses$sex == "male", 1, 0)

ui <- fluidPage(
  titlePanel("Logistic Regression Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("predictor1", "Select First Predictor Variable:", 
                  choices = names(expenses), selected = "years_older_than_min"),
      selectInput("predictor2", "Select Second Predictor Variable:", 
                  choices = names(expenses), selected = "bmi_greater_than_min"),
      selectInput("response", "Select Response Variable:", 
                  choices = names(expenses), selected = "charges_above_median"),
      actionButton("run", "Run Analysis")
    ),
    
    mainPanel(
      h4("Estimated Coefficients:"),
      verbatimTextOutput("coefficients"),
      h4("Confusion Matrix:"),
      verbatimTextOutput("conf_matrix"),
      h4("Performance Metrics:"),
      verbatimTextOutput("performance"),
      h4("Preview of Selected Variables and Predicted Probabilities:"),
      tableOutput("data_preview")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$run, {
    # Ensure response is binary
    if (!all(expenses[[input$response]] %in% c(0, 1))) {
      showModal(modalDialog(
        title = "Error",
        "The response variable must be binary (0 or 1). Please choose an appropriate column.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Subset predictors and response
    X <- as.matrix(expenses[, c(input$predictor1, input$predictor2)])
    y <- expenses[[input$response]]
    
    # Estimate beta coefficients
    model <- estimate_beta(X, y)
    
    # Make predictions
    y_pred <- predict_prob(X, model$coefficients)
    
    # Calculate confusion metrics
    metrics <- confusion_metrics(y, y_pred, cutoff = 0.5)
    
    # Render outputs
    output$coefficients <- renderPrint({
      model$coefficients
    })
    
    output$conf_matrix <- renderPrint({
      metrics$confusion_matrix
    })
    
    output$performance <- renderPrint({
      list(
        Prevalence = metrics$prevalence,
        Accuracy = metrics$accuracy,
        Sensitivity = metrics$sensitivity,
        Specificity = metrics$specificity,
        FDR = metrics$fdr,
        DOR = metrics$dor
      )
    })
    
    output$data_preview <- renderTable({
      # Create a dataframe with the chosen predictors, response, and predicted probabilities
      data_preview <- data.frame(
        Predictor1 = expenses[[input$predictor1]],
        Predictor2 = expenses[[input$predictor2]],
        Response = expenses[[input$response]],
        Predicted_Probability = y_pred
      )
      head(data_preview)  # Show the first few rows
    })
  })
}

shinyApp(ui, server)

## Resources
# https://chatgpt.com/share/67538dfa-89fc-800a-a616-8c5fb61c6c40