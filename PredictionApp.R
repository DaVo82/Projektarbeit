library(shiny)
library(dplyr)
library(shinythemes)
library(rgbif)
library(ggplot2)
library(caret)
library(ranger)
library(gbm)
library(DALEX)

ui <- fluidPage(
  titlePanel("Sepsis-Prädiktion"),
  sidebarLayout(
    sidebarPanel(
      radioButtons('model', 'Modell wählen:', 
                  c("Random Forest", "Gradient Boosting Machine")),
      selectInput('patient', 'Patienten wählen:', unique(test3$Patient_ID)),
      actionButton('predictButton', 'Berechnen')
    ),
    mainPanel(
      htmlOutput('probability'), tags$head(tags$style("#probability{font-size: 20px}")),
              plotOutput('plot')
      )
    )
  )

server <- function(input, output, session){
  observeEvent(input$predictButton, {
    if (input$model == "Gradient Boosting Machine") {
      explainer <- explain_gbm_ds3
      new_dataset <- test3
    }
    if (input$model == "Random Forest") {
      explainer <- explain_rf_ds3
      new_dataset <- test3_imputed
    }
    patient_index <- input$patient
    new_data <- new_dataset[patient_index, -2]
    withProgress(message = 'Berechne Vorhersage...', value = 0, {
      prediction <- predict(explainer$model, new_data, type = "prob")[,1]
      bd <- predict_parts(explainer = explainer,
                          new_observation = new_data,
                          keep_distribution = FALSE,
                          predict_function = predict.function,
                          type = "break_down") # interaction sehr langsam
      })
    output$probability <- renderUI(
      paste("Die Wahrscheinlichkeit für eine Sepsis beträgt", 
            round(prediction * 100, 1), 
            "%"))
    output$plot <- renderPlot(plot(bd))
    })
}

shinyApp(ui = ui, server = server)

# große Patient_ID funktionieren nicht
