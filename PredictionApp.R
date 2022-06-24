library(shiny)
library(shinythemes)
library(dplyr)
library(caret)
library(ranger)
library(gbm)
library(DALEX)

# Voraussetzungen: trainierte Modelle und DALEX-Explainer
#                  aufbereitete Analysedaten (Slopes, imputierte Variante für Rf)
#                  Datensatz mit den vollständigen Beobachtungen der Analysedaten

ui <- fluidPage(
  titlePanel("Sepsis-Prädiktion"),
  sidebarLayout(
    sidebarPanel(
      radioButtons('model', 'Modell wählen:', 
                  c("Random Forest", "Gradient Boosting Machine")),
      selectizeInput('patient', 'Patienten wählen:',
                     options = list(maxOptions = 1500),
                     unique(test3$Patient_ID)),
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
    new_data <- new_dataset %>% 
      filter(Patient_ID == input$patient) %>% 
      select(-Sepsis)
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

# evtl. zweiter Plot auf Anforderung mit einem Prädiktor im Zeitverlauf
# tableOutput('Basisdaten'),
# 
# 
# new_data_all <- data_sample %>% 
#   filter(Patient_ID == input$patient)
# output$Basisdaten <- renderTable({
#   new_data_all %>% 
#     transmute(Patient_ID, Age, Gender, Unit = case_when(!is.na(Unit1) ~ "MICU",
#                                                         !is.na(Unit1) ~ "SICU"),
#               HospAdmTime) %>% 
#     unique()
# })