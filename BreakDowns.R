library(iBreakDown)
library(DALEX)

predict.function <- function(model, new_observation) {
  predict(model, new_observation, type = "prob")[,1] # positiv = Event
}

##############################################################################

explain_gbm_ds3 <- explain(gbm_ds3,
                           data = train3[,3:40],
                           y = train3$Sepsis == "negativ",
                           label = "GBM")

bd_gbm3 <- break_down(explain_gbm_ds3,
           test3[21, -2],
           keep_distributions = TRUE,
           predict_function = predict.function) 

plot(bd_gbm3)

#############################################################################

explain_rf_ds3 <- explain(rf_ds3,
                           data = train3_imputed[,3:40],
                           y = train3_imputed$Sepsis == "negativ",
                           label = "Random Forest")

bd_rf3 <- break_down(explain_rf_ds3,
                      test3_imputed[1, -2],
                      keep_distributions = TRUE,
                      predict_function = predict.function) 

plot(bd_rf3)

#############################################################################

explain_rf_ds2 <- explain(rf_ds2,
                          data = train2_imputed[,4:41],
                          y = train2_imputed$Sepsis == "negativ",
                          label = "Random Forest")

bd_rf2 <- break_down(explain_rf_ds2,
                     test2_imputed[1, -2],
                     keep_distributions = TRUE,
                     predict_function = predict.function) 

plot(bd_rf2)