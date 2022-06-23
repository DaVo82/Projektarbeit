library(dplyr)
library(DALEX)

predict.function <- function(model, new_observation) {
  predict(model, new_observation, type = "prob")[,1] # positiv = Event
}

##############################################################################

explain_gbm_ds3 <- DALEX::explain(gbm_ds3,
                           data = train3[,3:40],
                           y = case_when(train3$Sepsis == "positiv" ~ 1, 
                                         TRUE ~ 0),
                           label = "GBM")

bd_gbm3 <- predict_parts(explainer = explain_gbm_ds3,
                           new_observation = test3[21, -2],
                           keep_distribution = FALSE,
                           predict_function = predict.function,
                           type = "break_down_interactions")

plot(bd_gbm3)

#############################################################################

explain_rf_ds3 <- DALEX::explain(rf_ds3,
                           data = train3_imputed[,3:40],
                           y = case_when(train3_imputed$Sepsis == "positiv" ~ 1, 
                                         TRUE ~ 0),
                           label = "Random Forest")

bd_rf3 <- predict_parts(explainer = explain_rf_ds3,
                        new_observation = test3_imputed[1, -2],
                        keep_distribution = FALSE,
                        predict_function = predict.function,
                        type = "break_down_interactions")

plot(bd_rf3)

#############################################################################

explain_rf_ds2 <- DALEX::explain(rf_ds2,
                          data = train2_imputed[,4:41],
                          y = case_when(train2_imputed$Sepsis  == "positiv" ~ 1, 
                                        TRUE ~ 0),
                          label = "Random Forest")

bd_rf2 <- predict_parts(explainer = explain_rf_ds2,
                        new_observation = test2_imputed[1, -2],
                        keep_distribution = FALSE,
                        predict_function = predict.function,
                        type = "break_down_interactions")

plot(bd_rf2)
