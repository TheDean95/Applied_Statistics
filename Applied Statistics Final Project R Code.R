#1

model_1_price_floor <- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ 
                            Applied_Statistics_Project_Condo_Data$FLOOR)
model_1_price_floor

model_1_plot <- plot(Applied_Statistics_Project_Condo_Data$FLOOR,Applied_Statistics_Project_Condo_Data$PRICE100,
                     main = "Model 1 Floor", xlab ="floor", ylab = "dollars")
abline(model_1_price_floor)
model_1_plot

model_2_price_distance <- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ 
                               Applied_Statistics_Project_Condo_Data$DISTANCE)
model_2_price_distance

model_2_plot <- plot(Applied_Statistics_Project_Condo_Data$DISTANCE,Applied_Statistics_Project_Condo_Data$PRICE100,
                     main = "Model 2 Distance", xlab ="distance", ylab = "dollars")
abline(model_2_price_distance)
model_2_plot

model_3_price_view <- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ Applied_Statistics_Project_Condo_Data$VIEW)
model_3_price_view

model_3_plot <- boxplot(Applied_Statistics_Project_Condo_Data$PRICE100 ~ Applied_Statistics_Project_Condo_Data$VIEW, main = "Model 3 View", xlab ="View", ylab = "dollars")
model_3_plot 

model_4_price_end <- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ Applied_Statistics_Project_Condo_Data$END)
model_4_price_end

model_4_plot <- boxplot(Applied_Statistics_Project_Condo_Data$PRICE100 ~ Applied_Statistics_Project_Condo_Data$END, main = "Model 4 End", xlab ="End", ylab = "dollars")
model_4_plot 

model_5_price_furnish<- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ 
                             Applied_Statistics_Project_Condo_Data$FURNISH)
model_5_price_furnish

model_5_plot <- boxplot(Applied_Statistics_Project_Condo_Data$PRICE100 ~ 
                          Applied_Statistics_Project_Condo_Data$FURNISH, main = "Model 5 Furnish", xlab ="Furnish", ylab = "dollars")
model_5_plot 

#2

#full model
Condo_linear_model_5pred<- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ 
                                Applied_Statistics_Project_Condo_Data$FLOOR + 
                                Applied_Statistics_Project_Condo_Data$DISTANCE + 
                                Applied_Statistics_Project_Condo_Data$VIEW + 
                                Applied_Statistics_Project_Condo_Data$END + 
                                Applied_Statistics_Project_Condo_Data$FURNISH)
summary(Condo_linear_model_5pred)
plot(Condo_linear_model_5pred)

#term by term model
Condo_linear_model_5pred_plot <- plot(Applied_Statistics_Project_Condo_Data$PRICE100 ~ 
                                        Applied_Statistics_Project_Condo_Data$FLOOR + 
                                        Applied_Statistics_Project_Condo_Data$DISTANCE + 
                                        Applied_Statistics_Project_Condo_Data$VIEW + 
                                        Applied_Statistics_Project_Condo_Data$END + 
                                        Applied_Statistics_Project_Condo_Data$FURNISH)
abline(Condo_linear_model_5pred)
Condo_linear_model_5pred_plot

#residuals
Condo_linear_5pred_resid <- resid(Condo_linear_model_5pred)
Condo_linear_5pred_resid 
plot(Condo_linear_5pred_resid)

#3 

#Quadratic 1 Floor
Condo_quadratic_model_floor<- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ 
                                   Applied_Statistics_Project_Condo_Data$FLOOR + 
                                   Applied_Statistics_Project_Condo_Data$DISTANCE + 
                                   Applied_Statistics_Project_Condo_Data$VIEW 
                                          + Applied_Statistics_Project_Condo_Data$END + 
                                   Applied_Statistics_Project_Condo_Data$FURNISH + 
                                   Applied_Statistics_Project_Condo_Data$FLOOR.SQ)

summary(Condo_quadratic_model_floor)

#Quadratic 2 Distance
Condo_quadratic_model_distance<- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ 
                                      Applied_Statistics_Project_Condo_Data$FLOOR + 
                                      Applied_Statistics_Project_Condo_Data$DISTANCE + 
                                      Applied_Statistics_Project_Condo_Data$VIEW 
                                          + Applied_Statistics_Project_Condo_Data$END + 
                                      pplied_Statistics_Project_Condo_Data$FURNISH + 
                                      Applied_Statistics_Project_Condo_Data$DISTANCE.SQ)

summary(Condo_quadratic_model_distance)

#Quadratic 3 Floor and Distance 
Condo_quadratic_model_floor_distance<- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ 
                                            Applied_Statistics_Project_Condo_Data$FLOOR + 
                                            Applied_Statistics_Project_Condo_Data$DISTANCE + 
                                            Applied_Statistics_Project_Condo_Data$VIEW 
                              + Applied_Statistics_Project_Condo_Data$END + 
                                Applied_Statistics_Project_Condo_Data$FURNISH +
                                Applied_Statistics_Project_Condo_Data$FLOOR.SQ + 
                                Applied_Statistics_Project_Condo_Data$DISTANCE.SQ)

summary(Condo_quadratic_model_floor_distance)

#4 

Condo_Interaction_model_1_2 <- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ 
                                    Applied_Statistics_Project_Condo_Data$FLOOR + 
                                    Applied_Statistics_Project_Condo_Data$DISTANCE + 
                                    Applied_Statistics_Project_Condo_Data$VIEW 
   + Applied_Statistics_Project_Condo_Data$END + Applied_Statistics_Project_Condo_Data$FURNISH + 
     Applied_Statistics_Project_Condo_Data$FLOOR:Applied_Statistics_Project_Condo_Data$DISTANCE)
summary(Condo_Interaction_model_1_2)

Condo_Interaction_model_1_3 <- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ Applied_Statistics_Project_Condo_Data$FLOOR + Applied_Statistics_Project_Condo_Data$DISTANCE + Applied_Statistics_Project_Condo_Data$VIEW 
                                  + Applied_Statistics_Project_Condo_Data$END + Applied_Statistics_Project_Condo_Data$FURNISH + Applied_Statistics_Project_Condo_Data$FLOOR:Applied_Statistics_Project_Condo_Data$VIEW)
summary(Condo_Interaction_model_1_3)

Condo_Interaction_model_1_4 <- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ Applied_Statistics_Project_Condo_Data$FLOOR + Applied_Statistics_Project_Condo_Data$DISTANCE + Applied_Statistics_Project_Condo_Data$VIEW 
                                  + Applied_Statistics_Project_Condo_Data$END + Applied_Statistics_Project_Condo_Data$FURNISH + Applied_Statistics_Project_Condo_Data$FLOOR:Applied_Statistics_Project_Condo_Data$END)
summary(Condo_Interaction_model_1_4)

Condo_Interaction_model_1_5 <- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ Applied_Statistics_Project_Condo_Data$FLOOR + Applied_Statistics_Project_Condo_Data$DISTANCE + Applied_Statistics_Project_Condo_Data$VIEW 
                                  + Applied_Statistics_Project_Condo_Data$END + Applied_Statistics_Project_Condo_Data$FURNISH + Applied_Statistics_Project_Condo_Data$FLOOR:Applied_Statistics_Project_Condo_Data$FURNISH)
summary(Condo_Interaction_model_1_5)

Condo_Interaction_model_2_3 <- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ Applied_Statistics_Project_Condo_Data$FLOOR + Applied_Statistics_Project_Condo_Data$DISTANCE + Applied_Statistics_Project_Condo_Data$VIEW 
                                  + Applied_Statistics_Project_Condo_Data$END + Applied_Statistics_Project_Condo_Data$FURNISH + Applied_Statistics_Project_Condo_Data$DISTANCE:Applied_Statistics_Project_Condo_Data$VIEW)
summary(Condo_Interaction_model_2_3)

Condo_Interaction_model_2_4 <- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ Applied_Statistics_Project_Condo_Data$FLOOR + Applied_Statistics_Project_Condo_Data$DISTANCE + Applied_Statistics_Project_Condo_Data$VIEW 
                                  + Applied_Statistics_Project_Condo_Data$END + Applied_Statistics_Project_Condo_Data$FURNISH + Applied_Statistics_Project_Condo_Data$DISTANCE:Applied_Statistics_Project_Condo_Data$END)
summary(Condo_Interaction_model_2_4)

Condo_Interaction_model_2_5 <- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ Applied_Statistics_Project_Condo_Data$FLOOR + Applied_Statistics_Project_Condo_Data$DISTANCE + Applied_Statistics_Project_Condo_Data$VIEW 
                                  + Applied_Statistics_Project_Condo_Data$END + Applied_Statistics_Project_Condo_Data$FURNISH + Applied_Statistics_Project_Condo_Data$DISTANCE:Applied_Statistics_Project_Condo_Data$FURNISH)
summary(Condo_Interaction_model_2_5)

Condo_Interaction_model_3_4 <- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ Applied_Statistics_Project_Condo_Data$FLOOR + Applied_Statistics_Project_Condo_Data$DISTANCE + Applied_Statistics_Project_Condo_Data$VIEW 
                                  + Applied_Statistics_Project_Condo_Data$END + Applied_Statistics_Project_Condo_Data$FURNISH + Applied_Statistics_Project_Condo_Data$VIEW:Applied_Statistics_Project_Condo_Data$END)
summary(Condo_Interaction_model_3_4)

Condo_Interaction_model_3_5 <- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ Applied_Statistics_Project_Condo_Data$FLOOR + Applied_Statistics_Project_Condo_Data$DISTANCE + Applied_Statistics_Project_Condo_Data$VIEW 
                                  + Applied_Statistics_Project_Condo_Data$END + Applied_Statistics_Project_Condo_Data$FURNISH + Applied_Statistics_Project_Condo_Data$VIEW:Applied_Statistics_Project_Condo_Data$FURNISH)
summary(Condo_Interaction_model_3_5)

Condo_Interaction_model_4_5 <- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ Applied_Statistics_Project_Condo_Data$FLOOR + Applied_Statistics_Project_Condo_Data$DISTANCE + Applied_Statistics_Project_Condo_Data$VIEW 
                                  + Applied_Statistics_Project_Condo_Data$END + Applied_Statistics_Project_Condo_Data$FURNISH + Applied_Statistics_Project_Condo_Data$END:Applied_Statistics_Project_Condo_Data$FURNISH)
summary(Condo_Interaction_model_4_5)

#4 - Final Model

Condo_Final_Model <- lm(Applied_Statistics_Project_Condo_Data$PRICE100 ~ 
                          Applied_Statistics_Project_Condo_Data$FLOOR + 
                          Applied_Statistics_Project_Condo_Data$DISTANCE + 
                          Applied_Statistics_Project_Condo_Data$VIEW + Applied_Statistics_Project_Condo_Data$END 
                        + Applied_Statistics_Project_Condo_Data$FURNISH + 
                          Applied_Statistics_Project_Condo_Data$FLOOR.SQ + 
                          Applied_Statistics_Project_Condo_Data$FLOOR:Applied_Statistics_Project_Condo_Data$VIEW 
                       + Applied_Statistics_Project_Condo_Data$VIEW:Applied_Statistics_Project_Condo_Data$FURNISH)

summary(Condo_Final_Model)

Condo_Final_Model_resid <- resid(Condo_Final_Model)
Condo_Final_Model_resid
plot(Condo_Final_Model_resid, main = "Final Model Residual Plot", ylab = "Standardized Value")



