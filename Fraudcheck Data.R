library(C50)
library(tree)
library(caret)
library(party)
library(gmodels)

hist(Fraud_check$Taxable.Income)
risk_good <- ifelse(Fraud_check$Taxable.Income<= 30000, "Risky", "Good")
fraud <-  data.frame(Fraud_check, risk_good)

fraud_train <- fraud[1:300,]
fraud_test <- fraud[301:600,]
View(fraud_train)
View(fraud_test)

fraud_tree <- ctree(risk_good ~ Undergrad + Marital.Status + City.Population + 
                Work.Experience + Urban, data = fraud)
summary(fraud_tree)
plot(fraud_tree)

fraud_tree1 <- ctree(risk_good ~ Undergrad + Marital.Status + City.Population + 
                       Work.Experience + Urban, data = fraud_train)
summary(fraud_tree1)
plot(fraud_tree1)

pred_tree <- as.data.frame(predict(fraud_tree,newdata=fraud_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(fraud_tree,newdata=fraud_test)
mean(pred_test_df==fraud_test$risk_good)

CrossTable(fraud_test$risk_good,pred_test_df)
confusionMatrix(fraud_test$risk_good,pred_test_df)
