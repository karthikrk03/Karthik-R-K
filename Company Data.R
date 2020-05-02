install.packages("C50")
install.packages("tree")
install.packages("caret")
install.packages("gmodels")
install.packages("party")
library(party)
library(C50)
library(caret)
library(tree)
library(gmodels)
hist(Company_Data$Sales)
High = ifelse(Company_Data$Sales<10,"No","Yes")
company <- data.frame(Company_Data, High)

company_train <- company[1:200,]
company_test <- company[201:400,]

tree = ctree(High ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                + Age + Education + Urban + US, data = company_train)
summary(tree)
plot(tree)

pred_tree <- as.data.frame(predict(tree,newdata=company_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(tree,newdata=company_test)

mean(pred_test_df==company$High)
CrossTable(company_test$High,pred_test_df)
confusionMatrix(company_test$High,pred_test_df)

company_tree_org <- tree(High~.-Sales,data=company)
summary(company_tree_org)

plot(company_tree_org)
text(company_tree_org,pretty = 0)

company_tree <- tree(High~.-Sales,data=company_train)
summary(company_tree)

plot(company_tree)
text(company_tree,pretty = 0)

pred_tree <- as.data.frame(predict(company_tree,newdata=company_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(company_tree,newdata=company_test)

pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]
pred_tree$final <- as.factor(pred_tree$final)
confusionMatrix(company_test$High,pred_tree$final)

summary(company_test$High)
mean(pred_tree$final==company$High)

confusionMatrix(company_test$High,pred_tree$final)
