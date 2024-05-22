rm(list = ls(all = T))
setwd("C:\\Users\\hp\\Desktop\\CEE")
data = read.csv("student.csv")

#EDA
install.packages("summarytools")
library(summarytools)
view(dfSummary(data))

str(data)
data$Target = as.factor(data$Target)
attach(data)

#Count of and graphical representation of categories in Target
table(data$Target)
plot(data$Target)

cor(Previous.qualification..grade.,Course)

#Separating the data into train and test data
set.seed(234)
rows = sample(nrow(data)*0.7, replace = FALSE) 
train = data[rows,]
test = data[-rows,]


train_model = glm(train$Target~.,family = "binomial",data = train)
summary(train_model)
##Significant variables in the model are 
# Course 
# Nationality 
# Mothers qualification
# Mothers occupation
# Displaced
# Debtor
# Tuition.fees.up.to.date
# Gender
# Scholarship.holder
# Age.at.enrollment 
# International
# Curricular.units.1st.sem..approved.
# Curricular.units.2nd.sem..enrolled.
# Curricular.units.2nd.sem..approved
# Curricular.units.2nd.sem..grade.
# Unemployment.rate

#Plot of count of categories of Target in the train dataset
plot(train$Target)

train_pred = predict(train_model,train, type = "response")
train_pred_cat = ifelse(train_pred>= 0.5,1,0)
summary(train_pred_cat)

table(train$Target,train_pred_cat)

install.packages("car")
library(car)
car:: vif(train_model)
#Multicolleniarity exists in the model
#variables that need to be removed (vif > 5)
#  Course 
#  Curricular.units.1st.sem..credited. 
#  Curricular.units.1st.sem..enrolled.
#  Curricular.units.1st.sem..evaluations.  
#  Curricular.units.1st.sem..approved.
#  Curricular.units.1st.sem..grade.
#  Curricular.units.2nd.sem..credited.  
#  Curricular.units.2nd.sem..enrolled.
#  Curricular.units.2nd.sem..evaluations. 
#  Curricular.units.2nd.sem..approved.
#  Curricular.units.2nd.sem..grade.


#Precision, recall and f1 score
train_precision = 1530 / (157+1530)
train_recall = 1530/(55+1530)
train_f1 = 2*train_precision*train_recall/(train_precision+train_recall)

#AUC ROC Curve
install.packages("ROCR")
library(ROCR)
train_pr = prediction(train_pred_cat,train$Target)
train_perf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(train_perf)



#####################
test_model = glm(test$Target~.,family = "binomial",data =test)
summary(test_model)
##Significant variables in the model are 
# Debtor
# Tuition.fees.up.to.date
# Gender
# Scholarship.holder
# Age.at.enrollment 
# Curricular.units.1st.sem..credited.
# Curricular.units.1st.sem..approved.
# Curricular.units.2nd.sem..enrolled.
# Curricular.units.2nd.sem..approved

#Plot of count of categories of Target in the test dataset
plot(test$Target)

test_pred = predict(test_model,test, type = "response")
test_pred_cat = ifelse(test_pred>= 0.5,1,0)
summary(test_pred_cat)

table(test$Target,test_pred_cat)

car:: vif(test_model)
#Multicolleniarity exists in the model
#variables that need to be removed (vif > 5)
#  Curricular.units.1st.sem..credited. 
#  Curricular.units.1st.sem..enrolled.
#  Curricular.units.1st.sem..evaluations.  
#  Curricular.units.1st.sem..approved.
#  Curricular.units.1st.sem..grade.
#  Curricular.units.2nd.sem..credited.  
#  Curricular.units.2nd.sem..enrolled.
#  Curricular.units.2nd.sem..evaluations. 
#  Curricular.units.2nd.sem..approved.
#  Curricular.units.2nd.sem..grade.


#Precision,recall and f1 score
test_precision = 595 / (58+595)
test_recall = 595/(29+595)
test_f1 = 2*train_precision*train_recall/(train_precision+train_recall)

#AUC ROC Curve
test_pr = prediction(test_pred_cat,test$Target)
test_perf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(test_perf)

