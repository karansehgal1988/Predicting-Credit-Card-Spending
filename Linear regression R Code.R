options(scipen = 999)

#importing Data
library(readxl)
spent = read_excel("C:/Users/Karan/Desktop/ML Assignments/Linear Regression Case.xlsx")
View(spent)
str(spent)

#Sum of total cost of two type of cards i.e.primary and secondary card
spent$totalspent = spent$cardspent+spent$card2spent

# dropping of unecessary variables and those variables that have direct correlation with dependent variables
spent[,c("cardspent","card2spent","carditems","card2items")] = list(NULL)

colnames(spent)

spent[,c("lncreddebt","lnothdebt","lnlongmon","lnlongten","lninc","spoused","pets_cats" ,"pets_dogs", "pets_birds" ,      
 "pets_reptiles", "pets_small","pets_saltfish","pets_freshfish")] = list(NULL)


#Selecting the all the numeric and char class variables
spent_num = sapply(spent,is.numeric)    # Numeric Variables 
spent_char = !sapply(spent,is.numeric)  # Char Variables

colnames(spent)

#Using User defined function for getting descriptive statistics about the dataset

mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  pctls <-quantile(a,probs=c(0.01,0.1,0.25,0.5,0.75,0.9,0.99))
  pctl_5 = quantile(a,probs = 0.05) 
  pctl_95 = quantile(a,probs = 0.95) 
  max <- max(a)
  uc = pctl_95
  lc = pctl_5
  flag = max>uc|min<lc
  return(c(n=n, nmiss=nmiss,  mean=m, stdev=s,pctls=pctls,pctl_5=pctl_5, pctl_95=pctl_95,min = min,  max=max,uc = uc,lc=lc,flag = flag))
}

summary(spent)
stats = apply(spent[,spent_num],2,mystats)
stats = t(stats)
stats = as.data.frame(stats)
write.csv(stats,file = "summary after Outliers and missing values treatment.csv")

View(stats)


#Outliers Treatment

outliers = function(x){
  up = quantile(x,probs= 0.95,na.rm = T)
  down = quantile(x,probs=0.05,na.rm = T) 
  x[x<down] = down
  x[x>up] = up
  x
}

#applying the 95% and 5% cap on Outliers
spent[,spent_num] = data.frame(apply(spent[,spent_num],2,outliers)) 



#Missing Values Treatment

#Count of missing values column-wise
stats[stats$nmiss>1,]

#deleting the columns containing missing values more than 25%
spent[,c("lntollmon","lntollten","lnequipmon","lnequipten","lnwiremon","lnwireten","lncardten","lncardmon")] = list(NULL)


#knn impute for the missing values

library(mice)
spent = complete(mice(spent,m=2,method = "pmm"))


#checking catergorical variables significance using Anova method
spent$birthmonth = as.factor(spent$birthmonth)
categ_sig = aov(totalspent~birthmonth,data = spent)
summary(categ_sig)

# Not significant as per the p value
spent[,"birthmonth"] = NULL
# unuseful varaible
spent[,"custid"] = NULL


spent1 = spent
str(spent1)

# Applying Assumptions

# Feature Selection using StepAIC function

#creating model with all variables for variable selection using stepAIC function
model = lm(totalspent~.,data = spent1)
summary(model)
library(MASS)
stepAIC(model,direction = "both")


#Target Variable should be normally distributed - Getting less R Square (So I have not used it)  
hist(spent$totalspent)
spent1$totalspent = log(spent1$totalspent)
hist(spent1$totalspent)


#preparing model with final selected features from stepAIC function


model1 = lm( totalspent ~ region + gender + age + jobcat + retire + 
               income + inccat + debtinc + creddebt + othdebt + hometype + 
               carvalue + carcatvalue + carbought + card + cardtenure + 
               cardtenurecat + card2 + card2fee + cardmon + multline + voice + 
               internet + owndvd + response_03 + commutebus, data = spent1)


summary(model1)
# For checking multicollinearity
library(car)
vif(model1)

caret::varImp(model1)

#New model created by removing highly correlated independent variables 

model2 = lm(formula = totalspent ~ card+card2+income+gender+region+jobcat+retire+creddebt+longmon+internet+
              ownvcr+response_03+cardtenurecat+voice, 
            data = spent1)


summary(model2)
car::vif(model2)


# Converting Categorical Variables to factor class Variables

spent1$region = as.factor(spent1$region)
spent1$gender = as.factor(spent1$gender)
spent1$jobcat = as.factor(spent1$jobcat)
spent1$retire = as.factor(spent1$retire)
spent1$voice = as.factor(spent1$voice)
spent1$card = as.factor(spent1$card)
spent1$cardtenurecat = as.factor(spent1$cardtenurecat)
spent1$card2 = as.factor(spent1$card2)
spent1$internet = as.factor(spent1$internet)
spent1$ownvcr = as.factor(spent1$ownvcr)
spent1$response_03 = as.factor(spent1$response_03)



# Transforming the independent variables to make it more correlated to dependent variables

spent1$longmon  = log(spent1$longmon)
spent1$creddebt = sqrt(spent1$creddebt)

str(spent1)
# modelling done on the transformed independent variables


model3 = lm(formula = totalspent ~ card+card2+income+gender+region+jobcat+retire+creddebt+longmon+internet+
              ownvcr+response_03+cardtenurecat+voice, 
            data = spent1)

summary(model3)

car::vif(model3)  

#Converting Categorical Variables into Dummy Variables using one hot encoding

impvar = caret::dummyVars( ~totalspent +card+card2+income+gender+region+jobcat+retire+creddebt+longmon+internet+
                             ownvcr+response_03+cardtenurecat+voice, 
                           data = spent1,sep = ".")


spent1 = predict(impvar,spent1)  
spent1 = data.frame(spent1)
str(spent1)


model4 = lm(formula = totalspent ~card.1 +card.2+card.3+card2.1+card2.2+card2.3+card2.4+income+gender.0+region.1+region.3
            +jobcat.3+retire.0+creddebt+ownvcr.0+cardtenurecat.2+voice.0 , data = spent1)

summary(model4)

vif(model4)

# Partitioning of data to train and test datasets 

library(caret)
split = createDataPartition(spent1$totalspent,p = 0.70,times = 1,list = FALSE)

traindata = spent1[split,]
testdata = spent1[-split,]



model5 = lm(formula = totalspent ~card.1 +card.2+card.3+card2.1+card2.2+card2.3+card2.4+income+gender.0+region.1+region.3
            +jobcat.3+retire.0+creddebt+ownvcr.0+cardtenurecat.2+voice.0 , data = traindata)

summary(model5)



#Cooks Distance

cooksd <- cooks.distance(model5)
influential <- as.numeric(names(cooksd)[(cooksd > (4/3500))])

# Alternatively, you can try to remove the top x outliers to have a look

traindata <- traindata[-influential, ]


# Final Model

model6 = lm(formula = totalspent ~card.1 +card.2+card.3+card2.1+card2.4+income+gender.0+region.1+retire.0+creddebt+ownvcr.0+cardtenurecat.2+voice.0 , data = traindata)
summary(model6)
car::vif(model6)

variable_importance = caret::varImp(model6)

variable_importance$features = row.names(variable_importance)

variable_importance[order(variable_importance$Overall,decreasing = T),c("features","Overall")]

#Predictions for Training Data

prediction_train = predict(model6,traindata)
prediction_train = exp(prediction_train)
traindata$totalspent =exp(traindata$totalspent)
traindata = cbind(traindata,prediction_train)

# Prediction for Test Dataset

prediction_test = predict(model6,testdata)
prediction_test = exp(prediction_test)
testdata$totalspent =exp(testdata$totalspent)
testdata = cbind(testdata,prediction_test)
testdata

#Decile analysis for Training and Test Data

#for training dataset
delocations_train = quantile(traindata$prediction_train,probs = seq(0.1,0.9,by=0.1))

traindata$decile = findInterval(traindata$prediction_train,c(-Inf,delocations_train,Inf))
colnames(traindata)
library(dplyr)
gr_train =   group_by(traindata,decile)  
summary_train = summarise(gr_train,counts =n(),average_actuals = mean(totalspent),average_predict = mean(prediction_train))

summary_train = as.data.frame(summary_train)
decile_train=summary_train[order(summary_train$decile,decreasing = T),]

write.csv(Decile_test,file = "decile_test.csv")

#for test dataset
delocations_test = quantile(testdata$prediction_test,probs = seq(0.1,0.9,by=0.1))

testdata$decile = findInterval(testdata$prediction_test,c(-Inf,delocations_train,Inf))

library(dplyr)
gr_test =   group_by(testdata,decile)  
summary_test = summarise(gr_test,counts =n(),average_actuals = mean(totalspent),average_predict = mean(prediction_test))

summary_test = as.data.frame(summary_test)
Decile_test = summary_test[order(summary_test$decile,decreasing = T),]


#   ******************* End of Linear Regression Modelling ***********************



#Graph for Training Dataset for Actual vs Predicted data

traindata$totalspent_status = "Actual"
traindata$prediction_status = "Prediction"

graph1 = traindata[,c("totalspent","totalspent_status")]
graph2 = traindata[,c("prediction_train","prediction_status")]

graph2 = rename(graph2, totalspent= prediction_train,totalspent_status=prediction_status)


graph = rbind(graph1,graph2)

min(graph$totalspent) 

graph$intervals = ifelse(graph$totalspent>=0&graph$totalspent<=200,1,ifelse(graph$totalspent>=200.01&graph$totalspent<=400,2,ifelse(graph$totalspent>=400.01&graph$totalspent<=600,3,
                        ifelse(graph$totalspent>=600.01&graph$totalspent<=800,4,ifelse(graph$totalspent>=800.01&graph$totalspent<=1000,5,ifelse(graph$totalspent>=1000.01,6,""))))))


graph$totalspent_status = as.factor(graph$totalspent_status)
graph$intervals = as.numeric(graph$intervals)
str(graph)

library(ggplot2)
ggplot(graph) +
  aes(x = intervals, y = totalspent, colour = totalspent_status) +
  geom_line(size = 1L) +
  scale_color_hue() +
  theme_minimal()



#Graph for Test Dataset for Actual vs Predicted data

testdata$totalspent_status = "Actual"
testdata$prediction_status = "Prediction"

graph3 = testdata[,c("totalspent","totalspent_status")]
graph4 = testdata[,c("prediction_test","prediction_status")]
library(dplyr )
graph4 = rename(graph4, totalspent= prediction_test,totalspent_status=prediction_status)
graph5 = rbind(graph3,graph4)
graph5$intervals = ifelse(graph5$totalspent>=0&graph5$totalspent<=200,1,ifelse(graph5$totalspent>=200.01&graph5$totalspent<=400,2,ifelse(graph5$totalspent>=400.01&graph5$totalspent<=600,3,
                                                                                                                                    ifelse(graph5$totalspent>=600.01&graph5$totalspent<=800,4,ifelse(graph5$totalspent>=800.01&graph5$totalspent<=1000,5,ifelse(graph5$totalspent>=1000.01,6,""))))))
graph5$totalspent_status = as.factor(graph5$totalspent_status)
graph5$intervals = as.numeric(graph5$intervals)
str(graph5)

library(ggplot2)

ggplot(graph5) +
 aes(x = intervals, y = totalspent, colour = totalspent_status) +
 geom_line(size = 1L) +
 scale_color_hue() +
 theme_minimal()
