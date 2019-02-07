library(nnet)
library(car)
library(caret)
library(tidyverse)
library(readxl)
library(magrittr )
library(dplyr)
library(tidyr)
#install.packages("MASS")
library(glm2)
library(glmnet)
library(MASS)
library(nnet)
library(ROCR)
library("corrdat")
library(zoo)      
library(lubridate)
#install.packages("HandTill2001")
library(diagis)
library(mice)
library(HandTill2001)
library(stats4) #Load package stats
library(splines) #Load package splines
#install.packages("VGAM")
library(VGAM)

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("mice", "car", "tidyverse", "readxl", "caret","ROCR","lift","glmnet","MASS","e1071")

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("mice", "car", "tidyverse", "readxl", "caret","ROCR","lift","glmnet","MASS","e1071")

data <- read.csv(file.choose())
head(data)
str(data)

#seconds remaining in each quarter
data$secondsremaining <- ((data$minutes_remaining *60) + data$seconds_remaining)

#2 min remaining in regulation
data$remaining2min <- as.factor(ifelse((data$secondsremaining<121) & data$period ==4 , 1,0))

#buzzer beater
data$buzzer <- as.factor(ifelse((data$secondsremaining<6) , 1,0))

#remove data points with no shotmade or missed
data <- na.omit(data, cols="shot_made_flag")

#home game vs away binary
data$Home <- as.factor(with(data, grepl("vs", matchup)))
data$Home <- as.factor(ifelse((data$Home =="TRUE") , 1,0))

#Overtime binary
data$OT <- as.factor(ifelse(data$period>4, 1,0))

#regulation time binary
data$regulation <- as.factor(ifelse(data$period<=4, 1,0))

#team no longer in the league
data$TeamsGone <-  as.factor( ifelse((data$opponent %in% c("VAN", "SEA", "NJN")) , 1, 0))

#western conference teams
data$WestConf <-  as.factor( ifelse((data$opponent %in% c("VAN", "SEA", "POR", "UTA", "LAC", "HOU" ,"SAS", "DEN" ,"SAC", "GSW", "MIN","DAL" , "PHX", "MEM", "NOP" ,"OKC")) , 1, 0))

#running per game shot percentage
data <- data %>% group_by(game_date) %>% mutate(shotpercentage = running_mean(shot_made_flag))


# jump shot zone average
data$center <-   ifelse((data$shot_zone_area == "Center(C)"), 1, 0)
data$left.side <-   ifelse((data$shot_zone_area == "Left Side(L)") == 1, 1, 0)
data$left.side.center <-   ifelse((data$shot_zone_area == "Left Side Center(LC)") == 1, 1, 0)
data$right.side.center <-   ifelse((data$shot_zone_area == "Right Side Center(RC)") == 1, 1, 0)
data$right.side <-   ifelse((data$shot_zone_area == "Right Side(R)") == 1, 1, 0)

data <- data %>% group_by(game_date) %>% mutate(zone1 = running_mean(center))
data <- data %>% group_by(game_date) %>% mutate(zone2 = running_mean(left.side))
data <- data %>% group_by(game_date) %>% mutate(zone3 = running_mean(left.side.center))
data <- data %>% group_by(game_date) %>% mutate(zone4 = running_mean(right.side.center))
data <- data %>% group_by(game_date) %>% mutate(zone5 = running_mean(right.side))



#change to date class
data$game_date <- as.Date(data$game_date, format("%Y-%m-%d"))

#month column
data$Month <- format(as.Date(data$game_date), "%m")

#special games
data$Begseason <-   as.factor(ifelse((data$Month == 10), 1, 0))

#day of week and weekend binary
data$day <- weekdays(as.Date(data$game_date))
data$weekend <- as.factor(ifelse(data$day == "Friday", 1,
                       ifelse(data$day == "Saturday", 1,
                              ifelse(data$day == "Sunday", 1,
                                     0))))


#running point total
data$point <-   (ifelse((data$shot_type == "2PT Field Goal"), 2, 3))
data <- data %>% group_by(game_date) %>% mutate(pointstotal = cumsum(shot_made_flag *point ))


#2nd game of back to back games
data$day.difference <-   ifelse((data$game_date - lag(data$game_date)) == 1, 1, 0)
datafilter  <-  data %>% filter(day.difference==1)
data$back2back <-   as.factor(ifelse( data$game_date %in% datafilter$game_date *1,1, 0))



#factorize
data$playoffs   <- as.factor(data$playoffs)
data$shot_made_flag   <- as.factor(data$shot_made_flag)
data$period    <- as.factor(data$period)
data$day <- as.factor(data$day)
data$Month <- as.numeric(data$Month)


#not needed
data<-within(data, rm("team_name"))
data<-within(data, rm("team_id"))
data<-within(data, rm("matchup"))
data<-within(data, rm("day.difference"))
data<-within(data, rm("minutes_remaining"))
data<-within(data, rm("seconds_remaining"))
data<-within(data, rm("lat"))
data<-within(data, rm("lon"))
data<-within(data, rm("loc_x"))
data<-within(data, rm("loc_y"))
data<-within(data, rm("game_event_id"))
data<-within(data, rm("game_id"))
data<-within(data, rm("combined_shot_type"))
data<-within(data, rm("center"))
data<-within(data, rm("left.side"))
data<-within(data, rm("left.side.center"))
data<-within(data, rm("right.side.center"))
data<-within(data, rm("right.side"))
data<-within(data, rm("shot_distance"))
data<-within(data, rm("shot_id"))
data<-within(data, rm("point"))
data<-within(data, rm("shot_made_flag"))




#shot action types that arent common have been labelled as other
combinerarecategories<-function(data_frame,mincount){ 
  for (i in 1 : ncol(data_frame)){
    a<-data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
    data_frame[,i]<-a }
  return(data_frame) }

data<-combinerarecategories(data,50)


#write.csv(data,"Predictionresults.csv")


#test and train
set.seed(77850) #set a random number generation seed to ensure that the split is the same everytime
inTrain <- createDataPartition(y = data$back2back,
                               p = 16/100, list = FALSE)
training <- data[ inTrain,]
testing <- data[ -inTrain,]

summary(training)

str(training)    


# First include all variables use glm(Retained.in.2012.~ ., data=training, family="binomial"(link="logit")) Then see which ones have "NA" in coefficients and remove those

model_logistic<-multinom(shot_zone_area~ ., data=training)

summary(model_logistic) 


## Stepwise regressions. 
model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1) #AIC stepwise
summary(model_logistic_stepwiseAIC) 

#finding data leakage
varImp(model_logistic_stepwiseAIC)



par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC) #Error plots: similar nature to lm plots
par(mfrow=c(1,1))

###Finding predicitons: probabilities and classification
logistic_probabilities <- predict(model_logistic_stepwiseAIC,newdata = testing, type = "probs") #Predict probabilities
logistic_classification<-predict(model_logistic_stepwiseAIC,newdata = testing)


str(logistic_probabilities)

###Confusion matrix  
confusionMatrix(logistic_classification,testing$shot_zone_area) #Display confusion matrix

##AUC for multinom
auc(multcap( response = testing$shot_zone_area, predicted = as.matrix(logistic_probabilities)))

str(data)


#Logloss
logLoss = function(pred, actual){
  -1*mean(log(pred[model.matrix(~ actual + 0) - pred > 0]))
}

logLoss(logistic_probabilities, testing$shot_zone_area)


#####Prediction
data$logisticPredict<-predict(model_logistic_stepwiseAIC,newdata=data) #Predict probabilities



#NN
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("nnet","caret", "lift") #Check, and if needed install the necessary packages


##start Neural Network analysis
my.grid <- expand.grid( .size = c(1,2,3,4,5,6,7,8),.decay = c(.25,.5, 1,2)) # Tuning grid for Neural Net

#shot distance has a 100% weight

model_NN <- train(shot_zone_area  ~ shot_type + shot_zone_basic + 
                    shot_zone_range + game_date + secondsremaining + remaining2min + 
                    buzzer + Home + OT + regulation + TeamsGone + WestConf + 
                    shotpercentage + zone1 + zone3 + zone5 + Month + Begseason + 
                    pointstotal + back2back, data = training, method = "nnet", tuneGrid = my.grid, trace = TRUE, na.action =na.omit)



plot(model_NN) #Visualize the relationship between the number of layers, decay and accuracy

NN_prediction<-predict(model_NN, newdata=testing, type = "prob") #Predict classification 

#NN_classification <- as.integer(NN_prediction > mean(testing$default.payment.next.month == "1"))
NN_classification<-predict(model_NN, newdata=testing)
head(NN_classification)

confusionMatrix(NN_classification,testing$shot_zone_area) # 72% accuracy

#finding data leakage
varImp(model_NN)



##AUC for multinom
auc(multcap( response = testing$shot_zone_area, predicted = as.matrix(NN_prediction)))

str(data$shot_zone_area)



#Logloss
logLoss = function(pred, actual){
  -1*mean(log(pred[model.matrix(~ actual + 0) - pred > 0]))
}


logLoss(NN_prediction, testing$shot_zone_area)


#Predit 

newattempt <- read.csv(file.choose())

data <- newattempt
data$game_date <- as.Date(data$game_date, format("%d/%m/%Y"))
data <- na.omit(data, cols="game_date")

View(data)

data$NNpredict<-predict(model_NN,newdata=data) #Predict probabilities

#Seperating the test and train model matrix
training.x <-model.matrix(shot_zone_area~ ., data = training)
testing.x <-model.matrix(shot_zone_area~ ., data = testing)


#XG boost
numberOfClasses <- length(unique(training$shot_zone_area))


model_XGboost<-xgboost(data = data.matrix(training.x), 
                       label = (training$shot_zone_area), 
                       eta = 0.1,
                       max_depth = 5,
                       num_class = numberOfClasses +1,
                       nround=10,
                       eval_metric = "mlogloss",
                       prediction = TRUE,
                       objective = "multi:softprob")

str(model_XGboost)




#prediction
model_XGboost$params$prediction

#Xgboost cross validation
cv_model<-xgb.cv(data = data.matrix(training.x), 
                 label = (training$shot_zone_area), 
                 nfold =  5,
                 num_class = numberOfClasses +1,
                 nround=10,
                 eval_metric =list("mlogloss","rmse","auc"),
                 prediction = TRUE,
                 objective = "multi:softprob")

str(cv_model)


## final predicition
xgboost_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = (training$shot_zone_area))

head(xgboost_prediction)
str(xgboost_prediction)

xgboost_prediction$label
xgboost_prediction$max_prob

#logloss

logLoss = function(pred, actual){
  -1*mean(log(pred[model.matrix(~ actual + 0) - pred > 0]))
}

logLoss(xgboost_prediction,testing$shot_zone_area)


pridicted <- factor(OOF_prediction$max_prob)
real <- factor(OOF_prediction$label)

my_data1 <- data.frame(data = pridicted, type = "prediction")
my_data2 <- data.frame(data = real, type = "real")
my_data3 <- rbind(my_data1,my_data2)

# Check if the levels are identical
identical(levels(my_data3[my_data3$type == "prediction",1]) , levels(my_data3[my_data3$type == "real",1]))
confusionMatrix(my_data3[my_data3$type == "prediction",1], my_data3[my_data3$type == "real",1],  dnn = c("Prediction", "Reference"))


xtab<-table(OOF_prediction$label,OOF_prediction$max_prob)
xtab

table(factor(OOF_prediction$max_prob, levels=min(OOF_prediction$label):max(OOF_prediction$label)), 
      factor(OOF_prediction$label, levels=min(OOF_prediction$label):max(OOF_prediction$label)))


# confusion matrix
confusionMatrix((OOF_prediction$label), 
                (OOF_prediction$max_prob))


# confusion matrix
confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label),
                mode = "everything")



lvs <- OOF_prediction$label
truth <- factor(rep(lvs, times = OOF_prediction$max_prob),
                levels = rev(lvs))
pred <- factor(
  c(
    rep(lvs, times = c(54, 32)),
    rep(lvs, times = c(27, 231))),
  levels = rev(lvs))

xtab <- table(pred, truth)

confusionMatrix(xtab,  mode = "everything")
confusionMatrix(pred, truth)
confusionMatrix(xtab, prevalence = 0.25)



