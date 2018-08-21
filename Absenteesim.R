#Set wroking Directory
setwd("D:/Edvisor")
getwd()


#Load library
library(readxl)
library(dplyr)
library(ggplot2)
library(DMwR)
library(corrgram)
library(rpart)
library(usdm)
library(MLmetrics)
library(randomForest)

#Load CSV
Absent_data = read_excel("Absenteeism.xls")
Absent_data = as.data.frame(Absent_data)

#check the summary and distribution of the data
summary(Absent_data)
str(Absent_data)

#Rename the variables
names(Absent_data)[2] <- "Reason_for_absence"
names(Absent_data)[3] <- "Month_of_absence"
names(Absent_data)[4] <- "Day_of_the_week"
names(Absent_data)[6] <- "Transportation_expense"
names(Absent_data)[7] <- "Distance_from_Residence_to_Work"
names(Absent_data)[8] <- "Service_time"
names(Absent_data)[10] <- "Work_load_Average_per_day"
names(Absent_data)[11] <- "Hit_target"
names(Absent_data)[12] <- "Disciplinary_failure"
names(Absent_data)[15] <- "Social_drinker"
names(Absent_data)[16] <- "Social_smoker"
names(Absent_data)[20] <- "Body_mass_index"
names(Absent_data)[21] <- "Absenteeism_time_in_hours"


sampledata = Absent_data %>% mutate(Absenteeism_time_in_hours = log(Absenteeism_time_in_hours+1))
#QQ graph for the target variable price
qqnorm(sampledata$Absenteeism_time_in_hours)

#before
ggplot(data = Absent_data,aes(Absent_data$Absenteeism_time_in_hours)) + geom_histogram(bins=30)

#after
ggplot(data = sampledata,aes(sampledata$Absenteeism_time_in_hours)) + geom_histogram(bins=30)

#now we can convert the dependent variable into normal distributed data
Absent_data$Absenteeism_time_in_hours = log(Absent_data$Absenteeism_time_in_hours+1)

#Convert desire variable in factor

Absent_data$Month_of_absence = as.factor(Absent_data$Month_of_absence)
Absent_data$Day_of_the_week = as.factor(Absent_data$Day_of_the_week)
Absent_data$Seasons = as.factor(Absent_data$Seasons)
Absent_data$Disciplinary_failure = as.factor(Absent_data$Disciplinary_failure)
Absent_data$Education = as.factor(Absent_data$Education)
Absent_data$Social_drinker = as.factor(Absent_data$Social_drinker)
Absent_data$Social_smoker = as.factor(Absent_data$Social_smoker)

#Create dummy variable 
#For son
Absent_data$Son_1 <-  as.factor(ifelse((Absent_data$Son <= 1 ),1,0))
Absent_data$Son_2 <-  as.factor(ifelse((Absent_data$Son > 1 & Absent_data$Son <= 2),1,0))
Absent_data$Son_3 <-  as.factor(ifelse((Absent_data$Son >= 3 ),1,0))

#for age
Absent_data$age_1 <-  as.factor(ifelse((Absent_data$Age <= 30 ),1,0))
Absent_data$age_2 <-  as.factor(ifelse((Absent_data$Age > 30 & Absent_data$Age <= 45),1,0))
Absent_data$age_3 <-  as.factor(ifelse((Absent_data$Age > 45 ),1,0))

#for reason for absence
Absent_data$ICD_Disease <-  as.factor(ifelse((Absent_data$Reason_for_absence <= 21 ),1,0))
Absent_data$CID_Disease <-  as.factor(ifelse((Absent_data$Reason_for_absence >= 22),1,0))


#for Reason for absence
Absent_data$No_absent_month <-  factor ( with ( Absent_data, ifelse ( ( Month_of_absence == 0 ), 1 , 0 ) ) )
Absent_data$absent_jan <-  factor ( with ( Absent_data, ifelse ( ( Month_of_absence == 1 ), 1 , 0 ) ) )
Absent_data$absent_feb <-  factor ( with ( Absent_data, ifelse ( ( Month_of_absence == 2 ), 1 , 0 ) ) )
Absent_data$absent_mar <-  factor ( with ( Absent_data, ifelse ( ( Month_of_absence == 3 ), 1 , 0 ) ) )
Absent_data$absent_april <-  factor ( with ( Absent_data, ifelse ( ( Month_of_absence == 4 ), 1 , 0 ) ) )
Absent_data$absent_may <-  factor ( with ( Absent_data, ifelse ( ( Month_of_absence == 5 ), 1 , 0 ) ) )
Absent_data$absent_june <-  factor ( with ( Absent_data, ifelse ( ( Month_of_absence == 6 ), 1 , 0 ) ) )
Absent_data$absent_july <-  factor ( with ( Absent_data, ifelse ( ( Month_of_absence == 7 ), 1 , 0 ) ) )
Absent_data$absent_aug <-  factor ( with ( Absent_data, ifelse ( ( Month_of_absence == 8 ), 1 , 0 ) ) )
Absent_data$absent_sep <-  factor ( with ( Absent_data, ifelse ( ( Month_of_absence == 9 ), 1 , 0 ) ) )
Absent_data$absent_oct <-  factor ( with ( Absent_data, ifelse ( ( Month_of_absence == 10 ), 1 , 0 ) ) )
Absent_data$absent_nov <-  factor ( with ( Absent_data, ifelse ( ( Month_of_absence == 11 ), 1 , 0 ) ) )
Absent_data$absent_dec <-  factor ( with ( Absent_data, ifelse ( ( Month_of_absence == 12 ), 1 , 0 ) ) )

#for Day_of_the_week
Absent_data$mon_absent  <- factor ( with ( Absent_data, ifelse ( ( Day_of_the_week == 2 ), 1 , 0 ) ) )
Absent_data$Tues_absent <- factor ( with ( Absent_data, ifelse ( ( Day_of_the_week == 3 ), 1 , 0 ) ) )
Absent_data$wed_absent  <- factor ( with ( Absent_data, ifelse ( ( Day_of_the_week == 4 ), 1 , 0 ) ) )
Absent_data$thus_absent <- factor ( with ( Absent_data, ifelse ( ( Day_of_the_week == 5 ), 1 , 0 ) ) )
Absent_data$fri_absent  <- factor ( with ( Absent_data, ifelse ( ( Day_of_the_week == 6 ), 1 , 0 ) ) )

#for Seasons
Absent_data$Seasons_summer <- factor ( with ( Absent_data, ifelse ( ( Seasons == 1 ), 1 , 0 ) ) )
Absent_data$Seasons_autumn <- factor ( with ( Absent_data, ifelse ( ( Seasons == 2 ), 1 , 0 ) ) )
Absent_data$Seasons_winter <- factor ( with ( Absent_data, ifelse ( ( Seasons == 3 ), 1 , 0 ) ) )
Absent_data$Seasons_spring <- factor ( with ( Absent_data, ifelse ( ( Seasons == 4 ), 1 , 0 ) ) )

#for Education
Absent_data$Edu_high_school <- factor ( with ( Absent_data, ifelse ( ( Education == 1 ), 1 , 0 ) ) )
Absent_data$Edu_grad        <- factor ( with ( Absent_data, ifelse ( ( Education == 2 ), 1 , 0 ) ) )
Absent_data$Edu_post_grad   <- factor ( with ( Absent_data, ifelse ( ( Education == 3 ), 1 , 0 ) ) )
Absent_data$Edu_master      <- factor ( with ( Absent_data, ifelse ( ( Education == 4 ), 1 , 0 ) ) )

#for pet
Absent_data$No_pet    <- factor ( with ( Absent_data, ifelse ( ( Pet == 0 ), 1 , 0 ) ) )
Absent_data$less_Pets      <-  as.factor(ifelse((Absent_data$Pet >= 1 & Absent_data$Pet <= 2),1,0))
Absent_data$more_Pets      <-  as.factor(ifelse((Absent_data$Pet > 2),1,0))


#Remove the varible for which dummy variables are created
Absent_data <- Absent_data[, setdiff(names(Absent_data), c('Son','Age','Month_of_absence','Seasons',
                                                           'Day_of_the_week','Education','Pet',
                                                           'Reason_for_absence'))]

#Numeric data
Numeric_index = sapply(Absent_data,is.numeric)
Numeric_data = Absent_data[,Numeric_index]
cnames = colnames(Numeric_data)

#categorical data
categorical_data = Absent_data[,!Numeric_index]
cate_name = colnames(categorical_data)


#Plot boxplot for outlier detection

#For Transportation_expense
ggplot(data = Absent_data,aes_string(x= "Absenteeism_time_in_hours",y= cnames[2]))+
  geom_boxplot(outlier.colour = "red",Fill = "Grey", outlier.shape = 18, outlier.size =1) +
  theme(legend.position = "Bottom") + 
  labs(x= "Absenteeism_time_in_hours",y = cnames[2] )+
  ggtitle(paste("BoxPlot for",cnames[2]))

#For Distance_from_Residence_to_Work
ggplot(data = Absent_data,aes_string(x= "Absenteeism_time_in_hours",y= cnames[3]))+
  geom_boxplot(outlier.colour = "red",Fill = "Grey", outlier.shape = 18, outlier.size =1) +
  theme(legend.position = "Bottom") + 
  labs(x= "Absenteeism_time_in_hours",y = cnames[3] )+
  ggtitle(paste("BoxPlot for",cnames[3]))

#For Service_time
ggplot(data = Absent_data,aes_string(x= "Absenteeism_time_in_hours",y= cnames[4]))+
  geom_boxplot(outlier.colour = "red",Fill = "Grey", outlier.shape = 18, outlier.size =1) +
  theme(legend.position = "Bottom") + 
  labs(x= "Absenteeism_time_in_hours",y = cnames[4] )+
  ggtitle(paste("BoxPlot for",cnames[4]))

#For Work_load_Average_per_day
ggplot(data = Absent_data,aes_string(x= "Absenteeism_time_in_hours",y= cnames[5]))+
  geom_boxplot(outlier.colour = "red",Fill = "Grey", outlier.shape = 18, outlier.size =1) +
  theme(legend.position = "Bottom") + 
  labs(x= "Absenteeism_time_in_hours",y = cnames[5] )+
  ggtitle(paste("BoxPlot for",cnames[5]))

#For Hit_target
ggplot(data = Absent_data,aes_string(x= "Absenteeism_time_in_hours",y= cnames[6]))+
  geom_boxplot(outlier.colour = "red",Fill = "Grey", outlier.shape = 18, outlier.size =1) +
  theme(legend.position = "Bottom") + 
  labs(x= "Absenteeism_time_in_hours",y = cnames[6] )+
  ggtitle(paste("BoxPlot for",cnames[6]))

#For Weight
ggplot(data = Absent_data,aes_string(x= "Absenteeism_time_in_hours",y= cnames[7]))+
  geom_boxplot(outlier.colour = "red",Fill = "Grey", outlier.shape = 18, outlier.size =1) +
  theme(legend.position = "Bottom") + 
  labs(x= "Absenteeism_time_in_hours",y = cnames[7] )+
  ggtitle(paste("BoxPlot for",cnames[7]))

#For Height
ggplot(data = Absent_data,aes_string(x= "Absenteeism_time_in_hours",y= cnames[8]))+
  geom_boxplot(outlier.colour = "red",Fill = "Grey", outlier.shape = 18, outlier.size =1) +
  theme(legend.position = "Bottom") + 
  labs(x= "Absenteeism_time_in_hours",y = cnames[8] )+
  ggtitle(paste("BoxPlot for",cnames[8]))

#For Body_mass_index
ggplot(data = Absent_data,aes_string(x= "Absenteeism_time_in_hours",y= cnames[9]))+
  geom_boxplot(outlier.colour = "red",Fill = "Grey", outlier.shape = 18, outlier.size =1) +
  theme(legend.position = "Bottom") + 
  labs(x= "Absenteeism_time_in_hours",y = cnames[9] )+
  ggtitle(paste("BoxPlot for",cnames[9]))



#So we have found out that the outliers are present in the below variables
#Transportation_expense,Service_time,Hight, Work_load_Average_per_day
#now we have to remove the outliers

#dataset got convert into tibble and it will return matrix which boxplot.stats cannot use
#so convert it back to data frame
Absent_data = as.data.frame(Absent_data)
class(Absent_data)

for (i in cnames){
  print(i)
  index = Absent_data[,i][Absent_data[,i] %in% boxplot.stats(Absent_data[,i])$out]
  #Replace the outlier with the NA
  Absent_data[,i][Absent_data[,i] %in% index] = NA
}

#Find out the number of missing valuein each variable
missing_value = data.frame(apply(Absent_data,2,function(x){sum(is.na(x))}))

#Impute the missing value using knn imputation
Absent_data = knnImputation(Absent_data,k = 5)


#Find out the correlation between independent variable and dependent variable
corrgram(Absent_data[,Numeric_index],order = FALSE,upper.panel = panel.pie , text.panel = panel.txt,
         main = "correlation plot") 
#As we have seen from the correlation plot that wieght and body mass index in highing positive correlated
#we have drop any one of the variable.

Absent_data <- Absent_data[, setdiff(names(Absent_data), c('Body_mass_index'))]

#Now as we have seen that there are some continuous varible with much higher scale we need to
#bring in same scale as other
#For that need to so the normalization.

#let me save my dependent variable in different dateframe

dependent = as.data.frame(Absent_data[,c("Absenteeism_time_in_hours")])
colnames(dependent)[1] = "Absenteeism_time_in_hours"

#Remove the dependent variable from the dataset.
Absent_data = Absent_data[,setdiff(names(Absent_data), c('Absenteeism_time_in_hours'))]

#Numeric data with dependent variable
Numeric_index_1 = sapply(Absent_data,is.numeric)
Numeric_data_1 = Absent_data[,Numeric_index_1]
cnames_new = colnames(Numeric_data_1)

#check the normality of continuous variable

qqnorm(Absent_data$Transportation_expense)
hist(Absent_data$Transportation_expense)

qqnorm(Absent_data$Service_time)
hist(Absent_data$Service_time)

qqnorm(Absent_data$Distance_from_Residence_to_Work)
hist(Absent_data$Distance_from_Residence_to_Work)


#As we have from the above qq graph and histogram plot, continuous variable are not normally distributed
#highly skewed.
#now we can perform normallity on continous variables

for (i in cnames_new){
  print(i)
  Absent_data[,i] = (Absent_data[,i] - min(Absent_data[,i]))/
                    (max(Absent_data[,i] - min(Absent_data[,i])))
}

#Join the dependent dataset and absent_data
Absent_data = cbind(data.frame(dependent,Absent_data))

#Divide the data in training and test sample

# 70% of the sample size
smp_size <- floor(0.70 * nrow(Absent_data))

# set the seed
set.seed(123)
Train_Index <- sample(seq_len(nrow(Absent_data)), size = smp_size)

train <- Absent_data[Train_Index, ]
test <- Absent_data[-Train_Index, ]

#As we have done our data pre processing 
#Create Models

#Linear regression model
lm_model = lm(Absenteeism_time_in_hours ~. , data = train)

#final one
lm_model = lm(Absenteeism_time_in_hours ~ ID +Transportation_expense + Disciplinary_failure+
                ICD_Disease + absent_jan + absent_feb + absent_mar+
                absent_april + absent_may + absent_june +
                absent_july + absent_aug + absent_sep +Distance_from_Residence_to_Work+
                absent_oct + absent_nov + absent_dec + Seasons_summer
              , data = train)

summary(lm_model)

#F-statistic shows that there is gud relationship between our predictor and response variable
#it should be more than 1
#total p value of model is less than 0.05 that means its good model

#Make predictions
prediction_lm = predict(lm_model,test[,-1])

MAPE(test[,1],prediction_lm)

#Error rate is 36%
#accuracy is 64%
#Even thought Adj r sq is less which is 40
#it means 40% of the variance can explain the rest of the observation i.e 60% in the test data
# with 64% Accuracy



##########################  CROSS VALIDATION #########################################

# NOw we do the cross validation to check whether our model is overfit or underfit
#first will pridiction the dependent variable using training data set and then calculate the 
#difference between actual and predicted.

#Repeat the same with test data set

#First we will take create model
lm_model_cv = lm(Absenteeism_time_in_hours ~ ID +Transportation_expense + Disciplinary_failure+
                ICD_Disease + absent_jan + absent_feb + absent_mar+
                absent_april + absent_may + absent_june +
                absent_july + absent_aug + absent_sep +Distance_from_Residence_to_Work+
                absent_oct + absent_nov + absent_dec + Seasons_summer + No_absent_month
              , data = train)

#Make predictions with train data
prediction_lm_cv = predict(lm_model_cv,train[,-1])

#Calculate MAPE for train data set
MAPE(train[,1],prediction_lm_cv)
#ERROR : 35%

#Make predictions with test data
prediction_lm_cv_test = predict(lm_model_cv,test[,-1])

#Calculate MAPE for test data set
MAPE(test[,1],prediction_lm_cv_test)
#ERROR : 36%

#From above we have seen that the error for training and testing data set both are high
#this might be the case of underfititng(bias).

#Now we will split the data into 90:10 ratio of train and testing data set


# 90% of the sample size
smp_size1 <- floor(0.90 * nrow(Absent_data))

# set the seed
set.seed(123)
Train_Index1 <- sample(seq_len(nrow(Absent_data)), size = smp_size1)

train_cv <- Absent_data[Train_Index1, ]
test_cv <- Absent_data[-Train_Index1, ]

#Create the model with new training data set
lm_model_cv_train = lm(Absenteeism_time_in_hours ~ ID +Transportation_expense + Disciplinary_failure+
                   ICD_Disease + absent_jan + absent_feb + absent_mar+
                   absent_april + absent_may + absent_june +
                   absent_july + absent_aug + absent_sep +Distance_from_Residence_to_Work+
                   absent_oct + absent_nov + absent_dec + Seasons_summer
                 , data = train_cv)

#Make predictions
prediction_lm_new = predict(lm_model_cv_train,test_cv)

#convert the absent hours inot Original absent hours
hours_absent <- 10^(prediction_lm_new-1)

MAPE(test_cv[,1],prediction_lm_new)

#Now we have seen that the MAPE is reduced
#MAPE ERROR : 28%
#ACCURACY : 72%

#create a new data set
Predicted_absent_hours = data.frame("Predicted_absent_hours" = hours_absent)

#we will create a small data set with predicted hours and actual hours
Predicted_dataset = cbind(data.frame(Predicted_absent_hours,test_cv))

#Plot residual vs Fitted plot
plot(lm_model_cv_train)

######################################RANDOM FOREST####################################

# set the seed
set.seed(123)

rf = randomForest(Absenteeism_time_in_hours ~., data = train)
print(rf)
attributes(rf)

rf_predict_test = predict(rf,test[,-1])
MAPE(test[,1],rf_predict_test)

#Above we have calculated the MAPE with training data set and testing data set 
#with training dataset error rate will be less because data is already seen by the model
#but when we calculated the error with the data which is not seen by the model(Also call OOB sample)
#there is difference between the error and it is more accurate result.

#Now we plot the graph of the error
plot(rf)

#from above plot we have abserved that the as the number of tree grows the oob error intially drop down 
#and then became constant. We are not able to improve the error after 250 trees.so we tune the model

#tune mtry

tune_mtry = tuneRF(train_cv[,-1],train_cv[,1],
                   stepFactor = 0.5,
                   plot = TRUE,
                   ntreeTry = 250,
                   trace = TRUE,
                   improve = 0.5)

#stepFactor = at each step mtry will be inflated of deflated by this value
#plot = whether to plot oob error as a function of mtry
#ntreetry = number of tree that we want to try i.e 300
#improve = the relative improvement in the oob error must be by this much(value) for search to continue

# so we have seen the oob error is relatively very high when mtry is 7 and minimum when mtry is 15
#let create the rf model again with ntree = 300 and mtry = 15

rf_improve = randomForest(Absenteeism_time_in_hours ~., data = train_cv,
                          ntree = 300,
                          mtry = 16,
                          importance = TRUE,
                          proximity = TRUE)
print(rf_improve)

#so we have seen that eariler our oob error for train dataset was 0.3697 and now it reduce 0.364

rf_improve_predict = predict(rf_improve,test_cv[,-1])

#convert the absent hours inot Original absent hours
hours_absent_RF <- 10^(rf_improve_predict-1)


#Calculate Mean absolute percentage error
MAPE(test_cv[,1],rf_improve_predict)


#create a new data set
Predicted_absent_hours_rf = data.frame("Predicted_absent_hours" = hours_absent_RF)

#we will create a small data set with predicted hours and actual hours
Predicted_dataset_rf = cbind(data.frame(Predicted_absent_hours_rf,test_cv))

#now we have see the actual error is reduce to 0.28 
#Accuracy is 72%

#now we will make histogram of number of the nodes for the tree size
hist(treesize(rf_improve), main = "Number of the nodes for tree", col="red")

#so we have seen that some tree which is having 130 node and there are few which are having
# 170 node but majority of the trees has close to 80 nodes

#variable importance 
varImpPlot(rf_improve)
#there will be graph first will tell if we remove one variable what will be the increase in mean square error
#second graph will capture the gini and it tell us that how pure the node is at the end of the tree if we 
#remove significat variable it show how much on an average gini decreases.
varImpPlot(rf_improve,sort = TRUE,n.var = 15, main = "Top-15-important variable")

#Actual mse and gini value of each variable
importance(rf_improve)

#how many time Variable used in the random forest model.
varUsed(rf_improve)

#Not find the table for the first random forest tree
#Note for terminal node we don't have the left and right daughter
getTree(rf_improve,1,labelVar = TRUE)

#