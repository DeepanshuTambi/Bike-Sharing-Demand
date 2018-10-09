rm(list=ls())
library(rpart)
library(MASS)
library(ggplot2)
library("scales")
library("psych")
library("gplots")
library(corrgram)
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees',"dplyr","plyr","reshape","data.table")
getwd()
#devtools::install_github("hadley/dplyr")
.libPaths()
library(githubinstall)
#githubinstall("dplyr")
#install.packages(x)
#rm(x)


#Extracting Data
marketing_train= read.csv("C:\\Users\\Deepanshu\\Desktop\\Edwisor\\Projects\\Problem\\Data_Project_1.csv", header = T)

df= read.csv("C:\\Users\\Deepanshu\\Desktop\\Edwisor\\Projects\\Problem\\Data_Project_1.csv", header = T)




#Visualizing the Raw Data
install.packages("dplyr")
library(dplyr)


#Histograms
par(mfrow=c(4,2))
par(mar = rep(2, 4))
hist(df$season)
hist(df$weathersit)
hist(df$hum)
hist(df$holiday)
hist(df$workingday)
hist(df$temp)
hist(df$atemp)
hist(df$windspeed)


#Missing Values Analysis
missing_val = data.frame(apply(marketing_train,2,function(x){sum(is.na(x))}))


##Data Manupulation; converting numeric categories into factor numeric
num_names= c('temp','atemp','hum','windspeed','casual','registered','cnt')
cat_names= c('yr','season','mnth','holiday','weekday','workingday','weathersit')

#Converting variables to categoric and numeric
for(i in c(1,2,3,4,5,6,7,8,9)){                  
  marketing_train[,i] = as.factor(marketing_train[,i])                          # Converting multiple variable into data types using loops
}
rm(i)
marketing_train$instant=NULL
marketing_train$dteday=NULL


# ## BoxPlots - Distribution and Outlier Check
numeric_index = sapply(marketing_train,is.numeric) #selecting only numeric
numeric_data = marketing_train[,numeric_index]
cnames = colnames(numeric_data)
rm(num_names)
boxplot(marketing_train$cnt~marketing_train$mnth,xlab="mnth", ylab="count of users")
boxplot(marketing_train$hum~marketing_train$mnth,xlab="mnth", ylab="count of users")
boxplot(marketing_train$windspeed~marketing_train$mnth,xlab="mnth", ylab="count of users")
boxplot(marketing_train$casual~marketing_train$mnth,xlab="mnth", ylab="count of users")



#Bar Plots 
cnt.mean <- t(tapply(marketing_train$cnt, 
                         list(marketing_train$mnth, marketing_train$yr), mean))
 barplot(cnt.mean, col=c("darkblue","red"), beside=TRUE, legend=rownames(cnt.mean))

cnt1.mean <- t(tapply(marketing_train$cnt, 
                      list(marketing_train$weekday, marketing_train$yr), mean))
barplot(cnt1.mean, col=c("darkblue","red"), beside=TRUE, legend=rownames(cnt1.mean))
#install.packages(ggplot2)
#setwd("C:/Users/Deepanshu/Documents/R/win-library")
#getwd()
for (i in 1:length(cnames))
  {
     assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "mnth"), data = subset(marketing_train))+ 
              stat_boxplot(geom = "errorbar", width = 0.5) +
              geom_boxplot(outlier.colour="red", fill = "white" ,outlier.shape=18,
                           outlier.size=1, notch=FALSE) +
              theme(legend.position="bottom")+
              labs(y=cnames[i],x="months"))
   }
   
# Plotting plots together
gridExtra::grid.arrange(gn1,gn5,gn2,gn6,gn7,gn3,ncol=3, nrow=2)


                                    

#loop to remove outliers from all variables
  for(i in cnames){
    print(i)
   val = marketing_train[,i][marketing_train[,i] %in% boxplot.stats(marketing_train[,i])$out]
 print(length(val))
    marketing_train = marketing_train[which(!marketing_train[,i] %in% val),]
  } 
 
 
#Feature Selection####
 

 # Correlation Plot 
 
 corrgram(marketing_train[,numeric_index], order = F,
          upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


  ## Chi-squared Test of Independence

#Convert numeric target variable to categorical through binning
 
 marketing_train$cnt_binned=marketing_train$cnt
 
 marketing_train$cnt_binned <- cut(marketing_train$cnt_binned, 
                                   breaks = c(-Inf, 1000, 2000, 3000, 4000, 5000,6000,7000,8000, Inf), 
                                   labels = c("1", "2", "3", "4", "5", "6","7","8","9"), 
                                   right = FALSE)
 
 factor_index = sapply(marketing_train,is.factor)
 factor_data = marketing_train[,factor_index]
 
  
 for (i in 1:7)
 {
   print(names(factor_data)[i])
   print(chisq.test(table(factor_data$cnt_binned,factor_data[,i])))
 } 
 
 
 if(!require(car)){install.packages("car")}

#ANOVA Analysis
  anova_multi_way <- aov(cnt~(yr)+(holiday)+(workingday)+ (mnth)+(weekday)+ (weathersit)+(season), data = marketing_train)
 summary(anova_multi_way)
 

 ## Dimension Reduction
 marketing_train = subset(marketing_train, 
                          select = -c(casual,registered,atemp))

 

 
 ###Model Development#### 
library(DataCombine)
  rmExcept("marketing_train") 
  original_data= read.csv("C:\\Users\\Deepanshu\\Desktop\\Edwisor\\Projects\\Problem\\Data_Project_1.csv", header = T)
  

  library(caret)
  set.seed(1234)
  train.index = createDataPartition(marketing_train$cnt, p = .80, list = FALSE)
  train = marketing_train[ train.index,]
  test  = marketing_train[-train.index,]
  test_cf=test
  train_cf=train
  train_cf$cnt=NULL
  test_cf$cnt=NULL
  train$cnt_binned=NULL
  test$cnt_binned=NULL
  
  library(C50)  
  
  ##Decision tree for classification
  #Develop Model on training data with binned target variable
  C50_model = C5.0(cnt_binned ~., train_cf, trials = 100, rules = TRUE)
  
  #Summary of DT model
  summary(C50_model)
  
  #Lets predict for test cases
  C50_Predictions = predict(C50_model, test_cf[,-11], type = "class")

    ConfMatrix_C50 = table(test_cf$cnt_binned, C50_Predictions)
  confusionMatrix(ConfMatrix_C50)

    

  #Decision tree regression  
  library(rpart)
  library(MASS)
  library(DMwR)
  d=train[which(train$cnt == 22),]
  
  test[which(test$cnt == 22),]
  train1=train[!d,]
  rm(train1)
  train1 <- train[!(train$cnt==22),]
  fit = rpart(cnt ~ ., data = train1, method = "anova")
  
  
  #Predict for new test cases
  predictions_DT = predict(fit, test[,-11])

  #MAPE
  #calculate MAPE
  MAPE = function(y, yhat){
    mean(abs((y - yhat)/y))
  }
  
  MAPE(test[,11], predictions_DT)
  #0.18
  
   #Using packages
  regr.eval(test[,11], predictions_DT, stats=c('mae','rmse','mape','mse'))

  
  #Linear Regression
  #check multicollearity
  #install.packages(usdm)
  library(usdm)
  
  
  #running regression model
  lm_model = lm(cnt ~., data = train)
  
  #Summary of the model
  summary(lm_model)
  
  #Predict
  predictions_LR = predict(lm_model, test[,1:10])
 
  #Calculate MAPE
  MAPE(test[,11], predictions_LR)
  #0.157  
  
  library(randomForest) 
  ###Random Forest
  RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 500)
  
  library(inTrees)
  #Extract rules fromn random forest
  #transform rf object to an inTrees' format
   treeList = RF2List(RF_model)  
  # 
  # #Extract rules
   exec = extractRules(treeList, train[,-11])  # R-executable conditions
  # 
  # #Visualize some rules
   exec[1:2,]
  # 
  # #Make rules more readable:
   readableRules = presentRules(exec, colnames(train))
   readableRules[1:2,]
  # 
  # #Get rule metrics
   ruleMetric = getRuleMetric(exec, train[,-11], train$cnt)  # get rule metrics
  # 
  # #evaulate few rules
   ruleMetric[1:2,]
  
  #Predict test data using random forest model
  RF_Predictions = predict(RF_model, test[,-11])
  
  plot_pred = as.data.frame(RF_Predictions)
  vec1=c(1:132)
  vec1['index']=as.data.frame(vec1)
  plot_pred['index']=original_data['instant']
  plot_test['index']=original_data['instant']
  
  plot_test = as.data.frame(test[,11])
  
  #Calculate MAPE
  MAPE(test[,11], RF_Predictions)
 #0.132  
  
  #Looing at the scatter plot of test and predicted values of random forest model
  require(ggplot2)
  
  ggplot() +
    geom_point(data=plot_pred, aes(index, RF_Predictions), color="red") +
    geom_point(data=plot_test, aes(index, test[,11]), color='blue')
  
               