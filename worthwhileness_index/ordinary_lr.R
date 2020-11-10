### REF
# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
# https://stats.idre.ucla.edu/r/faq/ologit-coefficients/
# https://towardsdatascience.com/implementing-and-interpreting-ordinal-logistic-regression-1ee699274cf5
# plot: https://www.r-bloggers.com/how-to-perform-ordinal-logistic-regression-in-r/
# binary models: https://towardsdatascience.com/simple-trick-to-train-an-ordinal-regression-with-any-classifier-6911183d2a3c
# binary models: https://github.com/liorsidi/OrdinalClassifier/blob/master/OrdinalClassifierExplained.pdf

# balance data: https://dzone.com/articles/handle-class-imbalance-data-with-r
# https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/


library(MLmetrics)
library(MASS)
library(dplyr)

set.seed(20191216)

#setwd("~/Projects/MOTIV/data-analysis/SRC/V2_30.10.2019/worthwhilness_correlation/")
setwd("~/Eurecat/Motiv/repo/WI_index")
df = read.csv('values_from_trip_pivot.csv')
df = df[,c('Enjoyment', 'Productivity', 'Fitness', 'wastedTime')]

sapply(df, class)
df$Enjoyment = factor(df$Enjoyment, labels=c("0","1","2"))
df$Fitness = factor(df$Fitness, labels=c("0","1","2"))
df$Productivity = factor(df$Productivity, labels=c("0","1","2"))
df$wastedTime = factor(df$wastedTime, labels=c("1","2","3","4","5"))



# MODEL -------------------------------------------------------------------

model <- polr(wastedTime ~ Enjoyment + Fitness + Productivity, 
              data=df, Hess=T)
summary(model)
# t value: the ratio of the coefficient to its standard error. 
# There is no significance test by default.
# Intercepts or cutpoints. The intercepts indicate where the latent variable
# is cut to make the groups that we observe in our data. 
# Note that this latent variable is continuous. 
# In general, these are not used in the interpretation of the results.
# The cutpoints are closely related to thresholds.

##### Interpretation
# The coefficients from the model can be difficult to interpret because 
# they are scaled in terms of logs. 
# Another way to interpret logistic regression models is to 
# convert the coefficients into odds ratios. 
# To get the OR and confidence intervals,
# we just exponentiate the estimates and confidence intervals.

# ex. given an increment of 1 unit (from 0 to 1) of Enjoyment 
# we expected an increment of 0.47 n the expected value of WT in log odds scale,
# all other conditions beign equal. 

### Significance of coefficients and intercepts 
# we can calculate p-value by comparing 
# t value against the standard normal distribution
summary_table <- coef(summary(model))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

# Since the p-value for all the variables <0.05, 
# hence they are statistically significant at 95% CI



# PREDICTIONS -------------------------------------------------------------------

# ex. If a new user come with E=1, P=1, F=1
# we can predict P(Y<=1) 
# logit(P(Y<=1)) = -2.16 - 0.46*1 - 0.255*1 - 0.51*1 = -3.385
# P(Y<=1) = exp(-3.385)/(1+exp(-3.385)) = 0.032

# logit(P(Y<=3)) = 0.63 - 0.46*1 - 0.255*1 - 0.51*1 =  -0.595
# P(Y<=3) = 0.355
# P(Y = 3) = P(Y<=3) - P(Y<=5) = 0.355 - 0.10

new_user =  data.frame("Enjoyment"= "0","Fitness"="0","Productivity"="0")
pred_vec = round(predict(model, new_user, type = "p"), 3)
# 1     2     3     4     5 
# 0.036 0.069 0.255 0.325 0.316
# 
pred = pred_vec[which.max(pred_vec)] #---> 4


# TRAIN E TEST  -------------------------------------------------------------------

set.seed(100)

th = 0.8
trainingRows <- sample(1:nrow(df), th * nrow(df))
trainingData <- df[trainingRows, ]
testData <- df[-trainingRows, ]

### Build the model on Train
mod2 = polr(wastedTime ~ Enjoyment + Fitness + Productivity, 
            data=trainingData, Hess=T)
summary(mod2)

# predictions on train
pred_train = predict(mod2, trainingData)
# Confusion Matrix on Train
tab_train = table(trainingData$wastedTime, pred_train)
# Missclassification Error
1 - sum(diag(tab_train))/sum(tab_train)

### Predict on Test

# Class predictions
predict_class <- predict(mod2, testData) 
# Probability predictions
predict_score = predict(mod2, testData, type='p')

### Confusion Matrix
tab_test = table(testData$wastedTime, predict_class)

# Misclassification Error
mean(as.character(testData$wastedTime) != as.character(predict_class))  
1-sum(diag(tab_test))/sum(tab_test) # 0.5274251

# MSE and MAE  -------------------------------------------------------------------

# using RMSE and MSE as metrics can actually be a good idea 
# if the classes are ordinal. In this case, there is a natural order between the categories,
# i.e. good > moderate > poor. Because missing by one class is less bad than missing by two or more,
# you want to use a metric that takes this into account.

# MSE
MSE(as.integer(predict_class), as.integer(testData$wastedTime)) # 1.256971
# RMSE = 1.106987
# MAE
MAE(as.integer(predict_class), as.integer(testData$wastedTime)) # 0.7472182


# ALL COMBINATIONS  -------------------------------------------------------------------

# create a dataframe with all the possible combinations of PEF (27) 
# and the related wasted time prediction
x = c(0,1,2)
all_comb = expand.grid(as.factor(x),as.factor(x),as.factor(x))
colnames(all_comb) = c('Enjoyment', 'Fitness', 'Productivity')
all_comb$wt_pred = rep(NA, length(all_comb)) 

for(comb in 1:nrow(all_comb)){
  
  new_user = data.frame('Enjoyment' = as.character(all_comb[comb,1]), 
                        'Fitness' = as.character(all_comb[comb,2]), 
                        'Productivity' = as.character(all_comb[comb, 3]))
  pred_vec = round(predict(model, new_user, type = "p"), 3)
  pred = which.max(pred_vec)
  all_comb$wt1[comb] = pred_vec[1]
  all_comb$wt2[comb] = pred_vec[2]
  all_comb$wt3[comb] = pred_vec[3]
  all_comb$wt4[comb] = pred_vec[4]
  all_comb$wt5[comb] = pred_vec[5]
  all_comb$wt_pred[comb] = pred
}

all_comb = all_comb[order(all_comb$wt_pred),]
write.csv(all_comb, file = "all_combinations_with_probs.csv")


#### Conclusions
# il modello di regressione e' stato allenato sui dati disponibili, 
# e ci fornisce dei coefficienti specifici che,
# moltiplicati per le caratteristiche di un nuovo utente
# forniscono la classe WT con maggiore prob di appartenenza. 
# 
# Il modello funziona con un misclassification error di XXX
#
# il caso peggiore PEF = 0,0,0 corrisponde alla categoria wt=3
# di conseguenza non saranno mai predette le classi 1 e 2

# idea: forse ci sono altre variabili che influenzano il WT
# non sono PEF che contribuiscono a rendere un viaggio sgradevole


# CLEAN DATA  -------------------------------------------------------------------

# remove all data with PEF = 0,0,0 and WT > 2
cleaned_df = df[!(df$Enjoyment==0 & df$Fitness==0 & df$Productivity==0 & (df$wastedTime==3 | df$wastedTime== 4 | df$wastedTime== 5)) ,]

head(cleaned_df)

# prima 
table(df$wastedTime)
# dopo 
table(cleaned_df$wastedTime)

### MODEL 

model_clean <- polr(wastedTime ~ Enjoyment + Fitness + Productivity, 
              data=cleaned_df, Hess=T)
summary(model_clean)

### PREDICTIONS

new_user =  data.frame("Enjoyment"= "0","Fitness"="0","Productivity"="0")
pred_vec = round(predict(model_clean, new_user, type = "p"), 3)
# 1     2     3     4     5 
# 0.174 0.241 0.343 0.168 0.073
# 
pred = pred_vec[which.max(pred_vec)] #---> 3


### ALL COMBINATIONS

# create a dataframe with all the possible combinations of PEF (27) 
# and the related wasted time prediction
x = c(0,1,2)
all_comb = expand.grid(as.factor(x),as.factor(x),as.factor(x))
colnames(all_comb) = c('Enjoyment', 'Fitness', 'Productivity')
all_comb$wt_pred = rep(NA, length(all_comb)) 

for(comb in 1:nrow(all_comb)){
  
  new_user = data.frame('Enjoyment' = as.character(all_comb[comb,1]), 
                        'Fitness' = as.character(all_comb[comb,2]), 
                        'Productivity' = as.character(all_comb[comb, 3]))
  pred_vec = round(predict(model_clean, new_user, type = "p"), 3)
  pred = which.max(pred_vec)
  all_comb$wt1[comb] = pred_vec[1]
  all_comb$wt2[comb] = pred_vec[2]
  all_comb$wt3[comb] = pred_vec[3]
  all_comb$wt4[comb] = pred_vec[4]
  all_comb$wt5[comb] = pred_vec[5]
  all_comb$wt_pred[comb] = pred
}

all_comb = all_comb[order(all_comb$wt_pred),]
write.csv(all_comb, file = "all_combinations_with_probs.csv")


### TRAIN - TEST

set.seed(100)

th = 0.8
trainingRows <- sample(1:nrow(cleaned_df), th * nrow(cleaned_df))
trainingData <- cleaned_df[trainingRows, ]
testData <- cleaned_df[-trainingRows, ]

### Build the model on Train
mod2_cleaned = polr(wastedTime ~ Enjoyment + Fitness + Productivity, 
            data=trainingData, Hess=T)
summary(mod2_cleaned)

# predictions on train
pred_train = predict(mod2_cleaned, trainingData)
# Confusion Matrix on Train
tab_train = table(trainingData$wastedTime, pred_train)
# Missclassification Error
1 - sum(diag(tab_train))/sum(tab_train)

### Predict on Test

# Class predictions
predict_class <- predict(mod2_cleaned, testData) 
# Probability predictions
predict_score = predict(mod2_cleaned, testData, type='p')

### Confusion Matrix
tab_test = table(testData$wastedTime, predict_class)

# Misclassification Error
mean(as.character(testData$wastedTime) != as.character(predict_class))  
1-sum(diag(tab_test))/sum(tab_test) # 0.5274251

# MSE
MSE(as.integer(predict_class), as.integer(testData$wastedTime)) # 1.204069


# BALANCING THE DATA  -------------------------------------------------------------------

#In R, Random Over Sampling Examples (ROSE) and DMwR packages are used to quickly perform sampling strategies.
# The ROSE package is used to generate artificial data based on sampling methods and smoothed bootstrap approach
library(ROSE)

## idea. Balance each class in order to get all classes with 10K obs. 
# classes 1 and 2 will be oversampled
# classes 4,5 will be undersampled.

balanced_df = data.frame(cleaned_df)

# oversampling 
#1. 
df1 = balanced_df[(balanced_df$wastedTime==1) | (balanced_df$wastedTime==3),]
df1_b = ovun.sample(wastedTime ~ ., data = df1, method='over', N=18500)$data
df1_b = df1_b[df1_b$wastedTime==1,]

balanced_df = rbind(balanced_df, df1_b)
table(balanced_df$wastedTime)

#2. 
df2 = balanced_df[(balanced_df$wastedTime==2) | (balanced_df$wastedTime==3),]
df2_b = ovun.sample(wastedTime ~ ., data = df2, method='over', N=16500)$data
df2_b = df2_b[df2_b$wastedTime==2,]

balanced_df = rbind(balanced_df, df2_b)
table(balanced_df$wastedTime)


#undersampling
#4. 
df4 = balanced_df[(balanced_df$wastedTime==4) | (balanced_df$wastedTime==3),]
df4_b = ovun.sample(wastedTime ~ ., data = df4, method='under', N=20700)$data
df4_b = df4_b[df4_b$wastedTime==4,]

balanced_df = balanced_df[balanced_df$wastedTime!= 4,]
balanced_df = rbind(balanced_df, df4_b)
table(balanced_df$wastedTime)

#5. 
df5 = balanced_df[(balanced_df$wastedTime==5) | (balanced_df$wastedTime==3),]
df5_b = ovun.sample(wastedTime ~ ., data = df5, method='under', N=20700)$data
df5_b = df5_b[df5_b$wastedTime==5,]

balanced_df = balanced_df[balanced_df$wastedTime!= 5,]
balanced_df = rbind(balanced_df, df5_b)
table(balanced_df$wastedTime)


### MODEL

model_balanced <- polr(wastedTime ~ Enjoyment + Fitness + Productivity, 
                    data=balanced_df, Hess=T)
summary(model_balanced)

### PREDICTIONS

new_user =  data.frame("Enjoyment"= "0","Fitness"="0","Productivity"="0")
pred_vec = round(predict(model_balanced, new_user, type = "p"), 3)
# 1     2     3     4     5 
# 0.522 0.275 0.130 0.053 0.020 
# 
pred = pred_vec[which.max(pred_vec)] #---> 1


### ALL COMBINATIONS

# create a dataframe with all the possible combinations of PEF (27) 
# and the related wasted time prediction
x = c(0,1,2)
all_comb = expand.grid(as.factor(x),as.factor(x),as.factor(x))
colnames(all_comb) = c('Enjoyment', 'Fitness', 'Productivity')
all_comb$wt_pred = rep(NA, length(all_comb)) 

for(comb in 1:nrow(all_comb)){
  
  new_user = data.frame('Enjoyment' = as.character(all_comb[comb,1]), 
                        'Fitness' = as.character(all_comb[comb,2]), 
                        'Productivity' = as.character(all_comb[comb, 3]))
  pred_vec = round(predict(model_balanced, new_user, type = "p"), 3)
  pred = which.max(pred_vec)
  all_comb$wt1[comb] = pred_vec[1]
  all_comb$wt2[comb] = pred_vec[2]
  all_comb$wt3[comb] = pred_vec[3]
  all_comb$wt4[comb] = pred_vec[4]
  all_comb$wt5[comb] = pred_vec[5]
  all_comb$wt_pred[comb] = pred
}

all_comb = all_comb[order(all_comb$wt_pred),]
write.csv(all_comb, file = "all_combinations_with_probs_balanced.csv")


### TRAIN - TEST

set.seed(100)

th = 0.8
trainingRows <- sample(1:nrow(balanced_df), th * nrow(balanced_df))
trainingData <- balanced_df[trainingRows, ]
testData <- balanced_df[-trainingRows, ]

### Build the model on Train
mod2_balanced = polr(wastedTime ~ Enjoyment + Fitness + Productivity, 
                    data=trainingData, Hess=T)
summary(mod2_balanced)

# predictions on train
pred_train = predict(mod2_balanced, trainingData)
# Confusion Matrix on Train
tab_train = table(trainingData$wastedTime, pred_train)
# Missclassification Error
1 - sum(diag(tab_train))/sum(tab_train)

dim(trainingData[(trainingData$Enjoyment==0) & (trainingData$Fitness==0) &(trainingData$Productivity==0),])

### Predict on Test

# Class predictions
predict_class <- predict(mod2_balanced, testData) 
# Probability predictions
predict_score = predict(mod2_balanced, testData, type='p')

### Confusion Matrix
tab_test = table(testData$wastedTime, predict_class)

round(prop.table(tab_test,2),3)

# Misclassification Error
mean(as.character(testData$wastedTime) != as.character(predict_class))  
1-sum(diag(tab_test))/sum(tab_test) # 0.5274251

# MSE
MSE(as.integer(predict_class), as.integer(testData$wastedTime)) # 1.472166
