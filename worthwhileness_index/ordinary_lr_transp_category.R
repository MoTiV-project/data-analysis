library(MLmetrics)
library(MASS)
library(dplyr)
library(ROSE)

set.seed(20191216)

# WALKING -------------------------------------------------------------------

setwd("~/Eurecat/Motiv/repo/WI_index")
df = read.csv('../../2019-12-16.out/WI_results/OLR_results/walking.csv')
df = df[,c('Enjoyment', 'Productivity', 'Fitness', 'wastedTime')]

sapply(df, class)
df$Enjoyment = factor(df$Enjoyment, labels=c("0","1","2"))
df$Fitness = factor(df$Fitness, labels=c("0","1","2"))
df$Productivity = factor(df$Productivity, labels=c("0","1","2"))
df$wastedTime = factor(df$wastedTime, labels=c("1","2","3","4","5"))

table(df$wastedTime)

balanced_df = data.frame(df)

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
write.csv(all_comb, file = "all_combinations_with_probs_walking.csv")

df = read.csv('../../2019-12-16.out/WI_results/OLR_results/walking.csv')
df = df[,c('Enjoyment', 'Productivity', 'Fitness', 'wastedTime')]

sapply(df, class)
df$Enjoyment = factor(df$Enjoyment, labels=c("0","1","2"))
df$Fitness = factor(df$Fitness, labels=c("0","1","2"))
df$Productivity = factor(df$Productivity, labels=c("0","1","2"))
df$wastedTime = factor(df$wastedTime, labels=c("1","2","3","4","5"))

table(df$wastedTime)

balanced_df = data.frame(df)

# oversampling 
#1. 
df1 = balanced_df[(balanced_df$wastedTime==1) | (balanced_df$wastedTime==3),]
df1_b = ovun.sample(wastedTime ~ ., data = df1, method='over', N=6500)$data
df1_b = df1_b[df1_b$wastedTime==1,]

balanced_df = rbind(balanced_df, df1_b)
table(balanced_df$wastedTime)

#2. 
df2 = balanced_df[(balanced_df$wastedTime==2) | (balanced_df$wastedTime==3),]
df2_b = ovun.sample(wastedTime ~ ., data = df2, method='over', N=6000)$data
df2_b = df2_b[df2_b$wastedTime==2,]

balanced_df = rbind(balanced_df, df2_b)
table(balanced_df$wastedTime)


#undersampling
#4. 
df4 = balanced_df[(balanced_df$wastedTime==4) | (balanced_df$wastedTime==3),]
df4_b = ovun.sample(wastedTime ~ ., data = df4, method='under', N=7000)$data
df4_b = df4_b[df4_b$wastedTime==4,]

balanced_df = balanced_df[balanced_df$wastedTime!= 4,]
balanced_df = rbind(balanced_df, df4_b)
table(balanced_df$wastedTime)

#5. 
df5 = balanced_df[(balanced_df$wastedTime==5) | (balanced_df$wastedTime==3),]
df5_b = ovun.sample(wastedTime ~ ., data = df5, method='under', N=7000)$data
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
write.csv(all_comb, file = "all_combinations_with_probs_balanced_walking.csv")

# CYCLING -------------------------------------------------------------------

# setwd("~/Desktop")
setwd("~/Eurecat/Motiv/repo/WI_index")
df = read.csv('../../2019-12-16.out/WI_results/OLR_results/cycling_emerging_micromobility.csv')
df = df[,c('Enjoyment', 'Productivity', 'Fitness', 'wastedTime')]

sapply(df, class)
df$Enjoyment = factor(df$Enjoyment, labels=c("0","1","2"))
df$Fitness = factor(df$Fitness, labels=c("0","1","2"))
df$Productivity = factor(df$Productivity, labels=c("0","1","2"))
df$wastedTime = factor(df$wastedTime, labels=c("1","2","3","4","5"))

table(df$wastedTime)

balanced_df = data.frame(df)

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
write.csv(all_comb, file = "all_combinations_with_probs_cycling.csv")

df = read.csv('../../2019-12-16.out/WI_results/OLR_results/cycling_emerging_micromobility.csv')
df = df[,c('Enjoyment', 'Productivity', 'Fitness', 'wastedTime')]

sapply(df, class)
df$Enjoyment = factor(df$Enjoyment, labels=c("0","1","2"))
df$Fitness = factor(df$Fitness, labels=c("0","1","2"))
df$Productivity = factor(df$Productivity, labels=c("0","1","2"))
df$wastedTime = factor(df$wastedTime, labels=c("1","2","3","4","5"))

table(df$wastedTime)

balanced_df = data.frame(df)

# oversampling 
#1. 
df1 = balanced_df[(balanced_df$wastedTime==1) | (balanced_df$wastedTime==3),]
df1_b = ovun.sample(wastedTime ~ ., data = df1, method='over', N=4100)$data
df1_b = df1_b[df1_b$wastedTime==1,]

balanced_df = rbind(balanced_df, df1_b)
table(balanced_df$wastedTime)

#2. 
df2 = balanced_df[(balanced_df$wastedTime==2) | (balanced_df$wastedTime==3),]
df2_b = ovun.sample(wastedTime ~ ., data = df2, method='over', N=3900)$data
df2_b = df2_b[df2_b$wastedTime==2,]

balanced_df = rbind(balanced_df, df2_b)
table(balanced_df$wastedTime)


#undersampling
#4. 
df4 = balanced_df[(balanced_df$wastedTime==4) | (balanced_df$wastedTime==3),]
df4_b = ovun.sample(wastedTime ~ ., data = df4, method='under', N=4300)$data
df4_b = df4_b[df4_b$wastedTime==4,]

balanced_df = balanced_df[balanced_df$wastedTime!= 4,]
balanced_df = rbind(balanced_df, df4_b)
table(balanced_df$wastedTime)

#5. 
df5 = balanced_df[(balanced_df$wastedTime==5) | (balanced_df$wastedTime==3),]
df5_b = ovun.sample(wastedTime ~ ., data = df5, method='under', N=4300)$data
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
write.csv(all_comb, file = "all_combinations_with_probs_balanced_cycling.csv")


# PRIVATE MOTORIZED  -------------------------------------------------------------------

# setwd("~/Desktop")
setwd("~/Eurecat/Motiv/repo/WI_index")
df = read.csv('../../2019-12-16.out/WI_results/OLR_results/private_motorized.csv')
df = df[,c('Enjoyment', 'Productivity', 'Fitness', 'wastedTime')]

sapply(df, class)
df$Enjoyment = factor(df$Enjoyment, labels=c("0","1","2"))
df$Fitness = factor(df$Fitness, labels=c("0","1","2"))
df$Productivity = factor(df$Productivity, labels=c("0","1","2"))
df$wastedTime = factor(df$wastedTime, labels=c("1","2","3","4","5"))

table(df$wastedTime)

balanced_df = data.frame(df)

# oversampling 
#1. 
df1 = balanced_df[(balanced_df$wastedTime==1) | (balanced_df$wastedTime==3),]
df1_b = ovun.sample(wastedTime ~ ., data = df1, method='over', N=5100)$data
df1_b = df1_b[df1_b$wastedTime==1,]

balanced_df = rbind(balanced_df, df1_b)
table(balanced_df$wastedTime)

#2. 
df2 = balanced_df[(balanced_df$wastedTime==2) | (balanced_df$wastedTime==3),]
df2_b = ovun.sample(wastedTime ~ ., data = df2, method='over', N=4900)$data
df2_b = df2_b[df2_b$wastedTime==2,]

balanced_df = rbind(balanced_df, df2_b)
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
write.csv(all_comb, file = "all_combinations_with_probs_private_motorized.csv")

df = read.csv('../../2019-12-16.out/WI_results/OLR_results/private_motorized.csv')
df = df[,c('Enjoyment', 'Productivity', 'Fitness', 'wastedTime')]

sapply(df, class)
df$Enjoyment = factor(df$Enjoyment, labels=c("0","1","2"))
df$Fitness = factor(df$Fitness, labels=c("0","1","2"))
df$Productivity = factor(df$Productivity, labels=c("0","1","2"))
df$wastedTime = factor(df$wastedTime, labels=c("1","2","3","4","5"))

table(df$wastedTime)

balanced_df = data.frame(df)

#undersampling
#4. 
df4 = balanced_df[(balanced_df$wastedTime==4) | (balanced_df$wastedTime==3),]
df4_b = ovun.sample(wastedTime ~ ., data = df4, method='under', N=6200)$data
df4_b = df4_b[df4_b$wastedTime==4,]

balanced_df = balanced_df[balanced_df$wastedTime!= 4,]
balanced_df = rbind(balanced_df, df4_b)
table(balanced_df$wastedTime)

#5. 
df5 = balanced_df[(balanced_df$wastedTime==5) | (balanced_df$wastedTime==3),]
df5_b = ovun.sample(wastedTime ~ ., data = df5, method='under', N=6200)$data
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
write.csv(all_comb, file = "all_combinations_with_probs_balanced_private_motorized.csv")


# PUBLIC LONG  -------------------------------------------------------------------

# setwd("~/Desktop")
setwd("~/Eurecat/Motiv/repo/WI_index")
df = read.csv('../../2019-12-16.out/WI_results/OLR_results/public_transp_long_dist.csv')
df = df[,c('Enjoyment', 'Productivity', 'Fitness', 'wastedTime')]

sapply(df, class)
df$Enjoyment = factor(df$Enjoyment, labels=c("0","1","2"))
df$Fitness = factor(df$Fitness, labels=c("0","1","2"))
df$Productivity = factor(df$Productivity, labels=c("0","1","2"))
df$wastedTime = factor(df$wastedTime, labels=c("1","2","3","4","5"))

table(df$wastedTime)

balanced_df = data.frame(df)

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
write.csv(all_comb, file = "all_combinations_with_probs_publ_long.csv")

df = read.csv('../../2019-12-16.out/WI_results/OLR_results/public_transp_long_dist.csv')
df = df[,c('Enjoyment', 'Productivity', 'Fitness', 'wastedTime')]

sapply(df, class)
df$Enjoyment = factor(df$Enjoyment, labels=c("0","1","2"))
df$Fitness = factor(df$Fitness, labels=c("0","1","2"))
df$Productivity = factor(df$Productivity, labels=c("0","1","2"))
df$wastedTime = factor(df$wastedTime, labels=c("1","2","3","4","5"))

table(df$wastedTime)

balanced_df = data.frame(df)

# oversampling 
#1. 
df1 = balanced_df[(balanced_df$wastedTime==1) | (balanced_df$wastedTime==3),]
df1_b = ovun.sample(wastedTime ~ ., data = df1, method='over', N=160)$data
df1_b = df1_b[df1_b$wastedTime==1,]

balanced_df = rbind(balanced_df, df1_b)
table(balanced_df$wastedTime)

#2. 
df2 = balanced_df[(balanced_df$wastedTime==2) | (balanced_df$wastedTime==3),]
df2_b = ovun.sample(wastedTime ~ ., data = df2, method='over', N=180)$data
df2_b = df2_b[df2_b$wastedTime==2,]

balanced_df = rbind(balanced_df, df2_b)
table(balanced_df$wastedTime)


#undersampling
#4. 
df4 = balanced_df[(balanced_df$wastedTime==4) | (balanced_df$wastedTime==3),]
df4_b = ovun.sample(wastedTime ~ ., data = df4, method='under', N=210)$data
df4_b = df4_b[df4_b$wastedTime==4,]

balanced_df = balanced_df[balanced_df$wastedTime!= 4,]
balanced_df = rbind(balanced_df, df4_b)
table(balanced_df$wastedTime)

#5. 
df5 = balanced_df[(balanced_df$wastedTime==5) | (balanced_df$wastedTime==3),]
df5_b = ovun.sample(wastedTime ~ ., data = df5, method='under', N=210)$data
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
write.csv(all_comb, file = "all_combinations_with_probs_balanced_publ_long.csv")



# PUBLIC SHORT  -------------------------------------------------------------------

# setwd("~/Desktop")
setwd("~/Eurecat/Motiv/repo/WI_index")
df = read.csv('../../2019-12-16.out/WI_results/OLR_results/public_transp_short_dist.csv')
df = df[,c('Enjoyment', 'Productivity', 'Fitness', 'wastedTime')]

sapply(df, class)
df$Enjoyment = factor(df$Enjoyment, labels=c("0","1","2"))
df$Fitness = factor(df$Fitness, labels=c("0","1","2"))
df$Productivity = factor(df$Productivity, labels=c("0","1","2"))
df$wastedTime = factor(df$wastedTime, labels=c("1","2","3","4","5"))

table(df$wastedTime)

balanced_df = data.frame(df)

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
write.csv(all_comb, file = "all_combinations_with_probs_publ_short.csv")

df = read.csv('../../2019-12-16.out/WI_results/OLR_results/public_transp_short_dist.csv')
df = df[,c('Enjoyment', 'Productivity', 'Fitness', 'wastedTime')]

sapply(df, class)
df$Enjoyment = factor(df$Enjoyment, labels=c("0","1","2"))
df$Fitness = factor(df$Fitness, labels=c("0","1","2"))
df$Productivity = factor(df$Productivity, labels=c("0","1","2"))
df$wastedTime = factor(df$wastedTime, labels=c("1","2","3","4","5"))

table(df$wastedTime)

balanced_df = data.frame(df)

# oversampling 
#1. 
df1 = balanced_df[(balanced_df$wastedTime==1) | (balanced_df$wastedTime==3),]
df1_b = ovun.sample(wastedTime ~ ., data = df1, method='over', N=2300)$data
df1_b = df1_b[df1_b$wastedTime==1,]

balanced_df = rbind(balanced_df, df1_b)
table(balanced_df$wastedTime)

#2. 
df2 = balanced_df[(balanced_df$wastedTime==2) | (balanced_df$wastedTime==3),]
df2_b = ovun.sample(wastedTime ~ ., data = df2, method='over', N=2100)$data
df2_b = df2_b[df2_b$wastedTime==2,]

balanced_df = rbind(balanced_df, df2_b)
table(balanced_df$wastedTime)


#undersampling
#4. 
df4 = balanced_df[(balanced_df$wastedTime==4) | (balanced_df$wastedTime==3),]
df4_b = ovun.sample(wastedTime ~ ., data = df4, method='under', N=2700)$data
df4_b = df4_b[df4_b$wastedTime==4,]

balanced_df = balanced_df[balanced_df$wastedTime!= 4,]
balanced_df = rbind(balanced_df, df4_b)
table(balanced_df$wastedTime)

#5. 
df5 = balanced_df[(balanced_df$wastedTime==5) | (balanced_df$wastedTime==3),]
df5_b = ovun.sample(wastedTime ~ ., data = df5, method='under', N=2700)$data
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
write.csv(all_comb, file = "all_combinations_with_probs_balanced_publ_short.csv")