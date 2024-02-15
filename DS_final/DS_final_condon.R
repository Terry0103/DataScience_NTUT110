#2021.12.30 DS final project: condonusage

#IMPORT AND OVERVIEW of DATA----
raw_data <-  read.csv("./codon_usage.csv", header = T)
names(raw_data)

dim(raw_data)
#[1] 13028    69
str(raw_data)
#feature 1 to 5 are the categorical variables, 6 to 69 are the numerical variable
summary(raw_data)


# Detecting missing value
sum(is.na(raw_data))
# The result shows that there is no missing value in the data


raw_data[,1] <- as.factor(raw_data$Kingdom)
table(raw_data[,1])

unique(raw_data[,1])

barplot(table(raw_data[,1]),
        xlab = 'class', col = c(1:11))

#----------------------------------------DATA pre-processing 


summary(raw_data)
#6th and 7 th feature be seted as char value so we need turn it to numerator
raw_data[,6] <- as.numeric(raw_data[,6])
raw_data[,7] <- as.numeric(raw_data[,7])

# But after we turn 6th & 7th to the numerator. System warning cause some NA value is found
# In the 6th and 7th variable and shows low missing rate.
# So I decide drop NA instead of filling the missing value

#detect missing value
mean(is.na(raw_data[,6]))
mean(is.na(raw_data[,7]))

#drop all NA value
raw_data <- na.omit(raw_data)
dim(raw_data)
#[1] 13026    69,  Just two instances are removed.


#cut the variables which useless (3rd, 4th, 5th)----
raw_data <- raw_data[,-c(3:5)]
#13026    66

#cut low variance independent features---- 
data <- raw_data

#normalize data
for (i in 3:66){
  data[,i] <- raw_data[,i] / mean(raw_data[,i])
}

q <- 0
for (i in 3:66){
  if(var(data[,i] < 0.0005)){
    q <- union(q,i)
  }
}
q <- q[-1]
#outcome seems not work very well 


#create dummy variable for DNAtype feature----
library(fastDummies)

raw_data$DNAtype <- as.factor(raw_data$DNAtype)
str(raw_data$DNAtype)
#the level of DNAtype is 11 (11 categories)
raw_data <- dummy_cols(raw_data, select_columns = 'DNAtype')
raw_data <- raw_data[,-2]
dim(raw_data)
#13026    76  , col: 66 + 11(the num of level of DNAtype ) - 1(DNAtype original)

#Random sampling----
set.seed(109)
n = 1500
sample_data <- NULL

for(i in 1:11){
  x <- NULL
  x <- which(raw_data$Kingdom == Y_level[i])
  if(length(x) >= n){
    y <- sample(x, size = n, replace = F)
    sample_data <- rbind(sample_data, raw_data[y,])
  }
  else{
  y <- sample(x, size = n, replace = T)
  sample_data <- rbind(sample_data, raw_data[y,])
  }
 
}
table(sample_data$Kingdom)

#MODEL FITTING----------------------------------------------------------------------
library(tree)

train <- sample(1:nrow(sample_data), size = nrow(sample_data)*0.8, replace = F)

fit.tree <- tree(Kingdom ~ ., data = sample_data, subset = train)
summary(fit.tree)
#misclassify error rate ~ 0.48
plot(fit.tree, type = 'uniform')
text(fit.tree)

prune.fit <- prune.tree(fit.tree, best = 11)
plot(prune.fit, type = 'uniform')
text(prune.fit)
summary(prune.fit)
#misclassify is 0.5215

yhat <- predict(fit.tree, newdata = sample_data[-train,], type = 'class')
head(yhat)
data.test <- sample_data[-train, 'Kingdom']

test_data <- sample_data[-train,]


confusion_tree <- table(Pred = predict(fit.tree, sample_data[-train,], type = 'class'),
                    True = sample_data[-train,]$Kingdom)

sum(diag(confusion_tree))/sum(confusion_tree)
# fit.tree: error rate over 0.5
# fit.prune: error rate near to  0.5 

#random Forest ----
library(randomForest)
fit.tm <- randomForest(Kingdom ~ ., data = sample_data,
                       sub = train,
                       mtry = 75,
                       ntree = 100,
                       importance = T)
abline(v = 35, lwd = 3, col = 'green')

plot(fit.tm)
names(fit.tm)

sum(fit.tm$confusion[,12])

rm.test <- predict(fit.tm, test_data)

confu_test <- table(Pred = rm.test,
                    True = test_data$Kingdom)

sum(diag(confu_test))/nrow(test_data)
