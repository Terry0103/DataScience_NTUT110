#IMPORT DATA ----
#LAPTOP URL
raw_data <- read.csv("./fgd_data/train.csv", header = T)
dim(raw_data)
str(raw_data)

#remove id and Time and label variables
raw_data <- raw_data[,-c(1:2)]
str(raw_data)

#DATA IMBLANCE ----
#the histgram shows that there is imbalance in the this data
table(raw_data$Y)
hist(raw_data[,25], xlab = 'Cluster', col = raw_data$Y)
abline(h = 400, col = 'blue')




#CUT LOW VARIANCE FEATURES ----
nor_data <- raw_data
#normalize data
for (i in 1:24){
  nor_data[,i] <- raw_data[,i] / mean(raw_data[,i])
}

q <- 0
for (i in 1:24){
  if(var(nor_data[,i] < 0.005)){
    q <- union(q,i)
  }
}
q <- q[-1]

cuted_data <- raw_data[,-q]
cuted_data[,18] <- as.factor(cuted_data[,18])
str(cuted_data)
dim(cuted_data)


remove(nor_data)

id <- sample(1:nrow(cuted_data), nrow(cuted_data) * 0.8, replace = F)
train <- cuted_data[id,]
test <- cuted_data[-id,]

#RANDOM SAMPLING BY HAND----
set.seed(1228)
n = 3000

sample_data <- NULL

label <- c('1', '2', '3', '4', '5')
for(i in 1:5){
  x <- NULL
  x <- which(cuted_data$Y == i)
  if(length(x) >= n){
    y <- sample(x, size = n, replace = F)
    sample_data <- rbind(sample_data, cuted_data[y,])
  }
  else{
    y <- sample(x, size = n, replace = T)
    sample_data <- rbind(sample_data, cuted_data[y,])
  }
  
}
table(sample_data$Y)

#----
set.seed(1228)
n = 3000

sample_data <- NULL

label <- c('1', '2', '3', '4', '5')
for(i in 1:5){
  x <- NULL
  x <- which(train$Y == i)
  if(length(x) >= n){
    y <- sample(x, size = n, replace = F)
    sample_data <- rbind(sample_data, train[y,])
  }
  else{
    y <- sample(x, size = n, replace = T)
    sample_data <- rbind(sample_data, train[y,])
  }
  
}
table(sample_data$Y)
#Dimension reduction ----

#correlation between each feature
scale_data <- cuted_data
for(i in 1:17){
  scale_data[,i] <- (cuted_data[,i] - mean(cuted_data[,i])) / sd(cuted_data[,i])
}

pairs(scale_data[,1:17], upper.panel = NULL, panel = panel.smooth)

str(scale_data)
scale_data[,18] <- as.factor(scale_data[,18])
cuted_data[,18] <- as.factor(cuted_data[,18])

prco <- princomp(cuted_data[,1:17], cor = F)

summary(prco)

names(prco)
prco$call
screeplot(prco, type = 'line', lwd = 3)

new <- prco$scores
new <- new[,1:2]





#TREE MODEL ----
library(tree)

set.seed(1228)
train <- sample(1:nrow(sample_data), nrow(sample_data)*0.7, replace = F)


fit.tree <- tree( Y ~ ., data = sample_data)
#visualize
names(fit.tree)
fit.tree$y
summary(fit.tree)
plot(fit.tree, type = 'uniform')
text(fit.tree)

#cv.fit <- cv.tree(fit.tree, k = 5, method = 'misclass')
#plot(cv.fit, type = 'b', pch = 20)


#prune.fit <- prune.tree(fit.tree, best = 5)
#plot(prune.fit, type = 'uniform')
#text(prune.fit)

yhat <- predict(fit.tree, newdata = test)
head(yhat)
data.test <- test$Y


#MSE
mean((yhat - as.integer(data.test))^ 2)
#SEED = 1223 no sampling 4.254224
#SEED = 1228 and sampling n = 3000 RMSE 9.693247
#there no difference between standardize and non-standardize


#DESKTOP
#test <- read.csv("C:/Users/?„­?¯???/Desktop/DS HW2_fgd/fgd_data/test.csv", header = T)  
#LAPTOP
test <- read.csv("C:/Users/user/Desktop/DS HW2_fgd/fgd_data/test.csv", header = T)
test <- test[,-c(1:2)]
test <- test[,-q]

yhat_test <- predict(fit.tree, newdata = test)


#RANDOMFOREST MODEL ----
library(randomForest)

train <- sample(1:nrow(sample_data), size = nrow(sample_data)*0.8, replace = F)

fit.tm <- randomForest(Y ~ ., data = train,
                       mtry = 17,
                       ntree = 200,
                       importance = T)

fit.tm
plot(fit.tm)


names(fit.tm)
nrow(fit.tm$votes)

importance(fit.tm, type = 1)
varImpPlot(fit.tm) #by MDA X09, X22,  X08, (X23, X24X18,)



str(yhat.rm)



#data.test <- sample_data[-train, 'Y']

#plot(yhat.rm, as.numeric(data.test))
#abline(a = 0, b = 1, lwd = 2 ,col = 'gray')
#shows that the outcome is good fit by train data

#MSE
#mean((as.integer(yhat) - as.integer(data.test))^ 2)
#tail(fit.tm$votes)


str(yhat_test.rm)

#important extraction----
label_cut <- c('Y','X09', 'X18', 'X22', 'X08', 'X23', 'X24')
cut_data <- sample_data[,label_cut]

fit.tm <- randomForest(Y ~ ., data = cut_data,
                       subset = train,
                       mtry = 6,
                       ntree = 90,
                       importance = T)


#CLASSIFICATION TREE----
install.packages('adabag')
install.packages('caret')
library(adabag)
library(caret)

index <- createDataPartition(cuted_data$Y, p = .80, list = F)
train <- cuted_data[index, ]
test <- cuted_data[-index, ]

train[,18] <- as.factor(train[,18])

ada.fit <- boosting(Y ~ ., data = train,
                    boos=TRUE, mfinal=100,
                    coeflearn = 'Breiman',  
                    control=rpart.control(maxdepth=1))
names(ada.fit)
ada.fit$trees[[1]]
rpart.plot :: rpart.plot(ada.fit$trees[[1]])





#OUTPUT FILE----

yhat.rm <- predict(fit.tm, newdata = test, type = 'vote')

name <- c('ID','C1','C2','C3','C4','C5')
col <- c(1:500)

summit <- cbind(col,yhat_test)
colnames(summit) <- name


head(summit)


write.csv(yhat_test,"C:/Users/user/Desktop/DS HW2_fgd/fgd_data/rm0110.csv", 
          row.names = F)
write.csv(summit,"C:/Users/user/Desktop/DS HW2_fgd/fgd_data/tree0110.csv", 
          row.names = F)
