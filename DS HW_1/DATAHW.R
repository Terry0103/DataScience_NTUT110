
#install.packages('tidyverse') #detecting the missing value
#install.packages('moments') #calculate the missing rate
#install.packages('VIM') #knn interpolation
#install.packages('mice') #a data interpolation with iter and data set 
#install.packages('MASS') #LDA
#install.packages('e1071')


rdata <- read.csv('./semi.csv', header = T)
dim(rdata)
str(rdata)

#The reason of removing 1st column are that 
#Time feature was considered useless by me and
#I can't deal with the time feature.
pdata <- rdata[,-1]

#Transforming the target feature to the factor(category)
pdata[,591] <- as.factor(pdata[,591])
str(pdata[,591])
library(ggplot2)
ggplot(pdata, aes(pdata[,591])) + geom_bar()



###The features with variance = 0 are removed.----
constant <- integer()
for(i in 1:590){ #591th feature is a factor
  if(sd(pdata[,i], na.rm = T) == 0){
    constant <- union(constant, i)
  }
}

sdata <- pdata[,-constant]
dim(sdata)
#After remove the feature which variance equals 0. 
#There are 4765features left.

#Detecting the missing value.
#library(tidyverse)

#mean(is.na(sdata[,3]))

#catch all missing variable rate
#missing <- sdata %>%
#  summarise_all(funs(mean(is.na(.))))
#dim(missing)

###The features with missing rate > 0.45 are removed----
missing <- NULL
for(i in 1:474){
  temp <- mean(is.na(sdata[,i]))
  #missing <- union(missing, mean(is.na(sdata[,i])))
  if(temp > 0.45){
    missing <- union(missing, i)
  }
}
c45data <- sdata[,-missing]
#443 features are left.

#sort(missing, decreasing = T)
#show the missing rate with order from big to small

#cut the variable which missing rate greater than .45
#missindex <- 0
#for(i in 1:475){
#  if(missing[i] >= 0.45 ){
#    missindex <- union(missindex,i)
#  }
#}
#missindex <- missindex[-1]
#make index which missing rate >= .45

#missing[68]
#mean(is.na(sdata[,68]))
#check the index are correct
# c45data <- sdata[,-missindex]
#cut the variable which missing rate greater than .45



#?----
nor_c45 <- c45data
#normalizing data
for (k in 1:442){
  nor_c45[,k] <-  c45data[,k] / mean(c45data[,k], na.rm = T)
}


q <- vector(
)
for (i in 1:442){
  if(var(nor_c45[,i], na.rm = T) < 0.005){
    q <- union(q, i)
  }
}
#0.003 = 57
#0.005 = 61
cn3data <- c45data
cn3data <- cn3data[,-q]





###Imputation(KNN)----
library(VIM)
miss <- aggr(c45data, plot = T, number = T, prob = T)
names(miss)
miss$count
#This visualization method is not work very well in the high dimensional data.

knn1 <- kNN(c45data)
#this method takes lots of time in R.


imputed.data <- knn1[,-c(444:886)]
mean(is.na(imputed.data))

###Diumension reduction(PCA)----
completekNN[,-382] <- scale(completekNN[,-382])
prco <- princomp(completekNN, cor = T)
names(prco)
prco$loadings

#Spliting data----
set.seed(30678)
id <- sample(1:nrow(imputed.data), size = nrow(imputed.data)*0.8,
             replace = F)

train <- imputed.data[id,]
test <- imputed.data[-id,]


table(train$Pass.Fail)
table(test$Pass.Fail)


#detecting the out-lier (standard deviation)(SKIP)----
a <- 0
for(i in 1:381){
  for(j in 1:1254){
    if(abs(train[j,i]) > 3){
      a <- union(a,j)
    }
  }
}
str(train)

###dimension reduction(LDA)----
library(MASS)
head(train$Pass.Fail)


Line <- lda(Pass.Fail ~ ., train)

plot(Line)

LDA.train = as.data.frame(predict(Line, train))
head(LDA.train)
LDA.train = LDA.train[,c(4,1)]
head(LDA.train)


LDA.test = as.data.frame(predict(Line, test))
LDA.test = LDA.test[,c(4,1)]

head(LDA.test)


###Classification(SVM)----
library(e1071)
??e1071
?svm
Linear <- svm(Pass.Fail ~ .,
        data = train,
        kernel = 'linear',
        cost = 1,
        scale = T)

table(True = train[,443],
      Pred = predict(Linear, train[-443]))
table(True = test[,443],
      Pred = predict(Linear, test[-443]))

#Dimention Reduction esti
Line.LDA <- svm(class ~ .,
              data = LDA.train,
              kernel = 'linear',
              cost = 1,
              scale = F)
table(Pred = predict(Line.LDA, LDA.test),
      True = LDA.test$class)
###############3
ra.LDA <- svm(class ~ .,
                data = LDA.train,
                kernel = 'radial',
                cost = 1,
                scale = F)
table(Pred = predict(ra.LDA, LDA.test),
      True = LDA.test$class)


Poly <- svm(Pass.Fail ~ .,
            data = train,
            kernel = 'polynomial',
            degree = 5,
            scale = T)

Radial <- svm(Pass.Fail ~ .,
              data = train,
              kernel = 'radial',
              scale = T)


summary(Linear)
#default (cost=1) num of support vector is 481
#cost = 10, n = 232



#confusion matrix of train data
table(Pred = predict(Linear, train),
      True = train$Pass.Fail)
#c = 1  0.007177033 error rate 
#c = 3  0.001594896 error rate 


table(Pred = predict(Poly, train),
      True = train$Pass.Fail)
#hyper parameters are default
#0.05023923

table(Pred = predict(Radial, train),
      True = train$Pass.Fail)
#hyper parameters are default
#0.05980861

table(Pred = predict(Poly, test),
      True = test$Pass.Fail)
#L:0.1565495 c = 1
#L:0.06070288 c >4
#P:0.0.07348243
#R :0.06070288 P.S. outcome same as (L, c >4)





###Clssification(RM)----
library(randomForest)
?randomForest
fit.rm <- randomForest(Pass.Fail ~., data = imputed.data, subset = id,
                       mtry = 100, importance = T)
plot(fit.rm)
fit.rm


###Resampling----

