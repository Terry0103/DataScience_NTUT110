##2021/11/27 6 PM

#data <- read.csv("C:/Users/鄭可雍/Desktop/fgd_data/train.csv", header = T)

data <- read.csv("C:/Users/user/Desktop/fgd_data/train.csv", header = T)
dim(data)

#remove id and Time and label variables
rdata <- data[,-c(1:2)]
str(rdata)
str(as.factor(rdata$Y))

hist(data[,27], xlab = 'Cluster', col = data$Y)
abline(h = 400, col = 'blue')
#the histgram shows that there is imbalance in the this data

set.seed(1222)
index <- 0
for(i in 1:5){
  x <- NULL
  x <- which(rdata$Y == i)
  index <- union(index, sample(x, size = 400))
}
index <- index[-1]

sampple_data <- rdata[index,]
str(sampple_data)
hist(sampple_data[,25], xlab = 'Cluster', col = cdata[,25])

#----

#PRE-PROCESSING ----
mean(is.na(rdata)) #there is no missing value in the data


#cut low variable ----
nor_data <- rdata
#normalize data
for (i in 1:24){
  nor_data[,i] <- rdata[,i] / mean(rdata[,i])
}

for (i in 1:5){
  print(var(nor_data[,i]))
}
#detect which variance are low respectively
q <- 0
for (i in 1:24){
  if(var(nor_data[,i] < 0.005)){
    q <- union(q,i)
  }
}
q <- q[-1]

cdata <- sampple_data[,-q]
str(cdata)
cleared_data <- cdata
cleared_data[,18] <- as.factor(cleared_data[,18])
str(cleared_data)
#After we remove which variances low respectively there are 18 variable left
#----

#standardize data
data_scale <- scale(c003data)
data_scale <- scale(rdata)
#KMEANS CLUSTERING ----
?kmeans
fitK <- kmeans(data_scale, 5)
str(fitK)

table(Pred = fitK$cluster, True = data[,27])

sam_fitK <- kmeans(SDC, 5)
str(sam_fitK)
table(Pred = sam_fitK$cluster, True = sample_data[,25])


#choosing k ----
k <- list()
for(i in 1:10){
  k[[i]] <- kmeans(data_scale, i)
}


#we can overview that the outcome of Kmeans by different k
#Based on k increasing the SSW/SSTO are increasing. 
#It's seems look great, but it's wrong.

betweenss_totss <- list()
for(i in 1:10){
  betweenss_totss[[i]] <- k[[i]]$betweenss / k[[i]]$totss
}
plot(1:10, betweenss_totss, type = 'b',
     ylab = "Between_SS / Total_SS",
     xlab = "Cluster(k)")





#HIERARCHICAL CLUSTERING ----
?dist
d <- dist(data_scale, "manhattan")
?hclust
fitH <- hclust(d, "ward.D2")

clusterH <- cutree(fitH, 5)

table(Pred = clusterH, True = data[,27])

d <- dist(SDC, "manhattan")
sam_fitH <- hclust(d, "ward.D2")
clusterH <- cutree(sam_fitH, 5)
table(Pred = clusterH, True = sample_data$Y)


#DENSITY-BASED CLUSTERING ----
library(dbscan)
kNNdistplot(data_scale, k = 5)
abline(h = 3, col = 'red', lty = 2)
#the position of elbow seems around 0.7 we add a line the point it out
#indicate the elbow the pick it as the eps
fitD <- dbscan(data_scale, eps = 5, minPts = 25 )

fitD

#MODEL-BASED CLUSTERING ----
library(mclust)
fitM <- Mclust(data_scale)
fitM


#CLARA CLUSTERING ----
install.packages('cluster')
install.packages('fpc')
library(cluster)
library(fpc)
?clara
fitC <- clara(data_scale, k = 5, metric = "manhattan", samples = 350)
fitC
str(fitC)
table(Pred = fitC$clustering, True = data[,27])

?pamk
fitP <- pamk(data_scale)
fitP


id <- sample(1:dim(rdata)[1], size = dim(rdata)[1]*0.5, replace = F)
rdata[,25] <- as.factor(rdata$Y)
str(rdata$Y)
train <- rdata[-id,]
test <- rdata[id,]









#TREE MODEL ----
install.packages('tree')
install.packages('randomForest')
library(tree)
library(randomForest)

set.seed(1221)
train <- sample(1:nrow(data), nrow(data)*0.7, replace = F)


fit.tree <- tree(Y ~ ., data = data, subset = train)

summary(fit.tree)
plot(fit.tree, type = 'uniform')
text(fit.tree)

cv.fit <- cv.tree(fit.tree, k = 5, method = 'misclass')
plot(cv.fit, type = 'b', pch = 20)


#prune.fit <- prune.tree(fit.tree, best = 5)
#plot(prune.fit, type = 'uniform')
#text(prune.fit)

yhat <- predict(fit.tree, newdata = cdata[-train,])
data.test <- data[-train, 'Y']


#MSE
mean((yhat - data.test)^ 2)

test <- read.csv("C:/Users/user/Desktop/fgd_data/test.csv", header = T)
test <- test[,-c(1:2)]
test <- test[,-q]

yhat_test <- predict(fit.tree, newdata = test)
str(yhat_test)

colnames(yhat_test,)

write.csv(yhat_test,"C:/Users/user/Desktop/fgd_data/outcome.csv", 
          row.names = F)

#RANDOMFOREST MODEL ----
fit.tm <- randomForest(Y ~ ., data = cleared_data,
                       mtry = 17,
                       importance = T)
yhat_test <- predict(fit.tm, newdata = test)
str(yhat_test)
write.csv(yhat_test,"C:/Users/user/Desktop/fgd_data/rm_yhat.csv", 
          row.names = F)
