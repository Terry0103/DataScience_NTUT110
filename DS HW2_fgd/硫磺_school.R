##2021/11/27 6 PM

#data <- read.csv("C:/Users/鄭可雍/Desktop/fgd_data/train.csv", header = T)
data <- read.csv("C:/Users/user/Desktop/fgd_data/train.csv", header = T)
dim(data)

#remove id and Time and label variables
rdata <- data[,-c(1:2)]
str(rdata)
str(rdata$Y)

data_hist <- hist(data[,27], xlab = 'Cluster', col = data$Y)
abline(h = 400, col = 'blue')
#the histgram shows that there is imbalance in the this data

#test----
tally(rdata, Y == 1)

id <- c()
for(i in 1:5){
  id[i] <- sample(rdata$Y == i, size = 300, replace = F)
}

id<- list()
for(i in 1:5){
  id[[i]] <- rdata$Y == i 
}

for(i in 1:5){
  if(id[i] == TRUE){
    id[i] = i
  }
}



index <- 0
for(i in 1:5){
  index <- union(index, sample(id[[i]] == i, size = 300, replace = F))
}
index <- sample(id[[1]] == 1, size = 300, replace = F)
#----

#PRE-PROCESSING ----
mean(is.na(rdata)) #there is no missing value in the data


#library(tidyverse)
#rdata %>% group_by(Y) %>%
#  summarise(Rate = sum(Y)/20621)
#count(rdata$Y)

#cut low variable ----
nor_data <- rdata
#standardize data
for (i in 1:24){
  nor_data[,i] <- rdata[,i] / mean(rdata[,i])
}

for (j in 1:5){
  print(var(nor_data[,j]))
}
#detect which variance are low respectively
q <- 0
for (k in 1:24){
  if(var(nor_data[,k] < 0.005)){
    q <- union(q,k)
  }
}
q <- q[-1]

c003data <- rdata[,-q]
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

#supervised SVM

set.seed(30678)
id <- sample(1:20621, size = dim(rdata)[1]*0.2)

as.factor(rdata$Y)

#rdata[,-25] <- scale(rdata[,-25])

train <- rdata[-id,]
test <- rdata[id,]

#TREE MODEL ----

set.seed(30678)

#UNDER SAMPLING ----
index <- 0
for(i in 1:5){
  x <- NULL
  x <- which(rdata$Y == i)
  index <- union(index, sample(x,size = 400))
}
index <- index[-1]
data_UD <- rdata[index,]

hist(as.integer(data_UD[,25]), xlab = 'Cluster', col = data_UD$Y)
#----

#SPLIT DATA TO TRAIN AND TEST ----
id <- sample(1:20621, size = dim(rdata)[1]*0.2)

rdata[,25] <- as.factor(rdata$Y)
str(rdata$Y)

c003data[,18] <- as.integer(c003data$Y)
str(c003data$Y)

train <- c003data[-id,]
hist(as.integer(train[,25]), xlab = 'Cluster', col = train$Y)

test <- c003data[id,]
hist(as.integer(test[,25]), xlab = 'Cluster', col = test$Y)
#----
install.packages('ISLR')
install.packages('tree')
library(ISLR)
library(tree)

str(rdata)
?tree
trmo <- tree(Y ~ ., 
             data = train)

summary(trmo)


plot(trmo, type='uniform', lwd=2)
text(trmo, 
     pos=3,     # pos 變數標示位置
     offset=0,  # offset 變數標示位置與解點空間調整
     col='blue')

cv.trmo <- cv.tree(trmo)
cv.trmo 
plot(cv.trmo, type='l')

prune.trmo <- prune.tree(trmo, best =5)
summary(prune.trmo)


prune.trmo <- prune.tree(trmo, best=5)
plot(prune.trmo, type='uniform', lwd=2)
text(prune.trmo, 
     pos=3,     # pos 變數標示位置
     offset=0,  # offset 變數標示位置與解點空間調整
     col='blue')

table(Pred = predict(prune.trmo, train),
      True = train$Y)

install.packages('maptree')
library(maptree)
draw.tree(prune.trmo, nodeinfo=TRUE, digits=2)

install.packages("rpart.plot")
library(rpart.plot)
prp(trmo,           # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col='gray')

#SAMPLE DATA TREE ----
trmo.sam <- tree(Y ~ ., 
             data = data_UD)

summary(trmo)


plot(trmo, type='uniform', lwd=2)
text(trmo, 
     pos=3,     # pos 變數標示位置
     offset=0,  # offset 變數標示位置與解點空間調整
     col='blue')

