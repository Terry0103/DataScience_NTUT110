#2022.01.14 2:40 P.M.

raw_data <- read.csv("C:/Users/user/Desktop/DS_HW3/milk_data.csv",
                     header = T)
#待定:最近分娩日期,前次分娩日期, 檢測日期(chr)
  

#REMOVE: 'ID', 登錄日期,'資料年度',犢牛編號1(chr), 犢牛編號2(chr),採樣日期,   
#第一次配種日期,出生日期, 犢牛性別(chr),'最後配種日期', 第一次配種精液(chr)
#'最後配種精液'(chr),分娩日期(dupelicated)
#'
#'
#leave----
#(dummy):'酪農場代號', 父親牛精液編號, 母親乳牛編號, 計算胎次(logi),'資料月份'
#
#leave: 胎次(int), 泌乳天數(int), 月齡(int), '配種次數'(int), 母牛體重(int), 
#     犢牛體型(chr), 分娩難易度(int)
#
#other processing:1. 檢測日期(chr,6th)-最近分娩日期(7th) name = birth_nearest
#                 2. 檢測日期(chr)-前次分娩日期(8th) name = birth_pre
#
#target :乳量(num)(11th)
str(as.factor(raw_data$分娩難易度))
raw_data$月齡
str(raw_data[, 11])
mean(is.na(raw_data$分娩難易度))

head(raw_data$出生日期)
#test----
qq <- as.POSIXlt.character(raw_data$出生日期)
head(qq)
as.integer(qq[1] - qq[2])
as.integer(as.POSIXlt.character(pro_data[,6]) - as.POSIXlt.character(pro_data[,7]))


#data preprocessing----

feature <- c('酪農場代號', '計算胎次','資料月份','分娩難易度', '犢牛體型',
             '檢測日期','最近分娩日期', '前次分娩日期',
             '胎次', '泌乳天數', '月齡', '配種次數', '母牛體重',
             '乳量', 'ID')
#1-5 dummy, 6-8 date, 9 -13 int, 14 target , 15 ID
pro_data <- raw_data[, feature]

#1-5 ----
str(pro_data[,1:5])
for(i in 1:5){
  pro_data[,i] <- as.factor(pro_data[,i]) 
}
str(pro_data[,1:5])

#9-13 & target 14----
str(pro_data[, 9:13])
str(pro_data$乳量)

#CUT LOW VARIANCE (USELESS)---- 
nor_data <- pro_data
#normalize data
for (i in 9:13){
  nor_data[,i] <- pro_data[,i] / mean(pro_data[,i], rm.na = T)
}

q <- 0
for (i in 9:13){
  if(var(nor_data[,i], na.rm = T) < 0.005){
    q <- union(q,i)
  }
}
q <- q[-1]

cuted_data <- pro_data[,-q]
cuted_data[,18] <- as.factor(cuted_data[,18])


#deal with NA----
 #37517  11
for (i in 1:15){
  print(mean(is.na(pro_data[,i])))
}
pro_data <- pro_data[,-c(6:8,13)]
cut_data <- na.omit(pro_data)
#Dim 22875  10
mean(is.na(pro_data))

#pro_data[,7] <- as.integer(as.POSIXlt.character(pro_data[,6]) -
#                             as.POSIXlt.character(pro_data[,7]))

#pro_data <- pro_data[,-c(6,8)]
#22875   14
#1-5 dummy, 6 processed date, 7-10 int, 11 target , 12 ID

#install.packages("VIM")
library(VIM)
knn1 <- kNN(pro_data)
new_data <- knn1[,-c(12:22)]
#one hot encoding----

library(fastDummies)

str(new_data[,1:5])
#level = 3,1,12,4,4
label <- c('酪農場代號', "計算胎次", '資料月份', 
       '分娩難易度','犢牛體型')

for(i in 1:5){
  new_data <- dummy_cols(new_data, select_columns = label[i])
}
dim(new_data)
#22875 35
new_data <- new_data[,-c(1:5)]
dim(new_data)
#22875 30


#model.fit -----

#tree----
install.packages('tree')
library(tree)

set.seed(0114)
train <- sample(1:nrow(new_data), size = nrow(new_data) * .8, 
                replace = F)
?tree

fit.t <- tree(乳量 ~ . -ID , data = new_data, sub = train)
names(fit.t)
summary(fit.t)
plot(fit.t, type = 'uniform')
text(fit.t)

cv.t <- cv.tree(fit.t)
names(cv.t)
#回傳原始數量NODE以下之各NODE樹的決策數變異程度
plot(cv.t, type='b', pch=20)
#node = 7 have the most small


yhat <- predict(fit.t, newdata = new_data[-train ,], type = 'vector')    
milk.test <- new_data[-train ,'乳量']


plot(yhat, milk.test, ylim = c(0, 50),
     xlim = c(0, 50))
abline(0, 1, lwd = 2, col='gray')
mean( (yhat - milk.test) ^ 2 ) #43.17835
sqrt(mean( (yhat - milk.test) ^ 2 )) #6.571024



#randomforest(non-efficiency and too slow)----
install.packages("randomForest")
library(randomForest)

fit.r <- randomForest(乳量 ~ . - ID, data = new_data,
                      subset = train,
                      ntree = 100,
                      mtry = 28,
                      importance = T)
plot(fit.r)
names(fit.r)

varImpPlot(fit.r, type = 1)
#酪C 泌乳天數 月齡 配種次數  胎次 酪A 酪B
varImpPlot(fit.r, type = 2)
#泌乳天數 酪C 月齡 胎次

yhat_r <- predict(fit.r, newdata = new_data[-train ,], type = 'response')    
milk.test <- new_data[-train ,'乳量']


plot(yhat_r, milk.test)
abline(0,1, lwd=2, col='gray')
mean( (yhat_r - milk.test) ^ 2 ) #43.17835
sqrt(mean( (yhat_r - milk.test) ^ 2 )) #6.571024


#FEATURE EXTRACTIING
i <- c('酪農場代號_C', '泌乳天數', '月齡', '配種次數', '胎次',
       '酪農場代號_A', '酪農場代號_B', '乳量')
ext_data <- new_data[,i]

fit.r.ext <- randomForest(乳量 ~ ., data = ext_data,
                        subset = train,
                        ntree = 100,
                        mtry = 7,
                        importance = T)
ext_hat <- predict(fit.r.ext, newdata = ext_data[-train, ], type = 'response')

sqrt(mean( (ext_hat  - ext_data[-train,'乳量']) ^ 2 ))


#OUTPUT DATA----


sub <- read.csv("C:/Users/user/Desktop/DS_HW3/summit.csv",
                     header = T)

ID <- sub$ID

summit_hat <- predict(fit.r, newdata = new_data[ID, ], type = 'response')
sqrt(mean( (summit_hat  - new_data[ID, '乳量']) ^ 2 ))

head(summit_hat)


name <- c('ID','1')
names(summit_hat)

as.data.frame(summit_hat)[,1]
summit <- cbind(ID,as.data.frame(summit_hat)[,1])


colnames(summit) <- name


head(summit)
tail(summit)

write.csv(summit,"C:/Users/user/Desktop/DS_HW3/milk_rm0114.csv", 
          row.names = F)

