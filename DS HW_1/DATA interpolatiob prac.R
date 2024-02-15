stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks

stocks %>%
  pivot_wider(names_from = year, values_from = return)

stocks %>%
  pivot_wider(names_from = year, values_from = return) %>%
  pivot_longer(
    cols = c(`2015`, `2016`),
    names_to = 'year',
    values_to = 'return)',
    values_drop_na = T
    )

stocks %>%
  complete(year, qtr)


#test KNN interpolation
install.packages("missForest")
install.packages('VIM')
library(missForest)
library(VIM)

set.seed(30678)
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)
iris.mis <- iris.mis[,-5]

?knn
knn1 <- kNN(iris.mis)
summary(iris)

install.packages('mice')
library(mice)

?mice

imputeddata <- mice(iris.mis, m = 1, maxit = 30, method = 'pmm', seed = 30678)
summary(imputeddata)
#check imputed data
imputeddata$imp$Sepal.Width
head(completeData)
completeData <- complete(imputeddata,2)
mean(is.na(completeData))
