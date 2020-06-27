#load data from CSV
CustData = read.csv("Telco-Customer-Churn.csv" , header = TRUE)
summary(CustData)
head(CustData)
#get structure of dataset
str(CustData)
#remove missing values
CustData = na.omit(CustData)

#convert char into numeric
#One-hot-encoding features:
library(ade4)
library(data.table)
ohe_feats = c('gender', 'Partner', 'Dependents', 'PhoneService','MultipleLines','InternetService','OnlineSecurity'
              ,'OnlineBackup','DeviceProtection','TechSupport','StreamingTV','StreamingMovies','Contract'
              ,'PaperlessBilling','PaymentMethod')
for (f in ohe_feats){
  df_all_dummy = acm.disjonctif(CustData[f])
  CustData[f] = NULL
  CustData = cbind(CustData, df_all_dummy)
}
#get structure of dataset
str(CustData)

#get number of colums
print(ncol(CustData))
#remove cust ID and organize columns
CustData = CustData[,c(2,3,4,5,7:47,6)]

ncol(CustData)

CustData[1:45]


#Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

CustDataNormalized <- as.data.frame(lapply(CustData[,1:44], normalize))

head(CustDataNormalized)


set.seed(123)
#random selection of 70% data.
dat.d = sample(1:nrow(CustDataNormalized),size=nrow(CustDataNormalized)*0.7,replace = FALSE) 
# 70% training data
train.CustData = CustDataNormalized[dat.d,]
# remaining 30% test data
test.CustData = CustDataNormalized[-dat.d,] 

#seperate dataframe for 'Churn' feature 
train.CustData.labels = CustData[dat.d,46]
test.CustData.labels  = CustData[-dat.d,46]


#pCA ON train dataset
train.CustData.pca = prcomp(train.CustData, center = TRUE, scale = TRUE)
train.CustData.pca

summary(train.CustData.pca)

test.CustData.pca = predict(train.CustData.pca, newdata = test.CustData)





#get number of rows in training dataset
nrow(train.CustData)
library(class)


#find optimum k value 
i=1
k.optm=1
for (i in 1:12){
  knn.mod = knn(train=train.CustData.pca$x[,1:12], test=test.CustData.pca[,1:12], cl=train.CustData.labels, k=i)
  k.optm[i] = 100 * sum(test.CustData.labels == knn.mod)/NROW(test.CustData.labels)
  k=i
  cat(k,'=',k.optm[i],'
    ')
}

# k =11 gives highest accuracy.

knn.11 = knn(train=train.CustData.pca$x[,1:12], test=test.CustData.pca[,1:12], cl=train.CustData.labels, k=11)
#knn.71 = knn(train=train.CustData, test=test.CustData, cl=train.CustData.labels, k=71)

#Calculate the proportion of correct classification for k = 26, 27
ACC.11 = 100 * sum(test.CustData.labels == knn.11)/NROW(test.CustData.labels)
#ACC.71 = 100 * sum(test.CustData.labels == knn.71)/NROW(test.CustData.labels)

table(knn.11 ,test.CustData.labels)

library(caret)

confusionMatrix(table(knn.11 ,test.CustData.labels))



