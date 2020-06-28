library(ade4)
library(data.table)
library(class)
library(caret)

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
featureSet = c('gender', 'Partner', 'Dependents', 'PhoneService','MultipleLines','InternetService','OnlineSecurity'
              ,'OnlineBackup','DeviceProtection','TechSupport','StreamingTV','StreamingMovies','Contract'
              ,'PaperlessBilling','PaymentMethod')
for (f in featureSet){
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
#get number of columns
ncol(CustData)


#Normalization of the dataset
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

CustDataNormalized = as.data.frame(lapply(CustData[,1:44], normalize))

#head(CustDataNormalized)

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
#summary of PCA results
summary(train.CustData.pca)

test.CustData.pca = predict(train.CustData.pca, newdata = test.CustData)


#get number of rows in training dataset
nrow(train.CustData)

#test accuracy with K = 70 and 71
knn.70 = knn(train=train.CustData.pca$x[,1:12], test=test.CustData.pca[,1:12], cl=train.CustData.labels, k=70)
knn.71 = knn(train=train.CustData.pca$x[,1:12], test=test.CustData.pca[,1:12], cl=train.CustData.labels, k=71)

#Calculate the proportion of correct classification for k = 70, 71
ACC.70 = 100 * sum(test.CustData.labels == knn.70)/NROW(test.CustData.labels)
ACC.71 = 100 * sum(test.CustData.labels == knn.71)/NROW(test.CustData.labels)

#evaluate modle accuarcy using k =71 since it gives higher accuarcy 
table(knn.70 ,test.CustData.labels)

#confution Matrix
confusionMatrix(table(knn.70 ,test.CustData.labels))



