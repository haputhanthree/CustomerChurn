#load data from CSV
CustData = read.csv("Telco-Customer-Churn.csv" , header = TRUE)
summary(CustData)
head(CustData)
#get structure of dataset
str(CustData)

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

library(caTools)
#set seed to random numbers
set.seed(123)
#split data
sample = sample.split(CustData,SplitRatio = 0.8)
train = subset(CustData, sample == TRUE)
test = subset(CustData, sample == FALSE)

