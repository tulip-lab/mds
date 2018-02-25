#===============
rm(list=ls())
dev.off()
library(e1071)

#use synthetic data
set.seed(1)
x=matrix(rnorm(110*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:110,]=x[101:110,]*4
y=c(rep(1,100),rep(2,10))
dat=data.frame(x=x,y=as.factor(y))
par(mfrow=c(1,2))
plot(x, col=y)

svmfit=svm(x,kernel="radial",type="one-classification",gamma=0.1, nu=0.1, scale=FALSE)
#svmfit=svm(x,kernel="poly",type="one-classification", scale=FALSE)
svmfit


#index (support vectors)
plot(x[svmfit$index,],col="red")
par(new=T)
points(x[-svmfit$index,],col="blue")
par(new=F)


##################################################
##################################################
#USe breast cancer data set from Uci reoisitory
#ttps://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Diagnostic)
#Variable 2 is Diagnosis (M = malignant, B = benign) 

bCancerData<- read.csv("wdbcdata.csv",header = FALSE)
summary(bCancerData)
#remove id
bCancerData<- bCancerData[,-1]
summary(bCancerData)
#firsr variable after removal is the class (M = malignant, B = benign)
#select training data
trainData1<-bCancerData[1:449,]
#select only B class data (one class)
trainData<-trainData1[trainData1$V2=="B",]
summary(trainData)

testData<-bCancerData[450:569,,]
summary(testData)

#train one clas svm
svmfit<-svm(trainData[,-1], type="one-classification")
svmfit
# predict with test data
pred <- predict(svmfit, trainData[,-1])
pred
table(pred,trainData$V2=="B")

pred <- predict(svmfit, testData[,-1])
pred
table(pred,testData$V2=="B")

##################################################################3
###################################################################3
#TUning SVM to find the best nu and gamma
svm_tune <- tune(svm, train.x=trainData[,-1], train.y=trainData$V2=="B", type="one-classification", kernel="radial", ranges=list(nu=seq(0.1,0.5,0.1), gamma=c(.5,1,2)))
print(svm_tune)
svm_tune

#create one class svm model again and  try to run again
svm_model_after_tune <- svm(trainData[,-1], type="one-classification",kernel="radial", nu=0.1, gamma=0.5)
summary(svm_model_after_tune)

# predict with train data
pred <- predict(svm_model_after_tune, trainData[,-1])
#pred  # false - means malignant
table(pred,trainData$V2=="B")

pred <- predict(svm_model_after_tune, testData[,-1])
#pred
table(pred,testData$V2=="B")

