#install.packages("SparseM")
#https://cran.r-project.org/web/packages/e1071/vignettes/svmdoc.pdf
#https://rischanlab.github.io/SVM.html
#

#library("SparseM")
library(e1071)
#library(rpart)

data(iris)
summary(iris)
plot(iris)

## classification mode
# default with factor response:
model <- svm(Species ~ ., data = iris)
model

# alternatively the traditional interface:
#attach(iris) # to enable bjects in the database be accessed by simply giving their names
x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y) 

print(model)
summary(model)

# test with train data
pred <- predict(model, x)
# (same as:)
pred <- fitted(model)

# Check accuracy:
table(pred, y)

################################################
#################################################
#TUning SVM to find the best cost and gamma
svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)

#create svm model again and try to run again
svm_model_after_tune <- svm(Species ~ ., data=iris, kernel="radial", cost=1, gamma=0.5)
summary(svm_model_after_tune)

#Run Prediction again with new model
pred <- predict(svm_model_after_tune,x)
table(pred,y)

#####################################################
#select only two variables and do the SVM run
col<-c("Petal.Length", "Petal.Width","Species")
selIris<-iris[,col];
model2 <- svm(Species ~ ., data = selIris, kernel="radial", cost=1, gamma=0.5)
plot(model2,selIris)

model2 <- svm(Species ~ ., data = selIris, kernel="radial", cost=1e5, gamma=0.5)
plot(model2,selIris)

model2 <- svm(Species ~ ., data = selIris, kernel="radial", cost=1e-5, gamma=0.5)
plot(model2,selIris)

