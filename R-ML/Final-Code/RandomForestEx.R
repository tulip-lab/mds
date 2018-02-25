#install.packages("randomForest")
#https://cran.r-project.org/web/packages/randomForest/randomForest.pdf

library("randomForest")
data(iris)
summary(iris)
#inclide first 4 variables as input and the last variable (species) as target 
# ntree-  no of trees, 
# mtry - no of variables randomly sampled (default is sqrt(p), p-no of variables).
iris.rf <- randomForest(iris[,-5], iris[,5], ntree=10, mtey=2,importance=TRUE, prox=TRUE)

print(iris.rf)

#importance
iris.rf$importance
importance(iris.rf)

# plot the variable importance plot
# variables with a large 'mean decrease in accuracy' are 
# more important for classification of the data
# The Gini coefficient is a measure of homogeneity from 0 (homogeneous) to 1 (heterogeneous)
# Variables that result in nodes with higher purity have a higher decrease in Gini coefficient.

varImpPlot(iris.rf)
varImpPlot(iris.rf,type=1)  # mean decrease in accuracy
varImpPlot(iris.rf, type=2) # mean decrease in Gini

#The element (i,j) of the proximity matrix is the fraction of trees in which elements i
#and j fall in the same terminal node 
iris.rf$prox

#plot points using two variables
iris.p <- classCenter(iris[,-5], iris[,5], iris.rf$prox)
plot(iris[,3], iris[,4], pch=21, xlab=names(iris)[3], ylab=names(iris)[4],
     bg=c("red", "blue", "green")[as.numeric(factor(iris$Species))],
     main="Iris Data with Prototypes")
points(iris.p[,3], iris.p[,4], pch=21, cex=2, bg=c("red", "blue", "green"))

#Refere to the following paper for more info:
# "Classification and Regression by randomForest" by Andy Liaw and Matthew Wiener in R News (ISSN 1609-3631).


