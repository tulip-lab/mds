#Adapted from:https://ipub.com/id3-with-data-tree/
#R package used: data.tree
library(data.tree)
#completely pure if it contains only a single class
IsPure <- function(data) {
  length(unique(data[,ncol(data)])) == 1
}

Entropy <- function( vls ) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
  -sum(res)
}

InformationGain <- function( tble ) {
  tble <- as.data.frame.matrix(tble)
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}

#For the following implementation, we assume that the 
#classifying features are in columns 1 to n-1, whereas
#the class (the edibility) is in the last column.
TrainID3 <- function(node, data) {
  node$obsCount <- nrow(data)
  #if the data-set is pure (e.g. all toxic), then
  if (IsPure(data)) {
    #construct a leaf having the name of the pure feature (e.g. 'toxic')
    child <- node$AddChild(unique(data[,ncol(data)]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  } else {
    #chose the feature with the highest information gain (e.g. 'color')
    ig <- sapply(colnames(data)[-ncol(data)], 
                    function(x) InformationGain(
                      table(data[,x], data[,ncol(data)])
                    )
    )
    feature <- names(ig)[ig == max(ig)][1]
    node$feature <- feature
    #take the subset of the data-set having that feature value
    childObs <- split(data[,!(names(data) %in% feature)], data[,feature], drop = TRUE)
    for(i in 1:length(childObs)) {
      #construct a child having the name of that feature value (e.g. 'red')
      child <- node$AddChild(names(childObs)[i])
      #call the algorithm recursively on the child and the subset      
      TrainID3(child, childObs[[i]])
    }
  }
}

Predict <- function(tree, features) {
  if (tree$children[[1]]$isLeaf) return (tree$children[[1]]$name)
  child <- tree$children[[features[[tree$feature]]]]
  return ( Predict(child, features))
}
#####################################################


########################################################
# Use the 'WeatherData.csv' dataset provided to 
# create the ID3 tree
########################################################
library(data.tree)
WeatData<- read.csv("WeatherData.csv")
WeatData
#remove the first column (Day)
WeatData[,1]<-NULL 

summary(WeatData)
WeatData

#Training with data
tree <- Node$new("WeatherData")
TrainID3(tree, WeatData)
print(tree, "feature", "obsCount")

plot(tree)
#tree$fieldsAll

# predict
Predict(tree, c(Outlook = 'Sunny', 
                Humidity = 'High')
        )

