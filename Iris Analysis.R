# Iris Analysis

# Importing the data
data=iris
head(data)

# Box Plots for the variables
for( i in 1:ncol(data)){
  boxplot(data[i],main = colnames(data[i]))
}

# Calculating the Correlation among the variables
cor(data[-5])

# Creating new Variables Based on the Proportion
data2<-data
data2<-data.frame(data2,SL_PL=data2$Sepal.Length/data2$Petal.Length)

data2<-data.frame(data2,SL_PW=data2$Sepal.Length/data2$Petal.Width)

data2<-data.frame(data2,SL_SW=data2$Sepal.Length/data2$Sepal.Width)

data2<-data.frame(data2,PL_PW=data2$Petal.Length/data2$Petal.Width)

head(data2)

# Splitting the Dataset
train_ind<-sample(1:nrow(data2),0.7*nrow(data2))
train<-data2[train_ind,]
test<-data2[-train_ind,]

# Fitting a neural netowrk classifcation algorithm
library(nnet)
neural<-nnet(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data = data2,size=5)
print(neural)
neu_pred<-as.factor(predict(neural,newdata = test,type="class"))
library(caret)
# Confusion matrix for the Neural Netwok Algorithm
confusionMatrix(neu_pred,test$Species)
# The accuracy for the neural network model is 1

# Decision Tree Classifier
library(rpart)
library(rpart.plot)
dt_model<-rpart(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=train,method = "class") # Decision Tree Model 
rpart.plot(dt_model)
dt_model
treePredictions <- predict(dt_model, newdata = test, type = "class")
confusionMatrix(treePredictions, test$Species)
# Accuracy = 0.9778

#Now using the new variables that we have created from the original variabeles
dt_model2<-rpart(Species~SL_PL+SL_PW+SL_SW+PL_PW,data=train,method = "class")
rpart.plot(dt_model2)
dt_model
treePredictions2 <- predict(dt_model, newdata = test, type = "class")
confusionMatrix(treePredictions2, test$Species)
# The model acuracy stays the same even with the newly created variables

# Random Forest Classifier
library(randomForest)
rf_model<-randomForest(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data = train)
rf_model
rf_pred<-predict(rf_model,newdata=test)
confusionMatrix(rf_pred,test$Species)

# Now newly created variables 
rf_model2<-randomForest(Species~SL_PL+SL_PW+SL_SW+PL_PW,data = train)
rf_model2
rf_pred2<-predict(rf_model2,newdata=test)
confusionMatrix(rf_pred2,test$Species)
# Here the accuracy has fallen for the model with new variables

# Naive Bayes Classifier
library(e1071)
nb<-naiveBayes(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=train)
nb
nb_predict<-predict(nb,newdata = test)
confusionMatrix(nb_predict,test$Species)
# Trying he same on the new variables
nb2<-naiveBayes(Species~SL_PL+SL_PW+SL_SW+PL_PW,data = train)
nb2_predict<-predict(nb2,newdata = test )
confusionMatrix(nb2_predict,test$Species)
# Accuracy has stayed the same

# Trying heirarchial clustering on the Iris

dist_mat<-dist(iris[-5])
hclust_res<-hclust(dist_mat,method = "average")
plot(hclust_res)
hclust_clusters <- cutree(hclust_res, k = 3)
# View the clustering result
table(iris$Species, hclust_clusters)
plot(hclust_clusters)

# SVM directly on IRIS
set.seed(123)

train_ind<-sample(1:nrow(iris),0.7*nrow(iris))
train3<-iris[train_ind,]
test3<-iris[-train_ind,]

sup_vec_mac_iris<-svm(Species~.,data = train3,kernel="linear")
pred3<-predict(sup_vec_mac_iris,newdata = test3)
confusionMatrix(pred3,test3$Species)

# Dimensionality reductionin pca 
pca<-princomp(data[-5],cor = F)
summary(pca)
plot(pca,type="l")
c<-cor(data[-5])
library(psych)
fa<-psych::fa(data[-5],nfactors = 4,rotate ='none')
summary(fa)
fa$loadings

# Trying pca on the dataset 

pca<-princomp(data[-5],cor = F)
summary(pca)
plot(pca,type="l")
screeplot(pca,type="l", main = "PCA on Iris")
# First component explains 92% of the variation in the data 

pca$loadings      # checking the loadings of the data 
pca_scores<-data.frame(pca$score)       # creating a dataframe of the scores
head(pca_scores )

# Extracting just the first component from the pca 
com_data<-data.frame(pca_scores$Comp.1,data$Species)

# using the neural networn model for testing the accuracy retained after pca
set.seed(123)
train_ind<-sample(1:nrow(com_data),0.7*nrow(com_data))
train<-com_data[train_ind,]
test<-com_data[-train_ind,]

neural2<-nnet(data.Species~pca_scores.Comp.1,data = com_data,size = 5)
pred<-as.factor(predict(neural2,newdata = test,type = 'class'))
confusionMatrix(pred,test$data.Species)

# Support Vector Machine 
sup_vec_mac<-svm(data.Species~pca_scores.Comp.1,data = train)
pred2<-predict(sup_vec_mac,newdata = test)
confusionMatrix(pred2,test$data.Species)

# Trying the ratio made variables for SVM

# Creating new Variables Based on the Proportion
data2<-data
data2<-data.frame(data2,SW_PL=data2$Sepal.Width/data2$Petal.Length)

data2<-data.frame(data2,SL_PW=data2$Sepal.Length/data2$Petal.Width)

data2<-data.frame(data2,SL_SW=data2$Sepal.Length/data2$Sepal.Width)

data2<-data.frame(data2,PL_PW=data2$Petal.Length/data2$Petal.Width)
data2<-data2[5:9]
head(data2)

# Splitting the Dataset
train_ind<-sample(1:nrow(data2),0.7*nrow(data2))
train2<-data2[train_ind,]
test2<-data2[-train_ind,]

# Support Vector Machine  on ratio data 
sup_vec_mac_rat<-svm(Species~SW_PL+SL_PW,data = train2)
pred3<-predict(sup_vec_mac_rat,newdata = test2)
confusionMatrix(pred3,test2$Species)

