index.train <- as.factor(rep(0:1,c(nrow(dtm.madison.train),nrow(dtm.hamilton.train))))
dtm.train <- cbind(index.train,rbind(dtm.madison.train,dtm.hamilton.train))
index.test <- as.factor(rep(0:1,c(nrow(dtm.madison.test),nrow(dtm.hamilton.test))))
dtm.test<-cbind(index.test,rbind(dtm.madison.test,dtm.hamilton.test))
library(rpart)
#using gini impurity coefficient splits
tree<-rpart(index.train~.,data=as.data.frame(dtm.train),method = "class",parms=list(split="gini"))
plot(tree, margin=0.1, main="Classification Tree (Gini)")
text(tree, pretty=TRUE, fancy=TRUE)
tree.pred<-predict(tree,as.data.frame(dtm.test),type = "class")
library(caret)
confusionMatrix(tree.pred,as.data.frame(dtm.test)[,1])

#using information gain splits
tree.info<-rpart(index.train~.,data=as.data.frame(dtm.train),method = "class",parms=list(split="information"))
plot(tree.info,margin = 0.1, main="Classification Tree (Information gain)")
text(tree.info,pretty = T,fancy = T)
tree.info.pred <- predict(tree.info, as.data.frame(dtm.test),type="class")
confusionMatrix(tree.info.pred, as.data.frame(dtm.test)[,1])
