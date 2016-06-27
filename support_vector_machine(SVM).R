library(e1071)
library(caret)
index.train <- as.factor(rep(0:1,c(nrow(dtm.madison.train),nrow(dtm.hamilton.train))))
dtm.train <- cbind(index.train,rbind(dtm.madison.train,dtm.hamilton.train))
index.test <- as.factor(rep(0:1,c(nrow(dtm.madison.test),nrow(dtm.hamilton.test))))
dtm.test<-cbind(index.test,rbind(dtm.madison.test,dtm.hamilton.test))
dtm.train[,2:ncol(dtm.train)] <- scale(dtm.train[,2:ncol(dtm.train)] , center = TRUE, scale = TRUE)
dtm.test[,2:ncol(dtm.test)] <- scale(dtm.test[,2:ncol(dtm.test)],center = T, scale = T) 
dtm.train[is.nan(dtm.train)]=0
dtm.test[is.nan(dtm.test)]=0
svm.model<-svm(dtm.train[,2:101],dtm.train[,1],type="C",kernel='linear')
model.pred<-predict(svm.model,dtm.test[,2:101])
confusionMatrix(as.numeric(model.pred),dtm.test[,1])

model<-svm(dtm.train[,2:101],dtm.train[,1],type="C",kernel="radial")
model.pred<-predict(svm.model,dtm.test[,2:101])
confusionMatrix(as.numeric(model.pred),dtm.test[,1])