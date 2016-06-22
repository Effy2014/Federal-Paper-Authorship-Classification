library(glmnet)
library(caret)
dtm.train[,2:ncol(dtm.train)] <- scale(dtm.train[,2:ncol(dtm.train)] , center = TRUE, scale = TRUE)
dtm.test[,2:ncol(dtm.test)] <- scale(dtm.train[,2:ncol(dtm.test)],center = T, scale = T) 
dtm.train[is.nan(dtm.train)]=0
dtm.test[is.nan(dtm.test)]=0
#using ridge regression model on the training data
colnames(dtm.train)<-c("index.train",as.vector(dictionary$word))
fit.ridge <- cv.glmnet(dtm.train[,2:ncol(dtm.train)],dtm.train[,1], family="binomial", alpha = 0, standardize=F)
plot(fit.ridge)
pred.ridge<-predict(fit.ridge,dtm.test[,2:ncol(dtm.test)],s="lambda.min",type="class")
confusionMatrix(as.numeric(pred.ridge),dtm.test[,1])
coef.ridge<-coef(fit.ridge, s="lambda.min")	
coef.ridge[order(abs(coef.ridge[,1]),decreasing = T),][1:11]
#using Lasso regression model on the training data 
fit.lasso <- cv.glmnet(dtm.train[,2:ncol(dtm.train)],dtm.train[,1], family="binomial", alpha = 1, standardize=F)
plot(fit.lasso)
pred.lasso<-predict(fit.lasso,dtm.test[,2:ncol(dtm.test)],s="lambda.min",type="class")
confusionMatrix(as.numeric(pred.lasso),dtm.test[,1])
coef.lasso<-coef(fit.lasso, s="lambda.min")	
coef.lasso[order(abs(coef.lasso[,1]),decreasing = T),][1:11]
