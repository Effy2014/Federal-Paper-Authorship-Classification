setwd("~/Desktop/Federal-Paper-Authorship-Classification")
##########################################
make.log.pvec <- function(dtm,mu){
    # Sum up the number of instances per word
    pvec.no.mu <- colSums(dtm)
    # Sum up number of words
    n.words <- sum(pvec.no.mu)
    # Get dictionary size
    dic.len <- length(pvec.no.mu)
    # Incorporate mu and normalize
    log.pvec <- log(pvec.no.mu + mu) - log(mu*dic.len + n.words)
    return(log.pvec)
}
##########################################
mu=100/dim(dictionary)[1]
logp.hamilton.train <- make.log.pvec(dtm.hamilton.train,mu)
logp.hamilton.test <- make.log.pvec(dtm.hamilton.test,mu)
logp.madison.test <-make.log.pvec(dtm.madison.test,mu)
logp.madison.train <- make.log.pvec(dtm.madison.train,mu)

naive.bayes = function(logp.hamilton.train, logp.madison.train,log.prior.hamilton,log.prior.madison,dtm.test){
    class.test=vector()
    for(i in 1:nrow(dtm.test)){
         post.hamilton = log.prior.hamilton+sum(dtm.test[i,]*logp.hamilton.train)
         post.madison=log.prior.hamilton+sum(dtm.test[i,]*logp.madison.train)
         class.test[i]<-ifelse(post.hamilton>=post.madison,1,0)
    }
    return(class.test)
}

log.prior.hamilton=log(nrow(dtm.hamilton.train)/(nrow(dtm.madison.train)+nrow(dtm.hamilton.train)))
log.prior.madison=log(nrow(dtm.madison.train)/(nrow(dtm.madison.train)+nrow(dtm.hamilton.train)))
result_hamilton<-naive.bayes(logp.hamilton.train,logp.madison.train,log.prior.hamilton,log.prior.madison,dtm.hamilton.test)
result_madison<-naive.bayes(logp.hamilton.train,logp.madison.train,log.prior.hamilton,log.prior.madison,dtm.madison.test)
true_positive <- sum(result_hamilton==1)/length(result_hamilton)
true_negative <- sum(result_madison==0)/length(result_madison)
false_positive <- 1-true_negative
false_negative <- 1-true_positive

#####finding the best tunable parameter mu. Cross validation is used.
nb <- function(dtm.hamilton.train,dtm.madison.train,mu){
    correctly_classified <- vector()
    false_negative <- vector()
    false_positive <- vector()
    hamilton_index<-sample(rep(1:5,nrow(dtm.hamilton.train)/5))
    madison_index<-sample(rep(1:5,nrow(dtm.madison.train)/5))
    for(i in 1:5){
        hamilton.test.sample <- dtm.hamilton.train[hamilton_index==i,]
        hamilton.train.sample <- dtm.hamilton.train[hamilton_index!=i,]
        madison.train.sample<-dtm.madison.train[madison_index!=i,]
        madison.test.sample<-dtm.madison.train[madison_index==i,]
        
        logp.hamilton.train <- make.log.pvec(hamilton.train.sample,mu)
        logp.hamilton.test <- make.log.pvec(hamilton.test.sample,mu)
        logp.madison.test <-make.log.pvec(madison.test.sample,mu)
        logp.madison.train <- make.log.pvec(madison.train.sample,mu)
        
        log.prior.hamilton=log(nrow(hamilton.train.sample)/(nrow(madison.train.sample)+nrow(hamilton.train.sample)))
        log.prior.madison=log(nrow(madison.train.sample)/(nrow(madison.train.sample)+nrow(hamilton.train.sample)))
        result_hamilton<-naive.bayes(logp.hamilton.train,logp.madison.train,log.prior.hamilton,log.prior.madison,hamilton.test.sample)
        result_madison<-naive.bayes(logp.hamilton.train,logp.madison.train,log.prior.hamilton,log.prior.madison,madison.test.sample)
        
        correctly_classified[i]=(sum(result_hamilton==1)+sum(result_madison==0))/(length(result_hamilton)+length(result_madison))
        false_positive[i]=1-sum(result_madison==0)/length(result_madison)
        false_negative[i]=1-sum(result_hamilton==1)/length(result_hamilton)
    }
    result=c(mean(correctly_classified),mean(false_negative),mean(false_positive))
    return(result)
}
mu_vec = c(1,10,100,1000,10000)*1/nrow(dictionary)
Res=vector()
for (i in mu_vec){
    res=nb(dtm.hamilton.train,dtm.madison.train,i)
    Res=rbind(Res,res)
}

#for each value of mu, train on the full training dataset and test on full testing dataset
result=vector()
for (i in c(1:length(mu_vec))){
    mu=mu_vec[i]
    logp.hamilton.train <- make.log.pvec(dtm.hamilton.train,mu)
    logp.hamilton.test <- make.log.pvec(dtm.hamilton.test,mu)
    logp.madison.test <-make.log.pvec(dtm.madison.test,mu)
    logp.madison.train <- make.log.pvec(dtm.madison.train,mu)
    
    log.prior.hamilton=log(nrow(dtm.hamilton.train)/(nrow(dtm.madison.train)+nrow(dtm.hamilton.train)))
    log.prior.madison=log(nrow(dtm.madison.train)/(nrow(dtm.madison.train)+nrow(dtm.hamilton.train)))
    result_hamilton<-naive.bayes(logp.hamilton.train,logp.madison.train,log.prior.hamilton,log.prior.madison,dtm.hamilton.test)
    result_madison<-naive.bayes(logp.hamilton.train,logp.madison.train,log.prior.hamilton,log.prior.madison,dtm.madison.test)
    correctly_classified=(sum(result_hamilton==1)+sum(result_madison==0))/(length(result_hamilton)+length(result_madison))
    true_positive <- sum(result_hamilton==1)/length(result_hamilton)
    true_negative <- sum(result_madison==0)/length(result_madison)
    false_positive <- 1-true_negative
    false_negative <- 1-true_positive
    result=rbind(result,c(correctly_classified,false_negative,false_positive))
}

#################
# End of Script
#################