setwd("~/Desktop/Federal-Paper-Authorship-Classification")
#install.packages("tm")
library(tm)
library(SnowballC)
# This code uses tm to preprocess the papers into a format useful for NB
## 1) remove non-letter characters, 2)remove stopwords, and 3)remove stem words
preprocess.directory = function(dirname){
    # the directory must have all the relevant text files
    ds = DirSource(dirname)
    # Corpus will make a tm document corpus from this directory
    fp = Corpus( ds )
    # inspect to verify
    # inspect(fp[1])
    # another useful command
    # identical(fp[[1]], fp[["Federalist01.txt"]])
    # now let us iterate through and clean this up using tm functionality
    # make all words lower case
    fp = tm_map( fp , content_transformer(tolower));
    # remove all punctuation
    fp = tm_map( fp, removePunctuation);
    # remove stopwords like the, a, and so on. 
    fp = tm_map( fp, removeWords, stopwords("english"));
    # remove stems like suffixes
    fp = tm_map( fp, stemDocument)
    # remove extra whitespace
    fp = tm_map( fp, stripWhitespace)
    # now write the corpus out to the files for our future use.
    # MAKE SURE THE _CLEAN DIRECTORY EXISTS
    writeCorpus( fp , sprintf('%s_clean',dirname), filenames = names(fp))
}
##########################################

preprocess.directory("fp_hamilton_test")
preprocess.directory('fp_hamilton_train')
preprocess.directory('fp_madison_test')
preprocess.directory('fp_madison_train')

##########################################
# To read in data from the directories:
# Partially based on code from C. Shalizi
read.directory <- function(dirname) {
    # Store the infiles in a list
    infiles = list();
    # Get a list of filenames in the directory
    filenames = dir(dirname,full.names=TRUE);
    for (i in 1:length(filenames)){
        infiles[[i]] = scan(filenames[i],what="",quiet=TRUE);
         }
    return(infiles)
}
##########################################
hamilton.test<-read.directory("fp_hamilton_test_clean")
hamilton.train<-read.directory("fp_hamilton_train_clean")
madison.test<-read.directory("fp_madison_test_clean")
madison.train<-read.directory("fp_madison_train_clean")
##########################################
# Make dictionary sorted by number of times a word appears in corpus 
# (useful for using commonly appearing words as factors)
# NOTE: Use the *entire* corpus: training, testing, spam and ham
make.sorted.dictionary.df <- function(infiles){
    # This returns a dataframe that is sorted by the number of times 
    # a word appears
    
    # List of vectors to one big vetor
    dictionary.full <- unlist(infiles) 
    # Tabulates the full dictionary
    tabulate.dic <- tabulate(factor(dictionary.full)) 
    # Find unique values
    dictionary <- unique(dictionary.full) 
    # Sort them alphabetically
    dictionary <- sort(dictionary)
    dictionary.df <- data.frame(word = dictionary, count = tabulate.dic)
    sort.dictionary.df <- dictionary.df[order(dictionary.df$count,decreasing=TRUE),];
    return(sort.dictionary.df)
}
##########################################
dictionary <- make.sorted.dictionary.df(c(hamilton.train,hamilton.test,madison.train,madison.test))


##########################################
# Make a document-term matrix, which counts the number of times each 
# dictionary element is used in a document
make.document.term.matrix <- function(infiles,dictionary){
    # This takes the text and dictionary objects from above and outputs a 
    # document term matrix
    num.infiles <- length(infiles);
    num.words <- nrow(dictionary);
    # Instantiate a matrix where rows are documents and columns are words
    dtm <- mat.or.vec(num.infiles,num.words); # A matrix filled with zeros
    for (i in 1:num.infiles){
        num.words.infile <- length(infiles[[i]]);
        infile.temp <- infiles[[i]];
        for (j in 1:num.words.infile){
            ind <- which(dictionary == infile.temp[j])[[1]];
            # print(sprintf('%s,%s', i , ind))
            dtm[i,ind] <- dtm[i,ind] + 1;
        }
    }
return(dtm);
}
##########################################
dtm.hamilton.test<-make.document.term.matrix(hamilton.test,dictionary)
dtm.madison.test <- make.document.term.matrix(madison.test,dictionary)
dtm.hamilton.train<-make.document.term.matrix(hamilton.train,dictionary)
dtm.madison.train<-make.document.term.matrix(madison.train,dictionary)

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


