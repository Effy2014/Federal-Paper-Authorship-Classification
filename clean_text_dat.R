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