#############################
# < Your Name Here >
# STAT W4240 
# Homework 04 
# < Homework Due Date >
#
# The following code analyzes the federalist papers
#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine


# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.
# Use the package installer and be sure to install all dependencies
library(tm)

#################
# Problem 1a
#################

##########################################
# This code uses tm to preprocess the papers into a format useful for NB
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
	for (i in 1:length(fp)){
		# make all words lower case
		fp[i] = tm_map( fp[i] , tolower);
		# remove all punctuation
		fp[i] = tm_map( fp[i] , removePunctuation);
		# remove stopwords like the, a, and so on.	
		fp[i] = tm_map( fp[i], removeWords, stopwords("english"));
		# remove stems like suffixes
		fp[i] = tm_map( fp[i], stemDocument)
		# remove extra whitespace
		fp[i] = tm_map( fp[i], stripWhitespace)	
	}
	# now write the corpus out to the files for our future use.
	# MAKE SURE THE _CLEAN DIRECTORY EXISTS
	writeCorpus( fp , sprintf('%s_clean',dirname) )
}
##########################################

#################
# Problem 1b
#################

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

#################
# Problem 1c
#################

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

#################
# Problem 1d
#################

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

#################
# Problem 1e
#################

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



#################
# End of Script
#################


