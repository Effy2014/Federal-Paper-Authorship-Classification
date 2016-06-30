# Federal Paper Authorship Classification

* Cleaning the dataset, storing the data in the form of tokens, building a dictionary containing document number, word number and count for that word-document combination. 
 
The size of dictionary is 4875*2. 

The size of training dataset is 50*4875. 

The size of test dataset is 27*4875.

* Building a Naive Bayes classifier; using cross validation finding the best tunable parameter mu for Naive Bayes.

* Using decision tree with Gini impurity cofficient splits and information gain splits.  

* Centered and scaled document term matrices were used for regularized logistic regression. The data is regularized because p>n for this data.

* Mutual information is used to select features

* SVM with linear and Gaussian kernel is used to classify authorship
