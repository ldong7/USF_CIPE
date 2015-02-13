################################################################################################
# data import section
################################################################################################
# import libraries and fucntions
source('data_cleaning_functions.R')
source('Random_Forest_Functions.R')
library(Hmisc)
library(randomForest)
library(caret)
library(base)

# these are the columns from the original data sets that need to be changed:
column <- c("FAFSA_FAMILY_CONTRIBUTION", 
            "INST_MERIT_SCHOLAR_AMOUNT", 
            "INST_NEED_SCHOLAR_AMOUNT",
            "FED_PELL_GRANT_AMOUNT",
            "CAL_GRANT_AMOUNT")

# import data
dorm <- read.csv('../raw_data/ID_TERM_DORM.csv', na.strings=c(""," ","NA"))
gender <- read.csv('../raw_data/ID_GENDER.csv', na.strings=c(""," ","NA"))
income <- read.csv('../raw_data/Enrollment Data 12-07-2014 Additional Fields All terms.csv', na.strings=c(""," ","NA"))
add <- read.csv('../raw_data/Enrollment Data 12-07-2014 Additional Fields All terms2.csv', na.strings=c(""," ","NA"))


# import data and clean data
f11 <- read.csv('../raw_data/Enrollment Dataset 201140_201240.csv', na.strings=c(""," ","NA"))
cf11 <- cleaning(f11, column, dorm, gender, income, add)
s12 <- read.csv('../raw_data/Enrollment Dataset 201220_201240.csv', na.strings=c(""," ","NA"))
cs12 <- cleaning(s12, column, dorm, gender,income, add)
f12 <- read.csv('../raw_data/Enrollment Dataset 201240_201340.csv', na.strings=c(""," ","NA"))
cf12 <- cleaning(f12, column, dorm, gender, income, add)
s13 <- read.csv('../raw_data/Enrollment Dataset 201320_201340.csv', na.strings=c(""," ","NA"))
cs13 <- cleaning(s13, column, dorm, gender, income, add)
f13 <- read.csv('../raw_data/Enrollment Dataset 201340_201440.csv', na.strings=c(""," ","NA"))
cf13 <- cleaning(f13, column, dorm, gender, income, add)
s14 <- read.csv('../raw_data/Enrollment Dataset 201420_201440.csv', na.strings=c(""," ","NA"))
cs14 <- cleaning(s14, column, dorm, gender, income ,add)
################################################################################################


################################################################################################
# final model training section, and predict using new data(not yet given)
################################################################################################
# choose combine dataset to use
all <- rbind(cs12, cs13, cs14, cf11, cf12, cf13) 

# define whether to down_sample or not and the number of tree use
down_samp = NULL
numt = 500

# train model, function has arguments input that can be used)
model <- RF_generator(all, numtree=numt, downsampling=down_samp)

# predict using test data(new data)
test_predictions = predict(model, newdata = test_data, type = "response")
################################################################################################


################################################################################################
# Cross Validation Section single iteration
################################################################################################
# combine datasets
all <- rbind(cs12, cs13, cs14) #, cf11, cf12, cf13

# compute random sample index and split data
train_idx <- train_sampling(all)
data_train <- train_set(all,train_idx)
data_test <- test_set(all,train_idx)

# define whether to down_sample or not and the number of tree use
down_samp = NULL
numt = 500
trname = '90% of random split of Spring Semesters'
tename = '10% of remaining split of Spring Semesters'

# train model
model <- RF_generator(data_train, numtree=numt, downsampling=down_samp)

# test model 
test_accuracy <- RF_tester(model, data_test, data_train, trainname=trname, testname=tename)

# predict using test data
test_predictions = predict(model, newdata = data_test, type = "response")

# put both prediction and true value in a data frame to compare
test_df <- data.frame(data_test, test_predictions)

# find certain error and write to file
a <- subset(test_df, test_df$outcome == 'returned' & test_df$test_predictions =='graduated')
write.csv(a, file='predict_graduated_actual_returned.csv', row.names=FALSE)
b <- subset(test_df, test_df$outcome == 'left' & test_df$test_predictions =='graduated')
write.csv(b, file='predict_graduated_actual_left.csv', row.names=FALSE)
c <- subset(test_df, test_df$outcome == 'graduated' & test_df$test_predictions =='returned')
write.csv(c, file='predict_returned_actual_graduated.csv', row.names=FALSE)
d <- subset(test_df, test_df$outcome == 'graduated' & test_df$test_predictions =='left')
write.csv(d, file='predict_left_actual_graduated.csv', row.names=FALSE)
e <- subset(test_df, test_df$outcome == 'graduated' & test_df$test_predictions =='graduated')
write.csv(e, file='predict_graduated_actual_graduated.csv', row.names=FALSE)
################################################################################################


################################################################################################
# Cross Validation n iterations and find average accuracy
################################################################################################
# choose combine dataset to use
all <- rbind(cs12, cs13, cs14, cf11, cf12, cf13) 

n <- 2

# initialize empty vector
acc <- c()

# define whether to down_sample or not, the number of tree use, training set name, testing name
down_samp = NULL
numt = 500
trname = '90% of all six sets combined'
tename = '10% of all six sets combined'

# for loop finding the tesing set accuracy for each fold
for(i in 1:n){
	# compute random sample index and split data
	train_idx <- train_sampling(all)
	data_train <- train_set(all,train_idx)
	data_test <- test_set(all,train_idx)
  
	# train model, find accuracy on test set and append to vector
  model <- RF_generator(data_train, numtree=numt, downsampling=down_samp)
  test_accuracy <- RF_tester(model, data_test, data_train, trainname=trname, testname=tename)
  acc <- append(acc, test_accuracy)  
}

# display average accuracy
print(paste('accuracy of ', as.character(n), 'different random samples:'))
print(acc)
print(paste('average accuracy of 10 different random samples:', mean(acc)))
################################################################################################


################################################################################################
# indiviudal dataset exploration
################################################################################################
# input: training set, testing set, traningset name, testing set name, number of tree, and downsampling or not
down_samp = NULL
numt = 500
trname = 'Fall 2011'

# train on fall 2011 and test on all other dataset
run(cf11, cf12, trainname=trname, testname='Fall 2012', numtree=numt, downsampling=down_samp)
run(cf11, cf13, trainname=trname, testname='Fall 2013', numtree=numt, downsampling=down_samp)
run(cf11, cs12, trainname=trname, testname='Spring 2012', numtree=numt, downsampling=down_samp)
run(cf11, cs13, trainname=trname, testname='Spring 2013', numtree=numt,downsampling=down_samp)
run(cf11, cs14, trainname=trname, testname='Spring 2014', numtree=numt, downsampling=down_samp)
################################################################################################