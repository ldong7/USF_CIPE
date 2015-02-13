# import libraries
library(Hmisc)
library(randomForest)
library(caret)

########################################################################################
# sampling function that finds index
########################################################################################
train_sampling <- function(df){ 
  n <- nrow(df)
  
  # choose percentage of data to train
  percent_train <- 0.9   # 90 %
  
  # set sample size of training data from above percentage
  samp_size <- floor(percent_train * n)
 
  # set seed so results are "random", but able to be duplicated across runs
  #set.seed(42)
  
  # specify indices of training data by "randomly" sampling
  train_idx <- sample(seq_len(n), size=samp_size)
  
  return(train_idx)
}
########################################################################################


########################################################################################
# using index to find training set
########################################################################################
train_set <- function(df, train_idx){
  
  # subset data into training set
  df_train <- df[train_idx,]
  
  return(df_train)
}
########################################################################################


########################################################################################
# using index to find testing set
########################################################################################
test_set <- function(df, train_idx){
  
  # subset data into testing sets
  df_test <- df[-train_idx,]

  return(df_test)
}
########################################################################################


########################################################################################
# generate random forest models (samsize will be a vector: c() if used)
########################################################################################
RF_generator <- function(df_train, numtree=500, downsampling=NULL, samsize=NULL){
  print('')
  print('')
  print(paste('Number of trees:', numtree))
  
  # specify features to leave out as predictor variables(leave_out1: permanent; leave_out2: temporary)
  leave_out1 <- c('id', 'term_range', 'major', 'academic_period_admitted', 
                  'graduated','loa',  'academic_disq', 'registered', 'outcome',
                  'admit_type')
  leave_out2 <- c('adjusted_gross_income')
  leave_out <- c(leave_out1, leave_out2)
  
  # create vector of columns which will be treated as predictor variables and print the number of features
  keep <- names(df_train)[which(!names(df_train) %in% leave_out)]
  print(paste('Number of features:', length(keep)))

  # specify formula for predicting outcome based on desired predictor columns
  form <- paste(keep, collapse ='+')
  formula <- as.formula(paste('outcome~',form))
  
  # create random forest model using above formula
  if(is.null(downsampling)){
    print('No downsampling')
    model <- randomForest(formula, 
                          data = df_train, 
                          importance=TRUE,
                          keep.forest=TRUE, 
                          ntree=numtree,
                          #mtry=9,
                          na.action=na.roughfix
                          )    
  }
  else{
    ctrl <- trainControl(method = "cv",
                         classProbs = TRUE,
                         summaryFunction = defaultSummary)
    
    # perform down sampling using minority class, "left"

    if(is.null(samsize)){
      # downsample to the size of the minority class
      nmin <- sum(df_train$outcome == "left")
      print(paste('Downsampling with all three sample sizes equal to the size of the minority class:', nmin))
      model <- randomForest(formula, 
                            data = df_train, 
                            importance=TRUE,
                            keep.forest=TRUE, 
                            ntree=numtree,
                            #mtry=9,
                            na.action=na.roughfix,
                            trControl = ctrl,
                            strata = df_train$outcome,
                            sampsize = c(nmin #(graduated)
                                         ,nmin #(left)
                                         , nmin#(returned)
                                        )
                            )             
    }
    else{
      # downsample with user define ratio
      nmin <- sum(df_train$outcome == "left")
      size1 <- floor(samsize[1]*nmin)
      size2 <- floor(samsize[2]*nmin)
      size3 <- floor(samsize[3]*nmin)
      print(paste('Downsampling to: graduated(', size1, '),', ' left(', size2, '),', ' returned(', size3, ')', sep=''))
      model <- randomForest(formula, 
                            data = df_train, 
                            importance=TRUE,
                            keep.forest=TRUE, 
                            ntree=numtree,
                            #mtry=9,
                            na.action=na.roughfix,
                            trControl = ctrl,
                            strata = df_train$outcome,
                            sampsize = c(size1 #(graduated)
                                         ,size2 #(left)
                                         , size3#(returned)
                                        )
                             )        
    }    
  }  
  return (model)
}
########################################################################################


########################################################################################
# Using model to find accuracy, confusion matrix, out of bag error rates, variable importance
########################################################################################
RF_tester <- function(model,df_test, df_train=NULL, trainname='NA', testname="NA"){
  # plot OOB error rates
  plot(model)
  legend("top", colnames(model$err.rate),col=1:4,cex=0.8,fill=1:4)
  round(head(model$err.rate, 15), 4)
  
  # set plot to default
  par(mfrow=c(1,1))
  
  # variable importance plot
  varImpPlot(model)
  
  print(paste('Model Train on', trainname))
  print('#########################################')
  
  # if input has training set, comute training set statistics as well as testing set
  if(!is.null(df_train)){
    # find training set predictions
    train_predictions <- predict(model,
                                 newdata = df_train,
                                 type = "response")
    # get test set predictions
    test_predictions = predict(model, 
                               newdata = df_test, 
                               type = "response")
    

    print('Training set accuracy and confusion matrix')
    print(paste("Number of NA predictions:", sum( is.na( train_predictions ) )))
    # print training set accuracy
    train_accuracy <- compute_accuracy(train_predictions, df_train$outcome)
    print(paste('Training set accuracy:', train_accuracy)) 
    matrix <- confuse_matrix(train_predictions, df_train$outcome)
    print(matrix)
    print('retention rate accuracy (number of total returned_predict over number of total returned_actual:')
    print(paste(as.character(sum(matrix$returned_predict)),'/', as.character(sum(matrix['returned_actual',])), ':', sum(matrix$returned_predict)/sum(matrix['returned_actual',])))
    print('left rate accuracy (number of total left_predict over number of total left_actual:')
    print(paste(as.character(sum(matrix$left_predict)),'/', as.character(sum(matrix['left_actual',])), ':', sum(matrix$left_predict)/sum(matrix['left_actual',])))
    print('graduated rate accuracy (number of total graduated_predict over number of total graduated_actual:')
    print(paste(as.character(sum(matrix$graduated_predict)),'/', as.character(sum(matrix['graduated_actual',])), ':', sum(matrix$graduated_predict)/sum(matrix['graduated_actual',])))
    
    print('#########################################')
    print('holdout-sample accuracy and confusion matrix')      
    print(paste("Number of NA predictions:", sum( is.na( test_predictions ) )))
    test_accuracy <- compute_accuracy(test_predictions, df_test$outcome)
    print(paste('Test set accuracy:', test_accuracy, sep = ' '))
    matrix <- confuse_matrix(test_predictions, df_test$outcome)
    print(matrix)
    print('retention rate accuracy (number of total returned_predict over number of total returned_actual:')
    print(paste(as.character(sum(matrix$returned_predict)),'/', as.character(sum(matrix['returned_actual',])), ':', sum(matrix$returned_predict)/sum(matrix['returned_actual',])))
    print('left rate accuracy (number of total left_predict over number of total left_actual:')
    print(paste(as.character(sum(matrix$left_predict)),'/', as.character(sum(matrix['left_actual',])), ':', sum(matrix$left_predict)/sum(matrix['left_actual',])))
    print('graduated rate accuracy (number of total graduated_predict over number of total graduated_actual:')
    print(paste(as.character(sum(matrix$graduated_predict)),'/', as.character(sum(matrix['graduated_actual',])), ':', sum(matrix$graduated_predict)/sum(matrix['graduated_actual',])))
        
    print('#########################################') 
  }
  else{
    # get test set predictions
    test_predictions = predict(model, 
                               newdata = df_test, 
                               type = "response")
    
    print(paste('Model Test on', testname, sep=' '))
    print(paste("Number of NA predictions:", sum( is.na( test_predictions ) )))
    test_accuracy <- compute_accuracy(test_predictions, df_test$outcome)
    # print test set accuracy
    print(paste('Test set accuracy:', test_accuracy, sep = ' '))
    matrix <- confuse_matrix(test_predictions, df_test$outcome)
    print(matrix)
    print('retention rate accuracy (number of total returned_predict over number of total returned_actual:')
    print(paste(as.character(sum(matrix$returned_predict)),'/', as.character(sum(matrix['returned_actual',])), ':', sum(matrix$returned_predict)/sum(matrix['returned_actual',])))
    print('left rate accuracy (number of total left_predict over number of total left_actual:')
    print(paste(as.character(sum(matrix$left_predict)),'/', as.character(sum(matrix['left_actual',])), ':', sum(matrix$left_predict)/sum(matrix['left_actual',])))
    print('graduated rate accuracy (number of total graduated_predict over number of total graduated_actual:')
    print(paste(as.character(sum(matrix$graduated_predict)),'/', as.character(sum(matrix['graduated_actual',])), ':', sum(matrix$graduated_predict)/sum(matrix['graduated_actual',])))
        
    print('#########################################') 
  }
  return(test_accuracy)
}
########################################################################################


########################################################################################
# compute accuracy given prediction and actual values
########################################################################################
compute_accuracy <- function(predicted_values, actual_values){
  # put both predict and actual in a data frame and find correct ones
  test_df <- data.frame(predicted_values, actual_values)
  test_df$correct <- 0
  names(test_df) <- c("predicted", "actual", "correct")
  test_df$correct[test_df$predicted == test_df$actual] <- 1
  
  # sum the number of correct predictions
  num_correct <- sum(test_df$correct)
  total <- nrow(test_df)
  
  # compute accuracy over all predictions
  accuracy <- num_correct/total
  
  return(accuracy)
}
########################################################################################


########################################################################################
# generate confusion matrix given prediction and actual values
########################################################################################
confuse_matrix <- function(predicted_values, actual_values){
  test_df <- data.frame(predicted_values, actual_values)
  names(test_df) <- c("predicted", "actual")
  
  # find the number of each case out of a total of 9 cases
  a <- nrow(subset(test_df, test_df$actual == 'returned' & test_df$predicted =='returned'))
  b <- nrow(subset(test_df, test_df$actual == 'returned' & test_df$predicted =='left'))
  c <- nrow(subset(test_df, test_df$actual == 'returned' & test_df$predicted =='graduated'))
  d <- nrow(subset(test_df, test_df$actual == 'left' & test_df$predicted =='returned'))
  e <- nrow(subset(test_df, test_df$actual == 'left' & test_df$predicted =='left'))
  f <- nrow(subset(test_df, test_df$actual == 'left' & test_df$predicted =='graduated'))
  g <- nrow(subset(test_df, test_df$actual == 'graduated' & test_df$predicted =='returned'))
  h <- nrow(subset(test_df, test_df$actual == 'graduated' & test_df$predicted =='left'))
  i <- nrow(subset(test_df, test_df$actual == 'graduated' & test_df$predicted =='graduated'))
  
  # construct matrix
  matrix <- data.frame(returned_predict = c(a,d,g), left_predict = c(b,e,h), graduated_predict = c(c,f,i),
                       row.names=c('returned_actual', 'left_actual', 'graduated_actual'))
  return(matrix)
}
########################################################################################


########################################################################################
# merge the separate two dataframe so they share common factor levels
########################################################################################
train_test_prep1 <- function(df1, df2){
  all_data <- rbind(df1, df2)
  n1 <- nrow(df1)
  df1 <- all_data[1:n1,]
  return(df1)
}
########################################################################################


########################################################################################
# merge the separate two dataframe so they share common factor levels
########################################################################################
train_test_prep2 <- function(df1, df2){
  all_data <- rbind(df1, df2)
  n1 <- nrow(df1)
  df2 <- all_data[(n1+1):nrow(all_data),]
  return(df2)
}
########################################################################################


########################################################################################
# train model and then test it
########################################################################################
run <- function(train, target, trainname='NA', testname="NA", numtree=500, downsampling=NULL, samsize=NULL){
  # merge the separate two dataframe so they share common factor levels
  test <- train_test_prep2(train, target)
  train <- train_test_prep1(train, target)

  # train model and test it
  model <- RF_generator(train, numtree=numtree, downsampling=downsampling, samsize=samsize)
  RF_tester(model, test, trainname=trainname, testname=testname)
}
########################################################################################