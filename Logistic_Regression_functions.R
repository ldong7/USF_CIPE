# import libraries
library(Hmisc)
library(caret)
library(base)

########################################################################################
# subsetting data based on terms attended and admittype if necessary
########################################################################################
breakdown <- function(data, termsattended, admittype=NULL){
  if(!is.null(admittype)){
    # subset admit type first
    student <- subset(data, data$admissions_population_desc == admittype)
  }
  else{
    student <- data
  }

  # subset based on terms attended
  student <- subset(student, student$terms_attended == termsattended)
  return(student)
}
########################################################################################


########################################################################################
# generate logistic regression model with data, leave out feature if necessary, and add on feature(interaction terms) if necessary
########################################################################################
LG_generator <- function(data, leaveout=c(), add=c()){
  print('#######################################################################')
  # permannet leave out features
  leave_out1 <- c('id', 'term_range', 'major', 'academic_period_admitted', 
                  'graduated','loa',  'academic_disq', 'registered', 'outcome', 'terms_attended',
                  'admit_type')
  # temporary leave out features
  leave_out2 <- c('adjusted_gross_income')
  # user define leave out features
  leave_out3 <- leaveout 
  # all leave out features
  leave_out <- c(leave_out1, leave_out2, leave_out3)
  
  # create vector of columns which will be treated as predictor variables:
  keep <- names(data)[which(!names(data) %in% leave_out)]
  
  # specify formula for predicting outcome based on desired predictor columns:
  form <- paste(keep, collapse ='+')

  # incorporate add on features if necessary for example('a:b') and generate formula
  if(!is.null(add)){
    addform <- paste(add, collapse = '+')
    formaddform <- paste(form, addform, sep='+')
    formula <- as.formula(paste('outcome~',formaddform))
    print(formula)    
  }
  else{
    formula <- as.formula(paste('outcome~',form))
    print(formula)        
  }
  
  # generate logistic regression model
  model <- glm(formula, data=data, family= "binomial")  
  return(model)  
}
########################################################################################


########################################################################################
# print summary of the model (can add different output if necessary)
########################################################################################
LG_tester <- function(model){
  print('#######################################################################')
  print(summary(model))
  print('#######################################################################')
  print('                                                                       ')
}
########################################################################################