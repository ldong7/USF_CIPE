################################################################################################
# data import section
################################################################################################

# import libraries and fucntions
source('data_cleaning_functions.R')
source('Logistic_Regression_functions.R')
library(Hmisc)
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

# combine datasets
fall <- rbind(cf11, cf12, cf13)
spring <- rbind(cs12, cs13, cs14)
all <- rbind(fall, spring)

# subsetting data based on number of terms attended
first <- breakdown(all, c(1,2))
second <- breakdown(all, c(2))
third_fall <- breakdown(fall, c(3,4))
third_spring <- breakdown(spring, c(3,4))


print('1. first-to-second year retention')
# generate model and test it(both functions have arguments input that can be used)
model1 <- LG_generator(first)
LG_tester(model1)

print('2. students in their second term at USF')
# generate model and test it(both functions have arguments input that can be used)
model2 <- LG_generator(second)
LG_tester(model2)

print('3. information as of the (second-year) fall semester')
# generate model and test it(both functions have arguments input that can be used)
model3 <- LG_generator(third_fall)
LG_tester(model3)

print('4. information as of the (second-year) spring semester')
# generate model and test it(both functions have arguments input that can be used)
model4 <- LG_generator(third_spring)
LG_tester(model4)
