# import libraries
library(utils)
library(base)

# name diretory
diretory <- "Output/"
# create diretory if not exist
dir.create(diretory)

# create filename using datetime
filename <- format(Sys.time(),"%Y-%m-%d_%H:%M:%S")
filename <- as.character(filename)
filename <- paste(diretory, filename, '_output.txt', sep='')

# create file using file name
sink(filename)

# start timer
ptm <- proc.time()

# run other codes
# source('Logistic_Regression.R')
source('Random_Forest.R')

# compute and display runtime
time <- proc.time()-ptm
names(time) <- NULL
runtime <- paste('Runtime (in seconds): ', time[1])
print(runtime)

# close file input
sink()

# open file in the system
system(paste('open', filename))

# announce completion
system(paste('say "Your output file', 'is ready and opened for you"'))
