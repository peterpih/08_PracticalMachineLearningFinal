#
# Code for Peer Review project in Practical Machine Learning
#
setwd("C:/R/PracticalMachineLearning/")
#
# read in the data
trainFile <- "pml-training.csv"
testFile <- "pml-testing.csv"


train <- read.csv(trainFile, stringsAsFactors=FALSE, na.strings="NA")   # explicitly set NAs
realtest <- read.csv(testFile, stringsAsFactors=FALSE, na.strings="NA")     # explicitly set NAs

# see what data is there
train_names <- names(train)
realtest_names <- names(realtest)

# what is the difference between the two data sets?
length(train_names)
length(realtest_names)
sum(train_names %in% realtest_names)

# the difference between the variables in the two data sets is the last variable "classe" in train
# and "problem_id" in test

exclude_names <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window",
                   "num_window", "problem_id")

# What data is there for the test data?
use_name <- as.character()
for (n in realtest_names){
    if (sum(!is.na(realtest[,n])) > 0){
        if (! n %in% exclude_names){
            use_name <- c(use_name, n)
        }
    }
}

# add back the prediction
use_name <- c("classe", use_name)

# get the relevant columns
newtrain <- train[, use_name]

# split into training and test data sets
set.seed(123)
inTrain <- createDataPartition(y=newtrain$classe, p=0.7, list=FALSE)

newtrain_train <- newtrain[inTrain, ]
newtrain_test <- newtrain[-inTrain, ]

t <- data.frame()
names <- names(newtrain_train)
for (n in names){
    t <- rbind(t, data.frame(name=n, count=length(unique(newtrain_train[,n]))))
}


w_train <- train(classe ~ ., data=newtrain_train, method="rt")

model <- train(mpg ~ wt, data=mtcars, method="glm")

q<-subset(train, classe=="A")
unique(q$classe)
unique(q$user_name)

train_pedro <- subset(q, q$user_name=="pedro")
unique(train_pedro$classe)
unique(train_pedro$user_name)