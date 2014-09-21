library(caret)

# Download the training and question data
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "pml-training.csv", method="curl")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "pml-testing.csv", method="curl")

# Load the training and testing data
projectData <- read.csv("pml-training.csv", na.strings = c("NA", "", "#DIV/0!"))
projectQuestions <- read.csv("pml-testing.csv", na.strings = c("NA", "", "#DIV/0!"))

# Find NA variables
empty <- subset(names(projectQuestions), 
                apply(projectQuestions, 2, function(x){ all(is.na(x) | x == 0)}))
# Define useless variables: NA's and user_name, raw_timestamp_part_1, 
# raw_timestamp_part_2, cvtd_timestamp, new_window
useless <- c(empty, "X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
             "cvtd_timestamp", "new_window", "num_window")

# Data Cleaning: remove useless empty columns in both sets
projectData <- projectData[, !names(projectData) %in% useless]
projectQuestions <- projectQuestions[, !names(projectQuestions) %in% useless]

#{gyros,accel,magnet}_{belt,arm,dumbbell,forearm}_{x,y,z}
#{roll,yaw,pitch,total_accel}_{belt,arm,dumbbell,forearm}

set.seed(1234)
# Because the testing data had no classe variable, we split the 
# pml-training set in a test set and a training set
inTrain <- createDataPartition(projectData$classe, p=0.75, list=F)
training <- projectData[inTrain,]
testing <- projectData[-inTrain,]

# Create a model using a random forest with 300 trees
modelFit <- randomForest(classe ~ ., data=training, ntree=300)
# Predict the outcome of the test set using the model
pred <- predict(modelFit, newdata=testing)
# Validate the outcome op the prediction of the test set
confusionMatrix(pred, testing$classe)

# The Accuracy of the model is 99,8% which is enough to use it 
# for predicting the outcome of the project questions

# Predict the classe for 20 project questions
finalPred <- predict(modelFit, newdata=projectQuestions)
