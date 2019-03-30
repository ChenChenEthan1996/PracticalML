```{r warning=FALSE, error=FALSE}
library(caret)
library(randomForest) #Random forest for classification and regression
library(rpart) # Regressive Partitioning and Regression trees
library(rpart.plot) # Decision Tree plot
```

```{r warning=FALSE, error=FALSE}
# setting the overall seed for reproduceability
set.seed(6666)
```

```{r warning=FALSE, error=FALSE}
# Loading the training data set and testing data while replacing all missing with "NA"
trainingset <- read.csv("C:/Users/Chen Chen/Desktop/8、Practical Machine Learning/pml-training.csv", na.strings=c("NA","#DIV/0!", ""))
testingset <- read.csv("C:/Users/Chen Chen/Desktop/8、Practical Machine Learning/pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))
```

```{r warning=FALSE, error=FALSE}
# Check dimensions for number of variables and number of observations
dim(trainingset)
dim(testingset)
```

```{r warning=FALSE, error=FALSE}
# Columns with all missing values in the two data sets were deleted.  
trainingset<-trainingset[,colSums(is.na(trainingset)) == 0]
testingset <-testingset[,colSums(is.na(testingset)) == 0]
```

```{r warning=FALSE, error=FALSE}
# Deleted variables that are irrelevant with prediction (x, user_name, raw_timestamp_part_1, raw_timestamp_part_,2 cvtd_timestamp, new_window, and  num_window, that are columns 1 to 7).
trainingset <- trainingset[,-c(1:7)]
testingset <- testingset[,-c(1:7)]
```

```{r warning=FALSE, error=FALSE}
# and have a look at our new datasets:
dim(trainingset)
dim(testingset)
head(trainingset)
head(testingset)
```

```{r warning=FALSE, error=FALSE}
# To perform cross-validation, the training data set is partionned into 2 sets: subTraining (75%) and subTest (25%). This will be performed using random subsampling without replacement.
subsamples <- createDataPartition(y=trainingset$classe, p=0.75, list=FALSE)
subTraining <- trainingset[subsamples, ] 
subTesting <- trainingset[-subsamples, ]
dim(subTraining)
dim(subTesting)
head(subTraining)
head(subTesting)
```

```{r warning=FALSE, error=FALSE}
# Visulized the data.
plot(subTraining$classe, col="red", main="Bar Plot of levels of the variable classe within the subTraining data set", xlab="classe levels", ylab="Frequency")
```

```{r warning=FALSE, error=FALSE}
# Used Decision Tree as the first prediction model.
DT <- rpart(classe ~ ., data=subTraining, method="class")

DT

# Prediction using the first model.
prediction_01 <- predict(DT, subTesting, type = "class")
```

```{r warning=FALSE, error=FALSE}
# Plot of the Decision Tree.
rpart.plot(DT, main="Classification Tree", extra=102, under=TRUE, faclen=0)
```

```{r warning=FALSE, error=FALSE}
# Tested results on our subTesting data set.
confusionMatrix(prediction_01, subTesting$classe)
```

```{r warning=FALSE, error=FALSE}
# Used Random Forest as the second prediction model. 
RF <- randomForest(classe ~. , data=subTraining, method="class")
```

```{r warning=FALSE, error=FALSE}
# Prediction using the RF model.
prediction_02 <- predict(RF, subTesting, type = "class")
```

```{r warning=FALSE, error=FALSE}
# Tested results on subTesting data set.
confusionMatrix(prediction_02, subTesting$classe)
```
