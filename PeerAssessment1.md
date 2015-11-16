# Practical Machine Learmning: Prediction Assignment Writeup


### 1. Loading training data, split the data on traing and cross validation sets.
60% of data - traingng set
40% of data - cross validation set

```r
# Loading traing data
data<-read.table("pml-training.csv", head=TRUE, sep=",")
inTrain <- createDataPartition( data$classe, p=0.6, list = FALSE)
training<-data[inTrain,]
crossval<-data[-inTrain,]
```
**How much rows are in original, training and cross validation sets?**

```r
dim(data)
```

```
## [1] 19622   160
```

```r
dim(training)
```

```
## [1] 11776   160
```

```r
dim(crossval)
```

```
## [1] 7846  160
```


### 2. Choose predictor columns
There are 160 columns (featues) in original data set. This is too much for building any prediction model. We would like to drop some columns, i.e. not use them for prediction.  
_**What columns we would like to drop?**_  
1. Columns which contain reference information (the name of actor, time of measurement, number of row, etc.)  
2. Columns which contains close to zero values.  
3. Character columns  

### 2.1. Reference information
_**Will skip columnts the following columns**_  
user_name  
raw_timestamp_part_1  
raw_timestamp_part_2  
cvtd_timestamp  
new_window  
num_window  


### 2.2. Find and remove zero- and near-zero columnts
We will cut off zero and near zero columns.

```r
nzv <- nearZeroVar(training, saveMetrics = TRUE)
nzv[nzv$nzv,]
```

```
##                           freqRatio percentUnique zeroVar  nzv
## new_window                 49.54077    0.01698370   FALSE TRUE
## kurtosis_roll_belt       2885.75000    1.96161685   FALSE TRUE
## kurtosis_picth_belt       524.68182    1.64741848   FALSE TRUE
## kurtosis_yaw_belt          49.54077    0.01698370   FALSE TRUE
## skewness_roll_belt       2885.75000    1.94463315   FALSE TRUE
## skewness_roll_belt.1      524.68182    1.72384511   FALSE TRUE
## skewness_yaw_belt          49.54077    0.01698370   FALSE TRUE
## max_yaw_belt              607.52632    0.43308424   FALSE TRUE
## min_yaw_belt              607.52632    0.43308424   FALSE TRUE
## amplitude_yaw_belt         51.99550    0.03396739   FALSE TRUE
## avg_roll_arm               47.00000    1.58797554   FALSE TRUE
## stddev_roll_arm            47.00000    1.58797554   FALSE TRUE
## var_roll_arm               47.00000    1.58797554   FALSE TRUE
## avg_pitch_arm              47.00000    1.58797554   FALSE TRUE
## stddev_pitch_arm           47.00000    1.58797554   FALSE TRUE
## var_pitch_arm              47.00000    1.58797554   FALSE TRUE
## avg_yaw_arm                47.00000    1.58797554   FALSE TRUE
## stddev_yaw_arm             49.00000    1.57099185   FALSE TRUE
## var_yaw_arm                49.00000    1.57099185   FALSE TRUE
## kurtosis_roll_arm         245.59574    1.59646739   FALSE TRUE
## kurtosis_picth_arm        235.57143    1.57948370   FALSE TRUE
## kurtosis_yaw_arm         1923.83333    1.94463315   FALSE TRUE
## skewness_roll_arm         245.59574    1.59646739   FALSE TRUE
## skewness_pitch_arm        235.57143    1.57948370   FALSE TRUE
## skewness_yaw_arm         1923.83333    1.94463315   FALSE TRUE
## kurtosis_roll_dumbbell   3847.66667    1.96161685   FALSE TRUE
## kurtosis_picth_dumbbell 11543.00000    1.98709239   FALSE TRUE
## kurtosis_yaw_dumbbell      49.54077    0.01698370   FALSE TRUE
## skewness_roll_dumbbell   3847.66667    1.96161685   FALSE TRUE
## skewness_pitch_dumbbell  5771.50000    1.96161685   FALSE TRUE
## skewness_yaw_dumbbell      49.54077    0.01698370   FALSE TRUE
## max_yaw_dumbbell         1049.36364    0.50951087   FALSE TRUE
## min_yaw_dumbbell         1049.36364    0.50951087   FALSE TRUE
## amplitude_yaw_dumbbell     50.18696    0.02547554   FALSE TRUE
## kurtosis_roll_forearm     262.34091    1.60495924   FALSE TRUE
## kurtosis_picth_forearm    262.34091    1.62194293   FALSE TRUE
## kurtosis_yaw_forearm       49.54077    0.01698370   FALSE TRUE
## skewness_roll_forearm     262.34091    1.61345109   FALSE TRUE
## skewness_pitch_forearm    262.34091    1.61345109   FALSE TRUE
## skewness_yaw_forearm       49.54077    0.01698370   FALSE TRUE
## max_yaw_forearm           262.34091    0.35665761   FALSE TRUE
## min_yaw_forearm           262.34091    0.35665761   FALSE TRUE
## amplitude_yaw_forearm      61.07407    0.02547554   FALSE TRUE
## stddev_roll_forearm        47.00000    1.58797554   FALSE TRUE
## var_roll_forearm           47.00000    1.58797554   FALSE TRUE
## avg_pitch_forearm          44.00000    1.61345109   FALSE TRUE
## stddev_pitch_forearm       22.00000    1.60495924   FALSE TRUE
## var_pitch_forearm          44.00000    1.61345109   FALSE TRUE
## avg_yaw_forearm            44.00000    1.61345109   FALSE TRUE
## stddev_yaw_forearm         44.00000    1.61345109   FALSE TRUE
## var_yaw_forearm            44.00000    1.61345109   FALSE TRUE
```



### 3. List of columns will be used for building fit model

```r
predictors <- c(
  "roll_belt", "pitch_belt",  "yaw_belt",
  "gyros_belt_x",  "gyros_belt_y",  "gyros_belt_z",
  "accel_belt_x",  "accel_belt_y",	"accel_belt_z",
  "magnet_belt_x",  "magnet_belt_y",	"magnet_belt_z",
  "roll_arm",  "pitch_arm",	"yaw_arm",
  "gyros_arm_x",  "gyros_arm_y",	"gyros_arm_z",
  "accel_arm_x",  "accel_arm_y",	"accel_arm_z",
  "magnet_arm_x",	"magnet_arm_y",	"magnet_arm_z",
  "roll_dumbbell",  "pitch_dumbbell",	"yaw_dumbbell",
  "gyros_dumbbell_x", "gyros_dumbbell_y",	"gyros_dumbbell_z",
  "accel_dumbbell_x", "accel_dumbbell_y",	"accel_dumbbell_z",	
  "magnet_dumbbell_x", "magnet_dumbbell_y", "magnet_dumbbell_z",
  "roll_forearm", "pitch_forearm", "yaw_forearm",
  "gyros_forearm_x", "gyros_forearm_y", "gyros_forearm_z",
  "accel_forearm_x", "accel_forearm_y", "accel_forearm_z",	
  "magnet_forearm_x", "magnet_forearm_y", "magnet_forearm_z"
)

training <- training[, c(predictors, "classe")]
crossval <- crossval[, c(predictors, "classe")]
```


### Building model
#### Creating prediction model based on training data set and columns described in Item 3 "List of columns"
We will use random forest model.

```r
fit <- train(classe~., data=training, method="rf")
```

```
## Loading required package: randomForest
## randomForest 4.6-12
## Type rfNews() to see new features/changes/bug fixes.
```

```r
fit
```

```
## Random Forest 
## 
## 11776 samples
##    48 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## Summary of sample sizes: 11776, 11776, 11776, 11776, 11776, 11776, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
##    2    0.9859961  0.9822791  0.001743297  0.002206328
##   25    0.9856449  0.9818355  0.001874245  0.002371687
##   48    0.9796832  0.9742941  0.003917988  0.004948617
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 2.
```
Accuracy is about 0.9856449, kappa is 0.9822791.


```r
fit$finalModel
```

```
## 
## Call:
##  randomForest(x = x, y = y, mtry = param$mtry) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 2
## 
##         OOB estimate of  error rate: 0.9%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 3346    2    0    0    0 0.0005973716
## B   15 2255    9    0    0 0.0105309346
## C    1   16 2031    6    0 0.0111976631
## D    0    0   42 1885    3 0.0233160622
## E    0    1    3    8 2153 0.0055427252
```

#### Cross validate model

```r
cm <- confusionMatrix(crossval$classe, predict(fit, crossval))
cm$overall
```

```
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.9940097      0.9924218      0.9920420      0.9955953      0.2856232 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN
```
Accuracy is 0.9940097, kapps is 0.9924218. This is higher than accuracy of model on training data set. Good results!

## 4. Test data set 
Now we will load test data sets (20 measurements) and will classify each measument.


```r
testing<-read.table("pml-testing.csv", head=TRUE, sep=",")
dim(testing)
```

```
## [1]  20 160
```

```r
testing <- testing[, c(predictors)]

result <- predict( fit, testing)
result
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

```r
answers <- as.character(result)
answers
```

```
##  [1] "B" "A" "B" "A" "A" "E" "D" "B" "A" "A" "B" "C" "B" "A" "E" "E" "A"
## [18] "B" "B" "B"
```


### 4.1 Save results of prediction to files for submission to Coursera

```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)
```


## Appendix
### Appendix 1. Example of exploratory data analisys
Below is an eample of how some of the selected columns features) affects on outcome (classe).

```r
colGroup1 <- c(
  "roll_dumbbell",  "pitch_dumbbell",  "yaw_dumbbell",
  "roll_forearm", "pitch_forearm", "yaw_forearm"
)

featurePlot(x = training[, colGroup1],
            y = training$classe,
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 5))
```

![](PeerAssessment1_files/figure-html/epl-1.png) 

### Appendix 2. Fitting model. Dependency of error from number of trees

```r
plot(fit$finalModel)
```

![](PeerAssessment1_files/figure-html/plot_model-1.png) 

### Appendix 3. The most important features
This diagramm shows which columns were choosen as most important by random forest training algorithm.

```r
gbmImp <- varImp(fit, scale = FALSE)
plot(gbmImp, top = 20)
```

![](PeerAssessment1_files/figure-html/imp_cols-1.png) 
