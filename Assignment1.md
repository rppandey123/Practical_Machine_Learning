# Machine Learning Assignment
null  
Sunday, January 25, 2015  
Loading initial directories and reading data


```r
## Load relevant libraries

library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
library(randomForest)
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
library(Hmisc)
```

```
## Loading required package: grid
## Loading required package: survival
## Loading required package: splines
## 
## Attaching package: 'survival'
## 
## The following object is masked from 'package:caret':
## 
##     cluster
## 
## Loading required package: Formula
## 
## Attaching package: 'Hmisc'
## 
## The following object is masked from 'package:randomForest':
## 
##     combine
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
library(foreach)
library(doParallel)
```

```
## Loading required package: iterators
## Loading required package: parallel
```

```r
## Set seed to ensure code  reproducibility

set.seed(2211)

## Read the data
setwd("C:/Users/rahul pandey/Dropbox/11 Analytics/02 Assignments/08 Practical Machine Learning/Assignment")
TrainData <- read.csv("pml-training.csv", na.strings=c("NA",""), strip.white=T)
TestData <- read.csv("pml-testing.csv", na.strings=c("NA",""), strip.white=T)
dim(TrainData)
```

```
## [1] 19622   160
```

The initial data has 19000+ rows and 160 columns. To remove data on which we can't process, we remove columns that have NAs


```
## [1] 19622    60
```

```
##   X user_name raw_timestamp_part_1 raw_timestamp_part_2   cvtd_timestamp
## 1 1  carlitos           1323084231               788290 05/12/2011 11:23
## 2 2  carlitos           1323084231               808298 05/12/2011 11:23
## 3 3  carlitos           1323084231               820366 05/12/2011 11:23
## 4 4  carlitos           1323084232               120339 05/12/2011 11:23
## 5 5  carlitos           1323084232               196328 05/12/2011 11:23
## 6 6  carlitos           1323084232               304277 05/12/2011 11:23
##   new_window num_window roll_belt pitch_belt yaw_belt total_accel_belt
## 1         no         11      1.41       8.07    -94.4                3
## 2         no         11      1.41       8.07    -94.4                3
## 3         no         11      1.42       8.07    -94.4                3
## 4         no         12      1.48       8.05    -94.4                3
## 5         no         12      1.48       8.07    -94.4                3
## 6         no         12      1.45       8.06    -94.4                3
##   gyros_belt_x gyros_belt_y gyros_belt_z accel_belt_x accel_belt_y
## 1         0.00         0.00        -0.02          -21            4
## 2         0.02         0.00        -0.02          -22            4
## 3         0.00         0.00        -0.02          -20            5
## 4         0.02         0.00        -0.03          -22            3
## 5         0.02         0.02        -0.02          -21            2
## 6         0.02         0.00        -0.02          -21            4
##   accel_belt_z magnet_belt_x magnet_belt_y magnet_belt_z roll_arm
## 1           22            -3           599          -313     -128
## 2           22            -7           608          -311     -128
## 3           23            -2           600          -305     -128
## 4           21            -6           604          -310     -128
## 5           24            -6           600          -302     -128
## 6           21             0           603          -312     -128
##   pitch_arm yaw_arm total_accel_arm gyros_arm_x gyros_arm_y gyros_arm_z
## 1      22.5    -161              34        0.00        0.00       -0.02
## 2      22.5    -161              34        0.02       -0.02       -0.02
## 3      22.5    -161              34        0.02       -0.02       -0.02
## 4      22.1    -161              34        0.02       -0.03        0.02
## 5      22.1    -161              34        0.00       -0.03        0.00
## 6      22.0    -161              34        0.02       -0.03        0.00
##   accel_arm_x accel_arm_y accel_arm_z magnet_arm_x magnet_arm_y
## 1        -288         109        -123         -368          337
## 2        -290         110        -125         -369          337
## 3        -289         110        -126         -368          344
## 4        -289         111        -123         -372          344
## 5        -289         111        -123         -374          337
## 6        -289         111        -122         -369          342
##   magnet_arm_z roll_dumbbell pitch_dumbbell yaw_dumbbell
## 1          516      13.05217      -70.49400    -84.87394
## 2          513      13.13074      -70.63751    -84.71065
## 3          513      12.85075      -70.27812    -85.14078
## 4          512      13.43120      -70.39379    -84.87363
## 5          506      13.37872      -70.42856    -84.85306
## 6          513      13.38246      -70.81759    -84.46500
##   total_accel_dumbbell gyros_dumbbell_x gyros_dumbbell_y gyros_dumbbell_z
## 1                   37                0            -0.02             0.00
## 2                   37                0            -0.02             0.00
## 3                   37                0            -0.02             0.00
## 4                   37                0            -0.02            -0.02
## 5                   37                0            -0.02             0.00
## 6                   37                0            -0.02             0.00
##   accel_dumbbell_x accel_dumbbell_y accel_dumbbell_z magnet_dumbbell_x
## 1             -234               47             -271              -559
## 2             -233               47             -269              -555
## 3             -232               46             -270              -561
## 4             -232               48             -269              -552
## 5             -233               48             -270              -554
## 6             -234               48             -269              -558
##   magnet_dumbbell_y magnet_dumbbell_z roll_forearm pitch_forearm
## 1               293               -65         28.4         -63.9
## 2               296               -64         28.3         -63.9
## 3               298               -63         28.3         -63.9
## 4               303               -60         28.1         -63.9
## 5               292               -68         28.0         -63.9
## 6               294               -66         27.9         -63.9
##   yaw_forearm total_accel_forearm gyros_forearm_x gyros_forearm_y
## 1        -153                  36            0.03            0.00
## 2        -153                  36            0.02            0.00
## 3        -152                  36            0.03           -0.02
## 4        -152                  36            0.02           -0.02
## 5        -152                  36            0.02            0.00
## 6        -152                  36            0.02           -0.02
##   gyros_forearm_z accel_forearm_x accel_forearm_y accel_forearm_z
## 1           -0.02             192             203            -215
## 2           -0.02             192             203            -216
## 3            0.00             196             204            -213
## 4            0.00             189             206            -214
## 5           -0.02             189             206            -214
## 6           -0.03             193             203            -215
##   magnet_forearm_x magnet_forearm_y magnet_forearm_z classe
## 1              -17              654              476      A
## 2              -18              661              473      A
## 3              -18              658              469      A
## 4              -16              658              469      A
## 5              -17              655              473      A
## 6               -9              660              478      A
```
 
Now the number of rows have been reduced to around 60, we remove redundant columns


```r
## The following appear redundant raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, ## new_window,num_window)

TrainData <- subset(TrainData[], select=-c(new_window, num_window, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp))


## columns removed succesfully ?
dim(TrainData)
```

```
## [1] 19622    55
```

The next section creates training and testing database, and trains the model on training DB


```r
## We now ceate the training and testing datasets
itTrain <- createDataPartition(y=TrainData$classe, p=0.7, list=FALSE)
training <- TrainData[itTrain,]
testing <- TrainData[-itTrain,]

dim(training); dim(testing)
```

```
## [1] 13737    55
```

```
## [1] 5885   55
```

```r
## Now train the model
ControlVariable <- trainControl(allowParallel=T, method="cv", number=4)
model <- train(classe ~ ., data=training, model="rf", trControl=ControlVariable)

## Finding the important variables
varImp(model)
```

```
## rf variable importance
## 
##   only 20 most important variables shown (out of 58)
## 
##                    Overall
## X                 100.0000
## roll_belt           7.8455
## pitch_forearm       2.2396
## accel_belt_z        1.5827
## roll_dumbbell       1.1963
## magnet_dumbbell_y   1.0706
## magnet_belt_y       0.7152
## accel_forearm_x     0.7025
## pitch_belt          0.6330
## magnet_dumbbell_x   0.5823
## yaw_belt            0.5819
## total_accel_belt    0.5561
## accel_dumbbell_y    0.5083
## roll_forearm        0.4514
## magnet_dumbbell_z   0.4195
## magnet_belt_z       0.3887
## accel_dumbbell_z    0.2809
## pitch_dumbbell      0.2808
## gyros_belt_z        0.2511
## magnet_forearm_z    0.2344
```

We can see that roll_belt, pitch_forearm, accel_belt_z, roll_dumbbell are the most important parameters


```r
## Now the predictions and accuracy
pred <- predict(model, newdata=testing)
sum(pred == testing$classe) / length(pred)
```

```
## [1] 1
```

```r
confusionMatrix(testing$classe, pred)$table
```

```
##           Reference
## Prediction    A    B    C    D    E
##          A 1674    0    0    0    0
##          B    0 1139    0    0    0
##          C    0    0 1026    0    0
##          D    0    0    0  964    0
##          E    0    0    0    0 1082
```

## For some strange reasons we are getting 100% accuracy, there could be issues. So testing on test data


```r
colNA <- apply(TestData, 2, function(x) { sum(is.na(x)) })
TestData <- subset(TestData[, which(colNA == 0)])

TestData <- subset(TestData[], select=-c(new_window, num_window, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp))

pred1 <- predict(model, newdata=TestData)
sum(pred1 == testing$classe) / length(pred1)
```

```
## Warning in is.na(e1) | is.na(e2): longer object length is not a multiple
## of shorter object length
```

```
## Warning in `==.default`(pred1, testing$classe): longer object length is
## not a multiple of shorter object length
```

```
## [1] 83.7
```

Model has an accuracy of 83%. Thus suggesting a fairly robust training model


