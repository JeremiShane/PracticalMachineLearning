
setwd("~/desktop/coursera/assignments/quantifySelfMovement")

## https://rpubs.com/Madhusri/65901

library(dplyr)
library(caret)
library(rpart)
library(randomForest)

## Get, Sample and Clean Data
trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

## traindf will be split 60/40 into train and validation
trainingdf <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
## remove timestamp, id, non-predictive variables
trainingdf <- trainingdf[-c(1,2,3,4,5,6,7)]

## testdf will  be used as out of sample
oosdf <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))

## clean up variables with near zero variance
myDataNZV <- nearZeroVar(trainingdf, saveMetrics=TRUE)
tcols <- data.frame("columname" = colnames(trainingdf), "nzv"=myDataNZV$nzv)
keepcols <- tcols[tcols$nzv==FALSE, ]
cols <- keepcols$columname
t <- trainingdf[cols]
dim(t)

## clean up variables with more than 60% NA values
t3 <- t #creating another subset to iterate in loop
for(i in 1:length(t)) { #for every column in the training dataset
        if( sum( is.na( t[, i] ) ) /nrow(t) >= .6 ) { #if n?? NAs > 60% of total observations
                for(j in 1:length(t3)) {
                        if( length( grep(names(t[i]), names(t3)[j]) ) ==1)  { #if the columns are the same:
                                t3 <- t3[ , -j] #Remove that column
                        }   
                } 
        }
}
cleantraindf <- t3
dim(cleantraindf)

## split using dplyr
traindf<-sample_frac(cleantraindf, 0.6)
sid<-as.numeric(rownames(traindf)) # because rownames() returns character
validationdf<-cleantraindf[-sid,]

cleancols <- colnames(subset(traindf, select=-c(classe)))
oosdf <- oosdf[cleancols]

summary(traindf$classe)

dim(traindf) ## training
dim(validationdf) ## validation
dim(oosdf) ## out of sample

t <- traindf[cleancols]  ## remove the classe variable and coerce

for (i in 1:length(oosdf) ) {
        for(j in 1:length(t)) {
                if( length( grep(names(t[i]), names(oosdf)[j]) ) ==1)  {
                        class(oosdf[j]) <- class(t[i])
                }      
        }      
}
## make sure it worked
te <- rbind(t , oosdf)
dim(traindf)
dim(te)

dtree <- rpart(classe ~ ., data=traindf, method="class")
## fancyRpartPlot(dtree)
predictvalidationdtree <- predict(dtree, validationdf, type = "class")
## Using confusion Matrix to test results:
confusionMatrix(predictvalidationdtree, validationdf$classe)


randforest <- randomForest(classe ~. , data=traindf)
## Predicting in-sample error:
predictrandforest <- predict(randforest, validationdf, type = "class")
## Using confusion Matrix to test results:
confusionMatrix(predictrandforest, validationdf$classe)


predictrandforestoos <- predict(randforest, oosdf, type = "class")
predictdtreeoos <- predict(dtree, oosdf, type = "class")

plot(dtree)
predictdtreeoos

# Out-of-Sample Test
We will determine the out-of-sample error with the test set provided.

For Random Forests we use the following formula, which yielded a much better prediction in in-sample:
        
        predictionsB2 <- predict(modFitB1, testing, type = "class")
Function to generate files with predictions to submit for assignment

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(predictionsB2)