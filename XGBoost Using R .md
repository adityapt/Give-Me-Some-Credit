# Give-Me-Some-Credit
Banks play a crucial role in market economies. They decide who can get finance and on what terms and can make or break investment decisions. For markets and society to function, individuals and companies need access to credit.   Credit scoring algorithms, which make a guess at the probability of default, are the method banks use to determine whether or not a loan should be granted. This competition requires participants to improve on the state of the art in credit scoring, by predicting the probability that somebody will experience financial distress in the next two years.  The goal of this competition is to build a model that borrowers can use to help make the best financial decisions
# This code is using XGboost in R

require(xgboost)

require(methods)

train = read.csv('D:/Google Drive/CIS508 Data Mining-1/Team Project/train.csv',header=TRUE,stringsAsFactors = F)

test = read.csv('D:/Google Drive/CIS508 Data Mining-1/Team Project/test.csv',header=TRUE,stringsAsFactors = F)

train = train[,-1]

test = test[,-1]

y = train[,ncol(train)]

y = gsub('Class_','',y)

y = as.integer(y)-1 #xgboost take features in [0,numOfClass)

x = rbind(train[,-ncol(train)],test)

x = as.matrix(x)

x = matrix(as.numeric(x),nrow(x),ncol(x))

trind = 1:length(y)

teind = (nrow(train)+1):nrow(x)

# Set necessary parameters

param <- list("objective" = "binary:logistic",
              "eval_metric" = "auc",
              "num_class" = 2,
              "nthread" = 10)

# Run Cross Valication

cv.nround = 50

bst.cv = xgb.cv(param=param, data = x[trind,] , label = y, 
                nfold = 10, nrounds=cv.nround)

# Train the model

nround = 100

bst = xgboost(param=param, data = x[trind,], label = y, nrounds=nround)

# Make prediction

pred = predict(bst,x[teind,])

pred = matrix(pred,2,length(pred)/2)

pred = t(pred)

# Output submission

pred = format(pred, digits=2,scientific=F) # shrink the size of submission

pred = data.frame(1:nrow(pred),pred)

write.csv(pred,file='submission.csv', quote=FALSE,row.names=FALSE)

period <- 120

FullList <- 1:120

x <- train$MonthlyIncome

# "randomly" make 15 of the points "missing"

MissingList <- sample(x,15)

x[MissingList] <- NA

 # Create sine curve with noise

y <- sin(2*pi*x/period) + runif(length(x),-1,1) 

 # Plot points on noisy curve

 plot(x,y, main="Sine Curve + 'Uniform' Noise")

 mtext("Using loess smoothed fit to impute missing values")
 
 y.loess <- loess(y ~ x, span=0.75, data.frame(x=x, y=y))

y.predict <- predict(y.loess, data.frame(x=FullList))
 
# Plot the loess smoothed curve showing gaps for missing data

  lines(x,y.predict,col=i)

  y.Missing <- predict(y.loess, data.frame(x=MissingList))

 points(MissingList, y.Missing, pch=FILLED.CIRCLE<-19, col=i)




