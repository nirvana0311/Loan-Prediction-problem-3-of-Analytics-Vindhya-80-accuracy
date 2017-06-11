
libs=c('caret','randomForest','mice','VIM','e1071')
lapply(libs,require,character.only=T)
setwd("C:\\Users\\nirva\\IdeaProjects\\R\\Loan Prediction")
training=read.csv("C:\\Users\\nirva\\IdeaProjects\\R\\Loan Prediction\\train.csv")
test=read.csv("C:\\Users\\nirva\\IdeaProjects\\R\\Loan Prediction\\test.csv")

training=training[,!(colnames(training) %in% 'Loan_ID')]
test=test[,!(colnames(training) %in% 'Loan_ID')]
pMiss=function(x){(sum(is.na(x))/length(x))*100}

aggr(training,col=c('blue','red'),numbers=T,sortVars=T,labels=names(training),cex.axis=.7,gap=3,  ylab=c("Histogram of missing data","Pattern"))

apply(training,2,pMiss)

tempdata=mice(data = training,m = 1,method = 'pmm',maxit = 50,seed = 518)
training=complete(tempdata,1)
densityplot(tempdata)
xyplot(tempdata,LoanAmount~Credit_History+Loan_Amount_Term,pch=18,cex=1)

colnames(training)


training$TotalIncome=training$ApplicantIncome+training$CoapplicantIncome
training_a=training[1:560,]
training_b=training[561:614,]

control=trainControl(method = 'repeatedcv',number = 10,repeats = 3,search = 'grid')
tunegrid=expand.grid(.mtry=3)
first_model=train(Loan_Status~.,method='rf',data = training,trControl=control,tuneGrid=tunegrid,ntree=2501,na.action = na.omit)
table(training$Loan_Status,predict(first_model))
first_model
plot(first_model)
names(training)

#SVM

second_model=svm(Loan_Status~.,data = training)
table(training$Loan_Status,predict(second_model))
tun=tune(svm,train.x =Loan_Status~.,data=training,ranges = list(epsilon=0.1,cost=4))
table(training$Loan_Status,predict(tun$best.model))

second_model
sum(is.na(training))
