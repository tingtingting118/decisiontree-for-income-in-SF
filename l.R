#Q1-l)
set.seed(1234)
index.1<-sample(2,nrow(income.data),replace=TRUE,prob=c(0.6,0.4))
traindata1<-income.data[index.1==1,] #group 1 with 60% of the data for training data
testdata1<-income.data[index.1==2,]

#build up a tree1-tree8
treegini<-rpart(hh_income~.,data=traindata1,method ="anova",parms = list(split="gini"))#hh_income is numeric
rpart.plot(treegini,main='household income level using GINI',type=2,extra=100)
summary(treegini)
treegini$variable.importance

treeinfo<-rpart(hh_income~.,data=traindata1,method ="anova",parms = list(split="information"))
rpart.plot(treeinfo,main='household income level using information gain',type=2,extra=100)
summary(treeinfo)
