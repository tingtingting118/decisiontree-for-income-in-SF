##########################create 8 datasets from income.data.xls############################

attach(income.data)
hh_income<-as.factor(hh_income)
Sex<-as.factor(Sex)
age<-as.factor(age)
education<-as.factor(education)
dualincomes<-as.factor(dualincomes)
under18<-as.factor(under18)

#1
set.seed(1234)
index.1<-sample(2,nrow(income.data),replace=TRUE,prob=c(0.6,0.4))
traindata1<-income.data[index.1==1,] #group 1 with 60% of the data for training data
testdata1<-income.data[index.1==2,]
#2
index.2<-sample(2,nrow(income.data),replace=TRUE,prob=c(0.6,0.4))
traindata2<-income.data[index.2==1,] #group 1 with 60% of the data for training data
testdata2<-income.data[index.2==2,]
#3
index.3<-sample(2,nrow(income.data),replace=TRUE,prob=c(0.6,0.4))
traindata3<-income.data[index.3==1,] #group 1 with 60% of the data for training data
testdata3<-income.data[index.3==2,]
#4
index.4<-sample(2,nrow(income.data),replace=TRUE,prob=c(0.6,0.4))
traindata4<-income.data[index.4==1,] #group 1 with 60% of the data for training data
testdata4<-income.data[index.4==2,]
#5
index.5<-sample(2,nrow(income.data),replace=TRUE,prob=c(0.6,0.4))
traindata5<-income.data[index.5==1,] #group 1 with 60% of the data for training data
testdata5<-income.data[index.5==2,]
#6
index.6<-sample(2,nrow(income.data),replace=TRUE,prob=c(0.6,0.4))
traindata6<-income.data[index.6==1,] #group 1 with 60% of the data for training data
testdata6<-income.data[index.6==2,]
#7
index.7<-sample(2,nrow(income.data),replace=TRUE,prob=c(0.6,0.4))
traindata7<-income.data[index.7==1,] #group 1 with 60% of the data for training data
testdata7<-income.data[index.7==2,]
#8
index.8<-sample(2,nrow(income.data),replace=TRUE,prob=c(0.6,0.4))
traindata8<-income.data[index.8==1,] #group 1 with 60% of the data for training data
testdata8<-income.data[index.8==2,]



#build up a tree1-tree8
tree1<-rpart(hh_income~.,data=traindata1,method ="anova")#hh_income is numeric
#prune the tree by using minimum cp
besttree1<-prune(tree1, cp= tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"])  
rpart.plot(besttree1,main='household1 income level',type=2,extra=100)
summary(besttree1)
print(besttree1)
besttree1$splits



tree2<-rpart(hh_income~.,data=traindata2,method ="anova")#hh_income is numeric
#prune the tree by using minimum cp
besttree2<-prune(tree2, cp= tree2$cptable[which.min(tree2$cptable[,"xerror"]),"CP"])  
rpart.plot(besttree2,main='household2 income level',type=2,extra=100)

tree3<-rpart(hh_income~.,data=traindata3,method ="anova")#hh_income is numeric
#prune the tree by using minimum cp
besttree3<-prune(tree3, cp= tree3$cptable[which.min(tree3$cptable[,"xerror"]),"CP"])  
rpart.plot(besttree3,main='household3 income level',type=2,extra=100)



tree4<-rpart(hh_income~.,data=traindata4,method ="anova")#hh_income is numeric
#prune the tree by using minimum cp
besttree4<-prune(tree4, cp= tree4$cptable[which.min(tree4$cptable[,"xerror"]),"CP"])  
rpart.plot(besttree4,main='household4 income level',type=2,extra=100)

tree5<-rpart(hh_income~.,data=traindata5,method ="anova")#hh_income is numeric
#prune the tree by using minimum cp
besttree5<-prune(tree5, cp= tree5$cptable[which.min(tree5$cptable[,"xerror"]),"CP"])  
rpart.plot(besttree5,main='household5 income level',type=2,extra=100)

tree6<-rpart(hh_income~.,data=traindata6,method ="anova")#hh_income is numeric
#prune the tree by using minimum cp
besttree6<-prune(tree6, cp= tree6$cptable[which.min(tree6$cptable[,"xerror"]),"CP"])  
rpart.plot(besttree6,main='household6 income level',type=2,extra=100)

tree7<-rpart(hh_income~.,data=traindata7,method ="anova")#hh_income is numeric
#prune the tree by using minimum cp
besttree7<-prune(tree7, cp= tree7$cptable[which.min(tree7$cptable[,"xerror"]),"CP"])  
rpart.plot(besttree7,main='household7 income level',type=2,extra=100)

tree8<-rpart(hh_income~.,data=traindata8,method ="anova")#hh_income is numeric
#prune the tree by using minimum cp
besttree8<-prune(tree8, cp= tree8$cptable[which.min(tree8$cptable[,"xerror"]),"CP"])  
rpart.plot(besttree8,main='household8 income level',type=2,extra=100)

