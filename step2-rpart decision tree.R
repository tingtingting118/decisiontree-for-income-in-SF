#b)

library(rpart.plot)
#make categorical variables as a factor variable
attach(income.data)
hh_income<-as.factor(hh_income)
Sex<-as.factor(Sex)
age<-as.factor(age)
education<-as.factor(education)
dualincomes<-as.factor(dualincomes)
under18<-as.factor(under18)


#C&R tree



#60% training, 40% testing dataset
set.seed(1234)
index<-sample(2,nrow(income.data),replace=TRUE,prob=c(0.6,0.4))
train.data<-income.data[index==1,] #group 1 with 60% of the data for training data
test.data<-income.data[index==2,]


#build up a tree by using rpart()
#plot(tree.rpart1)
#text(tree.rpart1,use.n=T,xpd=T)

tree.rpart2<-rpart(hh_income~.,data=train.data,method ="anova")#hh_income is numeric
#fancy way
rpart.plot(tree.rpart2,main='household income level',type=3,extra=100)
print(tree.rpart2)
#prp(tree.rpart2,extra=100,yesno=TRUE)
summary(tree.rpart2)

#error on the trainning data
tree.rpart2$cptable
plotcp(tree.rpart2,col='red')
#tree.rpart2$splits


#prune the tree by using minimum cp
best.tree<-prune(tree.rpart2, cp= tree.rpart2$cptable[which.min(tree.rpart2$cptable[,"xerror"]),"CP"])  
rpart.plot(best.tree,main='household income level',type=2,extra=100)
#legend("bottomleft", legend = c("1=less than 10k","2=10k to 14k","3=15k-19.99k"), fill = c("pink", "palegreen3"),
      # title = "Group")
summary(best.tree)
print(best.tree)
best.tree$splits



#prediction here start###############################################################

predict_class<-predict(best.tree,newdata=test.data[,-1],type = "class")
summary(predict_class)
table(test.data$hh_income,predict_class,dnn=c("acutual","predicted"))

#visualize the result



