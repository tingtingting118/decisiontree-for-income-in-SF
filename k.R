#qk
#size effect on training and testing data
attach(income.data)
hh_income<-as.factor(hh_income)
Sex<-as.factor(Sex)
age<-as.factor(age)
education<-as.factor(education)
dualincomes<-as.factor(dualincomes)
under18<-as.factor(under18)

#0.5,0.5

set.seed(1234)
i1<-sample(2,nrow(income.data),replace=TRUE,prob=c(0.5,0.5))
tdata1<-income.data[i1==1,] #group 1 with 50% of the data for training data
tree50<-rpart(hh_income~.,data=tdata1,method ="anova")#hh_income is numeric

rpart.plot(tree50,main='household income level',type=3,extra=100)
print(tree.rpart2)
#prp(tree.rpart2,extra=100,yesno=TRUE)
summary(tree.rpart2)
plotcp(tree50,col='red')
#0.7,0.3
set.seed(1234)
i2<-sample(2,nrow(income.data),replace=TRUE,prob=c(0.7,0.3))
tdata2<-income.data[i2==1,] #group 1 with 70% of the data for training data
tree70<-rpart(hh_income~.,data=tdata2,method ="anova")#hh_income is numeric
rpart.plot(tree70,main='household income level',type=3,extra=100)
plotcp(tree70,col='red')

#0.9,0.1
set.seed(1234)
i3<-sample(2,nrow(income.data),replace=TRUE,prob=c(0.9,0.1))
tdata3<-income.data[i3==1,] #group 1 with 90% of the data for training data
tree90<-rpart(hh_income~.,data=tdata3,method ="anova")#hh_income is numeric
rpart.plot(tree90,main='household income level',type=3,extra=100)
plotcp(tree90,col='red')
#0.1,0.9

set.seed(1234)
i4<-sample(2,nrow(income.data),replace=TRUE,prob=c(0.1,0.9))
tdata4<-income.data[i4==1,] #group 1 with 10% of the data for training data
tree10<-rpart(hh_income~.,data=tdata4,method ="anova")#hh_income is numeric
rpart.plot(tree10,main='household income level',type=3,extra=100)
plotcp(tree10,col='red')
