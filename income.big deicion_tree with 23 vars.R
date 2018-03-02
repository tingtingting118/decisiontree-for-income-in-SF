########income.big############################333
#importing data
library(readxl)
income_big <- read_excel("C:/Users/karen/Downloads/UIC/ids572/hw2/income.big.xlsx", col_names = FALSE)

#add title for each column 
names(income_big)<-c("hh_income","Sex","martial status","age","education","occupation","timeinSF","dualincomes",
                      "hh_size","under18","hh_status","home_type","ethinic_class","lang_speak")
#6508 observations, 23 predictors

#we call the new 10 variables as var1,var2,...etc
names(income_big)[15:24]<-c("var1","var2","var3","var4","var5","var6","var7",
                            "var8","var9","var10")

#1.find NA first
income_big[income_big=="NA"]<-NA
#calculate missing values
sum(is.na(income_big)) # there are 1937 missing values in the new data set
#clean that################################################################
#now start to find out all of them 
library(mice)

library("VIM")
na_plot1<-aggr(income_big,col=c('blue','red'),numbers=TRUE,prop=TRUE,sortVars=TRUE,labels=names(income_big),cex.axis=1,gap=0,
              ylab=c("histrogram of missing values","pattern"))

#inputing the missing values using mice
#transform categorical var to factor first......


#now we could start the imputation 
init2 = mice(income_big, maxit=0)
meth2 = init2$method
predM2 = init2$predictorMatrix

meth2[c("timeinSF","hh_size","education")]='polyreg'


#nominal: lang_speak,home_type,"hh_status","martial status","occupation","ethic_class", try pmm firsst
meth2[c("lang_speak","hh_status","martial status","occupation","ethinic_class","home_type")]='pmm'



#for the spurious variabls imputation, var2,3,4,5,6,7,8,9, 10select random values from 1-9 for them

attach(income_big)

a2<-which(is.na(var2))
a3<-which(is.na(var3))
a4<-which(is.na(var4))
a5<-which(is.na(var5))
a6<-which(is.na(var6))
a7<-which(is.na(var7))
a8<-which(is.na(var8))
a9<-which(is.na(var9))
a10<-which(is.na(var10))


#a<-(a2,...a10)

set.seed(123456789)
rd_value<-sample(1:9,9)

#assign the random values to these 9 missing values
income_big$var2[a2]<-rd_value[1]
income_big$var3[a3]<-rd_value[2]
income_big$var4[a4]<-rd_value[3]
income_big$var5[a5]<-rd_value[4]
income_big$var6[a6]<-rd_value[5]
income_big$var7[a7]<-rd_value[6]
income_big$var8[a8]<-rd_value[7]
income_big$var9[a9]<-rd_value[8]
income_big$var10[a10]<-rd_value[9]


#now all the missing values are those real variables, so we can start imputation now
imputation2<-mice(income_big,m=9,method=meth2,predictorMatrix = predM2)


#income ready to use now
income.big<-complete(imputation2)


##################DATASET IS READY TO BE USED NOW####################################



##########create the new training and testing dataset#########
set.seed(1234)
index2<-sample(2,nrow(income.big),replace=TRUE,prob=c(0.6,0.4))
train.data2<-income.big[index==1,] #group 1 with 60% of the data for training data
test.data2<-income.big[index==2,]

#make categorical variables as a factor variable#same as the one for cleaning data
attach(income.big)
hh_income<-as.factor(hh_income)
Sex<-as.factor(Sex)
age<-as.factor(age)
education<-as.factor(education)
dualincomes<-as.factor(dualincomes)
under18<-as.factor(under18)
income.big$`martial status`<-as.factor(income.big$`martial status`)
income.big$timeinSF<-as.factor(income.big$timeinSF)
income.big$hh_size<-as.factor(income.big$hh_size)
income.big$hh_status<-as.factor(income.big$hh_status)
income.big$home_type<-as.factor(income.big$home_type)
income.big$ethinic_class<-as.factor(income.big$ethinic_class)
income.big$lang_speak<-as.factor(income.big$lang_speak)

#for them as factor variable too
#list1<-list(names(income.big)[15:24])
as.factor(income.big$var1)
attach(income.big)
as.factor(var2)
as.factor(var3)
as.factor(var4)
as.factor(var5)
as.factor(var6)
as.factor(var7)
as.factor(var8)
as.factor(var9)
as.factor(var10)

myformula3<-hh_income~.
tree.rpart3<-rpart(myformula3,data=train.data2,method="anova")
print(tree.rpart3)
summary(tree.rpart3)
rpart.plot(tree.rpart3,main=' new household income level',type=2,extra=100)



#prune the tree by using minimum cp
best.tree2<-prune(tree.rpart3, cp= tree.rpart3$cptable[which.min(tree.rpart3$cptable[,"xerror"]),"CP"])  
rpart.plot(best.tree2,main='household income level',type=2,extra=100)
#legend("bottomleft", legend = c("1=less than 10k","2=10k to 14k","3=15k-19.99k"), fill = c("pink", "palegreen3"),
# title = "Group")
summary(best.tree2)
print(best.tree2)
best.tree2$splits



