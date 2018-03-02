
##############TREE WITH MINIMUM PRUNING########################################
big.tree<-prune(tree.rpart2, cp= tree.rpart2$cptable[which.max(tree.rpart2$cptable[,"xerror"]),"CP"])
rpart.plot(big.tree,main='large tree of household income level',type=2,extra=100)
###############CROSS-VALIDATION DECISION TREE###########################################
#minisplit=500 for parent node, minibukcet=100 for leaf node observations size
#once it reaches a group smaller than 500, we stop splitting out 
#onces it reaches a group smaller than 100 at the leaf node, terminal node with 100 minimum
CV_tree<-rpart(hh_income~.,data=train.data,method ="anova",control=rpart.control(minsplit=500,minibuket=100))

printcp(CV_tree)
#we choose the minimum value of cp as the cross-validation errors
bestcp<-CV_tree$cptable[which.min(CV_tree$cptable[,"xerror"]),"CP"]
#we have best cross validation error as cp value of 0.01
#therefore, we setup the final tree with this cp value
cv.finaltree<-rpart(hh_income~.,data=train.data,method ="anova",control=rpart.control(minsplit=500,minibuket=100,cp=0.01))
#plot the decision tree
rpart.plot(cv.finaltree,main="crossvalidation regression tree",type=2,extra=100)
