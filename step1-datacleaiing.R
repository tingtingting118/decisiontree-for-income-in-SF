#import data first
library(readxl)
income_data <- read_excel("C:/Users/karen/Downloads/UIC/ids572/hw2/income.data.xlsx", 
                            +     col_names = FALSE)

#add title for each column 
names(income_data)<-c("hh_income","Sex","martial status","age","education","occupation","timeinSF","dualincomes",
                      "hh_size","under18","hh_status","home_type","ethinic_class","lang_speak")

#clean outliers




#clean missing values first
summary(income_data)

#found that some of the data are characters , including the "NA"-NEED TO MAKE THEM LOGICAL TYPE


income_data[income_data=="NA"]<-NA
#double check again
sum(!complete.cases(income_data))

#now start to find out all of them 
library(mice)
md.pattern(income_data)
#only 6876 cases are completed
#visualize that first
library("VIM")
na_plot<-aggr(income_data,col=c('blue','red'),numbers=TRUE,prop=TRUE,sortVars=TRUE,labels=names(income_data),cex.axis=1,gap=0,
              ylab=c("histrogram of missing values","pattern"))
#10% of data in timeinSF are missing now followed by the hh_size


#inputing the missing values using mice
#transform categorical var to factor first
income_data$`martial status`<-as.factor(income_data$`martial status`)
income_data$education<-as.factor(income_data$education)
income_data$occupation<-as.factor(income_data$occupation)
income_data$timeinSF<-as.factor(income_data$timeinSF)
income_data$hh_size<-as.factor(income_data$hh_size)
income_data$hh_status<-as.factor(income_data$hh_status)
income_data$home_type<-as.factor(income_data$home_type)
income_data$ethinic_class<-as.factor(income_data$ethinic_class)
income_data$lang_speak<-as.factor(income_data$lang_speak)
#now we could start the imputation 
init = mice(income_data, maxit=0)
meth = init$method
predM = init$predictorMatrix




#now set different imputation for different variables
#ordinal
meth[c("timeinSF","hh_size","education")]='polyreg'


#nominal: lang_speak,home_type,"hh_status","martial status","occupation","ethic_class", try pmm firsst
meth[c("lang_speak","hh_status","martial status","occupation","ethinic_class","home_type")]='pmm'

#marital
summary(income_data$`martial status`)



#hh_type
summary(income_data$home_type)


#lang_speak
summary(income_data$lang_speak)


#hh_status

#after imputation
imputation<-mice(income_data,m=9,method=meth,predictorMatrix = predM)
complete(imputation)

#income ready to use now
income.data<-complete(imputation)
