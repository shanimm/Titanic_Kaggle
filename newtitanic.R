chooseCRANmirror(graphics=FALSE, ind=1)
knitr::opts_chunk$set(echo = TRUE)
train1=read.csv("train7.csv",header=TRUE)
head(train1)
#female=1 , male=2
train1$Sex=as.numeric(train1$Sex)
head(train1)
table(train1$Embarked)
# Checking if the embarked column has any empty values
sum(is.na(train1$Embarked)>0)
require(tidyr)
is.na(train1$Embarked)
#embarked values C=2,Q=3,S=4 other=0
train1$Embarked=as.numeric(train1$Embarked)

head(train1)
#checking comparison between survival rate and embarkment point
table(train1$Embarked,train1$Survived)
install.packages("humaniformat")
#in the name we try to get the salutation
require(humaniformat)
#the format in the column is not right, so we reverse it throughout the column
a=format_reverse(train1[1,]$Name)
#we get the salutation using the humaniformat library's function
salutation(a)
# we now create a new column called salutation 
train1$Salutation=salutation(format_reverse(train1$Name))
summary(train1)
str(train1$Salutation)
head(train1)
#family count=sibling count+parent or child count
train1$familycount=train1$SibSp+train1$Parch

head(train1)
train1$Name=NULL
head(train1)
#train1[is.na(Age)]=mean(train1$Age, na.rm = TRUE)
#here we get mean of all non NA ages and give it to the blank values in the age column
train1$Age[is.na(train1$Age)] <- mean(train1$Age, na.rm = T)
str(train1)
summary(train1)
table(train1$Salutation)
table(train1$Salutation,train1$Survived)
require(randomForest)
train1$Salutation1=NULL
require(tidyverse)
write.csv(train1,"train125.csv")
test1=read.csv("test.csv",header=T)
head(test1)
test1$Ticket=NULL
head(test1)
test1$Sex=as.numeric(test1$Sex)
test1$Embarked=1+as.numeric(test1$Embarked)
#here embarked has only 3 values and not 4, so we change accordingly
summary(test1$Embarked)
a=format_reverse(test1[1,]$Name)
test1$Salutation=salutation(format_reverse(test1$Name))
test1$familycount=test1$Parch+test1$SibSp
test1$Age[is.na(test1$Age)] <- mean(test1$Age, na.rm = T)
head(test1)
summary(test1)
test1$Fare[is.na(test1$Fare)]<-mean(test1$Fare,na.rm=T)
summary(test1)
summary(test1)
test1$Cabin=NULL
summary(train1)
install.packages("DataExplorer")
require(DataExplorer)
DataExplorer::create_report(train1,output_file = "report.html",output_dir = getwd())
table(test1$Salutation)
test11=write.csv(test1,"testnew.csv")
#basic data cleaning done?
install.packages("DataExplorer")
require(DataExplorer)
DataExplorer::plot_correlation(train1)
write.csv(train1,"train126.csv")
train3=read.csv("train126.csv",header = T)
write.csv(test1,"testn126.csv")
test3=read.csv("testn126.csv",header=T)


DataExplorer::plot_correlation(train3)
as.matrix(table(train1$Salutation,train1$Survived))
head(train1)

train2=read.csv("train123.csv",header=T)
require(DataExplorer)
require(randomForest)
DataExplorer::plot_correlation(train2)
head(train3)

rf.train21=randomForest(factor(Survived)~Pclass+Sex+Fare+Salutation+Embarked+Age+familycount,data=train3,na.action=na.omit,ntree=1000,mtry=2,importance=F,proximity=F,maxnodes=NULL,type="classification",cutoff=c(0.48,0.52),do.trace=2)
rf.train21
importance(rf.train21)
varImpPlot(rf.train21)
rf.train21
install.packages("party")
require(party)
set.seed(100)
cor(train3[,-11])
#Cforest being used
crf.train3=cforest(as.factor(Survived)~Pclass+Sex+Fare+Salutation+Embarked+Age+SibSp+Parch+familycount,data=train3,controls=cforest_unbiased(ntree=1700,mtry=3))
crf.train3
pred<-predict(rf.train21,newdata = test3,OOB=TRUE,type="response")
write.csv(as.matrix(pred),file="test128.csv")
predcrf<-predict(crf.train3,test3,OOB=TRUE,type="response")
write.csv(predcrf,"testcrf8.csv")

#abc<-tuneRF(train1[,-2:-3], factor(train1[,3]),1, ntreeTry=50, stepFactor=2, improve=0.05,
 #      trace=TRUE, plot=TRUE, doBest=FALSE,na.action=na.omit)

set.seed(100)
#xgboost
head(train3)

install.packages("tidyverse")

library(tidyverse)
#xgboost trial 1 failed

#column names in first
install.packages("randomForest")
require(randomForest)
library(dplyr)
require(dplyr)
#xgboost trial 2
#visualizations
require(ggplot2)
require(RColorBrewer)
head(train3)
heatmap(as.matrix(train3[,-11]))
install.packages("heatmaply") 
head(train3)
cor(train3[,-11])
require(heatmaply)
#interactive heatmap for correlation matrix in training set
heatmaply(cor(train3[,-11]))
#install.packages("iplots")
#library(iplots)
#require(iplots)
head(train3)
#imosaic(train3$Pclass,train3$Survived)
#ibox(train3$Survived)
my_packages <- c("tidyverse", "broom", "coefplot", "cowplot",
                 "gapminder", "GGally", "ggrepel", "ggridges", "gridExtra",
                 "here", "interplot", "margins", "maps", "mapproj",
                 "mapdata", "MASS", "quantreg", "rlang", "scales",
                 "survey", "srvyr", "viridis", "viridisLite", "devtools")
install.packages(my_packages,repos="http://cran.rstudio.com") 
library(ggplot2)
require(ggplot2)
a<- ggplot(data=train3,
           mapping=aes(x=Survived,y=Pclass))
a+geom_smooth()+geom_point()

remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)
#age vs class
b<-ggplot(data=train3,
          mapping=aes(x=Age,y=Pclass))
b+geom_point(color="green")+
  geom_smooth(method="loess")+
  scale_x_log10()
# hist/bar of familycount with percentage as y axis
c<-ggplot(data=train3,
          mapping=aes(x=familycount))
c+geom_bar(mapping=aes(y=..prop..,group=1))
#hist of salutation with survival rate proportion using ggplot
train77=read.csv("train125.csv",header=T)
d<-ggplot(data=train77,
          mapping=aes(x=Salutation))
d+geom_bar(position="dodge",
           mapping=aes(y=..prop..,group=Survived))+
  facet_wrap(~Survived,ncol=1)
train78=read.csv("train7.csv",header=T)
#not very useful- density plot of titanic embarkment
e<-ggplot(data=train78,
          mapping=aes(x=Embarked))
e+geom_density()
#use dplyr function to group class of passengers in embarkment points
require(dplyr)
#install.packages("rJava")
#require(rJava)
gp_by_Emclass <- train78 %>%
  group_by(Embarked,Pclass) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))
gp_by_Emclass
f<- ggplot(gp_by_Emclass, aes(x = Embarked, y = pct, fill = Pclass))
f + geom_col(position = "dodge") +
  labs(x = NULL,y = "Percentage", fill = "Passenger Class") +
  guides(fill=FALSE)+
  coord_flip()+
  facet_grid(~Embarked)
  theme(legend.position = "top")
#
g<-ggplot(data=train78,
          mapping=aes(x=Pclass,y=Survived))
g+geom_line(aes(group=Embarked))+facet_wrap(~Embarked)
# ctrl+shift+m for the %>% combo
#the objective is to get the mean of passenger details depending on if they were kids or mr or mrs or rev etc
train3means <-train3 %>% group_by(Salutation) %>%
  summarize(Survived_mean= mean(Survived, na.rm = TRUE),
            Sex_mean = mean(Sex, na.rm = TRUE),
            FamilyCount_mean = mean(familycount, na.rm = TRUE),
            Fare_mean = mean(Fare, na.rm = TRUE),
            Pclass_mean = mean(Pclass, na.rm = TRUE))
train3means
#take the means of familycount and class and plot per salutation
h<- ggplot(data=train3means,
           mapping=aes(x=FamilyCount_mean,y=Pclass_mean))
h+geom_point(size=2)+facet_wrap(~Salutation,ncol=1)

train3means$SalutationName=c("Rev.","Mr.","Dr.","Master","Miss.","Mrs.","Ms.","-")
#indicating the salutation in the graph itself
i<- ggplot(data=train3means,
           mapping=aes(x=FamilyCount_mean,y=Pclass_mean,color="green"))
i+geom_point(size=2)+geom_text(mapping=aes(label=SalutationName))
#adaboost
install.packages("fastAdaboost")
require(fastAdaboost)
head(train3)
train5=read.csv("train126.csv",header=T)
head(train5)
fastada.train4=adaboost(Survived~Pclass+Sex+Age+Fare+familycount+Parch+Salutation+Embarked,data=train5,10000,type="classification",cutoff=c(0.5,0.5))
fastada.train4
test5=read.csv("testn126.csv",header=T)
pred.ada=predict(fastada.train4,newdata = test5,type="response",cutoff=c(0.5,0.5))
pred.ada$class
getwd()
write.csv(as.matrix(pred.ada$class),file="test1288.csv")

pairs(train5)

library(MASS)
install.packages("ISLR")
require(MASS)
require(ISLR)
fix(BOSTON)
names(BOSTON)
lm1=lm(medv~lstat,data=Boston)
lm1
names(lm1)
confint(lm1)
abline(lm1)
plot(Boston$lstat,Boston$medv)
abline(lm1)
library(ISLR)
names(Smarket)
train=sample(392,196)
lm2=lm(mpg~horsepower,data=Auto,subset = train)
?sample  
train
