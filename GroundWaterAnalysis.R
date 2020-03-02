knnd<-read.csv("groundwater.csv",header = TRUE)
knnd<-as.data.frame(knnd[,-c(1,2,3)])
str(knnd)
attach(knnd)
#head(knnd)
cat ("Rows     : "  , dim(knnd)[1])
cat ("\nColumns  : " ,  dim(knnd)[2] ,"\n")
print("Features : " )
print(names(knnd))
sapply(knnd, class)
#visualization
library(gridExtra)
library(ggthemes)
#plot1 <- ggplot(knnd, aes(x=knnd$status.of.district)) + ggtitle("status of district") + xlab("status of district") +
#  geom_bar(aes(y = 100*(..count..)/sum(=)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
#plot1
any(is.na(knnd))
#cat ("Column which has null value :" , list_na)
library(mice)
library(Hmisc)
library(plyr)
library(ggplot2)
list_na <- colnames(knnd)[ apply(knnd,2, anyNA) ]
list_na
class(knnd$Recharge.from.Rainfall.during.Monsoon.season.Ham.)
knnd$Recharge.from.Rainfall.during.Monsoon.season.Ham.<-impute(knnd$Recharge.from.Rainfall.during.Monsoon.season.Ham.,mean)
knnd$Recharge.From.Other.Sources.during.monsoon.season.Ham.<-impute(knnd$Recharge.From.Other.Sources.during.monsoon.season.Ham.,mean)
knnd$Recharge.from.Rainfall.during.non.monsoon.season.Ham.<-impute(knnd$Recharge.from.Rainfall.during.non.monsoon.season.Ham.,mean)
knnd$Recharge.From.Other.Sources.during.non.monsoon.season.Ham.<-impute(knnd$Recharge.From.Other.Sources.during.non.monsoon.season.Ham.,mean)
knnd$Draft.due.to.Irrigation.Needs.Ham.<-impute(knnd$Draft.due.to.Irrigation.Needs.Ham.,mean)
#datatry$Projected.demand.for.Domestic.and.Industrial.uses..upto.2025..Ham.<-impute(datatry$Projected.demand.for.Domestic.and.Industrial.uses..upto.2025..Ham.,.ean)
knnd$Projected.demand.for.Domestic.and.Industrial.uses..upto.2025..Ham.<-impute(knnd$Projected.demand.for.Domestic.and.Industrial.uses..upto.2025..Ham.,mean)
knnd$Ground.Water.Availability.for.Future.Irrigation.use..Ham.<-impute(knnd$Ground.Water.Availability.for.Future.Irrigation.use..Ham.,mean)
knnd$Annual.Replenishable.resources.Ham.<-knnd$Recharge.from.Rainfall.during.Monsoon.season.Ham.+knnd$Recharge.From.Other.Sources.during.monsoon.season.Ham.+knnd$Recharge.from.Rainfall.during.non.monsoon.season.Ham.+knnd$Recharge.From.Other.Sources.during.non.monsoon.season.Ham.
#list_na
any(is.na(knnd))
sogd<- function(Stage.of.Ground.Water.Develop.ment...){
  if (Stage.of.Ground.Water.Develop.ment... <= 70){
    return('safe')
  }else if(Stage.of.Ground.Water.Develop.ment... > 70 & Stage.of.Ground.Water.Develop.ment... <= 90){
    return('semicritical')
  }else if (Stage.of.Ground.Water.Develop.ment... > 90 & Stage.of.Ground.Water.Develop.ment... <= 100){
    return('critical')
  }else if (Stage.of.Ground.Water.Develop.ment... > 100){
    return('overexploited')
  }
}
knnd$Stage.of.Ground.Water.Develop.ment....<- sapply(knnd$Stage.of.Ground.Water.Develop.ment....,sogd)
knnd$Stage.of.Ground.Water.Develop.ment....
#normalization
knn_try<-knnd[,-13]
knn_try
data_norm<-function(x){(((x-min(x)))/((max(x)-min(x))))}
knnd_df<-as.data.frame(sapply(knn_try,data_norm))
#head(knnd)
#ind<-sample(2,nrow(knnd_df),replace = TRUE, prob = c(0.7,0.3))
knnd_df
ran <- sample(1:nrow(knnd), 0.7 * nrow(knnd))
training1<-knnd_df[ran,]
testing1<-knnd_df[-ran,]
targetcat<-knnd[ran,13]
testcat<-knnd[-ran,13] 
library(class)
pred<-knn(training1,testing1,cl=targetcat,k=25)
tab<-table(pred,testcat)
tab
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
knnd$Stage.of.Ground.Water.Develop.ment....
csafe<-sum(knnd$Stage.of.Ground.Water.Develop.ment....=='safe')
#csafe
csaf<-sum(knnd$Stage.of.Ground.Water.Develop.ment....=='semicritical')
csa<-sum(knnd$Stage.of.Ground.Water.Develop.ment....=='critical')
cs<-sum(knnd$Stage.of.Ground.Water.Develop.ment....=='overexploited')
csafe
csaf
csa
cs
#datatry$Stage.of.Ground.Water.Develop.ment....
valuess<-c(csafe,csaf,csa,cs)
var12<-c('safe','critical','semicritical','overexploited')
pie(valuess,var12)

------------------------------------DECISION TREE ------------------------
  datatry1<-read.csv("groundwater.csv", header = TRUE)
head(datatry1)
#datatry1<-
datatry<-as.data.frame(datatry1[,c(-1,-2,-3)])
str(datatry)
attach(datatry)
cat("Rows:",dim(datatry)[1])
cat("column:",dim(datatry)[2])
any(is.na(datatry))
colnull<-colnames(datatry)[sapply(datatry,anyNA)]
colnull
library(Hmisc)
class(datatry$Recharge.from.Rainfall.during.Monsoon.season.Ham.)
datatry$Recharge.from.Rainfall.during.Monsoon.season.Ham.<-impute(datatry$Recharge.from.Rainfall.during.Monsoon.season.Ham.,mean)
datatry$Recharge.From.Other.Sources.during.monsoon.season.Ham.<-impute(datatry$Recharge.From.Other.Sources.during.monsoon.season.Ham.,mean)
datatry$Recharge.from.Rainfall.during.non.monsoon.season.Ham.<-impute(datatry$Recharge.from.Rainfall.during.non.monsoon.season.Ham.,mean)
datatry$Recharge.From.Other.Sources.during.non.monsoon.season.Ham.<-impute(datatry$Recharge.From.Other.Sources.during.non.monsoon.season.Ham.,mean)
datatry$Draft.due.to.Irrigation.Needs.Ham.<-impute(datatry$Draft.due.to.Irrigation.Needs.Ham.,mean)
#datatry$Projected.demand.for.Domestic.and.Industrial.uses..upto.2025..Ham.<-impute(datatry$Projected.demand.for.Domestic.and.Industrial.uses..upto.2025..Ham.,.ean)
datatry$Projected.demand.for.Domestic.and.Industrial.uses..upto.2025..Ham.<-impute(datatry$Projected.demand.for.Domestic.and.Industrial.uses..upto.2025..Ham.,mean)
datatry$Ground.Water.Availability.for.Future.Irrigation.use..Ham.<-impute(datatry$Ground.Water.Availability.for.Future.Irrigation.use..Ham.,mean)
datatry$Annual.Replenishable.resources.Ham.<-datatry$Recharge.from.Rainfall.during.Monsoon.season.Ham.+datatry$Recharge.From.Other.Sources.during.monsoon.season.Ham.+datatry$Recharge.from.Rainfall.during.non.monsoon.season.Ham.+datatry$Recharge.From.Other.Sources.during.non.monsoon.season.Ham.
#datatry$Annual.Replenishable.resources.Ham.
datatry$Net.Annual.Ground.Water.Availability...Ham.<-datatry$Annual.Replenishable.resources.Ham.-datatry$Annual.Natural.Discharge..Ham.
datatry$Total.annual.Draft.Ham.<-datatry$Draft.due.to.Irrigation.Needs.Ham.+datatry$Draft.due.to.Domestic...Industrial.Water.Supply.Needs.Ham.
datatry$Total.annual.Draft.Ham.

#datatry
datanorm<-datatry[,-13]
#datanorm
norm<-function(x){((x-min(x))/(max(x)-min(x)))}
dataafnorm<-sapply(datanorm, norm)
dataafnorm
catevar<-function(c1){
  if(c1<=70){
    return('safe')
  }
  if(c1>70 & c1<=90){
    return('semicritical')
  }  
  if(c1>90 & c1<=100){
    return('critical')
  }  
  if(c1>100){
    return('overexploited')
  }  
}
datatry$Stage.of.Ground.Water.Develop.ment....<-sapply(datatry$Stage.of.Ground.Water.Develop.ment....,catevar)
csafe<-sum(datatry$Stage.of.Ground.Water.Develop.ment....=='safe')
#csafe
csaf<-sum(datatry$Stage.of.Ground.Water.Develop.ment....=='semicritical')
csa<-sum(datatry$Stage.of.Ground.Water.Develop.ment....=='critical')
cs<-sum(datatry$Stage.of.Ground.Water.Develop.ment....=='overexploited')
#csafe
#csaf
#csa
#cs
#datatry$Stage.of.Ground.Water.Develop.ment....
valuess<-c(csafe,csaf,csa,cs)
var12<-c('safe','critical','semicritical','overexploited')
pie(valuess,var12)

datatry$Stage.of.Ground.Water.Develop.ment....<-factor(datatry$Stage.of.Ground.Water.Develop.ment....)
#library(gridExtra)
#library(ggthemes)
#p1 <- ggplot(datatry, aes(x=Stage.of.Ground.Water.Develop.ment....)) + ggtitle("status of gw") + xlab("status of gw") +
#  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
#p1

#pd<-sample(2,nrow(datatry),replace = TRUE, prob = c(0.4,0.6))
#train<-datatry[pd==1,]
#valid<-datatry[pd==2,]

ran <- sample(1:nrow(datatry), 0.6 * nrow(datatry))
train<-datatry[ran,]
valid<-datatry[-ran,]

library(party)
tree<-ctree(datatry$Stage.of.Ground.Water.Develop.ment....~datatry$Net.Annual.Ground.Water.Availability...Ham.+datatry$Total.annual.Draft.Ham.+datatry$Projected.demand.for.Domestic.and.Industrial.uses..upto.2025..Ham.+datatry$Ground.Water.Availability.for.Future.Irrigation.use..Ham.,data = train)
tree
plot(tree)
predict(tree,valid,type="prob")
library(rpart)
tree1<-rpart(datatry$Stage.of.Ground.Water.Develop.ment....~datatry$Net.Annual.Ground.Water.Availability...Ham.+datatry$Total.annual.Draft.Ham.+datatry$Projected.demand.for.Domestic.and.Industrial.uses..upto.2025..Ham.+datatry$Ground.Water.Availability.for.Future.Irrigation.use..Ham.,train)
library(rpart.plot)
rpart.plot(tree1,extra=3)
predict(tree1,valid)

tab<-table(predict(tree),datatry$Stage.of.Ground.Water.Develop.ment....)
print(tab)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

tab<-table(predict(tree1),datatry$Stage.of.Ground.Water.Develop.ment....)
print(tab)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
length(tree)
length(valid)

