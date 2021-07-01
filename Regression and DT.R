library(nortest)
library(MASS)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(caTools)
library(caret)
library(gridExtra)
library(ggpubr)
library(dplyr)
library(car)
library(gplots)
library(plyr)
library(leaps)
library("ggcorrplot")
library(MASS)
library(glmnet)
library(naniar)#NA plot
library(car)
library(lmtest)
options(scipen=9999)# no scientific notation




setwd("C:/Users/barla/OneDrive/Desktop/Academia/Data Science and Economics/Statistical Learning/individual project")
data<-read.csv("californiahousing.csv")


#NA'S HANDLING

vis_miss(data)

bedroom_mean = mean(data$total_bedrooms, na.rm=TRUE)
bedroom_median = median(data$total_bedrooms, na.rm=TRUE)
ggplot(data, aes(x = total_bedrooms)) +
  geom_histogram(bins = 40, color = "black", fill = "blue") +
  geom_vline(aes(xintercept = bedroom_mean, color = "Mean"), lwd = 1.5) +
  geom_vline(aes(xintercept = bedroom_median, color = "Median"), lwd = 1.5) +
  xlab("Total Bedrooms") +
  ylab("Frequency") +
  ggtitle("Histogram of Total Bedrooms (noncontinuous variable)") +
  scale_color_manual(name = "Summary Stats", labels = c("Mean", "Median"), values = c("red", "green"))

#replace missing values by the median
data$total_bedrooms[is.na(data$total_bedrooms)]<-median(data$total_bedrooms, na.rm=TRUE)
#drop ocean_proximity island
data<- data[data$ocean_proximity != "ISLAND", ]
data[,10]<-factor(data[,10],levels=c("<1H OCEAN","INLAND","NEAR BAY","NEAR OCEAN"))



#NORMALITY CHECK

ggqqplot(data$median_house_value)

n_res<-ad.test(data$median_house_value) #normality test
names(n_res)

no_res<-data.frame("Statistic"=n_res[1], "P.value"=n_res[2], "Method"=n_res[3])
grid.table(no_res)

ggqqplot(log(data$median_house_value))


nrow(data[data$median_house_value>500000,])#965

log_res<-ad.test(log(data$median_house_value))
log_res<-data.frame("Statistic"=log_res[1], "P.value"=log_res[2], "Method"=log_res[3])
grid.table(log_res)

#HISTOGRAMS



par(mfrow = c(3, 3))

hist(data$longitude, breaks = 20, main = "longitude", border="darkorange", col="dodgerblue")

hist(data$latitude, breaks = 20, main = "latitude", border="darkorange", col="dodgerblue")

hist(data$housing_median_age, breaks = 20, main = "housing_median_age", border="darkorange", col="dodgerblue")

hist(data$total_rooms, breaks = 20, main = "total_rooms", border="darkorange", col="dodgerblue")

hist(data$total_bedrooms, breaks = 20, main = "total_bedrooms", border="darkorange", col="dodgerblue")

hist(data$population, breaks = 20, main = "population", border="darkorange", col="dodgerblue")

hist(data$households, breaks = 20, main = "households", border="darkorange", col="dodgerblue")

hist(data$median_income, breaks = 20, main = "median_income", border="darkorange", col="dodgerblue")

hist(data$median_house_value, breaks = 20, main = "median_house_value", border="darkorange", col="dodgerblue")

plot(data$median_income,data$median_house_value)



#GET THE CALIFORNIAN MAP

states <- map_data("state")
west_coast <- subset(states, region %in% c("california"))
ca_df <- subset(states, region == "california")
counties <- map_data("county")
ca_county <- subset(counties, region == "california")

#combine housing dataset and the map-dataset
p1<-data[sample(nrow(data), 2977),] # a small random number of houses are plotted because otherwise it is not clearly visible.
ca_county<-cbind(ca_county,p1)

ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")



#Get the borders

ca_bordered<-ca_base + 
  geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top


gg1<-ca_bordered+geom_point(data=unique(subset(ca_county,subregion=="los angeles")[18,]),size=5)+
  geom_text(data=unique(subset(ca_county,subregion=="los angeles")[10,]), 
            aes(long,lat, label=subregion),size=5,hjust=0,nudge_x=0.05)+  
  geom_text(data=unique(subset(ca_county,subregion=="san francisco")[1,]), 
            aes(long,lat, label=subregion),size=5,hjust=0,nudge_x=0.05)+
  geom_point(data=unique(subset(ca_county,subregion=="san francisco")[18,]),size=5)





gg2<-ca_bordered+geom_point(data = ca_county, mapping = aes(x = longitude, y = latitude, color = median_house_value))+
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Median House Prices In California") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_distiller(palette = "Paired") +
  labs(color = "Median House Price (in $USD)")

gg1
gg2



#Multicollinearity and Variable Selection

conts_var<-select(data, median_house_value,longitude,latitude,housing_median_age,total_rooms,total_bedrooms,population,households,median_income)
res<-cor(conts_var)
ggcorrplot(res)

res<-cor(data[,1:9])
corrplot(res, method="number")

flattenCorrMatrix<-function(cormat,pmat){
  ut<-upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor = (cormat)[ut],
    p = pmat[ut]
  )
  
}

res1<-rcorr(as.matrix(conts_var))
flatcor<-flattenCorrMatrix(res1$r,res1$P)
grid.table(flatcor[flatcor$row=="median_house_value",])



boxplot(median_house_value~ocean_proximity, data=data)

#ANOVa
grid.table(ddply(data,~ocean_proximity,summarise,mean=mean(median_house_value),sd=sd(median_house_value),n=length(median_house_value)))
summary(aov(median_house_value~ocean_proximity,data=data))




#SCALE the data

scaled_data<-data
scaled_data[,1:9]<-scale(scaled_data[,1:9])




#Training and Test Set

set.seed(1)
train=sample(c(TRUE,FALSE),nrow(scaled_data),rep=TRUE)
test=(!train)  
  
  
  
  



#FIRST MODEL

first_mod<-lm(median_house_value~median_income+ocean_proximity,data=scaled_data[train,])
summary(first_mod)



predin<-predict(first_mod,scaled_data[train,])

(RMSE(scaled_data$median_house_value[train],predin))^2

predout<-predict(first_mod,scaled_data[test,])

(RMSE(scaled_data$median_house_value[test],predout)^2) #0.408



#Model selection
mod_full<-lm(median_house_value~.,data=scaled_data[train,])

mod.step=stepAIC(mod_full, direction="both", trace=FALSE, k=3)


summary(mod.step)

mean(mod.step$residuals^2) 


predin<-predict(mod_full,scaled_data[train,])
(RMSE(scaled_data$median_house_value[train],predin))^2

predout<-predict(mod_full,scaled_data[test,])
(RMSE(scaled_data$median_house_value[test],predout)^2)#0.35496




#remove bedrooms
no_bed<-scaled_data[train,]

no_bed<-no_bed[,-c(5)]

no_bed_test<-scaled_data[test,]
no_best_test<-no_bed_test[,-c(5)]



mod_no_bed<-lm(median_house_value~.,data=no_bed)
mod.step_no_bed=stepAIC(mod_no_bed, direction="both", trace=FALSE, k=3)

summary(mod.step_no_bed)


mean(mod.step_no_bed$residuals^2)



predin<-predict(mod_no_bed,no_bed)
(RMSE(no_bed$median_house_value,predin))^2

predout<-predict(mod_no_bed,no_bed_test)
(RMSE(no_bed_test$median_house_value,predout)^2) #0.3581


#subset selection


regfit.full <- regsubsets(median_house_value~., data=scaled_data[train,], nvmax=11)
reg.summary=summary(regfit.full)

par(mfrow=c(2,2))


plot(reg.summary$rss, xlab="Number of variables",ylab="RSS", type="l")
points(which.min(reg.summary$rss),reg.summary$rss[which.min(reg.summary$rss)],col="red",cex=2, pch=20)



plot(reg.summary$adjr2,xlab="Number of variables",ylab="Adjusted R square", type="l")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)],col="red",cex=2, pch=20)

plot(reg.summary$cp, xlab="Number of variables",ylab="CP", type="l")
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],col="red",cex=2, pch=20)

plot(reg.summary$bic, xlab="Number of variables",ylab="BIC", type="l")
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],col="red",cex=2, pch=20)






eight_var<-lm(median_house_value~longitude+latitude+housing_median_age+total_bedrooms+population+households+median_income+ocean_proximity,data=scaled_data[test,])

mean(summary(eight_var)$residuals^2)


predin<-predict(eight_var,scaled_data[train,])
(RMSE(scaled_data$median_hous e_value[train],predin))^2

predout<-predict(eight_var,scaled_data[test,])
(RMSE(scaled_data$median_house_value[test],predout)^2)#0.3531

#VALIDATION SET AND CV



regfit.best <- regsubsets(median_house_value~., data=scaled_data[train,], nvmax=11)

test.mat = model.matrix(median_house_value~.,data=scaled_data[test,])

val.errors=rep(NA,11)
for(i in 1:11){
  
  coef_i = coef(regfit.best, id = i)
  pred = test.mat[,names(coef_i)]%*%coef_i
  val.errors[i] = mean((scaled_data$median_house_value[test]-pred)^2)
  
}



which.min(val.errors)

plot(val.errors, type="l", xlab="Number of variables",ylab="Validation Error")
points(which.min(val.errors),val.errors[which.min(val.errors)],col="red", cex=2, pch=20)

regfit.best<-regsubsets(median_house_value~., data=scaled_data[test,], nvmax=11)
coef(regfit.best,8)




#predict function

predict.regsubsets = function(object, newdata,id,...){
  form = as.formula(object$call[[2]])
  mat =model.matrix(form,newdata)
  coefi= coef(object, id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}





#CV
k=10
set.seed(1)
folds=sample(1:k,nrow(scaled_data),replace = TRUE)
cv.errors = matrix(NA,k,11, dimnames=list(NULL,paste(1:11)))


for(j in 1:k){
  best.fit = regsubsets(median_house_value~.,data=scaled_data[folds!=j,],nvmax=11)
  for(i in 1:11){
    pred= predict(best.fit,scaled_data[folds==j,],id=i)
    cv.errors[j,i]=mean((scaled_data$median_house_value[folds==j]-pred)^2)
  }
  
}

mean.cv.errors=apply(cv.errors,2,mean)
plot(mean.cv.errors,type="b", ylab="Cross Validation Errors",xlab="Number of variables")
points(which.min(mean.cv.errors),mean.cv.errors[which.min(mean.cv.errors)],col="red", cex=2, pch=20)



#DIAGNOSTICS

vif_res<-sqrt(vif(mod.step_no_bed))
grid.table(vif_res)





no_lat_log<-lm(median_house_value~housing_median_age+population+households+median_income+ocean_proximity,data=no_bed)



no_lat_log.step<-stepAIC(no_lat_log, direction="both", trace=FALSE, k=3)

summary(no_lat_log.step)


mean(mod.step_no_bed$residuals^2)



predin<-predict(no_lat_log.step,no_bed)
(RMSE(no_bed$median_house_value,predin))^2

predout<-predict(no_lat_log.step,no_bed_test)
(RMSE(no_bed_test$median_house_value,predout)^2) #0.366

grid.table(sqrt(vif(no_lat_log.step)))  





#Residuals normality

ad.test(mod.step_no_bed$residuals) #normality test

ggqqplot(mod.step_no_bed$residuals)

#residuals vs Fitted
plot(mod.step_no_bed, which=c(1,1))

#Breusch-Pagan test
bp_test = bptest(mod.step_no_bed)
bp_test

#cook's distance

par(mfrow=c(1,2))
plot(mod.step_no_bed,4)
plot(mod.step_no_bed,5)



#BoxCox Transformation


no_bed_data<-data[train,]

no_bed_data<-no_bed_data[,-c(5)]

no_bed_test_data<-data[test,]
no_best_test_data<-no_bed_test_data[,-c(5)]



mod_no_bed_data<-lm(median_house_value~.,data=no_bed_data)
mod.step_no_bed_data=stepAIC(mod_no_bed, direction="both", trace=FALSE, k=3)


boxcox(mod.step_no_bed_data)

#log transformation after box-cox

log_housing <- lm(log(median_house_value)~.,data=no_bed_data)


par(mfrow = c(1, 2))
#QQ Plot
qqnorm(log_housing$residuals,main="log_housing")
qqline(log_housing$residuals)

#Scatter Plot of Residuals against fitted values
plot(log_housing$fitted.values,log_housing$residuals,pch=20)
abline(h=0,col="grey")


##POLYNOMIAL REGRESSION

#ANOVA FOR THE DEGREE

model1<-lm(median_house_value ~ ocean_proximity+polym(longitude,latitude,housing_median_age,total_bedrooms,total_rooms,population,households,median_income,
                                              degree=2,raw=TRUE),data=scaled_data[train,])

model2<-lm(median_house_value ~ ocean_proximity+polym(longitude,latitude,housing_median_age,total_bedrooms,total_rooms,population,households,median_income,
                                                      degree=3,raw=TRUE),data=scaled_data[train,])
model3<-lm(median_house_value ~ ocean_proximity+polym(longitude,latitude,housing_median_age,total_bedrooms,total_rooms,population,households,median_income,
                                                      degree=4,raw=TRUE),data=scaled_data[train,])
model4<-lm(median_house_value ~ ocean_proximity+polym(longitude,latitude,housing_median_age,total_bedrooms,total_rooms,population,households,median_income,
                                                      degree=5,raw=TRUE),data=scaled_data[train,])
model5<-lm(median_house_value ~ ocean_proximity+polym(longitude,latitude,housing_median_age,total_bedrooms,total_rooms,population,households,median_income,
                                                      degree=6,raw=TRUE),data=scaled_data[train,])
model6<-lm(median_house_value ~ ocean_proximity+polym(longitude,latitude,housing_median_age,total_bedrooms,total_rooms,population,households,median_income,
                                                      degree=7,raw=TRUE),data=scaled_data[train,])
model7<-lm(median_house_value ~ ocean_proximity+polym(longitude,latitude,housing_median_age,total_bedrooms,total_rooms,population,households,median_income,
                                                      degree=8,raw=TRUE),data=scaled_data[train,])


anova(model1,model2,model3,model4, model5,model6,model7)# I did not attach the result of this test because my computer could not run it. 
#I ran it a few days ago and it selected 7 degree model but today my computer says that due to memory it cant run it.

#CV for polynomial degree choice. This will take a while. It lasted around 15 min in my computer.


set.seed(1)
df.shuffled <- scaled_data[sample(nrow(scaled_data)),]

#define number of folds to use for k-fold cross-validation
K <- 10 

#define degree of polynomials to fit
degree <- 5

#create k equal-sized folds
folds <- cut(seq(1,nrow(df.shuffled)),breaks=K,labels=FALSE)

#create object to hold MSE's of models
mse = matrix(data=NA,nrow=K,ncol=degree)

#Perform K-fold cross validation
for(i in 1:K){
  
  #define training and testing data
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- df.shuffled[testIndexes, ]
  trainData <- df.shuffled[-testIndexes, ]
  
  #use k-fold cv to evaluate models
  for (j in 1:degree){
    fit.train = lm(median_house_value ~ ocean_proximity+polym(longitude,latitude,housing_median_age,total_bedrooms,total_rooms,population,households,median_income,
                                                              degree=j,raw=TRUE),data=trainData)
    fit.test = predict(fit.train, newdata=testData)
    mse[i,j] = mean((fit.test-testData$median_house_value)^2) 
  }
}

colMeans(mse)





grid.table(mse)
colls<-c("First-Degree","Second-Degree","Third-Degree","Fourth-Degree","Fifth-Degree")

mean_errors<-data.frame("First-Degree"=colMeans(mse)[1],"Second-Degree"=colMeans(mse)[2],"Third-Degree"=colMeans(mse)[3],"Fourth-Degree"=colMeans(mse)[4],
                   "Fifth-Degree"=colMeans(mse)[5])
grid.table(mean_errors)

grid.table(mse)










#FINAL MODEL


final_model<-lm(median_house_value ~(.)^2,data=scaled_data[train,])


             
predout<-predict(final_model,scaled_data[test,])
(RMSE(scaled_data$median_house_value[test],predout)^2) #0.297

summary(final_model)



#DECISION TREES

#Decision TREE 

library(MASS)
library("tree")
library(Hmisc)
library(caTools)
library(caret)
library(gridExtra)
library(ggpubr)
library(dplyr)
library(car)
library(gplots)
library(plyr)
library(leaps)
library("ggcorrplot")
library(MASS)
library(glmnet)
library(naniar)#NA plot
library(car)
library("randomForest")
library(gbm)
library(rpart)
library("rpart.plot")
options(scipen=9999)# no scientific notation

setwd("C:/Users/barla/OneDrive/Desktop/Academia/Data Science and Economics/Statistical Learning/individual project")
data<-read.csv("californiahousing.csv")
#replace missing values by the median
data$total_bedrooms[is.na(data$total_bedrooms)]<-median(data$total_bedrooms, na.rm=TRUE)
#drop ocean_proximity island
data<- data[data$ocean_proximity != "ISLAND", ]
data[,10]<-factor(data[,10],levels=c("<1H OCEAN","INLAND","NEAR BAY","NEAR OCEAN"))


scaled_data<-data
scaled_data[,1:9]<-scale(scaled_data[,1:9])



set.seed(1)
train=sample(1:nrow(scaled_data),nrow(scaled_data)/2)
tree.california =tree(median_house_value~.,scaled_data,subset=train)

summary(tree.california) 

tree.california
plot(tree.california)
text(tree.california,pretty=0)




cv.california = cv.tree(tree.california)
plot(cv.california$size,cv.california$dev, type="b")


yhat=predict(tree.california, newdata=scaled_data[-train,])

california.test=scaled_data[-train,"median_house_value"]
plot(yhat,california.test)
abline(0,1)

mean((yhat-california.test)^2)#0.44


##BAGGING
set.seed(1)

test_bag <- NULL
grid= seq(0,500,50)
grid[1]=1
j <- 1
for (i in grid) {
  set.seed(1)
  rf.california.bag <- randomForest(median_house_value~ ., scaled_data,subset=train, mtry=9,importance=TRUE, ntree=i)
  yhat.bag<- predict(rf.california.bag,newdata=scaled_data[-train,])
  test_bag[j] <-mean((yhat.bag-california.test)^2)
  j <- j+1
}

test_bag1<-test_bag


plot(x=grid,y=test_bag,type="b",
     xlab= "Nr of trees", ylab= "MSE",ylim=c(0.195,0.220))
res


res<-data.frame(cbind(grid,test_bag))
res
ggplot(res,aes(grid,test_bag))+geom_point(shape=16,color="black", size=2.2) +geom_line(color="black",size=1)+
  xlab("Number of Trees") +ylab("Test MSE")



bag.california = randomForest(median_house_value~.,scaled_data,subset=train, mtry=9,ntree=250,importance=TRUE)
yhat.bag = predict(bag.california, newdata=scaled_data[-train,])
mean((yhat.bag-california.test)^2)#0.1989



importance(bag.california_red)



bag.importance <- as.data.frame(importance(bag.california_red))
colnames(bag.importance)[1]<-"IncMSE"

bag.graph <- ggplot(bag.importance,aes(y=reorder(rownames(bag.importance),IncMSE),x=IncMSE))+
  geom_bar(stat="identity", fill="red",color="black") +
  geom_text(aes(label=IncMSE), hjust=1, color="white", size=3) +
  theme_minimal() 
bag.graph

varImpPlot(bag.california_red)#this is nicer

##RANDOM FOREST


#test MSE of RF
test_rf <- NULL
grid= seq(0,500,50)
grid[1]=1
j <- 1
for (i in grid) {
  set.seed(1)
  rf.california <- randomForest(median_house_value~ ., scaled_data,subset=train, mtry=3,importance=TRUE, ntree=i)
  yhat.rf<- predict(rf.california,newdata=scaled_data[-train,])
  test_rf[j] <-mean((yhat.rf-california.test)^2)
  j <- j+1
}




plot(x=grid,y=test_rf,type="b",
     xlab= "Nr of trees", ylab= "MSE")
points(9,which.min(rf.res$test_rf))

rf.res<-data.frame(cbind(grid,test_rf))
rf.res[9,]


rf.california.final<-randomForest(median_house_value~.,scaled_data,subset=train, mtry=3,ntree=400,importance=TRUE)



yhat.rf.final=predict(rf.california.final, newdata=scaled_data[-train,])
mean((yhat.rf.final-california.test)^2)#0.1986

varImpPlot(rf.california.final)

test_bag1<-test_bag[-1]
test_rf1<-test_rf[-1]
grid1<-grid[-1]

matplot(grid1, cbind(test_bag1,test_rf1), type="b", pch=c(19,18), col=c("red","blue"), xlab="Nr of Trees", ylab="MSE", ylim=c(0.195,0.21))

legend("right",inset=c(0, -1), legend=c("Bagging","Random Forest"),pch=c(19,18), col=c("red","blue"))




##ERROR table

methods<-c("Two Variable","Step-wise Full","Step-wise-No_rooms","Best Subset-eightvar","Polynomial-Reg","Single-tree","Bagging","Random-Forest")
value<-c(0.4089,0.35496,0.3518,0.3531,0.297,0.447,0.1994,0.1986)

errors<-matrix(nrow=8)
rownames(errors)<-methods
errors[,1]<-value
colnames(errors)<-"MSE"
errors<-as.data.frame(errors)

grid.table(errors)



