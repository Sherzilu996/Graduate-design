request <- function(method,list=FALSE,...){
  
  #加载包
  library(httr)
  library(readr)
  library(rjson)
  #获取调用凭证
  url <- "https://dataapi.joinquant.com/apis"
  body <- list(
    method = "get_token",
    mob = "18192851852", #手机号
    pwd = "20000915Jzl" #密码
  )
  r <- POST(url, body = body, encode = "json")
  token <- content(r)
  
  body1 <- list(
    method = method,
    token = token,
    ...
  )
  
  r1 <- POST(url, body = body1, encode = 'json')
  if(method == 'get_fund_info')
    {
      result = fromJSON(content(r1))
      return(result)
    }
  else
    {
    if(list==F)
      {
        df <- data.frame(read_csv(content(r1)),check.names = T)
        return(df)
      }
      else{
      l <- strsplit(content(r1),'\n')
      return(l)
    }
  }
}
df1<- request('get_security_info',code="000001.XSHE")

df2 <- request('get_price',code='000001.XSHE',count=476,unit='1d',end_date='2019-06-01')
df3 <- request('get_price',code='000001.XSHE',count=10,unit='1d',end_date='2019-06-15')
  traindata <- df2[,c(11)]
  library(forecastxgb)
  
  traindata.ts<-ts(traindata,,frequency=365,start=c(2018,06,20))
  model <- xgbar(traindata.ts)
  summary(model)
  fc <- forecast(model, h = 10)
  plot(fc)
  fc00<-forecast(model,h=19)
  testdata<-df3[,c(11)]
testdata.ts<-ts(testdata,frequency = 365,start = c(2019,05,20))
a<-matrix(fc$mean)
par(new=FALSE)
plot(testdata,type='b',pch=0,ylim=c(10,13),ylab="stock price")
par(new=TRUE)
plot(a,type='b',pch=17,ylim=c(10,13),ylab="stock price")

plot(fc00)
ls <- request('get_fund_info',code='000001.XSHE',date='2018-12-01')
  traindatam<-matrix(nrow =464 ,ncol = 12)
  
  traindatam[,1]<-traindata[1:469]
  traindatam[,2]<-traindata[2:470]
  traindatam[,3]<-traindata[3:471]
  traindatam[,4]<-traindata[4:472]
  traindatam[,5]<-traindata[5:473]
traindatam[,6]<-traindata[6:474]
traindatam[,7]<-traindata[7:475]


traindata1<-as.matrix(traindata)
traindata2<-traindata1[-c(1:12),]
testdata<-as.matrix(df3)
library(randomForest)
stock.rf0<-randomForest(traindata2~.,data=traindatam,importance=TRUE, ntree=1000)
stock.rf1<-randomForest(traindata2~.,data=traindatam,importance=TRUE, ntree=800)
stock.rf2<-randomForest(traindata2~.,data=traindatam,importance=TRUE, ntree=500)
stock.rf3<-randomForest(traindata2~.,data=traindatam,importance=TRUE, ntree=300)
stock.rf4<-randomForest(traindata2~.,data=traindatam,importance=TRUE, ntree=100)
stock.rf6<-randomForest(traindata2~.,data=traindatam,importance=TRUE, ntree=10)
stock.rf7<-randomForest(traindata2~.,data=traindatam,importance=TRUE, ntree=30)
stock.rf8<-randomForest(traindata2~.,data=traindatam,importance=TRUE, ntree=50)
print(stock.rf4)
print(stock.rf3)
print(stock.rf2)
print(stock.rf1)
print(stock.rf0)
round(importance(stock.rf4), 2)
round(importance(stock.rf0), 2)
df4<- request('get_price',code='000001.XSHE',count=42,unit='1d',end_date='2019-06-15')
testdata1<-df4[,c(11)]
testdatam<-matrix(nrow =30 ,ncol = 12)
testdatam[,1]<-testdata1[1:30]
testdatam[,2]<-testdata1[2:31]
testdatam[,3]<-testdata1[3:32]
testdatam[,4]<-testdata1[4:33]
testdatam[,5]<-testdata1[5:34]
testdatam[,6]<-testdata1[6:35]
testdatam[,7]<-testdata1[7:36]
testdatam[,8]<-testdata1[8:37]
testdatam[,9]<-testdata1[9:38]
testdatam[,10]<-testdata1[10:39]
testdatam[,11]<-testdata1[11:40]
testdatam[,12]<-testdata1[12:41]
fc1<-predict(stock.rf4,testdatam,type="class")
par(new=FALSE)
plot(fc1,type='b',lty=2,pch=17,ylim = c(12,15),ylab = "stock price")
par(new=TRUE)
plot(y0,type='b',pch=0,ylim = c(12,15),ylab = "stock price")
fc2<-predict(stock.rf3,testdatam,type="class")
par(new=FALSE)
plot(fc2,type='b',lty=2,pch=17,ylab = "stock price",ylim = c(12,15))
par(new=TRUE)
plot(y0,type='b',pch=0,ylab = "stock price",ylim = c(12,15))
print(stock.rf2)
fc3<-predict(stock.rf2,testdatam,type="class")
par(new=FALSE)
plot(fc3,type='b',lty=2,pch=17,ylab = "stock price",ylim = c(12,15))
par(new=TRUE)
plot(y0,type='b',pch=0,ylab = "stock price",ylim = c(12,15))
print(stock.rf1)
fc4<-predict(stock.rf1,testdatam,type="class")
par(new=FALSE)
plot(fc4,type='b',lty=2,pch=17,ylab = "stock price",ylim = c(12,15))
par(new=TRUE)
plot(y0,type='b',pch=0,ylab = "stock price",ylim = c(12,15))
print(stock.rf0)
fc5<-predict(stock.rf0,testdatam,type="class")
par(new=FALSE)
plot(fc5,type='b',lty=2,pch=17,ylab = "stock price",ylim = c(12,15))
par(new=TRUE)
plot(y0,type='b',pch=0,ylab = "stock price",ylim = c(12,15))
fc8<-predict(stock.rf8,testdatam,type="class")
par(new=FALSE)
plot(fc8,type='b',lty=2,pch=17,ylab = "stock price",ylim = c(12,15))
par(new=TRUE)
plot(y0,type='b',pch=0,ylab = "stock price",ylim = c(12,15))
fc6<-predict(stock.rf6,testdatam,type="class")
par(new=FALSE)
plot(fc6,type='b',lty=2,pch=17,ylab = "stock price",ylim = c(12,15))
par(new=TRUE)
plot(y0,type='b',pch=0,ylab = "stock price",ylim = c(12,15))
fc7<-predict(stock.rf7,testdatam,type="class")
par(new=FALSE)
plot(fc7,type='b',lty=2,pch=17,ylab = "stock price",ylim = c(12,15))
par(new=TRUE)
plot(y0,type='b',pch=0,ylab = "stock price",ylim = c(12,15))
print(stock.rf6)
print(stock.rf7)
print(stock.rf8)
#traindata2data=data.frame(traindata2)
traindatamdata=data.frame(traindatam)
t(traindatamdata)
#scale(traindata2data)
#scale(traindatamdata)
#import bartmachine
library(bartMachine)
bart_machine<-bartMachine(traindatamdata,traindata2,serialize = TRUE)
summary(bart_machine)
bart_machine1<-bartMachine(traindatamdata,traindata2,nu=1)
summary(bart_machine1)
bart_machine2<-bartMachine(traindatamdata,traindata2,nu=2)
bart_machine3<-bartMachine(traindatamdata,traindata2,nu=3)
bart_machine4<-bartMachine(traindatamdata,traindata2,nu=4)
bart_machine5<-bartMachine(traindatamdata,traindata2,nu=5)
bart_machine6<-bartMachine(traindatamdata,traindata2,nu=6)
bart_machine7<-bartMachine(traindatamdata,traindata2,nu=7)
bart_machine8<-bartMachine(traindatamdata,traindata2,nu=8)
bart_machine9<-bartMachine(traindatamdata,traindata2,nu=9)
bart_machine10<-bartMachine(traindatamdata,traindata2,nu=10)
x1<-c(1,bart_machine1$rmse_train)
x2<-c(2,bart_machine2$rmse_train)
x3<-c(3,bart_machine3$rmse_train)
x4<-c(4,bart_machine4$rmse_train)
x5<-c(5,bart_machine5$rmse_train)
x6<-c(6,bart_machine6$rmse_train)
x7<-c(7,bart_machine7$rmse_train)
x8<-c(8,bart_machine8$rmse_train)
x9<-c(9,bart_machine9$rmse_train)
x10<-c(10,bart_machine10$rmse_train)
x<-c(1:10)
y<-c(bart_machine1$rmse_train,bart_machine2$rmse_train,bart_machine3$rmse_train,bart_machine4$rmse_train,bart_machine5$rmse_train
     ,bart_machine6$rmse_train,bart_machine7$rmse_train,bart_machine8$rmse_train,bart_machine9$rmse_train,bart_machine10$rmse_train)
plot(x, y, xlim = c(0,12), ylim = c (0.14,0.16),ylab="rmse",xlab = "nu")

barpredictResult<-predict(bart_machine,traindatamdata[1:7, ])
testdatam1<-data.frame(testdatam)
barpredictResult1<-predict(bart_machine,testdatam1[1:10, ])
plot(barpredictResult1,type='b',lty=2,pch=17,ylab = "stock price",ylim = c(10,15))
par(new=TRUE)
plot(testdata[1:10],type='b',pch=12,ylab = "stock price",ylim = c(10,15))

rmse_by_num_trees(bart_machine, num_replicates = 20)
bartMachine01<-bartMachine(traindatamdata,traindata2,k=1)
bartMachine02<-bartMachine(traindatamdata,traindata2,k=2)
bartMachine03<-bartMachine(traindatamdata,traindata2,k=3)
bartMachine04<-bartMachine(traindatamdata,traindata2,k=4)
bartMachine05<-bartMachine(traindatamdata,traindata2,k=5)
bartMachine06<-bartMachine(traindatamdata,traindata2,k=6)
bartMachine07<-bartMachine(traindatamdata,traindata2,k=7)
bartMachine08<-bartMachine(traindatamdata,traindata2,k=8)
bartMachine09<-bartMachine(traindatamdata,traindata2,k=9)
bartMachine010<-bartMachine(traindatamdata,traindata2,k=10)
x<-c(1:10)
y<-c(bartMachine01$rmse_train,bartMachine02$rmse_train,bartMachine03$rmse_train,bartMachine04$rmse_train,bartMachine05$rmse_train
     ,bartMachine06$rmse_train,bartMachine07$rmse_train,bartMachine08$rmse_train,bartMachine09$rmse_train,bartMachine010$rmse_train)
plot(x, y, xlim = c(0,12), ylim = c (0.14,0.2),ylab="rmse",xlab = "nu")
install.packages('bartMachineCV')
acf(df2$avg)
df21<-diff(df2$avg)
acf(df21)
traindatam1<-matrix(nrow =463 ,ncol = 12)

traindatam1[,1]<-df21[1:463]
traindatam1[,2]<-df21[2:464]
traindatam1[,3]<-df21[3:465]
traindatam1[,4]<-df21[4:466]
traindatam1[,5]<-df21[5:467]
traindatam1[,6]<-df21[6:468]
traindatam1[,7]<-df21[7:469]
traindatam1[,8]<-df21[8:470]
traindatam1[,9]<-df21[9:471]
traindatam1[,10]<-df21[10:472]
traindatam1[,11]<-df21[11:473]
traindatam1[,12]<-df21[12:474]

traindata1<-as.matrix(traindata)
traindata2<-traindata1[-c(1:12)]
traindata21<-diff(traindata2)
testdata<-as.matrix(df3)
library(randomForest)
testdatam1<-diff(testdatam)
testdatam1<-data.frame(testdatam1)
stock.rf00<-randomForest(traindata21~.,data=traindatam1,importance=TRUE, ntree=500)
#print(stock.rf00)

fc00<-predict(stock.rf00,testdatam1)
fc000<-matrix(nrow = 10,ncol = 1)
fc000[1]=13.4
for (i in 2:10) {
  fc000[i]=fc000[i-1]-fc00[i-1]
  
}
par(new=FALSE)
plot(fc000,type='b',lty=2,pch=17,ylim = c(12,15),ylab = "stock price")
par(new=TRUE)
plot(testdatam[1:10,1],type='b',pch=0,ylim = c(12,15),ylab = "stock price")
x1<-matrix(nrow = 2,ncol = 5)
x1[1,]<-c(5,10,50,100,500)
x1[2,5]<-rmse(fc000,testdatam[1:10,1])
x<-x1[1,]
y<-x1[2,]
plot(x,y,xlab = "number of trees",ylab="rmse")
library(bartMachineCV)

traindatam1<-data.frame(traindatam1)

bartMachine00diff<-bartMachine(traindatam1,traindata21)
summary(bartMachine00diff)
testdatam1<-data.frame(testdatam1)
barpredictResult1<-predict(bartMachine00diff,testdatam1[1:10, ])
View(barpredictResult1)
fc000bart<-matrix(nrow = 10,ncol = 1)
fc000bart[1]=13.4
for (i in 2:10) {
  fc000bart[i]=fc000[i-1]-barpredictResult1[i-1]
  
}
par(new=FALSE)
plot(fc000bart,type='b',lty=2,pch=17,ylim = c(12,15),ylab = "stock price")
par(new=TRUE)
plot(testdatam[1:10,1],type='b',pch=0,ylim = c(12,15),ylab = "stock price")
library("xxIRT")
rmse(fc000bart,testdatam[1:10,1])
help("bartMachine")
help("bartMachineArr")
help("bartMachineCV")
##xgboost for MTS
library(AER)
library(readxl)
library(xts)
library(zoo)
library(dynlm)
library(forecastxgb)
library(fpp)
library(tidyverse)
library(fuzzyjoin)
consumption <- usconsumption[ ,1]
df2_open_ts<-ts(df2$open,df2$date)
df2_open_xts<-xts(df2$open,df2$date)
df2_open_ts_diff<-diff.Date(df2_open_ts)
df2_avg_ts<-ts(df2$avg,df2$date)
df2_avg_xts<-xts(df2$avg,df2$date)
df2_avg_ts_diff<-diff.Date(df2_avg_ts)
df2_high_ts<-ts(df2$high,df2$date)
df2_high_xts<-xts(df2$high,df2$date)
df2_high_ts_diff<-diff.Date(df2_high_ts)
df2_low_ts<-ts(df2$low,df2$date)
df2_low_xts<-xts(df2$low,df2$date)
df2_low_ts_diff<-diff.Date(df2_low_ts)
df2_reg_xts<-cbind.xts(df2_open_xts, df2_high_xts,df2_low_xts) 
df2_reg_ts_diff<-cbind(df2_open_ts_diff,df2_high_ts_diff,df2_low_ts_diff)
df2_reg_ts<-ts(df2_reg_xts)
income <- matrix(usconsumption[ ,2], dimnames = list(NULL, "Income"))
income_future <- matrix(forecast(xgbar(usconsumption[,2]), h = 10)$mean, dimnames = list(NULL, "Income"))
income_test<-forecast(xgbar(usconsumption[,2]), h = 10)
#Stopping. Best iteration: 1
#君埋泥下泉销骨 我寄人间雪满头
#我还记得高中的生活很无趣乏味 让人昏昏欲睡的课程 西工大永远狭小的天空 正午的阳关 那时的高三 像是悠闲缓慢地不真实 我在等待高考的来到
plot(forecast(consumption_model, xreg = income_future))
stock_model <- xgbar(y = df2_avg_ts, xreg = df2_reg_ts)
summary(stock_model)
reg_open_test<-forecast(xgbar(df2_open_ts),h=10)
reg_high_test<-forecast(xgbar(df2_high_ts),h=10)
reg_low_test<-forecast(xgbar(df2_low_ts),h=10)
reg_future_test<-cbind(reg_open_test$mean,reg_high_test$mean,reg_low_test$mean)
avg_future<-forecast(stock_model, xreg = reg_future_test)
plot(forecast(stock_model, xreg = reg_future_test))
##not fill just use fact to predict,will it be better
reg_true<-
##to do some diff to time series

#randomforest for MTS
library(predict.randomforest)
install.packages("predict.randomforest")
library(randomForest)
helinstall.packages("MultivariateRandomForest")
library(MultivariateRandomForest)
help(MultivariateRandomForest)
reg_data<-numeric()
for (i in c(1:10)) {
  reg_data<-cbind(reg_data,lag(df2_reg_ts_diff,i))
}
stock_randomforest_model<-randomForest(y=df2_avg_ts_diff,x=reg_data)
summary(stock_randomforest_model)
plot(stock_randomforest_model$importance)
stock_randomforest_model_choose<-randomForest(y=df2_avg_ts_diff,x=cbind(lag(df2_high_ts_diff,1),lag(df2_high_ts_diff,2),lag(df2_low_ts_diff,1)))
#stock_randomforest_predict_diff<-forecast(stock_randomforest_model_choose,h=10)

stock_randomforest_predict_result<-predict(stock_randomforest_model_choose,testdata_MTH_RF,type="class")

stock_randomforest_predict_result_final<-matrix(nrow = 10,ncol=1)
stock_randomforest_predict_result_final[1]<-12.22
for (i in 2:10) {
  stock_randomforest_predict_result_final[i]=stock_randomforest_predict_result_final[i-1]-stock_randomforest_predict_result[i-1]
  #stock_randomforest_predict_result_final[2]=stock_randomforest_predict_result_final[1]-stock_randomforest_predict_result[1]
}
#straight predict
df3_open_ts_diff<-diff(df3$open)
df3_high_ts_diff<-diff(df3$high)
df3_low_ts_diff<-diff(df3$low)
testdata_MTH_RF<-cbind(lag(df3_high_ts_diff,1),lag(df3_high_ts_diff,2),lag(df3_low_ts_diff,1))
par(new=FALSE)
plot(stock_randomforest_predict_result_final,type='b',lty=2,pch=17,ylim = c(12,15),ylab = "stock price")
par(new=TRUE)
plot(testdatam[1:10,1],type='b',pch=0,ylim = c(12,15),ylab = "stock price")
#first predict the reg part

#bartmachine for predict the stock price
library(bartMachine)
reg_data_frame<-data.frame(reg_data)
bartmachine_result<-bartMachine(y=df2_avg_ts_diff,X=reg_data_frame)
summary(bartmachine_result)
bartmachine_result$model_matrix_training_data
bartmachine_result$num_cores

#the ordinary VAR model and Granger analysis
stockprice_ADL11_lm <- lm(df2_avg_ts_diff ~ lag(df3_high_ts_diff,1) + lag(df3_low_ts_diff,1) )
coeftest(stockprice_ADL11_lm)
summary(stockprice_ADL11_lm)$r.squared
for (p in 1:4) {
  print(IC(lm(totalpay_2000_2020 ~ lag(totalpay_2000_2020,1:p) + lag(UK_UnemploymentRate_2000_2020,1:p)))) # Now, we can do the same thing but we add the 1:p lag for unemployment rate as well
}





