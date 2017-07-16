setwd('C:\\Users\\bigan\\learngit\\Titanic')
library(tseries)
# data manipulation
library("dplyr")
library(quantmod)
HS_data<-read.csv("HS300.csv",header = TRUE)
ZZ_data<-as.data.frame(read.csv("ZZ500.csv",header = TRUE))

data=bind_cols(HS_data,ZZ_data)[,c(1,2,4)]
names(data)<-c('date','HS_close','ZZ_close')

ratio = log(data$HS_close/data$ZZ_close)
data$sma = SMA(ratio,n=20)
data$bias = (ratio-data$sma)/data$sma
#将前20个NA数据删除
data = data[20:nrow(data),]
HS_close = data$HS_close
ZZ_close = data$ZZ_close
sma = data$sma
bias = data$bias


len = length(HS_close)
HS_posi = rep(0,len)
ZZ_posi = rep(0,len)
portfolio = rep(0,len)

for(i in 2:len)
{
  if (ratio[i]>=sma[i-1] && bias[i]>=0.1)
  {
    if (HS_posi[i-1]==0)
    {
      HS_posi[i] = 1
      portfolio[i] = portfolio[i-1]-HS_close[i]
    }
    else
    {
      HS_posi[i] = HS_posi[i-1]
      portfolio[i] = portfolio[i-1]
    }
    if (ZZ_posi[i-1]==-1)
    {
      ZZ_posi[i] = 0
      portfolio[i] = portfolio[i-1]+ZZ_close[i]
    }
    else
    {
      ZZ_posi[i] = ZZ_posi[i-1]
      portfolio[i] = portfolio[i-1]
    }
  }
  if (ratio[i]<=sma[i-1] && bias[i]<=-0.1)
  {
    if (ZZ_posi[i-1]==0)
    {
      ZZ_posi[i] = 1
      portfolio[i] = portfolio[i-1]-ZZ_close[i]
    }
    else
    {
      ZZ_posi[i] = ZZ_posi[i-1]
      portfolio[i] = portfolio[i-1]
    }
    if (HS_posi[i-1]==-1)
    {
      HS_posi[i] = 0
      portfolio[i] = portfolio[i-1]+HS_close[i]
    }
    else
    {
      HS_posi[i] = HS_posi[i-1]
      portfolio[i] = portfolio[i-1]
    }
  }
  
}
plot(portfolio)



