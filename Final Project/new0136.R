library(fBasics)
library(TSA)
library(fGarch)
library('timeSeries')
library('fPortfolio')
library(forecast)
library(aTSA)
library(forecast)


metrix=cbind(yuanda$日報酬率,yoda$日報酬率,daliguang$日報酬率,taigeda$日報酬率)

a=yuanda$日報酬率
b=yoda$日報酬率
c=daliguang$日報酬率
d=taigeda$日報酬率

plot(a,xlab='day',ylab='return percent',type='l')
plot(b,xlab='day',ylab='return percent',type='l')
plot(c,xlab='day',ylab='return percent',type='l')
plot(d,xlab='day',ylab='return percent',type='l')
adf.test(a,nlag=12)
adf.test(b,nlag=12)
adf.test(c,nlag=12)
adf.test(d,nlag=12)#---------check stationary


yu=auto.arima(a)
yo=auto.arima(b)
da=auto.arima(c)
ta=auto.arima(d)
yu
yo
da
ta#fit abcd into arima

resa=yu$residuals
resb=yo$residuals
resc=da$residuals
resd=ta$residuals
Box.test(resa^2,lag = 12,type = 'Ljung')
Box.test(resb^2,lag = 12,type = 'Ljung')
Box.test(resc^2,lag = 12,type = 'Ljung')
Box.test(resd^2,lag = 12,type = 'Ljung')#都有ARCH
par(mfcol=c(2,2)) 
acf(resa^2,lag=24)
pacf(resa^2,lag=24)
acf(resb^2,lag=24)
pacf(resb^2,lag=24)
acf(resc^2,lag=24)
pacf(resc^2,lag=24)
acf(resd^2,lag=24)
pacf(resd^2,lag=24)#跑平方圖


yur=yu$residuals
gyu=garchFit(~1+garch(1,1),data=yur,trace=F)
summary(gyu)
yor=yo$residuals
gyo=garchFit(~1+garch(1,1),data=yor,trace=F)
summary(gyo)
dar=da$residuals
gda=garchFit(~1+garch(1,1),data=dar,trace=F)
summary(gyo)
tar=ta$residuals
gta=garchFit(~1+garch(1,1),data=tar,trace=F)
summary(gyo)#garch fit well, residual have no arch
qqnorm(residuals(gta,standardize=F))


xa=rep(-1,3)
xb=rep(-1,3)
xc=rep(-1,3)
xd=rep(-1,3)
wei=matrix(0, nrow = 11, ncol = 4)
xa1=rep(-1,100)
xb1=rep(-1,1000)
xc1=rep(-1,1000)
xd1=rep(-1,1000)
cha=arima(da1,order=c(1,0,0))
y=cha$residuals
Box.test(  (y^-mean(y))^2,lag=12,type='Ljung')
ret=rep(0,73)


for (i in c(0:10)){
  #data=metrix[60i:250+60i]
  yu=auto.arima(a[(3*i+1):(250+i*3)])
  yo=auto.arima(b[(i*3+1):(250+i*3)])
  da=auto.arima(c[(i*3+1):(250+i*3)])
  ta=auto.arima(d[(i*3+1):(250+i*3)])
  yur=yu$residuals
  yor=yu$residuals
  dar=da$residuals
  tar=da$residuals
  gyu=garchFit(~1+garch(1,1),data=yur,trace=F,cond.dist = "std")#怎麼把ARMA吃進去
  gyo=garchFit(~1+garch(1,1),data=yor,trace=F,cond.dist = "std")
  gda=garchFit(~1+garch(1,1),data=dar,trace=F,cond.dist = "std")
  gta=garchFit(~1+garch(1,1),data=tar,trace=F,cond.dist = "std")
  
  
  yupre=predict(yu,3)
  gyupre=predict(gyu,3)
  yopre=predict(yo,3)
  gyopre=predict(gyo,3)
  dapre=predict(da,3)
  gdapre=predict(gda,3)
  tapre=predict(ta,3)
  gtapre=predict(gta,3)
  
  xa[1:3]=yupre$pred+gyupre$meanForecast
  #xa1[(1+3*i):(3*(i+1))]=yupre$pred+gyupre$meanForecast
  
  xb[1:3]=yopre$pred+gyopre$meanForecast
  #xb1[(1+3*i):(3*(i+1))]=yopre$pred+gyopre$meanForecast
  xc[1:3]=dapre$pred+gdapre$meanForecast
  # xc1[(1+3*i):(3*(i+1))]=dapre$pred+gdapre$meanForecast
  xd[1:3]=tapre$pred+gtapre$meanForecast
  # xd1[(1+3*i):(3*(i+1))]=tapre$pred+gtapre$meanForecast
  mt=cbind(xa,xb,xc,xd)
  mt=as.timeSeries(mt)
  
  tan=tangencyPortfolio(mt, spec = portfolioSpec(), constraints = "LongOnly")
  
  
  wei[i+1,]=tan@spec@portfolio$weights
} 
xa1
xa
tan
wei[]
tempa
sum(a[(251+601):(250+60*(11+1))])
a
ret
mt
for (j in c(0:10)){
  
  tempa=prod((100+ (a[(251+3*j):(250+3*(j+1))]))/100)
  
  tempb=prod((100+(b[(251+3*j):(250+3*(j+1))]))/100)
  
  tempc=prod((100+(c[(251+3*j):(250+3*(j+1))]))/100)
  tempd=prod((100+(d[(251+3*j):(250+3*(j+1))]))/100)
  ret[j+1]=tempa*wei[j+1,1]+tempb*wei[j+1,2]+tempc*wei[j+1,3]+tempd*wei[j+1,4]
  #ret[]=tempa*wei[13,1]+tempb*wei[13,2]+tempc*wei[13,3]+tempd*wei[13,4]
  
  
}

prod(ret[1:72])
ret

for (k in c(0:10)){
  
  mt1=cbind(a[(251+k*3):(250+3*(k+1))],b[(251+k*3):(250+3*(k+1))]
            ,c[(251+k*3):(250+3*(k+1))],d[(251+k*3):(250+3*(k+1))])
  mt1=as.timeSeries(mt1)
  tan1=tangencyPortfolio(mt1, spec = portfolioSpec(), constraints = "LongOnly")
  
  
  wei1[k+1,]=tan1@spec@portfolio$weights
} 
mt1
mt
wei1=matrix(0, nrow = 73, ncol = 4)
ret1=rep(-1,70)
for (j in c(0:10)){
  
  tempa=prod((100+ (a[(251+10*j):(250+10*(j+1))]))/100)
  
  tempb=prod((100+(b[(251+10*j):(250+10*(j+1))]))/100)
  
  tempc=prod((100+(c[(251+10*j):(250+10*(j+1))]))/100)
  tempd=prod((100+(d[(251+10*j):(250+10*(j+1))]))/100)
  ret1[j+1]=tempa*wei1[j+1,1]+tempb*wei1[j+1,2]+tempc*wei1[j+1,3]+tempd*wei1[j+1,4]
  #ret[]=tempa*wei[13,1]+tempb*wei[13,2]+tempc*wei[13,3]+tempd*wei[13,4]
  
  
}
ret1
ret
