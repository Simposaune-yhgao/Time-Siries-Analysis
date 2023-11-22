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
gyu=garchFit(~1+garch(1,1),data=yur,trace=F,cond.dist = "std")
summary(gyu)
yor=yo$residuals
gyo=garchFit(~1+garch(1,1),data=yor,trace=F,cond.dist = "std")
summary(gyo)
dar=da$residuals
gda=garchFit(~1+garch(1,1),data=dar,trace=F,cond.dist = "std")
summary(gyo)
tar=ta$residuals
gta=garchFit(~1+garch(1,1),data=tar,trace=F,cond.dist = "std")
summary(gyo)#garch fit well, residual have no arch
qqnorm(residuals(gta,standardize=F))

xa=rep(-1,10)
xb=rep(-1,10)
xc=rep(-1,10)
xd=rep(-1,10)
wei=matrix(0, nrow = 73, ncol = 4)
xa1=rep(-1,1000)
xb1=rep(-1,1000)
xc1=rep(-1,1000)
xd1=rep(-1,1000)
ret=rep(0,13)
mt[1:10,1:4]
da1=diff(a)
db=diff(b)
dc=diff(c)
dd=diff(d)


i=0
for (i in c(0:72)){
  #data=metrix[60i:250+60i]
  yu=auto.arima(da1)
  yo=auto.arima(db[(10*i+1):(250+10*i)])
  da=auto.arima(dc[(10*i+1):(250+10*i)])
  ta=auto.arima(dd[(10*i+1):(250+10*i)])
  yur=yu$residuals
  yor=yu$residuals
  dar=da$residuals
  tar=da$residuals
  gyu=garchFit(~1+garch(1,1),data=yur,trace=F,cond.dist = "std")#怎麼把ARMA吃進去
  gyo=garchFit(~1+garch(1,1),data=yor,trace=F,cond.dist = "std")
  gda=garchFit(~1+garch(1,1),data=dar,trace=F,cond.dist = "std")
  gta=garchFit(~1+garch(1,1),data=tar,trace=F,cond.dist = "std")
  
  
  yupre=predict(yu,10)
  gyupre=predict(gyu,10)
  yopre=predict(yo,10)
  gyopre=predict(gyo,10)
  dapre=predict(da,10)
  gdapre=predict(gda,10)
  tapre=predict(ta,10)
  gtapre=predict(gta,10)
  
  xa[1:10]=yupre$pred+gyupre$meanForecast
  #xa1[(1+10*i):(10*(i+1))]=yupre$pred+gyupre$meanForecast
  
  xb[1:10]=yopre$pred+gyopre$meanForecast
  #xb1[(1+10*i):(10*(i+1))]=yopre$pred+gyopre$meanForecast
  xc[1:10]=dapre$pred+gdapre$meanForecast
  # xc1[(1+10*i):(10*(i+1))]=dapre$pred+gdapre$meanForecast
  xd[1:10]=tapre$pred+gtapre$meanForecast
 # xd1[(1+10*i):(10*(i+1))]=tapre$pred+gtapre$meanForecast
  mt=cbind(xa,xb,xc,xd)
  mt=as.timeSeries(mt)
  tan=tangencyPortfolio(mt, spec = portfolioSpec(), constraints = "LongOnly")
  
  
  wei[i+1,]=tan@spec@portfolio$weights
} 

xa
tan
wei[]



for (j in c(0:71)){
  
  tempa=prod((100+ (a[(251+10*j):(250+10*(j+1))]))/100)
  
  tempb=prod((100+(b[(251+10*j):(250+10*(j+1))]))/100)
  
  tempc=prod((100+(c[(251+10*j):(250+10*(j+1))]))/100)
  tempd=prod((100+(d[(251+10*j):(250+10*(j+1))]))/100)
  ret[j+1]=tempa*wei[j+1,1]+tempb*wei[j+1,2]+tempc*wei[j+1,3]+tempd*wei[j+1,4]
  #ret[]=tempa*wei[13,1]+tempb*wei[13,2]+tempc*wei[13,3]+tempd*wei[13,4]
  
  
}

prod(ret[1:72])#跑出預測的return

wei1=matrix(0, nrow = 73, ncol = 4) 
for (k in c(0:71)){#跑出歷史的權重
 
  mt1=cbind(a[(251+k*10):(250+10*(k+1))],b[(251+k*10):(250+10*(k+1))]
           ,c[(251+k*10):(250+10*(k+1))],d[(251+k*10):(250+10*(k+1))])
  mt1=as.timeSeries(mt1)
  tan1=tangencyPortfolio(mt1, spec = portfolioSpec(), constraints = "LongOnly")
  
  
  wei1[k+1,]=tan1@spec@portfolio$weights
} 


ret1=rep(-1,70)
for (j in c(0:71)){#跑出歷史的RETURN
  
  tempa=prod((100+ (a[(251+10*j):(250+10*(j+1))]))/100)
  
  tempb=prod((100+(b[(251+10*j):(250+10*(j+1))]))/100)
  
  tempc=prod((100+(c[(251+10*j):(250+10*(j+1))]))/100)
  tempd=prod((100+(d[(251+10*j):(250+10*(j+1))]))/100)
  #ret1[j+1]=tempa*wei1[j+1,1]+tempb*wei1[j+1,2]+tempc*wei1[j+1,3]+tempd*wei1[j+1,4]
  #ret[]=tempa*wei[13,1]+tempb*wei[13,2]+tempc*wei[13,3]+tempd*wei[13,4]
  ret1[j+1]=tempd
  
}
prod(ret1[1:72])
tempa
j=1
prod((100+ (c[251:978]))/100)
prod((100+(b[(251+10*j):(250+10*(j+1))]))/100)
prod((100+(c[(251+10*j):(250+10*(j+1))]))/100)
prod((100+(d[(251+10*j):(250+10*(j+1))]))/100)
q=-1
i=2
for (i in c(1:798)){
  w<-(100+c[i])/100
  if( w  >  1){
  q=q+1
  }
}
q
w
(100+c[1:798])/100
c
plot(tan)

tempa=prod(a[251:798])

tempb=prod(b[251:798])

tempc=prod(c[251:798])
tempd=prod(d[(251+60*j):(250+60*(j+1))])


siga=var(a[251:798])
sigb=var(b[251:798])
sigc=var(c[251:798])
sigd=var(d[251:798])
coab=cov (a[251:798],b[251:798])
coac=cov(a[251:798],c[251:798])
coad=cov (a[251:798],d[251:798])
cobc=cov (b[251:798],c[251:798])
cobd=cov (b[251:798],d[251:798])
cocd=cov (c[251:798],d[251:798])
sigall=rep(-1,73)
for (j in c(0:72)){
sigall[j]= siga*(wei[j+1,1])^2+sigb*wei[j+1,2]^2+sigc*wei[j+1,3]^2
+sigd*wei[j+1,4]^2
+(wei[j+1,1])*(wei[j+1,2])*coab
+(wei[j+1,1])*(wei[j+1,3])*coac
+(wei[j+1,1])*(wei[j+1,4])*coad
+(wei[j+1,2])*(wei[j+1,3])*cobc
+(wei[j+1,2])*(wei[j+1,4])*cobd
+(wei[j+1,3])*(wei[j+1,4])*cocd
}
var(sigall)
1.0085^3
sharpe=(51.084-2.572)/sqrt (var(ret))/100
sharpe
