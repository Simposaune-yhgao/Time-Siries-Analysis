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




da1=diff(a)
db=diff(b)
dc=diff(c)
dd=diff(d)

yu=auto.arima(da1)
yo=auto.arima(db)
da=auto.arima(dc)
ta=auto.arima(dd)
mean(da)



mean(db)
mean(dc)
mean(dd)
acf(da)
pacf(da)

xa=rep(-1,60)
xb=rep(-1,60)
xc=rep(-1,60)
xd=rep(-1,60)
wei=matrix(0, nrow = 12, ncol = 4)

cha=arima(da1,order=c(1,0,0))
y=cha$residuals
Box.test(  (y^-mean(y))^2,lag=12,type='Ljung')
ret=rep(0,13)

for (i in c(0:12)){
  #data=metrix[60i:250+60i]
  yu=auto.arima(da1[(60*i+1):(250+60*i)])
  yo=auto.arima(db[(60*i+1):(250+60*i)])
  da=auto.arima(dc[(60*i+1):(250+60*i)])
  ta=auto.arima(dd[(60*i+1):(250+60*i)])
  yur=yu$residuals
  yor=yu$residuals
  dar=da$residuals
  tar=da$residuals
  gyu=garchFit(~1+garch(1,1),data=yur,trace=F)#怎麼把ARMA吃進去
  gyo=garchFit(~1+garch(1,1),data=yor,trace=F)
  gda=garchFit(~1+garch(1,1),data=dar,trace=F)
  gta=garchFit(~1+garch(1,1),data=tar,trace=F)
  
  
  yupre=predict(yu,60)
  gyupre=predict(gyu,60)
  yopre=predict(yo,60)
  gyopre=predict(gyo,60)
  dapre=predict(da,60)
  gdapre=predict(gda,60)
  tapre=predict(ta,60)
  gtapre=predict(gta,60)
  
  xa[1:60]=yupre$pred+gyupre$meanForecast
  #xa[(1+60*i):(60*(i+1))]=yupre$pred
  
  xb[1:60]=yopre$pred+gyopre$meanForecast
  #xb[(1+60*i):(60*(i+1))]=yopre$pred
  xc[1:60]=dapre$pred+gdapre$meanForecast
  # xc[(1+60*i):(60*(i+1))]=dapre$pred
  xd[1:60]=tapre$pred+gtapre$meanForecast
  #xd[(1+60*i):(60*(i+1))]=tapre$pred
  mt=cbind(xa,xb,xc,xd)
  mt=as.timeSeries(mt)
  tan=tangencyPortfolio(mt, spec = portfolioSpec(), constraints = "LongOnly")
  
  
  wei[i+1,]=tan@spec@portfolio$weights
  tempa=sum(a[(251+60*i):(250+60*(i+1))])
  
  tempb=sum(b[(251+60*i):(250+60*(i+1))])
  tempc=sum(c[(251+60*i):(250+60*(i+1))])
  tempd=sum(d[(251+60*i):(250+60*(i+1))])
  ret[1:13]=tempa*wei[i+1,1]+tempb*wei[i+1,2]+tempc*wei[i+1,3]+tempd*wei[i+1,4]
  
  
  
}



wei[]
tempa
sum(a[(251+601):(250+60*(11+1))])
a
ret
for (j in c(0:11)){
  
  tempa=sum(a[(251+60*j):(250+60*(j+1))])
  
  tempb=sum(b[(251+60*j):(250+60*(j+1))])
  
  tempc=sum(c[(251+60*j):(250+60*(j+1))])
  tempd=sum(d[(251+60*j):(250+60*(j+1))])
  ret[j]=tempa*wei[j+1,1]+tempb*wei[j+1,2]+tempc*wei[j+1,3]+tempd*wei[j+1,4]
  ret[12]=tempa*wei[13,1]+tempb*wei[13,2]+tempc*wei[13,3]+tempd*wei[13,4]
  
    
}


