library(fBasics)
library(TSA)
library(fGarch)
library('timeSeries')
library('fPortfolio')
library(forecast)
library(aTSA)
library(forecast)

a=yuanda$日報酬率
b=yoda$日報酬率
c=daliguang$日報酬率
d=taigeda$日報酬率

yu=auto.arima(da1)
yo=auto.arima(db)
da=auto.arima(dc)
ta=auto.arima(dd)


ryu=yu$residuals
ryo=yo$residuals
rda=da$residuals
rta=ta$residuals

Box.test(ryu,12)
Box.test(ryo,12)
Box.test(rda,12)
Box.test(rta,12)

da1=diff(a)
db=diff(b)
dc=diff(c)
dd=diff(d)

xa=rep(-1,60)
xb=rep(-1,60)
xc=rep(-1,60)
xd=rep(-1,60)
wei=matrix(0, nrow = 60, ncol = 4)




for (i in c(0:12)){
  
  
  
  #data=metrix[60i:250+60i]
  yu1=auto.arima(da1[(60*i+1):(250+60*i)])
  yo1=auto.arima(db[(60*i+1):(250+60*i)])
  da1=auto.arima(dc[(60*i+1):(250+60*i)])
  ta1=auto.arima(dd[(60*i+1):(250+60*i)])
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
  
  tan=tangencyPortfolio(mt, spec = spec, constraints = "LongOnly")
  tan@spec@portfolio$weights
  wei[i,]=tan@spec@portfolio$weights
  
  #要怎麼把wieght print出來
  
}
wei[]

# Show Default Portfolio Specifications:
spec = portfolioSpec()
# Change Risk Free Rate
setRiskFreeRate(spec) = 0.008
spec


