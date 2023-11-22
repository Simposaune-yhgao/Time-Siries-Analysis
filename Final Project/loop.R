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
adf.test(d,nlag=12)





yu=auto.arima(a)
yo=auto.arima(b)
da=auto.arima(c)
ta=auto.arima(d)
yu
Box.test(a,lag = 12,type = 'Ljung')
Box.test(a^2,lag = 12,type = 'Ljung')
yur=yu$residuals
gyu=garchFit(~1+garch(1,1),data=yur,trace=F)
summary(gyu)
resa=residuals(gyu)
par(mfcol=c(2,2)) # Obtain ACF & PACF

acf(resa,lag=24)
pacf(resa,lag=24)
acf(resa^2,lag=24)
pacf(resa^2,lag=24)


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

tempb=sum(b[(251+60*i):250+60*(i+1)])
tempc=sum(c[(251+60*i):250+60*(i+1)])
tempd=sum(d[(251+60*i):250+60*(i+1)])
ret[1:13]=tempa*wei[i+1,1]+tempb*wei[i+1,2]+tempc*wei[i+1,3]+tempd*wei[i+1,4]



}

sharpeRatio(tan, return = c("mean", "mu"),
                 risk = c("Cov", "Sigma", "CVaR", "VaR"), auto = TRUE)

wei[]
tempa
sum(a[(251+60*1):250+60*(1+1)])
 
 ret
 for (j in c(0:12)){
   j=12
   tempa=sum(a[(251+60*j):(250+60*(j+1))])
 
   tempb=sum(b[(251+60*j):(250+60*(j+1))])

   tempc=sum(c[(251+60*j):(250+60*(j+1))])
   tempd=sum(d[(251+60*j):(250+60*(j+1))])
   ret[j]=tempa*wei[j+1,1]+tempb*wei[j+1,2]+tempc*wei[j+1,3]+tempd*wei[j+1,4]
   
   siga=var(a[(251+60*j):(250+60*(j+1))])
   sigb=var(b[(251+60*j):(250+60*(j+1))])
   sigc=var(c[(251+60*j):(250+60*(j+1))])
   sigd=var(d[(251+60*j):(250+60*(j+1))])
   coab=cov= (a[(251+60*j):(250+60*(j+1))] b[(251+60*j):(250+60*(j+1))])
   coac=cov= (a[(251+60*j):(250+60*(j+1))],b[(251+60*j):(250+60*(j+1))])
   coad=cov= (a[(251+60*j):(250+60*(j+1))],b[(251+60*j):(250+60*(j+1))])
   cobc=cov= (a[(251+60*j):(250+60*(j+1))],b[(251+60*j):(250+60*(j+1))])
   cobd=cov= (a[(251+60*j):(250+60*(j+1))],b[(251+60*j):(250+60*(j+1))])
   cocd=cov= (a[(251+60*j):(250+60*(j+1))],b[(251+60*j):(250+60*(j+1))])
   
   
   sigall[j]= siga*(wei[j+1,1])^2+sigb*wei[j+1,2]^2+sigc*wei[j+1,3]^2
   +sigd*wei[j+1,4]^2+
     
   }
 VAR (A-B+C-D) = VAR(A)+VAR(B)+VAR(C)+
   VAR(D)-2COV(A,B)-2COV(C,D)+2COV(A,C)+2COV(B,D)-2COV(A,D)-2COV(B,C)
