library(fBasics)
library(TSA)
library(fGarch)
data1<-read.csv("0063R.csv")

da1 = data1$日報酬率..
tdx = c(1:1007)
plot(tdx[1:1007],da1,type ='l',xlab = 'data period',ylab = 'data1
     numerical')
abline(a = mean(da1), b = 0) #畫平均線
abline(a = 2*stdev(da1), b = 0) #畫 2 倍標準差線
abline(a= -2*stdev(da1), b = 0)

head(da1)
mean(da1)

diffdata1 <- diff(da1) #將原始資料做差分。
tdxx = c(1:1007)
plot(tdx[1:1006],diffdata1,type ='l',xlab = 'data period',ylab = 'data1
     numerical')
abline(a = mean(diffdata1), b = 0) #畫平均線
abline(a = 2*stdev(diffdata1), b = 0) #畫 2 倍標準差線
abline(a = -2*stdev(diffdata1), b = 0)

y=da1-mean(da1)
Box.test(y^2,lag=12,type='Ljung')#拒絕H0所以有ARCH效果

eacf(da1,12,12)#頂點在1.1所以fit ARMA(1,1)


m2 = arima(diffdata1,order = c(1,0,1),fixed = c(NA,NA,0))
m2
Box.test(m2$residuals,lag = 12,type = 'Ljung')
pv = 1-pchisq(14.595,2)
pv
sqrt(m2$sigma2)
tsdiag(m2, gof=12)

res= m2$residuals
par(mfcol=c(1,1))
hist(res)

d1=density(res)
range(res)
x=seq(-.1,.1,.001)
y1=dnorm(x,mean(res),stdev(res))
plot(d1$x,d1$y,xlab='Residual',ylab='density',type='l')
lines(x,y1,lty=4)
qqnorm(res)
normalTest(res,method='jb')


mr2 = garchFit(~1+garch(1,1),data = res,trace = F,cond.dist = "std")
summary(mr2)#AIC較低但omega變得更不顯著，t=7.208
plot(mr2)#select13畫QQ圖發現幾乎常態
resi = residuals(mr2,standardize=T)
qqnorm(resi)
normalTest(resi,method='jb')#幹剛剛QQ圖不是很直???

par(mfcol=c(1,1)) # Obtain ACF & PACF
acf(resi,lag=24)
pacf(resi,lag=24)
acf(resi^2,lag=24)
pacf(resi^2,lag=24)


#mr2 is residual 的garch






