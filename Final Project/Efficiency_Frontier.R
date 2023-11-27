library('timeSeries')
library('fPortfolio')
library('fBasics')


data<-read.csv("0063R.csv")
yuanda=data$日報酬率..
yuanda=yuanda[c(1:1007)]

data<-read.csv("2409.csv")
yoda=data$日報酬率..
yoda=yoda[c(1:1007)]

data<-read.csv("3008.csv")
daliguang=data$X3008
daliguang=daliguang[c(1:1007)]

data<-read.csv("3045.csv")
taigeda=data$日報酬率..
taigeda=taigeda[c(1:1007)]


dat=cbind(yuanda,yoda,daliguang,taigeda)

head(dat)

dat=as.timeSeries(dat)  

Frontier = portfolioFrontier(dat, spec = Spec)




plot(Frontier)
?portfolioFrontier
tangencyLines(Frontier, return = c("mean", "mu"), 
               risk = "Cov", auto = TRUE )
frontierPlot(Frontier, spec = Spec,frontier = c("both", "lower", "upper"),
             col = c("black", "grey"), add = FALSE, labels = TRUE,
             return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
             auto = TRUE, title = TRUE) 
sharpeRatioLines(Frontier, return = c("mean", "mu"), 
                 risk = c("Cov", "Sigma", "CVaR", "VaR"), auto = TRUE)
tailoredFrontierPlot(Frontier,
                     return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
                     mText = NULL, col = NULL, xlim = NULL, ylim = NULL, 
                     twoAssets = T, sharpeRatio = T, title = TRUE)
tangencyPortfolio(dat, spec = portfolioSpec(), constraints = "LongOnly")


Spec = portfolioSpec()
Spec
setRiskFreeRate(Spec) <-0.008648
