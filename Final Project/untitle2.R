data<-read.csv("0063R.csv")
yuanda=data$日報酬率..
yuanda=yuanda[c(1:1007)]
yuanda1=yuanda[c(1:1007)]-v[1:1007]
data<-read.csv("2409.csv")
yoda=data$日報酬率..
yoda=yoda[c(1:1007)]
yoda1=yoda[c(1:1007)]-v[1:1007]
data<-read.csv("3008.csv")
daliguang=data$X3008
daliguang=daliguang[c(1:1007)]
daliguang1=daliguang[c(1:1007)]-v[1:1007]
data<-read.csv("3045.csv")
taigeda=data$日報酬率..
taigeda1=taigeda[c(1:1007)]-v[1:1007]

mean(yuanda1)
mean(yoda1)
mean(daliguang1     )
mean(taigeda1)
dat=cbind(yuanda,yoda,daliguang,taigeda)

head(dat)

dat=as.timeSeries(dat)



Frontier = portfolioFrontier(dat)


par(mfcol=c(1,2))
plot(Frontier)
?portfolioFrontier
tangencyLines(Frontier, return = c("mean", "mu"), 
              risk = "Cov", auto = TRUE )
frontierPlot(Frontier, frontier = c("both", "lower", "upper"),
             col = c("black", "grey"), add = FALSE, labels = TRUE,
             return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
             auto = TRUE, title = TRUE) 
sharpeRatioLines(Frontier, return = c("mean", "mu"), 
                 risk = c("Cov", "Sigma", "CVaR", "VaR"), auto = TRUE)
tailoredFrontierPlot(Frontier,
                     return = c("mean", "mu"), risk = c("Cov", "Sigma", "CVaR", "VaR"),
                     mText = NULL, col = NULL, xlim = NULL, ylim = NULL, 
                     twoAssets = T, sharpeRatio = F, title = TRUE)
