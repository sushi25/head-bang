#multivariate ARIMA 
library(marima)
install.packages("marima")
library(marima)
data(C20)
selects <- c(2, 7, 11)
??selects
cat("Multivariate model for ", colnames(C20)[selects]," \n")

Data <- data.frame(C20[,selects])
colnames(Data) <- colnames(C20)[selects]

log.Data <- log(Data)
kvar <- lengths(selects)
k <- c(1:kvar)
difs <- rep(1, length(selects))
difference <- rbind (k, difs)
dlog.Data <- 100*t(define.dif(log.Data,difference)$y.dif)
cat("dlog.Data represents the percentage change from one day to another. \n")
mod <- define.model(kvar = kvar, ar=c(1:2),ma=c(1))
Model <- marima(dlog.Data, ar.pattern = mod$ar.pattern, ma.pattern = mod$ma.pattern, penalty = 2)
short.form(Model$ar.estimates, leading = FALSE)
short.form(Model$ma.estimates, leading = FALSE)
