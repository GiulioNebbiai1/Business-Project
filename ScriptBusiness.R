###Libraries##################################
library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)
library(lattice)
library(mgcv)
library(gridExtra)
library(grid)
library(ggplot2)
library(corrplot)
install.packages("car")
library(car)
##############################################

### Upload Data

data = read.csv("C:\\Users\\notebook\\Desktop\\dataset definitivo.csv", sep = ";", dec=",", header=TRUE)
tail(data)
### Data Presentation

summary(data)
str(data)
head(data)

###########################################
########### DATA PRE-PROCESSING ###########
###########################################

### Missing Data imputation ###

### inc_influ missing data imputation ###

################# LINEAR REGRESSION ######################
modello_reg <- lm(inc_influ ~ casi_covid + temp, data = data)
summary(modello_reg)
nuovi_dati <- data.frame(casi_covid = data$casi_covid[1:9], temp = data$temp[1:9])
nuovi_dati$inc_influ <- NA
previsioni <- predict(modello_reg, newdata = nuovi_dati[1:9, , drop = FALSE])
print(previsioni)

nuovi_dati2 <- data.frame(casi_covid = data$casi_covid[53:58], temp = data$temp[53:58])
nuovi_dati2$inc_influ <- NA
previsioni2 <- predict(modello_reg, newdata = nuovi_dati2[1:6, , drop = FALSE])
print(previsioni2)

################ POLINOMIAL REGRESSION ###################
modello_polinomiale <- lm(inc_influ ~ casi_covid + temp + I(casi_covid^2) + I(temp^2), data = data)
summary(modello_polinomiale)
nuovi_dati <- data.frame(casi_covid = data$casi_covid[1:9], temp = data$temp[1:9])
nuovi_dati$inc_influ <- NA
previsioni3 <- predict(modello_polinomiale, newdata = nuovi_dati[1:9, , drop = FALSE])
print(previsioni3)

previsioni4 <- predict(modello_polinomiale, newdata = nuovi_dati2[1:6, , drop = FALSE])
print(previsioni4)

################# REGRESSION + SEASONAL & TREND #######################
influ.ts<-ts(data$inc_influ, frequency=12)
fit_ts <- tslm(influ.ts~ trend+season)
summary(fit_ts)
nuovi_dati$inc_influ <- NA
previsioni5 <- predict(fit_ts, newdata = ts(nuovi_dati[1:9, , drop = FALSE], frequency=12))
print(previsioni5)
previsioni6 <- predict(fit_ts, newdata = ts(nuovi_dati2[1:6, , drop = FALSE], frequency=12))
print(previsioni6)

data$infl_lin = c(previsioni, data$inc_influ[10:52], previsioni2)
data$infl_quad = c(previsioni3, data$inc_influ[10:52], previsioni4)
data$infl_seas = c(previsioni5[1:9], data$inc_influ[10:52], previsioni6[53:58])

### Different Imputations Comparison ###

par(mfrow = c(1,1))
plot(data$inc_influ, ylim= c(min(data$infl_seas),15), ylab = "Flu incidence",xlab="Time",main="Flu Incidence + Data imputations")
lines(data$infl_lin, col = "green")
lines(data$infl_quad, col= "magenta")
lines(data$infl_seas, col="blue")
lines(data$inc_influ)
legend("topleft", legend = c("Linear Estimation", "Polynomial Estimation","Trend+Season Estimation"), col = c("green", "magenta","blue"), lty = c(1,1,1), bty = "n", cex=0.8)

### We select poly_infl as the best data imputation ###
data = data[, -c(7,12, 14)]
colnames(data)[colnames(data) == "infl_quad"] <- "poly_infl"
data$poly_infl = round(data$poly_infl, 2)
data

### We turn the variables into time series ###
sales_ts = ts(data$sales_c1, start=c(2019,1), frequency = 12)
units_ts = ts(data$units_c1, start=c(2019,1), frequency = 12)
sales.m_ts = ts(data$m_s_c1, start=c(2019,1), frequency = 12)
units.m_ts = ts(data$m_s_units_c1, start=c(2019,1), frequency = 12)
covid_ts = ts(data$casi_covid, start=c(2019,1), frequency = 12)
temp_ts = ts(data$temp, start=c(2019,1), frequency = 12)
gtrend_ts = ts(data$Gtrends_index, start=c(2019,1), frequency = 12)
disoc_ts = ts(data$Tasso_disoccupazione, start=c(2019,1), frequency = 12)
vecchiaia_ts = ts(data$Indice_vecchiaia, start=c(2019,1), frequency = 12)
influenza_ts = ts(data$poly_infl, start=c(2019,1), frequency = 12)

par(mfrow=c(2,1),mar=c(4,4,2,2))
#plot(sales_ts, ylab="Sales")
plot(decompose(sales_ts,type="multiplicative")$seasonal, ylab="Sales seasonality",lwd=1.5,cex=1.5)
plot(temp_ts, ylab= "Temperature",lwd=1.5,cex=1.5)

###############
##### EDA #####
###############

par(mfrow = c(1,2))
#sales
plot(sales_ts)
boxplot(sales_ts)

#units
plot(units_ts)
boxplot(units_ts)

#market_sales
plot(sales.m_ts)
boxplot(sales.m_ts)

#market_unit
plot(units.m_ts)
boxplot(units.m_ts)

#covid
plot(covid_ts)
boxplot(covid_ts)

#Gtrend
plot(gtrend_ts)
boxplot(gtrend_ts)

#Disoccupazione
plot(disoc_ts)
boxplot(disoc_ts)

#Indice_vecchiaia
plot(vecchiaia_ts)
boxplot(vecchiaia_ts)

#Influenza
plot(influenza_ts)
boxplot(influenza_ts)


### Correlations ####
#Analizziamo la correlazione tra le prime 4 variabili per vedere se vi è una differenza tra l'andamento delle vendite
# e quello delle unità prodotte, sia da Zambon che, in generale, nel mercato di riferimento
pairs(data[,c(2,3,4,5)], pch = 16, main = "Scatterplot Matrix")
### Sales vs Units/Market Sales/Market Units 
data_sales = data[, -c(3,4,5)]
#season plot 
par(mfrow = c(2,1))
### sales vs units
seasonplot(sales_ts, ylab="Sales", xlab="Month", main="Seasonal plot: Sales", year.labels=T, year.labels.left=T, col=1:5, pch=19)
seasonplot(units_ts, ylab="Units", xlab="Month", main="Seasonal plot: Units", year.labels=T, year.labels.left=T, col=1:5, pch=19)
### sales vs market sales
seasonplot(sales_ts, ylab="Sales", xlab="Month", main="Seasonal plot: Sales", year.labels=T, year.labels.left=T, col=1:5, pch=19)
seasonplot(sales.m_ts, ylab="Market Sales", xlab="Month", main="Seasonal plot: Market Sales", year.labels=T, year.labels.left=T, col=1:5, pch=19)
### units vs market units
seasonplot(units_ts, ylab="Units", xlab="Month", main="Seasonal plot: Units", year.labels=T, year.labels.left=T, col=1:5, pch=19)
seasonplot(units.m_ts, ylab="Market Units", xlab="Month", main="Seasonal plot: Market Units", year.labels=T, year.labels.left=T, col=1:5, pch=19)

data_sales
pairs(data_sales[,-1], pch = 16, main = "Scatterplot Matrix")
cor(data_sales[-1])
levelplot(cor(data_sales[-1]), col.regions = colorRampPalette(c("lightblue", "blue", "navy"))(19),
          main = "Correlation Matrix")

par(mfrow = c(3,1))
### vedere se mettere### 
### in tal caso aggiustare grafici
plot((sales_ts-min(sales_ts))/(max(sales_ts)-min(sales_ts)))
plot((temp_ts-min(temp_ts))/(max(temp_ts)-min(temp_ts)), col=2)
plot((influenza_ts-min(influenza_ts))/(max(influenza_ts)-min(influenza_ts)), col=3)


### DATA ANALYSIS ###

##########
#Confronto tra zambon e mercato
##############
par(mfrow = c(1,1))
market =(sales.m_ts - min(sales.m_ts)) / (max(sales.m_ts) - min(sales.m_ts))
min(sales.m_ts)
zambon =(sales_ts - min(sales_ts)) / (max(sales_ts) - min(sales_ts))
plot(market)
lines(zambon, col = "darkgreen", lty = 2)
legend("bottomright", legend = c("Market", "Zambon"), col = c("black", "darkgreen"), lty = c(1, 2), bty = "n")

# dati prodotto 1

data1 = read.csv("C:\\Users\\notebook\\Desktop\\dataset definitivo.csv", sep = ";", dec=",", header=TRUE)
data1 = data1[1:58,]
sales_p1 = data1$sales_c1
sales_p1_ts = ts(sales_p1, start=c(2019,01), frequency = 12)
plot(decompose(sales_p1_ts,type="multiplicative"))
trend_p1 = decompose(sales_p1_ts,type="multiplicative")$trend
trend_p1 = as.numeric(trend_p1)

# dati prodotto 2

data2 = read_excel("C:\\Users\\notebook\\Desktop\\pdt2_cnt1.xlsx")
summary(data2)
str(data2)
par(mfrow=c(1,1))
plot(data2$sales_c1)
lines(data2$sales_c1)
sales_c1_p2 = data2$sales_c1
sales_c1_p2_ts = ts(sales_c1_p2, start=c(2019,1), frequency = 12)

ts.plot(sales_c1_p2_ts)
decompose(sales_c1_p2_ts,type="multiplicative")
plot(decompose(sales_c1_p2_ts,type="multiplicative"))
trend_p2 = decompose(sales_c1_p2_ts,type="multiplicative")$trend
trend_p2 = as.numeric(trend_p2)

graf_p2_decomp =plot(decompose(sales_c1_p2_ts,type="multiplicative"))

# product 1 vs product 2 trend

par(mfrow=c(2,1))
plot(trend_p1)
lines(trend_p1)
plot(trend_p2)
lines(trend_p2)
min_trend_p2 = min(trend_p2, na.rm=TRUE)
max_trend_p2 = max(trend_p2, na.rm=TRUE)
trend_p2
trend_p2 = na.omit(trend_p2)
trend_p2_scale = (trend_p2-min_trend_p2)/(max_trend_p2-min_trend_p2)

trend_p1 = na.omit(trend_p1)
min_trend_p1 = min(trend_p1, na.rm=TRUE)  
max_trend_p1 = max(trend_p1, na.rm=TRUE)  
trend_p1_scale = (trend_p1-min_trend_p1)/(max_trend_p1-min_trend_p1)
par(mfrow=c(1,1))
plot(trend_p1_scale,type="l", col="magenta", xlab = "time", ylab="trend", lty=2, lwd=2, main="Trend Comparison")
lines(trend_p2_scale, col = 3, lty =1, lwd=2)
legend("bottomright", legend = c("Product 1 Trend", "Product 2 Trend"), col = c("magenta", 3), lty = c(2,1), bty = "o", cex=1)
corr_tp1_tp2 = cor(trend_p1,trend_p2)
corr_tp1_tp2
datii = data.frame(trend_p1,trend_p2)
library(lattice)
pairs(datii)

# product 2 vs market 2

market_p2 = data2$m_sales_c1
market_p2_ts = ts(market_p2, start=c(2019,1), frequency = 12)
trend_market_p2 = decompose(market_p2_ts,type="multiplicative")$trend
plot(trend_market_p2)
trend_market_p2 = na.omit(trend_market_p2)
trend_market_p2 = as.numeric(trend_market_p2)
min_trend_m_p2 = min(trend_market_p2, na.rm=TRUE)
max_trend_m_p2 = max(trend_market_p2, na.rm=TRUE)
trend_market_p2 = na.omit(trend_market_p2)
trend_p2_m_scale = (trend_market_p2-min_trend_m_p2)/(max_trend_m_p2-min_trend_m_p2)

par(mfrow=c(1,1))
plot(trend_p2_m_scale,type="l", col=1, xlab = "time", ylab="trend", lwd=2, main="Trend Comparison")
lines(trend_p2_scale, col = 3, lty =1, lwd=2)
legend("bottomright", legend = c("Market 2 Trend", "Product 2 Trend"), col = c(1, 3), lty = c(1,1), bty = "o", cex=0.8)
corr_tm2_tp2 = cor(trend_market_p2,trend_p2)

##Comparison between Zambon sales trend (prod 1) and Market sales trend----


trend_p1 = decompose(sales_p1_ts,type="multiplicative")$trend
trend_p1 = as.numeric(trend_p1)

ts.plot(sales.m_ts)
decompose(sales.m_ts,type="multiplicative")
plot(decompose(sales.m_ts,type="multiplicative"))
trend_m1 = decompose(sales.m_ts,type="multiplicative")$trend
trend_m1 = as.numeric(trend_m1)

graf_m1_decomp =plot(decompose(sales.m_ts,type="multiplicative"))

par(mfrow=c(2,1))
plot(trend_p1)
lines(trend_p1)
plot(trend_m1)
lines(trend_m1)
min_trend_m1 = min(trend_m1, na.rm=TRUE)
max_trend_m1 = max(trend_m1, na.rm=TRUE)
trend_m1
trend_m1 = na.omit(trend_m1)
trend_m1_scale = (trend_m1-min_trend_m1)/(max_trend_m1-min_trend_m1)

par(mfrow=c(1,1))
trend_p1_scale_ts = ts(trend_p1_scale, start=c(2019,1), frequency = 12)
trend_m1_scale_ts = ts(trend_m1_scale, start=c(2019,1), frequency = 12)
plot(trend_p1_scale_ts,type="l", col="magenta", xlab = "Time", ylab="Scaled trend", lwd=2, main="Trend Comparison")
lines(trend_m1_scale_ts, lty =1, lwd=2)
legend("bottomright", legend = c("Product 1 Trend", "Market 1 Trend"), col = c("magenta", 1), lty = c(1,1), bty = "o", cex=0.7)
trend_p1 = na.omit(trend_p1)
corr_tp1_tm1 = cor(trend_p1,trend_m1)
corr_tp1_tm1
datii = data.frame(trend_p1,trend_m1)
pairs(datii)


#####################
### Data Modeling ###
#####################

###########
#Linear model
###########

#Model with trend
fit1 <- tslm(sales_ts ~ trend)
summary(fit1)
AIC(fit1)
plot(sales_ts, ylab="Sales", xlab="Time")
lines(fitted(fit1), col=2)
#check the residuals
dwtest(fit1)
res1 <- residuals(fit1)
plot(res1, ylab="residuals")
Acf(res1)
Pacf(res1)

#Model with trend and seasonality
fit2 <- tslm(sales_ts ~ trend+season)
summary(fit2)
AIC(fit2)

plot(sales_ts, ylab="Sales", xlab="Time")
lines(fitted(fit2), col=2)
#check the residuals
dwtest(fit2)
res2 <- residuals(fit2)
plot(res2, ylab="residuals")
Acf(res2)
Pacf(res2)

#Model with trend and seasonality + regressors

fit3 <- tslm(sales_ts ~ trend + season + covid_ts + temp_ts + gtrend_ts + 
               disoc_ts + vecchiaia_ts + influenza_ts)
summary(fit3)
AIC(fit3)

plot(sales_ts ,type="l", xlab="Time", ylab="Sales")
lines(fitted(fit3), col=2)

dwtest(fit3)

###check the residuals
res3<- residuals(fit3)
plot(res3, xlab="Time", ylab="Residuals", type="l")
Acf(res3)
Pacf(res3)

### We drop temp_ts because there's collinearity with seasons
fit4 <- tslm(sales_ts ~ trend + season + covid_ts + gtrend_ts + 
               disoc_ts + vecchiaia_ts + influenza_ts)
summary(fit4)
AIC(fit4)

plot(sales_ts ,type="l", xlab="Time", ylab="Sales")
lines(fitted(fit4), col=2)

dwtest(fit4)

###check the residuals
res4<- residuals(fit4)
plot(res4, xlab="Time", ylab="Residuals", type="l")
Acf(res4)
Pacf(res4)

#### We drop gtrends_ts
fit5 <- tslm(sales_ts ~ trend + season + covid_ts + 
               disoc_ts + vecchiaia_ts + influenza_ts)
summary(fit5)
AIC(fit5)

plot(sales_ts ,type="l", xlab="Time", ylab="Sales")
lines(fitted(fit5), col=2)

dwtest(fit5)


###check the residuals
res5<- residuals(fit5)
plot(res5, xlab="Time", ylab="Residuals", type="l")
Acf(res5)
Pacf(res5)

### we drop vecchiaia_ts
fit6 <- tslm(sales_ts ~ trend + season + covid_ts + 
               disoc_ts + influenza_ts)
summary(fit6)
AIC(fit6)

plot(sales_ts ,type="l", xlab="Time", ylab="Sales", main="Linear model Fitting")
lines(fitted(fit6), col="4")
legend(legend=c("Observed Data", "Fitted Data"), "top", lty=c(1,1), col=c(1,4),cex=0.9)


accuracy(fitted(fit6), sales_ts)
dwtest(fit6)

###check the residuals
res6<- residuals(fit6)
par(mfrow=c(2,1))
plot(res6, xlab="Time", ylab="Residuals", type="l")
Acf(res6)
Pacf(res6)
checkresiduals(res6)

### We evaluate the AICS
AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(fit5)
AIC(fit6)
### We select fit6 because it has the lowest AIC

# armax-reg lineare
# with manual Arima
armax1<- Arima(sales_ts, xreg=fitted(fit6), order = c(1,0,1))
summary(armax1)
resarmax1<- residuals(armax1)
checkresiduals(resarmax1)
Acf(resarmax1)
Pacf(resarmax1)
fitted(armax1)
plot(sales_ts)
lines(fitted(armax1), col=2)

# with auto.arima
armax.auto = auto.arima(sales_ts,xreg=fitted(fit6))
summary(armax.auto)
resarmax.auto<- residuals(armax.auto)
checkresiduals(resarmax.auto)
Acf(resarmax.auto)
Pacf(resarmax.auto)
fitted(armax.auto)
par(mfrow=c(1,1))
plot(sales_ts,xlab="Time", ylab="Sales", main="ARMAX Fitting")
lines(fitted(armax.auto), col=4)
legend(legend=c("Observed Data", "Fitted Data"), "top", lty=c(1,1), col=c(1,4),cex=0.9)

### Selezione Stepwise o Backward

#modello saturo

fit7 <- lm(sales_ts ~ data$Gtrends_index + data$casi_covid + data$temp + data$Tasso_disoccupazione + data$Indice_vecchiaia + data$poly_infl, data = data)

model_stepwise<- step(fit7, direction = "both")
model_backward<- step(fit7, direction = "backward")
accuracy(fitted(fit7), sales_ts)
summary(model_stepwise)
summary(model_backward)
AIC(model_stepwise)
plot(sales_ts ,type="l", xlab="Time", ylab="Sales")
stepw = ts(fitted(model_stepwise), start=c(2019,1), frequency = 12)
lines(stepw, col=2)

dwtest(model_stepwise)

###check the residuals
res_stp<- residuals(model_stepwise)
plot(res_stp, xlab="Time", ylab="Residuals", type="l")
Acf(res_stp)
Pacf(res_stp)

# armax with auto.arima
armax.auto2 = auto.arima(sales_ts,xreg=stepw)
summary(armax.auto2)
resarmax.auto2<- residuals(armax.auto2)
checkresiduals(resarmax.auto2)
Acf(resarmax.auto2)
Pacf(resarmax.auto2)
fitted(armax.auto2)
plot(sales_ts)
lines(fitted(armax.auto2), col=4)

### ARMAX NON SIGNIFICATIVO

###############
#Arima
###############
plot(sales_ts)
Acf(sales_ts)
Pacf(sales_ts)
tsdisplay(sales_ts)

#First Arima Model
a1 = Arima(sales_ts, order = c(1, 1, 0))
summary(a1)
resida1<- residuals(a1)
tsdisplay(resida1)

plot(sales_ts)
lines(fitted(a1), col=2)

fora1<- forecast(a1) #we do forecast for ARIMA 1 and we can provide a plot fo some point ahead
plot(fora1)

#Second Arima Model

a2 = Arima(sales_ts, order = c(1, 0, 0), seasonal = c(0,1,1))
summary(a2)
resida2<- residuals(a2)
tsdisplay(resida2)

plot(sales_ts, ylim = c(1000, 4500000))
lines(fitted(a2), col=2)

fora2<- forecast(a2) #we do forecast for ARIMA 1 and we can provide a plot fo some point ahead
plot(fora2)

accuracy(sales_ts,fitted(a2))

#auto.arima
autoa = auto.arima(sales_ts, seasonal=TRUE)
summary(autoa)
residauto<- residuals(autoa)
tsdisplay(residauto)

plot(sales_ts, ylim=c(min(fitted(autoa)), max(sales_ts)+10))
lines(fitted(autoa), col=2)

forauto<- forecast(autoa) #we do forecast for ARIMA 1 and we can provide a plot fo some point ahead
plot(forauto)


### GAM model

gam_model <- gam(sales_ts ~ s(covid_ts) + s(temp_ts) + influenza_ts)
summary(gam_model)
plot(gam_model,main="Non linear effects")
par(mfrow=c(1,1))
plot(sales_ts, ylab="Sales")
lines(ts(fitted(gam_model),start=c(2019,1),frequency = 12),col=4)
legend(legend=c("Observed series", "Fitted values"), "top", lty=c(1,1), col=c(1, 4), cex=0.9)
dwtest(gam_model)

### TS DECOMPOSITION ANALYSIS ###

par(mfrow=c(1,1))
plot(decompose(sales_ts, type="additive"))
plot(decompose(sales_ts)$trend)
trend_p = ts(decompose(sales_ts)$trend, start = c(2019, 1),frequency=12)
plot(decompose(sales_ts)$seasonal)
seasonal_p = ts(decompose(sales_ts)$seasonal, start = c(2019, 1),frequency=12)
plot(decompose(sales_ts)$random)
random_p = ts(decompose(sales_ts)$random, start = c(2019, 1),frequency=12)


################
##1. Analisi trend
################
### gam + splines

plot(trend_p)
x = 1:58
y = trend_p
g2<- gam(y~s(x))
summary(g2)
par(mfrow=c(1,1))
plot(g2, se=T)
AIC(g2)

### We build an armax on g2
#######perform analysis of residuals
tsdisplay(residuals(g2))
aar1<- auto.arima(residuals(g2))
aar1
plot(as.numeric(y), type="l")
lines(c(rep(NA, 6), fitted(aar1)+ fitted(g2)), col=4)

####################
#2. analisi seasonality + random
####################
plot(seasonal_p+random_p)
fit3 <- tslm(seasonal_p+random_p ~ covid_ts + temp_ts + influenza_ts)
summary(fit3)
AIC(fit3)

plot(seasonal_p + random_p ,type="l", xlab="Time", ylab="Seasonality")
lines(fitted(fit3), col=2)
dwtest(fit3)

plot(sales_ts)

tr=ts(c(rep(NA, 6), fitted(aar1)+ fitted(g2), rep(NA, 6)), start = c(2019, 1), frequency=12)
length(tr)
seass_random= ts((fitted(fit3)), start = c(2019, 1), frequency=12)
length(seass_random)

ricostruzione= tr+seass_random
ricostruzione = ts(ricostruzione, start=c(2019, 1), frequency=12)
plot(sales_ts)
lines(ricostruzione, col=2)
pred_rico = forecast(ricostruzione,h=9)
plot(pred_rico)
#stimiamo i residui
resid_rico = sales_ts-ricostruzione
plot(resid_rico)
Acf(resid_rico)
Pacf(resid_rico)
checkresiduals(resid_rico)

#########################
### Exponential Smoothing
#########################

##1.Simple exponential smoothing
ses<- ses(sales_ts, initial="optimal", h=3) ##1 di default
summary(ses)
ses1<- ses(sales_ts, h=3, alpha=0.8)
summary(ses1)
ses2<- ses(sales_ts, alpha=0.6, initial="simple", h=3)
summary(ses2)


plot(sales_ts, ylab="Sales", xlab="Year")
points(sales_ts)
lines(fitted(ses), col="blue", type="o")
lines(fitted(ses1), col="red", type="o")
lines(fitted(ses2), col="green", type="o")
legend(legend=c("estimated", "0.8", "0.6"), "top", lty=c(1,1,1), col=c("blue","red","green"))

fc1<- ses(sales_ts, alpha=0.8, h=3)
round(accuracy(fc1), 2)

#scegliamo ses() calcolato con alpha 0.8
autoplot(fc1)+
  autolayer(fitted(fc1), series="Fitted")+ylab("0.8")+xlab("Year")

##2.Trend methods (Holt method)

fch<- holt(sales_ts, h=5)
fch3<- holt(sales_ts, damped=T, phi=0.9, h=5)
fch2<- holt(sales_ts, damped=T, phi = 0.8, h=5)

autoplot(sales_ts)+
  autolayer(fch, series="Holt's method", PI=F)+
  autolayer(fch2, series="Damped Holt's method", PI=F)+
  autolayer(fch3, series="Damped Holt's method 0.9", PI=F)

###3.Trend and seasonality methods (Holt-Winters method)

hw1<- hw(sales_ts, seasonal="additive")
summary(hw1)
hw2<- hw(sales_ts, seasonal="multiplicative")
summary(hw2)
hw3<- hw(sales_ts, beta=1e-01, seasonal="additive")
summary(hw3)
hw4<- hw(sales_ts,alpha=0.8, beta=1e-01, seasonal="multiplicative",h=5)
summary(hw4) # best one
par(mfrow=c(1,1))
plot(hw4)
p1 = autoplot(sales_ts)+
  autolayer(hw1, series="Holt-Winters' method", PI=F)+
  ggtitle("Additive Holt_Winters")

p2 = autoplot(sales_ts)+
  autolayer(hw2, series="Holt-Winters' method", PI=F)+
  ggtitle("Multiplicative Holt_Winters")

### beta=1e-01

p3 = autoplot(sales_ts)+
  autolayer(hw3, series="Holt-Winters' method", PI=F)+
  ggtitle("Additive Holt_Winters")

p4 = autoplot(sales_ts)+
  autolayer(hw4, series="Holt-Winters' method", PI=F)+
  ggtitle("Multiplicative Holt_Winters")

grid.arrange(p1, p2, p3, p4,  nrow=2, ncol = 2)



######################
###### FORECAST #####
### We selected the best models ###

### 1. Armax ###
# with auto.arima
fit6 <- tslm(sales_ts ~ trend + season + covid_ts + 
               disoc_ts + influenza_ts)
summary(fit6)
AIC(fit6)

plot(sales_ts ,type="l", xlab="Time", ylab="Sales")
lines(fitted(fit6), col=2)
accuracy(fitted(fit6), sales_ts)
dwtest(fit6)

###check the residuals
res6<- residuals(fit6)
plot(res6, xlab="Time", ylab="Residuals", type="l")
Acf(res6)
Pacf(res6)

armax.auto = auto.arima(sales_ts,xreg=fitted(fit6))
summary(armax.auto)
resarmax.auto<- residuals(armax.auto)
checkresiduals(resarmax.auto)
Acf(resarmax.auto)
Pacf(resarmax.auto)
fitted(armax.auto)
plot(sales_ts)
lines(fitted(armax.auto), col=4)

### The model is the one above

sales_ts = ts(data$sales_c1, start=c(2019,1), frequency = 12)
covid_ts = ts(data$casi_covid, start=c(2019,1), frequency = 12)
disoc_ts = ts(data$Tasso_disoccupazione, start=c(2019,1), frequency = 12)
influenza_ts = ts(data$poly_infl, start=c(2019,1), frequency = 12)


#previsione covid
covid_ndg = c(5277552, 4408757, 4408757,4408757,4408757)
covid_ndg = ts(covid_ndg, start=c(2023,11), frequency=12)
covid_ndg
covid_2024 = c(covid_ts, covid_ndg)
covid_2024 = ts(covid_2024, start=c(2019,1), frequency=12)
plot(covid_2024)

#previsione disoccupazione
mod_disoc = Arima(disoc_ts, order=c(2,1,1))

summary(mod_disoc)
plot(disoc_ts ,type="l", xlab="Time", ylab="Covid")
lines(fitted(mod_disoc), col=2)
disoc_ndg = predict(mod_disoc, n.ahead=5)
disoc_2024 = c(disoc_ts, disoc_ndg$pred)
disoc_2024 = ts(disoc_2024, start=c(2019,1), frequency=12)
disoc_2024
plot(disoc_2024)

#previsione influenza
mod_influ = auto.arima(influenza_ts, seasonal=TRUE)

summary(mod_influ)
plot(influenza_ts ,type="l", xlab="Time", ylab="Influenza")
lines(fitted(mod_influ), col=2)
influ_ndg = predict(mod_influ, n.ahead=5)
influ_ndg
influ_2024 = c(influenza_ts, influ_ndg$pred)
influ_2024 = ts(influ_2024, start=c(2019,1), frequency=12)
influ_2024
plot(influ_2024)


fit6$model$trend
fit6$model$season
season = c(11, 12, 1,2,3)
season = as.factor(season)
trend = c(59, 60, 61,62,63)
trend

influenza_ts = ts(data$poly_infl, start=c(2019,1), frequency = 12)
disoc_ts = ts(data$Tasso_disoccupazione, start=c(2019,1), frequency = 12)
covid_ts = ts(data$casi_covid, start=c(2019,1), frequency = 12)

fit6 <- tslm(sales_ts ~ trend + season + covid_ts + 
               disoc_ts + influenza_ts)

covid_ts = covid_ndg
disoc_ts = disoc_ndg$pred
influenza_ts = influ_ndg$pred
data_prev_linear = data.frame(trend, season, covid_ts, disoc_ts, influenza_ts)
data_prev_linear

prev_linear = forecast(fit6, newdata = data_prev_linear)
#armax.auto = auto.arima(sales_ts,xreg=fitted(fit6))
#summary(armax.auto)

previsioni <- forecast(armax.auto, h = 5, xreg = prev_linear$mean)
plot(previsioni)


### 2. Arima ###
# with auto.arima

autoa = auto.arima(sales_ts, seasonal=TRUE)

plot(sales_ts, ylim=c(min(fitted(autoa)), max(sales_ts)+10))
lines(fitted(autoa), col=2)

forauto<- forecast(autoa,h=5) 
plot(forauto)

par(mfrow=c(3,1))
plot(previsioni)
plot(forauto)
plot(hw4)
