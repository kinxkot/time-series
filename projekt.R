#Projekt - szeregi czasowe
#Kinga Kotwica

#wczytanie bibliotek
getwd()
library("stats")
library("forecast")
library("viridis")   #paleta kolorow

#Szereg I----------------------------

#wczytywanie danych
dane1 <- read.csv("MRTSSM45111USN.csv")

#tworzenie szeregu czasowego
szereg1 <- ts(dane1$MRTSSM45111USN, start=c(1992, 1), frequency = 12)

#wykres plot()
plot(szereg1, main="Sprzeda? detaliczna: sklepy z artyku?ami sportowymi",
     xlab="Lata", ylab="Warto?? sprzeda?y") 

#wykres tsdisplay
tsdisplay(szereg1, main="Sprzeda? detaliczna: sklepy z artyku?ami sportowymi",
          xlab="Lata", ylab="Warto?? sprzeda?y") 


#wykres monthplot
monthplot(szereg1, main = "Sprzeda? detaliczna: sklepy z artyku?ami sportowymi", 
          xlab = "Miesi?ce", ylab= "Warto?? sprzeda?y")

#wykres seasonplot
seasonplot(szereg1, col = plasma(31), year.labels = TRUE,
           pch = 20, main = "Wahania sezonowe",
           xlab = "Miesi?c", ylab = "Warto?? sprzeda?y") 

#wykres pude?kowy boxplot
boxplot(szereg1 ~ cycle(szereg1), main = "Wykres pude?kowy", 
        xlab = "Cykl", ylab = "Warto?? sprzeda?y", col = viridis(12))

#wykres rozrzutu dla warto?ci op??nionych
lag.plot(szereg1, lags=12, do.lines = FALSE,  
         main = "Wykres rozrzutu dla warto?ci op??nionych") 


#wykresy ACF i PACF
acf(szereg1, main="Sprzeda? detaliczna: sklepy z artyku?ami sportowymi")

pacf(szereg1,lag.max=100)
pacf(szereg1, main="Sprzeda? detaliczna: sklepy z artyku?ami sportowymi")


#dekompozycja szeregu
szer_dek <- decompose(szereg1)
plot(szer_dek) 

szer_add <- decompose(szereg1, type = "additive")
plot(szer_add)

szer_mult <- decompose(szereg1, type = "multiplicative")
plot(szer_mult)


#dekompozycja na podstawie modelu regresji
#trend liniowy i sezonowo??
sz_T <- tslm(szereg1 ~ trend)
sz_TS <- tslm(szereg1 ~ trend + season)
plot(szereg1, main="Dekompozycja na podstawie modelu regresji",
     xlab = "Rok", ylab = "Warto?? sprzeda?y")
lines(fitted(sz_T), col = "blue", lty = 2)
lines(fitted(sz_TS), col = "red", lty = 2)


szer_odsezonowane <- seasadj(szer_add)
plot(szereg1, main = "Szereg oryginalny i odsezonowany", ylab="Warto?? sprzeda?y", xlab="Lata")
lines(szer_odsezonowane, col = "red", lty = 1, lwd = 2)
legend("topleft", legend = c("Szereg oryginalny", "Dane odsezonowane"), col = c("black","red"),
       lty = c(1,2))


#transformacja Boxa-Coxa
(lamb.auto <- BoxCox.lambda(szereg1))
szerL <- BoxCox(szereg1, lambda = lamb.auto)
tsdisplay(szerL)

#szereg stacjonarny
szerLS <- diff(szerL, lag = 12) 
tsdisplay(szerLS)

szerLST <- diff(szerLS, lag = 1)
tsdisplay(szerLST)


#sprawdzenie czy szereg jest realizacj? szumu bia?ego 
Acf(szerLST, lag.max = 100) #warto wzi?? pod uwag? MA(12), MA(14), MA(17), MA(22), MA(37), MA(86), MA(88)
Pacf(szerLST, lag.max = 100) #warto wzi?? pod uwag? AR(12), AR(23), AR(24), AR(30), AR(36), AR(48)


#wyznaczanie wsp??czynnik?w dla metody AR()
szerLST.m.ar36 <- ar(szerLST, aic = FALSE, order.max = 36, method = c("yule-walker"))
print(szerLST.m.ar36)
szerLST.m.ar36.mle <- ar(szerLST, aic = FALSE, order.max = 36, method = c("mle"))
print(szerLST.m.ar36.mle)

szerLST.m.ar12 <- ar(szerLST, aic = FALSE, order.max = 12, method = c("yule-walker"))
print(szerLST.m.ar12)
szerLST.m.ar12.mle <- ar(szerLST, aic = FALSE, order.max = 12, method = c("mle"))
print(szerLST.m.ar12.mle)


#automatycznie dobrana warto?? rz?du
szerLST.m.ar <- ar(szerLST, aic = TRUE, order.max = NULL, method = c("yule-walker"))
print(szerLST.m.ar)


#wyznaczanie wsp??czynnik?w MA()
szerLST.m.ar0i0ma36 <- Arima(szerLST, order = c(0,0,36))
summary(szerLST.m.ar0i0ma36) 


#funkcja auto.arima
auto.arima(szerLST, ic = "aicc")
auto.arima(szerLST, ic = "aic")
auto.arima(szerLST, ic = "bic")


#prognozowanie

sz_rwf <- rwf(szereg1, h = 24, drift = TRUE)
plot(sz_rwf, main="Prognoza uwzgl?dniaj?ca dryf")  #z uwzgl?dnieniem dryfu

sz_naive <- naive(szereg1, h = 24) 
plot(sz_naive, main="Prognozowanie naiwne") #naiwne

sz_sr <- meanf(szereg1, h = 24)
plot(sz_sr, main="Prognoza na podstawie ?redniej") #na podstawie sredniej

sz_se <- snaive(szereg1, h = 24)
plot(sz_se, main = "Metoda Naiwna Sezonowa") #sezonowa


#najlepsza metoda prognozowania
accuracy(sz_rwf)
accuracy(sz_naive)
accuracy(sz_sr)
accuracy(sz_se)


#SZEREG II---------------------------------

dane2 <- read.csv(file = "XTEXVA01PLM667S.csv")

szereg2 <- ts(dane2$XTEXVA01PLM667S, 
         start = c(1990, 01), frequency =12)

plot(szereg2, main = "Eksport: towary warto?ciowe dla Polski",
     xlab = "Lata", ylab = "Warto?? eksportu")

tsdisplay(szereg2, main = "Szereg czasowy",
          xlab = "Lata", ylab = "Warto?? eksportu") 


#wykres monthplot()
monthplot(szereg2, main = "Wykres szereg?w w kolejnych okresach", 
          xlab = "Miesi?ce", ylab= "Warto?ci") 

#wykres seasonplot()
seasonplot(szereg2, main = "Wahania sezonowe ", col = viridis(33),
           year.labels = TRUE, pch=20,
           xlab = "Miesi?ce", ylab= "Warto?ci") 

#wykres pude?kowy
boxplot(szereg2 ~ cycle(szereg2), main = "Wykres pude?kowy", 
        xlab = "Miesi?c", ylab = "Warto??", col = plasma(12))

#wykres rozrzutu dla warto?ci op??nionych
lag.plot(szereg2, lags=12, do.lines = FALSE,  
         main = "Wykres rozrzutu dla warto?ci op??nionych") 


#wykresy ACF i PACF
acf(szereg2, main="Eksport: towary warto?ciowe dla Polski")
pacf(szereg2, lag.max=100)
pacf(szereg2, main="Eksport: towary warto?ciowe dla Polski")


#dekompozycja
sz2_dek <- decompose(szereg2) 
plot(sz2_dek)

szer.add <- decompose(szereg2, type = "additive")
plot(szer.add)

szer.mult <- decompose(szereg2, type = "multiplicative")
plot(szer.mult)


#dekompozycja na podstawie modelu regresji
#trend liniowy i sezonowo??
sz.T <- tslm(szereg2 ~ trend)
sz.TS <- tslm(szereg2 ~ trend + season)
plot(szereg2, main="Dekompozycja na podstawie modelu regresji",
     xlab = "Rok", ylab = "Warto?? sprzeda?y")
lines(fitted(sz.T), col = "blue", lty = 2)
lines(fitted(sz.TS), col = "red", lty = 2)


#szereg z wyeliminowan? sezonowo?ci?
szer.odsezonowane <- seasadj(szer.mult)
plot(szereg2, main = "Szereg oryginalny i odsezonowany", ylab="Warto?? eksportu", xlab="Lata")
lines(szer.odsezonowane, col = "red", lty = 1, lwd = 2)
legend("topleft", legend = c("Szereg oryginalny", "Dane odsezonowane"), col = c("black","red"),
       lty = c(1,2))


#transformacja Boxa-Coxa
(lamb.auto <- BoxCox.lambda(szereg2))
szL <- BoxCox(szereg2, lambda = lamb.auto)
tsdisplay(szL)

#szereg stacjonarny
szLS <- diff(szL, lag = 12)
tsdisplay(szLS)

szLST <- diff(szLS, lag = 1)
tsdisplay(szLST)


#czy szum bia?y 
Acf(szLST, lag.max = 100) 
Acf(szLST, lag.max = 500)

Acf(szLST, lag.max = 100) #warto wzi?? pod uwag? MA(12)
Pacf(szLST, lag.max = 100) #warto wzi?? pod uwag? AR(12), AR(24), AR(36), AR(48)


#wyznaczanie wsp??czynnik?w AR()
szLST.m.ar12 <- ar(szLST, aic = FALSE, order.max = 12, method = c("yule-walker"))
print(szLST.m.ar12)
szLST.m.ar12.mle <- ar(szLST, aic = FALSE, order.max = 12, method = c("mle"))
print(szLST.m.ar12)

#automatycznie dobrana warto?? rz?du
szLST.m.ar <- ar(szLST, aic = TRUE, order.max = NULL, method = c("yule-walker"))
print(szLST.m.ar)


#wyznaczanie wsp??czynnik?w MA()
szLST.m.ar0i0ma36 <- Arima(szLST, order = c(0,0,36))
summary(szLST.m.ar0i0ma36) 


#funkcja auto.arima

auto.arima(szLST, ic = "aicc")
auto.arima(szLST, ic = "aic")
auto.arima(szLST, ic = "bic")


#prognozowanie

sz.rwf <- rwf(szereg2, h = 24, drift = TRUE)
plot(sz.rwf, main="Prognoza uwzgl?dniaj?ca dryf")  #z uwzgl?dnieniem dryfu

sz.naive <- naive(szereg2, h = 24) 
plot(sz.naive, main="Prognozowanie naiwne") #naiwne

sz.sr <- meanf(szereg2,h =24)
plot(sz.sr, main="Prognoza na podstawie ?redniej") #na podstawie sredniej

sz.se <- snaive(szereg2, h=24)
plot(sz.se, main = "Metoda Naiwna Sezonowa") #sezonowa


#najlepsza metoda prognozowania
accuracy(sz.rwf)
accuracy(sz.naive)
accuracy(sz.sr)
accuracy(sz.se)

