# CTRL+Shift+C kommentoi tai poistaa kommentit valituilta riveiltä

# Asetetaan työhakemisto SET Working Directory
# Muokkaa tarvittaessa oman työhakemistosi polku
# RStudiossa tämän voi tehdä myös valikosta Session->Set working directory
# setwd('Z:/Documents')
# asetetaan manuaalisesti sessionin kautta...

# Luetaan sähkönkulutus- ja lämpötiladata, hypätään headerrivin yli
eletemp = read.table(file = "sahko.csv",
                     sep = ";",
                     dec = ",",
                     skip = 1,
                     col.names = c('kWh','Celcius'))

# Sähkönkulutus aikasarjaksi
ele = ts(eletemp$kWh[1:816], start = 1, frequency = 24)

# Lämpötila kahdeksi aikasarjaksi: 816 ensimmäistä havaintoa käytetään mallin estimointiin
# ja 24 viimeistä havaintoa ennustamiseen.
temp = ts(eletemp$Celcius, start = 1, frequency = 24)
#temp = temp - mean(temp)

temp816 = ts(eletemp$Celcius[1:816], start = 1, frequency = 24)
# start parametrina vektori: 817. tunti = 35. päivän ensimmäinen tunti
temp24 = ts(eletemp$Celcius[817:840], start = c(35,1), frequency = 24)


# Plotataan aikasarjat
ts.plot(ele,
        xlab = "aika/vrk",
        ylab = "kulutus/kWh")
ts.plot(temp816,temp24,
        xlab = "aika/vrk",
        ylab = expression(~degree~C),
        col = c("black", "blue"))

# Määritellään 2x2 plottausruudukko.
par(mfrow=c(2,2))
# Jos haluat katsoa kuvia 2x2 matriisin sijaan yksitellen, niin
# par(mfrow=c(1,1))

# Plotataan autokorrelaatio-, osittaisautokorrelaatio- ja ristikorrelaatiofunktiot.
acf(ele, lag.max=168*3)
acf(ele, lag.max=168*3, type = "partial")
acf(temp, lag.max=168*3)
acf(temp, lag.max=168*3, type = "partial")
# Piirretään ristikorrelaatiofunktio omaan kuvaan
par(mfrow=c(1,1))
ccf(ele,temp, lag.max=168, main = "Korskorrelationen mellan elförbrukning och temperatur")

# Stationarisoidaan aikasarjat. Määrittele parametrit d,S,D
# Huomaa, että sähkönkulutuksen ja lämpötilan aikasarjojen differointien asteiden ei välttämättä tarvitse olla samoja.
d = 1 # Differoinnin kertaluku d
S = 168 # Kausidifferoinnin jakso S
D = 1 # Kausidifferensoinnin kertaluku D
dele = ele
dtemp = temp
if (d > 0) {
  dele = diff(dele, lag = 1, differences = d)
  dtemp = diff(dtemp, lag = 1, differences = d)
}
if (D > 0) {
  dele = diff(dele, lag = S, differences = D)
  dtemp = diff(dtemp, lag = 24, differences = D)
}

# Differoitujen aikasarjojen autokorrelaatio-, osittaisautokorrelaatio- ja ristikorrelaatiofunktiot.
acf(dele, lag.max=168)
acf(dele, lag.max=168, type = "partial")
acf(dtemp, lag.max=168)
acf(dtemp, lag.max=168, type = "partial")
ccf(dele, dtemp, lag.max=168)

ts.plot(dele, main = "Elförbrukning, stationär")
ts.plot(dtemp, main = "Temperatur, stationär")

# Estimoidaan malli ja lasketaan ennusteet ilman ulkoista muuttujaa.
p = 1
q = 2
P = 0
Q = 1
#a variable to define where in the series one wants to predict
#disabled in this code, still haunts in some indexing...
t= 0

malli = arima(ele[1:(816-t*24)],
              order = c(p,d,q),
              seasonal = list(order = c(P, D, Q),period = S),
              method = "CSS")
enne = predict(malli, n.ahead = 24)

# Estimoidaan malli lämpötilan kanssa. Määrää lämpötilan mahdollinen viive L.
L = 13

tempestimointi = eletemp$Celcius[1:(816-L)]
tempennuste = eletemp$Celcius[(816-L+1):(816-L+24)]
eleestimointi = ts(eletemp$kWh[(1+L):816], start = 1+L/24, frequency = 24)

malli2 = arima(eleestimointi,
               order = c(p,d,q),
               seasonal = list(order = c(P, D, Q), period = S),
               xreg = tempestimointi,
               method = "CSS")
enne2 = predict(malli2,
                n.ahead = 24,
                newxreg = tempennuste)

# Esimerkki Portmanteau-testistä. Onko residuaaliaikasarjan alussa nollia?
p1 = c(1:174)
for(i in c(1:174)){
        a = Box.test(malli2$residuals[(168+24+4):812],
         lag = i,
         type = "Ljung-Box",
         fitdf = p + q + P + Q)
        p1[i] = a$p.value
        }
print(p1)

p2 = c(1:174)
for(i in c(1:174)){
  a = Box.test(malli$residuals[(168+24+4):812],
               lag = i,
               type = "Ljung-Box",
               fitdf = p + q + P + Q)
  p2[i] = a$p.value
}
print(p2)
print(table((p2<0.05)))


# Palautetaan plottaus normaaliin 1x1 ruutuun
par(mfrow=c(1,1))

# Plotataan kuva sahkonkulutusaikasarjasta, mallin (1) sovitteesta,
# ennusteesta ja ennusteen 95 %:n eli 1,96*sigma-luottamusväleistä.
# ts.plot(ele,
#         ele - malli$residuals,
#         enne$pred,
#         enne$pred + 1.96*enne$se,
#         enne$pred - 1.96*enne$se,
#         col = c("black", "red", "blue", "blue", "blue"),
#         main  = "Sovite ja ennuste")

# Plotataan kuva pelkästä ennusteesta.
# ts.plot(enne$pred,
#         ele[(816-t*24+1):(816-(t-1)*24)],
#         enne$pred + 1.96*enne$se,
#         enne$pred - 1.96*enne$se,
#         col = c("black", "red", "blue", "blue"),
#         main = "Ennuste ja  95 %:n luottamusvälit")



ts.plot(ele,
        ele - malli2$residuals,
        enne2$pred,
        enne2$pred + 1.96*enne2$se,
        enne2$pred - 1.96*enne2$se,
        col = c("black", "red", "blue", "blue", "blue"),
        main  = "Anpassning och prognos")

# Plotataan kuva pelkästä ennusteesta.
ts.plot(enne2$pred,#- 1.96*enne2$se,
        ele[(816-t*24+1):(816-(t-1)*24)],
        enne2$pred + 1.96*enne2$se,#- 1.96*enne2$se,
        enne2$pred - 1.96*enne2$se,#- 1.96*enne2$se,
        col = c("black", "red", "blue", "blue"),
        main = "Prognos och 95 %:s konfidensintervall")



# Kirjoitetaan ennuste ja luottamusvälit .csv-tiedostoon, jonka voi avata Excelillä.
output = cbind(enne2$pred,
               enne2$pred + 1.96*enne2$se,
               enne2$pred - 1.96*enne2$se)
write.csv2(output, file = 'ennuste.csv')



# ele[1:816] <- lapply(ele[1:816], as.numeric)
# lm_t = lm(ele[1:800]~temp816[1:800])
# predict(lm_t, newdata =  temp816[801:816])
# summary(lm_t)

acf(malli2$residuals, lag.max = 300)
acf(malli2$residuals, lag.max = 300, 'partial')
hist(p1, breaks = 12, main = "Ljung-Box p-värden med temperatur")
hist(p2, breaks = 12, main = "Ljung-Box p-värden utan temperatur")
plot(malli2$residuals, main = "Residualer för modelen")
#plot(malli$residuals)
print(table((p1<0.05)))
