rm(list=ls())


library(tidyverse)
library(irr)
library(psych)


## 2 farkli veri Nominal ve Ordinal

nominal <- read_csv("https://raw.githubusercontent.com/rnzbrk/R-files-/master/irrNom.csv")
ordinal <- read_csv("https://raw.githubusercontent.com/rnzbrk/R-files-/master/irrOrdinal.csv")



## agree fonksiyonu

agree(nominal[,-1])
agree(ordinal[,-1])


# tolerance argumani
agree(ordinal[,-1])
agree(ordinal[,-1], tolerance = 1)
agree(ordinal[,-1], tolerance = 2)


### Intraclass correlation 

iccout1 <- icc(
  ordinal[,-1], model = "twoway", 
  type = "agreement", unit = "single"
)

iccout1

iccout1$value


iccout2 <- icc(
  ordinal[,-1], model = "twoway", 
  type = "consistency", unit = "single"
)

iccout2


iccout3 <- icc(
  ordinal[,-1], model = "oneway", 
  type = "agreement", unit = "single"
)

iccout3


iccout4 <- icc(
  ordinal[,-1], model = "oneway", 
  type = "agreement", unit = "average"
)

iccout4



#### Kappa Capraz Tablo

# Contingency table
xtab <- as.table(rbind(c(25, 10), c(15, 20)))
# Descriptive statistics
kosegen <- diag(xtab)
N <- sum(xtab)
satir_oran <- rowSums(xtab)/N
sutun_oran <- colSums(xtab)/N
# Compute kappa (k)
Po <- sum(kosegen)/N
Pe <- sum(satir_oran*sutun_oran)
k <- (Po - Pe)/(1 - Pe)
k

rm(xtab,kosegen,N,satir_oran, sutun_oran, Po,Pe, k)
### Verimize bakalim

veri1 <- cbind(P1=sample(c(0,1), replace=TRUE, size=100), P2=sample(c(0,1), replace=TRUE, size=100))

xtab <- xtabs(~P1 + P2, data = veri1)

kosegen <- diag(xtab)
N <- sum(xtab)
satir_oran <- rowSums(xtab)/N
sutun_oran <- colSums(xtab)/N
# Compute kappa (k)
Po <- sum(kosegen)/N
Pe <- sum(satir_oran*sutun_oran)
k <- (Po - Pe)/(1 - Pe)
k


### simdi de irr ile bulalim
irr::kappa2(veri1)


