#getwd() # mi a munkakonyvtar
setwd("C:/Users/Biostat/Desktop/Infogyak") # munkakonyvtar beallitasa
#dir()

cs_adat <- read.table(file = "csilpcsalp_rbe.txt",
                      header = TRUE,
                      sep = "\t",
                      dec = ".",
                      na.strings = c("null","NA"))
class(cs_adat)
str(cs_adat)
head(cs_adat)

cs_adat$KOR
cs_adat[1,]
cs_adat[1,1]
cs_adat[,1]
cs_adat[,1:5]
cs_adat[,c(1,3,5,7)]

cs_adat[,c("KOR","VF")]

cs_adat[1:10,c("KOR","VF")]

#Azokat a madarakat akarom lev?logatni akikn?l az izom ?rt?ke 1
cs_adat$IZOM==1
cs_adat$IZOM>1
cs_adat$IZOM<=1

cs_adat$IZOM!=1 #nem egyenlo 1 el

cs_adat[cs_adat$IZOM==1,]

# k?l?n t?bl?zat ahol a VF == 0 ?s ahol VF == 1
adat_VF_0 <- cs_adat[cs_adat$VF==0,]
adat_VF_1 <- cs_adat[cs_adat$VF==1,]

# k?l?n t?bl?zat ahol a ZSIR > 0
adat_ZSIR_0 <- cs_adat[cs_adat$ZSIR>0,]

# k?l?n t?bl?zat ahol a ZSIR > 0 ?s csak az elso 5 oszlop van benne.
adat_ZSIR_05 <- cs_adat[cs_adat$ZSIR>0,1:5]

# Azok a sorok ahol zsir > 0 ?S az izom < 2
adat_ZSIR_IZOM <- cs_adat[cs_adat$ZSIR>0 & cs_adat$IZOM<2, ]

# Azok a sorok ahol az elso megfog?skor nagyobb a t?meg 7.9n?l
adat_VF_TOMEG <- cs_adat[cs_adat$VF==0 & cs_adat$TOMEG>7.9,]

# Azok a sorok ahol a (ZSIR 0 vagy 1) VAGY a t?meg nagyobb  7.9n?l
adat_VF_ZSIR_TOMEG <- cs_adat[cs_adat$ZSIR<2 | cs_adat$TOMEG>7.9,]

###########################
###Adat ?br?zol?s 1.  
###########################

str(cs_adat)

summary(cs_adat)

cs_adat[cs_adat$TOMEG==0,]

cs_adat$TOMEG==0

ifelse(cs_adat$TOMEG==0,NA,cs_adat$TOMEG)

cs_adat$tomeg <- ifelse(cs_adat$TOMEG==0,NA,cs_adat$TOMEG)

summary(cs_adat$tomeg)

cs_adat$szarny <- ifelse(cs_adat$SZARNY==0,NA,cs_adat$SZARNY)
cs_adat$harmadik <- ifelse(cs_adat$HARMADIK==0,NA,cs_adat$HARMADIK)
cs_adat$farok <- ifelse(cs_adat$FAROK==0,NA,cs_adat$FAROK)

summary(cs_adat)

hist(cs_adat$tomeg,breaks = 8)

hist(cs_adat$tomeg,
     breaks = 8,
     main="T?meg hisztogram",
     ylab="Gyakoris?g",
     xlab="T?meg (g)")

hist(cs_adat$szarny,
     breaks = 8,
     main="Sz?rnyhossz hisztogram",
     ylab="Gyakoris?g",
     xlab="Sz?rnyhossz (mm)")

boxplot(cs_adat$tomeg)
boxplot(cs_adat$szarny)

boxplot(cs_adat$tomeg~cs_adat$ZSIR,
        main = "T?meg boxplotok a zs?rkateg?ri?k szerint",
        ylab = "T?meg (g)",
        xlab = "Zs?rkateg?ri?k")

boxplot(cs_adat$szarny~cs_adat$ZSIR,
        main = "Sz?rnyhossz boxplotok a zs?rkateg?ri?k szerint",
        ylab = "Sz?rnyhossz (mm)",
        xlab = "Zs?rkateg?ri?k")

boxplot(cs_adat$tomeg~cs_adat$IZOM,
        main = "T?meg boxplotok a izomkateg?ri?k szerint",
        ylab = "T?meg (g)",
        xlab = "Izomkateg?ri?k")


par(mfrow=c(1,2))

boxplot(cs_adat$tomeg~cs_adat$IZOM,
        main = "T?meg boxplotok a \n izomkateg?ri?k szerint",
        ylab = "T?meg (g)",
        xlab = "Izomkateg?ri?k")

boxplot(cs_adat$tomeg~cs_adat$ZSIR,
        main = "T?meg boxplotok a \n zs?rkateg?ri?k szerint",
        ylab = "T?meg (g)",
        xlab = "Zs?rkateg?ri?k")

par(mfrow=c(1,1))

########
#Okt. 29 gyak
########
library(tidyverse)
library(lubridate)

cs_adat$DATUM

cs_adat$datum2 <- mdy(cs_adat$DATUM)
cs_adat$evnap <- yday(cs_adat$datum2)

plot(cs_adat$tomeg~cs_adat$evnap)

plot(cs_adat$tomeg~cs_adat$evnap,
     pch=16,
     col="tomato",
     ylab = "T?meg (g)",
     xlab = "?vnap")

hist(cs_adat$evnap)

plot(cs_adat$tomeg~cs_adat$evnap,
     pch=16,
     col="tomato",
     ylab = "T?meg (g)",
     xlab = "?vnap",
     xlim = c(60,130))

plot(cs_adat$tomeg~cs_adat$evnap,
     pch=16,
     col=as.numeric(cs_adat$IVAR),
     ylab = "T?meg (g)",
     xlab = "?vnap",
     xlim = c(60,130))

table(cs_adat$IVAR)

cs_adat$ivar2 <- as.factor(ifelse(
                 cs_adat$IVAR=="N","NA",cs_adat$IVAR))

plot(cs_adat$tomeg~cs_adat$evnap,
     pch=16,
     col=as.numeric(cs_adat$ivar2),
     ylab = "T?meg (g)",
     xlab = "?vnap",
     xlim = c(60,130))

plot(cs_adat$tomeg~jitter(cs_adat$evnap,2),
     pch=16,
     col=as.numeric(cs_adat$ivar2),
     ylab = "T?meg (g)",
     xlab = "?vnap",
     xlim = c(60,130))

sd(cs_adat$tomeg,na.rm = T)
plot(density(na.omit(cs_adat$tomeg)))
par(new=T)
hist(cs_adat$tomeg)
mean(cs_adat$tomeg,na.rm = T)

