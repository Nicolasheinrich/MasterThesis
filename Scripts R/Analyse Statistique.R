# Packages 

require(readxl)
require(tidyverse)
require(lubridate)
require(date)
require(lme4)
require(lmerTest)
require(emmeans)
require(plot.matrix)
require(Rcpp)
require(visreg)
require(pander)
require(dplyr)
require(patchwork)
require(ggpubr)

# Importations des données épinards

Bache1E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Bâche/Bache1.csv")
Bache2E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Bâche/Bache2.csv")
Bache3E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Bâche/Bache3.csv")
Bache4E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Bâche/Bache4.csv")
Bache5E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Bâche/Bache5.csv")
Bache6E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Bâche/Bache6.csv")
Bache7E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Bâche/Bache7.csv")
Bache8E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Bâche/Bache8.csv")
Bache9E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Bâche/Bache9.csv")
Bache10E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Bâche/Bache10.csv")
Bache11E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Bâche/Bache11.csv")
Bache12E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Bâche/Bache12.csv")
Bache13E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Bâche/Bache13.csv")
Bache14E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Bâche/Bache14.csv")
Bache15E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Bâche/Bache15.csv")

Feuilles1E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Feuilles/Feuilles1.csv")
Feuilles2E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Feuilles/Feuilles2.csv")
Feuilles3E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Feuilles/Feuilles3.csv")
Feuilles4E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Feuilles/Feuilles4.csv")
Feuilles5E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Feuilles/Feuilles5.csv")
Feuilles6E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Feuilles/Feuilles6.csv")
Feuilles7E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Feuilles/Feuilles7.csv")
Feuilles8E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Feuilles/Feuilles8.csv")
Feuilles9E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Feuilles/Feuilles9.csv")
Feuilles10E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Feuilles/Feuilles10.csv")
Feuilles11E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Feuilles/Feuilles11.csv")
Feuilles12E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Feuilles/Feuilles12.csv")
Feuilles13E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Feuilles/Feuilles13.csv")
Feuilles14E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Feuilles/Feuilles14.csv")
Feuilles15E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Feuilles/Feuilles15.csv")

Labour1E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Labour/Labour1.csv")
Labour2E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Labour/Labour2.csv")
Labour3E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Labour/Labour3.csv")
Labour4E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Labour/Labour4.csv")
Labour5E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Labour/Labour5.csv")
Labour6E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Labour/Labour6.csv")
Labour7E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Labour/Labour7.csv")
Labour8E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Labour/Labour8.csv")
Labour9E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Labour/Labour9.csv")
Labour10E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Labour/Labour10.csv")
Labour11E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Labour/Labour11.csv")
Labour12E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Labour/Labour12.csv")
Labour13E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Labour/Labour13.csv")
Labour14E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Labour/Labour14.csv")
Labour15E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Labour/Labour15.csv")

Paille1E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Paille/Paille1.csv")
Paille2E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Paille/Paille2.csv")
Paille3E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Paille/Paille3.csv")
Paille4E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Paille/Paille4.csv")
Paille5E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Paille/Paille5.csv")
Paille6E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Paille/Paille6.csv")
Paille7E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Paille/Paille7.csv")
Paille8E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Paille/Paille8.csv")
Paille9E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Paille/Paille9.csv")
Paille10E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Paille/Paille10.csv")
Paille11E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Paille/Paille11.csv")
Paille12E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Paille/Paille12.csv")
Paille13E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Paille/Paille13.csv")
Paille14E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Paille/Paille14.csv")
Paille15E <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Epinards/Paille/Paille15.csv")

#Bache <- list(Bache1E, Bache2E, Bache3E, Bache4E, Bache5E, Bache6E, Bache7E, Bache8E, Bache9E, Bache10E, Bache11E, Bache12E, Bache13E, Bache14E, Bache15E)

# Diamètres épinards 

min_D <- function(BacheE){
  
  QT <- quantile(BacheE$diameter, prob = seq(0.1, 1, by=0.1))
  BacheE_min <- as.numeric(QT[1]*10)
  return(BacheE_min)
}
max_D <- function(BacheE){
  
  QT <- quantile(BacheE$diameter, prob = seq(0.1, 1, by=0.1))
  BacheE_max <- as.numeric(QT[8]*10)
  return(BacheE_max)
}

Bache1E_min <- min_D(Bache1E)
Bache2E_min <- min_D(Bache2E)
Bache3E_min <- min_D(Bache3E)
Bache4E_min <- min_D(Bache4E)
Bache5E_min <- min_D(Bache5E)
Bache6E_min <- min_D(Bache6E)
Bache7E_min <- min_D(Bache7E)
Bache8E_min <- min_D(Bache8E)
Bache9E_min <- min_D(Bache9E)
Bache10E_min <- min_D(Bache10E)
Bache11E_min <- min_D(Bache11E)
Bache12E_min <- min_D(Bache12E)
Bache13E_min <- min_D(Bache13E)
Bache14E_min <- min_D(Bache14E)
Bache15E_min <- min_D(Bache15E)

Feuilles1E_min <- min_D(Feuilles1E)
Feuilles2E_min <- min_D(Feuilles2E)
Feuilles3E_min <- min_D(Feuilles3E)
Feuilles4E_min <- min_D(Feuilles4E)
Feuilles5E_min <- min_D(Feuilles5E)
Feuilles6E_min <- min_D(Feuilles6E)
Feuilles7E_min <- min_D(Feuilles7E)
Feuilles8E_min <- min_D(Feuilles8E)
Feuilles9E_min <- min_D(Feuilles9E)
Feuilles10E_min <- min_D(Feuilles10E)
Feuilles11E_min <- min_D(Feuilles11E)
Feuilles12E_min <- min_D(Feuilles12E)
Feuilles13E_min <- min_D(Feuilles13E)
Feuilles14E_min <- min_D(Feuilles14E)
Feuilles15E_min <- min_D(Feuilles15E)

Labour1E_min <- min_D(Labour1E)
Labour2E_min <- min_D(Labour2E)
Labour3E_min <- min_D(Labour3E)
Labour4E_min <- min_D(Labour4E)
Labour5E_min <- min_D(Labour5E)
Labour6E_min <- min_D(Labour6E)
Labour7E_min <- min_D(Labour7E)
Labour8E_min <- min_D(Labour8E)
Labour9E_min <- min_D(Labour9E)
Labour10E_min <- min_D(Labour10E)
Labour11E_min <- min_D(Labour11E)
Labour12E_min <- min_D(Labour12E)
Labour13E_min <- min_D(Labour13E)
Labour14E_min <- min_D(Labour14E)
Labour15E_min <- min_D(Labour15E)

Paille1E_min <- min_D(Paille1E)
Paille2E_min <- min_D(Paille2E)
Paille3E_min <- min_D(Paille3E)
Paille4E_min <- min_D(Paille4E)
Paille5E_min <- min_D(Paille5E)
Paille6E_min <- min_D(Paille6E)
Paille7E_min <- min_D(Paille7E)
Paille8E_min <- min_D(Paille8E)
Paille9E_min <- min_D(Paille9E)
Paille10E_min <- min_D(Paille10E)
Paille11E_min <- min_D(Paille11E)
Paille12E_min <- min_D(Paille12E)
Paille13E_min <- min_D(Paille13E)
Paille14E_min <- min_D(Paille14E)
Paille15E_min <- min_D(Paille15E)

Bache1E_max <- max_D(Bache1E)
Bache2E_max <- max_D(Bache2E)
Bache3E_max <- max_D(Bache3E)
Bache4E_max <- max_D(Bache4E)
Bache5E_max <- max_D(Bache5E)
Bache6E_max <- max_D(Bache6E)
Bache7E_max <- max_D(Bache7E)
Bache8E_max <- max_D(Bache8E)
Bache9E_max <- max_D(Bache9E)
Bache10E_max <- max_D(Bache10E)
Bache11E_max <- max_D(Bache11E)
Bache12E_max <- max_D(Bache12E)
Bache13E_max <- max_D(Bache13E)
Bache14E_max <- max_D(Bache14E)
Bache15E_max <- max_D(Bache15E)

Feuilles1E_max <- max_D(Feuilles1E)
Feuilles2E_max <- max_D(Feuilles2E)
Feuilles3E_max <- max_D(Feuilles3E)
Feuilles4E_max <- max_D(Feuilles4E)
Feuilles5E_max <- max_D(Feuilles5E)
Feuilles6E_max <- max_D(Feuilles6E)
Feuilles7E_max <- max_D(Feuilles7E)
Feuilles8E_max <- max_D(Feuilles8E)
Feuilles9E_max <- max_D(Feuilles9E)
Feuilles10E_max <- max_D(Feuilles10E)
Feuilles11E_max <- max_D(Feuilles11E)
Feuilles12E_max <- max_D(Feuilles12E)
Feuilles13E_max <- max_D(Feuilles13E)
Feuilles14E_max <- max_D(Feuilles14E)
Feuilles15E_max <- max_D(Feuilles15E)

Labour1E_max <- max_D(Labour1E)
Labour2E_max <- max_D(Labour2E)
Labour3E_max <- max_D(Labour3E)
Labour4E_max <- max_D(Labour4E)
Labour5E_max <- max_D(Labour5E)
Labour6E_max <- max_D(Labour6E)
Labour7E_max <- max_D(Labour7E)
Labour8E_max <- max_D(Labour8E)
Labour9E_max <- max_D(Labour9E)
Labour10E_max <- max_D(Labour10E)
Labour11E_max <- max_D(Labour11E)
Labour12E_max <- max_D(Labour12E)
Labour13E_max <- max_D(Labour13E)
Labour14E_max <- max_D(Labour14E)
Labour15E_max <- max_D(Labour15E)

Paille1E_max <- max_D(Paille1E)
Paille2E_max <- max_D(Paille2E)
Paille3E_max <- max_D(Paille3E)
Paille4E_max <- max_D(Paille4E)
Paille5E_max <- max_D(Paille5E)
Paille6E_max <- max_D(Paille6E)
Paille7E_max <- max_D(Paille7E)
Paille8E_max <- max_D(Paille8E)
Paille9E_max <- max_D(Paille9E)
Paille10E_max <- max_D(Paille10E)
Paille11E_max <- max_D(Paille11E)
Paille12E_max <- max_D(Paille12E)
Paille13E_max <- max_D(Paille13E)
Paille14E_max <- max_D(Paille14E)
Paille15E_max <- max_D(Paille15E)

## Root tissue density et IPD 

IPD_E <- function(BacheE){
  IPD_BacheE <- BacheE %>%
    filter(child_density != 0)
  
  IBD_BacheE <- (1/mean(IPD_BacheE$child_density))*10
  return(IBD_BacheE)
}

IBD_Bache1E <- IPD_E(Bache1E)
IBD_Bache2E <- IPD_E(Bache2E)
IBD_Bache3E <- IPD_E(Bache3E)
IBD_Bache4E <- IPD_E(Bache4E)
IBD_Bache5E <- IPD_E(Bache5E)
IBD_Bache6E <- IPD_E(Bache6E)
IBD_Bache7E <- IPD_E(Bache7E)
IBD_Bache8E <- IPD_E(Bache8E)
IBD_Bache9E <- IPD_E(Bache9E)
IBD_Bache10E <- IPD_E(Bache10E)
IBD_Bache11E <- IPD_E(Bache11E)
IBD_Bache12E <- IPD_E(Bache12E)
IBD_Bache13E <- IPD_E(Bache13E)
IBD_Bache14E <- IPD_E(Bache14E)
IBD_Bache15E <- IPD_E(Bache15E)

IBD_Feuilles1E <- IPD_E(Feuilles1E)
IBD_Feuilles2E <- IPD_E(Feuilles2E)
IBD_Feuilles3E <- IPD_E(Feuilles3E)
IBD_Feuilles4E <- IPD_E(Feuilles4E)
IBD_Feuilles5E <- IPD_E(Feuilles5E)
IBD_Feuilles6E <- IPD_E(Feuilles6E)
IBD_Feuilles7E <- IPD_E(Feuilles7E)
IBD_Feuilles8E <- IPD_E(Feuilles8E)
IBD_Feuilles9E <- IPD_E(Feuilles9E)
IBD_Feuilles10E <- IPD_E(Feuilles10E)
IBD_Feuilles11E <- IPD_E(Feuilles11E)
IBD_Feuilles12E <- IPD_E(Feuilles12E)
IBD_Feuilles13E <- IPD_E(Feuilles13E)
IBD_Feuilles14E <- IPD_E(Feuilles14E)
IBD_Feuilles15E <- IPD_E(Feuilles15E)

IBD_Labour1E <- IPD_E(Labour1E)
IBD_Labour2E <- IPD_E(Labour2E)
IBD_Labour3E <- IPD_E(Labour3E)
IBD_Labour4E <- IPD_E(Labour4E)
IBD_Labour5E <- IPD_E(Labour5E)
IBD_Labour6E <- IPD_E(Labour6E)
IBD_Labour7E <- IPD_E(Labour7E)
IBD_Labour8E <- IPD_E(Labour8E)
IBD_Labour9E <- IPD_E(Labour9E)
IBD_Labour10E <- IPD_E(Labour10E)
IBD_Labour11E <- IPD_E(Labour11E)
IBD_Labour12E <- IPD_E(Labour12E)
IBD_Labour13E <- IPD_E(Labour13E)
IBD_Labour14E <- IPD_E(Labour14E)
IBD_Labour15E <- IPD_E(Labour15E)

IBD_Paille1E <- IPD_E(Paille1E)
IBD_Paille2E <- IPD_E(Paille2E)
IBD_Paille3E <- IPD_E(Paille3E)
IBD_Paille4E <- IPD_E(Paille4E)
IBD_Paille5E <- IPD_E(Paille5E)
IBD_Paille6E <- IPD_E(Paille6E)
IBD_Paille7E <- IPD_E(Paille7E)
IBD_Paille8E <- IPD_E(Paille8E)
IBD_Paille9E <- IPD_E(Paille9E)
IBD_Paille10E <- IPD_E(Paille10E)
IBD_Paille11E <- IPD_E(Paille11E)
IBD_Paille12E <- IPD_E(Paille12E)
IBD_Paille13E <- IPD_E(Paille13E)
IBD_Paille14E <- IPD_E(Paille14E)
IBD_Paille15E <- IPD_E(Paille15E)

masse_seche <- read_excel("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Suivi Lauzelle.xlsx",sheet = 1)

TMD_BE1 <- mean(masse_seche$`MS [g]`[16]) / mean(Bache1E$volume)
TMD_BE2 <- mean(masse_seche$`MS [g]`[17]) / mean(Bache2E$volume)
TMD_BE3 <- mean(masse_seche$`MS [g]`[18]) / mean(Bache3E$volume)
TMD_BE4 <- mean(masse_seche$`MS [g]`[19]) / mean(Bache4E$volume)
TMD_BE5 <- mean(masse_seche$`MS [g]`[20]) / mean(Bache5E$volume)
TMD_BE6 <- mean(masse_seche$`MS [g]`[36]) / mean(Bache6E$volume)
TMD_BE7 <- mean(masse_seche$`MS [g]`[37]) / mean(Bache7E$volume)
TMD_BE8 <- mean(masse_seche$`MS [g]`[38]) / mean(Bache8E$volume)
TMD_BE9 <- mean(masse_seche$`MS [g]`[39]) / mean(Bache9E$volume)
TMD_BE10 <- mean(masse_seche$`MS [g]`[40]) / mean(Bache10E$volume)
TMD_BE11 <- mean(masse_seche$`MS [g]`[56]) / mean(Bache11E$volume)
TMD_BE12 <- mean(masse_seche$`MS [g]`[57]) / mean(Bache12E$volume)
TMD_BE13 <- mean(masse_seche$`MS [g]`[58]) / mean(Bache13E$volume)
TMD_BE14 <- mean(masse_seche$`MS [g]`[59]) / mean(Bache14E$volume)
TMD_BE15 <- mean(masse_seche$`MS [g]`[60]) / mean(Bache15E$volume)

TMD_FE1 <- mean(masse_seche$`MS [g]`[6]) / mean(Feuilles1E$volume)
TMD_FE2 <- mean(masse_seche$`MS [g]`[7]) / mean(Feuilles2E$volume)
TMD_FE3 <- mean(masse_seche$`MS [g]`[8]) / mean(Feuilles3E$volume)
TMD_FE4 <- mean(masse_seche$`MS [g]`[9]) / mean(Feuilles4E$volume)
TMD_FE5 <- mean(masse_seche$`MS [g]`[10]) / mean(Feuilles5E$volume)
TMD_FE6 <- mean(masse_seche$`MS [g]`[26]) / mean(Feuilles6E$volume)
TMD_FE7 <- mean(masse_seche$`MS [g]`[27]) / mean(Feuilles7E$volume)
TMD_FE8 <- mean(masse_seche$`MS [g]`[28]) / mean(Feuilles8E$volume)
TMD_FE9 <- mean(masse_seche$`MS [g]`[29]) / mean(Feuilles9E$volume)
TMD_FE10 <- mean(masse_seche$`MS [g]`[30]) / mean(Feuilles10E$volume)
TMD_FE11 <- mean(masse_seche$`MS [g]`[46]) / mean(Feuilles11E$volume)
TMD_FE12 <- mean(masse_seche$`MS [g]`[47]) / mean(Feuilles12E$volume)
TMD_FE13 <- mean(masse_seche$`MS [g]`[48]) / mean(Feuilles13E$volume)
TMD_FE14 <- mean(masse_seche$`MS [g]`[49]) / mean(Feuilles14E$volume)
TMD_FE15 <- mean(masse_seche$`MS [g]`[50]) / mean(Feuilles15E$volume)

TMD_LE1 <- mean(masse_seche$`MS [g]`[11]) / mean(Labour1E$volume)
TMD_LE2 <- mean(masse_seche$`MS [g]`[12]) / mean(Labour2E$volume)
TMD_LE3 <- mean(masse_seche$`MS [g]`[13]) / mean(Labour3E$volume)
TMD_LE4 <- mean(masse_seche$`MS [g]`[14]) / mean(Labour4E$volume)
TMD_LE5 <- mean(masse_seche$`MS [g]`[15]) / mean(Labour5E$volume)
TMD_LE6 <- mean(masse_seche$`MS [g]`[31]) / mean(Labour6E$volume)
TMD_LE7 <- mean(masse_seche$`MS [g]`[32]) / mean(Labour7E$volume)
TMD_LE8 <- mean(masse_seche$`MS [g]`[33]) / mean(Labour8E$volume)
TMD_LE9 <- mean(masse_seche$`MS [g]`[34]) / mean(Labour9E$volume)
TMD_LE10 <- mean(masse_seche$`MS [g]`[35]) / mean(Labour10E$volume)
TMD_LE11 <- mean(masse_seche$`MS [g]`[51]) / mean(Labour11E$volume)
TMD_LE12 <- mean(masse_seche$`MS [g]`[52]) / mean(Labour12E$volume)
TMD_LE13 <- mean(masse_seche$`MS [g]`[53]) / mean(Labour13E$volume)
TMD_LE14 <- mean(masse_seche$`MS [g]`[54]) / mean(Labour14E$volume)
TMD_LE15 <- mean(masse_seche$`MS [g]`[55]) / mean(Labour15E$volume)

TMD_PE1 <- mean(masse_seche$`MS [g]`[1]) / mean(Paille1E$volume)
TMD_PE2 <- mean(masse_seche$`MS [g]`[2]) / mean(Paille2E$volume)
TMD_PE3 <- mean(masse_seche$`MS [g]`[3]) / mean(Paille3E$volume)
TMD_PE4 <- mean(masse_seche$`MS [g]`[4]) / mean(Paille4E$volume)
TMD_PE5 <- mean(masse_seche$`MS [g]`[5]) / mean(Paille5E$volume)
TMD_PE6 <- mean(masse_seche$`MS [g]`[21]) / mean(Paille6E$volume)
TMD_PE7 <- mean(masse_seche$`MS [g]`[22]) / mean(Paille7E$volume)
TMD_PE8 <- mean(masse_seche$`MS [g]`[23]) / mean(Paille8E$volume)
TMD_PE9 <- mean(masse_seche$`MS [g]`[24]) / mean(Paille9E$volume)
TMD_PE10 <- mean(masse_seche$`MS [g]`[25]) / mean(Paille10E$volume)
TMD_PE11 <- mean(masse_seche$`MS [g]`[41]) / mean(Paille11E$volume)
TMD_PE12 <- mean(masse_seche$`MS [g]`[42]) / mean(Paille12E$volume)
TMD_PE13 <- mean(masse_seche$`MS [g]`[43]) / mean(Paille13E$volume)
TMD_PE14 <- mean(masse_seche$`MS [g]`[44]) / mean(Paille14E$volume)
TMD_PE15 <- mean(masse_seche$`MS [g]`[45]) / mean(Paille15E$volume)

# Dataframe all plants épinard 

params_rhizos_epinards <- read.csv("/Users/nicoh/Desktop/Mémoire/Paramètres/parametres_rhizos_epinards.csv")
erSem <- 0.12752

Prm_E <- c("erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC")
Prm_E <- replicate(n = 60, Prm_E)
Sp_E <- replicate(n = 1380, "Epinard")
Trt_E <- rep(c("Bache", "Feuilles", "Labour", "Paille"), each = 345)
Plt_E <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), each = 23)
Ti_E <- rep(c("T1", "T2", "T3"), each = 115)
Val_E <- c(erSem, 1, 1, 0, 0, 0, 0, 0, Bache1E_min, Bache1E_max, 19.6, 1, 0.03, 6.015, IBD_Bache1E, 0.8, 0, 0.151, 0.15, TMD_BE1, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Bache2E_min, Bache2E_max, 19.6, 1, 0.03, 6.015, IBD_Bache2E, 0.8, 0, 0.151, 0.15, TMD_BE2, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Bache3E_min, Bache3E_max, 19.6, 1, 0.03, 6.015, IBD_Bache3E, 0.8, 0, 0.151, 0.15, TMD_BE3, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Bache4E_min, Bache4E_max, 19.6, 1, 0.03, 6.015, IBD_Bache4E, 0.8, 0, 0.151, 0.15, TMD_BE4, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Bache5E_min, Bache5E_max, 19.6, 1, 0.03, 6.015, IBD_Bache5E, 0.8, 0, 0.151, 0.15, TMD_BE5, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Bache6E_min, Bache6E_max, 19.6, 1, 0.03, 6.015, IBD_Bache6E, 0.8, 0, 0.151, 0.15, TMD_BE6, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Bache7E_min, Bache7E_max, 19.6, 1, 0.03, 6.015, IBD_Bache7E, 0.8, 0, 0.151, 0.15, TMD_BE7, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Bache8E_min, Bache8E_max, 19.6, 1, 0.03, 6.015, IBD_Bache8E, 0.8, 0, 0.151, 0.15, TMD_BE8, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Bache9E_min, Bache9E_max, 19.6, 1, 0.03, 6.015, IBD_Bache9E, 0.8, 0, 0.151, 0.15, TMD_BE9, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Bache10E_min, Bache10E_max, 19.6, 1, 0.03, 6.015, IBD_Bache10E, 0.8, 0, 0.151, 0.15, TMD_BE10, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Bache11E_min, Bache11E_max, 19.6, 1, 0.03, 6.015, IBD_Bache11E, 0.8, 0, 0.151, 0.15, TMD_BE11, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Bache12E_min, Bache12E_max, 19.6, 1, 0.03, 6.015, IBD_Bache12E, 0.8, 0, 0.151, 0.15, TMD_BE12, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Bache13E_min, Bache13E_max, 19.6, 1, 0.03, 6.015, IBD_Bache13E, 0.8, 0, 0.151, 0.15, TMD_BE13, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Bache14E_min, Bache14E_max, 19.6, 1, 0.03, 6.015, IBD_Bache14E, 0.8, 0, 0.151, 0.15, TMD_BE14, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Bache15E_min, Bache15E_max, 19.6, 1, 0.03, 6.015, IBD_Bache15E, 0.8, 0, 0.151, 0.15, TMD_BE15, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Feuilles1E_min, Feuilles1E_max, 19.6, 1, 0.03, 6.015, IBD_Feuilles1E, 0.8, 0, 0.151, 0.15, TMD_FE1, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Feuilles2E_min, Feuilles2E_max, 19.6, 1, 0.03, 6.015, IBD_Feuilles2E, 0.8, 0, 0.151, 0.15, TMD_FE2, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Feuilles3E_min, Feuilles3E_max, 19.6, 1, 0.03, 6.015, IBD_Feuilles3E, 0.8, 0, 0.151, 0.15, TMD_FE3, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Feuilles4E_min, Feuilles4E_max, 19.6, 1, 0.03, 6.015, IBD_Feuilles4E, 0.8, 0, 0.151, 0.15, TMD_FE4, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Feuilles5E_min, Feuilles5E_max, 19.6, 1, 0.03, 6.015, IBD_Feuilles5E, 0.8, 0, 0.151, 0.15, TMD_FE5, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Feuilles6E_min, Feuilles6E_max, 19.6, 1, 0.03, 6.015, IBD_Feuilles6E, 0.8, 0, 0.151, 0.15, TMD_FE6, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Feuilles7E_min, Feuilles7E_max, 19.6, 1, 0.03, 6.015, IBD_Feuilles7E, 0.8, 0, 0.151, 0.15, TMD_FE7, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Feuilles8E_min, Feuilles8E_max, 19.6, 1, 0.03, 6.015, IBD_Feuilles8E, 0.8, 0, 0.151, 0.15, TMD_FE8, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Feuilles9E_min, Feuilles9E_max, 19.6, 1, 0.03, 6.015, IBD_Feuilles9E, 0.8, 0, 0.151, 0.15, TMD_FE9, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Feuilles10E_min, Feuilles10E_max, 19.6, 1, 0.03, 6.015, IBD_Feuilles10E, 0.8, 0, 0.151, 0.15, TMD_FE10, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Feuilles11E_min, Feuilles11E_max, 19.6, 1, 0.03, 6.015, IBD_Feuilles11E, 0.8, 0, 0.151, 0.15, TMD_FE11, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Feuilles12E_min, Feuilles12E_max, 19.6, 1, 0.03, 6.015, IBD_Feuilles12E, 0.8, 0, 0.151, 0.15, TMD_FE12, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Feuilles13E_min, Feuilles13E_max, 19.6, 1, 0.03, 6.015, IBD_Feuilles13E, 0.8, 0, 0.151, 0.15, TMD_FE13, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Feuilles14E_min, Feuilles14E_max, 19.6, 1, 0.03, 6.015, IBD_Feuilles14E, 0.8, 0, 0.151, 0.15, TMD_FE14, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Feuilles15E_min, Feuilles15E_max, 19.6, 1, 0.03, 6.015, IBD_Feuilles15E, 0.8, 0, 0.151, 0.15, TMD_FE15, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Labour1E_min, Labour1E_max, 19.6, 1, 0.03, 6.015, IBD_Labour1E, 0.8, 0, 0.151, 0.15, TMD_LE1, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Labour2E_min, Labour2E_max, 19.6, 1, 0.03, 6.015, IBD_Labour2E, 0.8, 0, 0.151, 0.15, TMD_LE2, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Labour3E_min, Labour3E_max, 19.6, 1, 0.03, 6.015, IBD_Labour3E, 0.8, 0, 0.151, 0.15, TMD_LE3, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Labour4E_min, Labour4E_max, 19.6, 1, 0.03, 6.015, IBD_Labour4E, 0.8, 0, 0.151, 0.15, TMD_LE4, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Labour5E_min, Labour5E_max, 19.6, 1, 0.03, 6.015, IBD_Labour5E, 0.8, 0, 0.151, 0.15, TMD_LE5, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Labour6E_min, Labour6E_max, 19.6, 1, 0.03, 6.015, IBD_Labour6E, 0.8, 0, 0.151, 0.15, TMD_LE6, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Labour7E_min, Labour7E_max, 19.6, 1, 0.03, 6.015, IBD_Labour7E, 0.8, 0, 0.151, 0.15, TMD_LE7, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Labour8E_min, Labour8E_max, 19.6, 1, 0.03, 6.015, IBD_Labour8E, 0.8, 0, 0.151, 0.15, TMD_LE8, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Labour9E_min, Labour9E_max, 19.6, 1, 0.03, 6.015, IBD_Labour9E, 0.8, 0, 0.151, 0.15, TMD_LE9, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Labour10E_min, Labour10E_max, 19.6, 1, 0.03, 6.015, IBD_Labour10E, 0.8, 0, 0.151, 0.15, TMD_LE10, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Labour11E_min, Labour11E_max, 19.6, 1, 0.03, 6.015, IBD_Labour11E, 0.8, 0, 0.151, 0.15, TMD_LE11, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Labour12E_min, Labour12E_max, 19.6, 1, 0.03, 6.015, IBD_Labour12E, 0.8, 0, 0.151, 0.15, TMD_LE12, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Labour13E_min, Labour13E_max, 19.6, 1, 0.03, 6.015, IBD_Labour13E, 0.8, 0, 0.151, 0.15, TMD_LE13, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Labour14E_min, Labour14E_max, 19.6, 1, 0.03, 6.015, IBD_Labour14E, 0.8, 0, 0.151, 0.15, TMD_LE14, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Labour15E_min, Labour15E_max, 19.6, 1, 0.03, 6.015, IBD_Labour15E, 0.8, 0, 0.151, 0.15, TMD_LE15, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Paille1E_min, Paille1E_max, 19.6, 1, 0.03, 6.015, IBD_Paille1E, 0.8, 0, 0.151, 0.15, TMD_PE1, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Paille2E_min, Paille2E_max, 19.6, 1, 0.03, 6.015, IBD_Paille2E, 0.8, 0, 0.151, 0.15, TMD_PE2, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Paille3E_min, Paille3E_max, 19.6, 1, 0.03, 6.015, IBD_Paille3E, 0.8, 0, 0.151, 0.15, TMD_PE3, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Paille4E_min, Paille4E_max, 19.6, 1, 0.03, 6.015, IBD_Paille4E, 0.8, 0, 0.151, 0.15, TMD_PE4, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Paille5E_min, Paille5E_max, 19.6, 1, 0.03, 6.015, IBD_Paille5E, 0.8, 0, 0.151, 0.15, TMD_PE5, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Paille6E_min, Paille6E_max, 19.6, 1, 0.03, 6.015, IBD_Paille6E, 0.8, 0, 0.151, 0.15, TMD_PE6, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Paille7E_min, Paille7E_max, 19.6, 1, 0.03, 6.015, IBD_Paille7E, 0.8, 0, 0.151, 0.15, TMD_PE7, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Paille8E_min, Paille8E_max, 19.6, 1, 0.03, 6.015, IBD_Paille8E, 0.8, 0, 0.151, 0.15, TMD_PE8, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Paille9E_min, Paille9E_max, 19.6, 1, 0.03, 6.015, IBD_Paille9E, 0.8, 0, 0.151, 0.15, TMD_PE9, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Paille10E_min, Paille10E_max, 19.6, 1, 0.03, 6.015, IBD_Paille10E, 0.8, 0, 0.151, 0.15, TMD_PE10, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Paille11E_min, Paille11E_max, 19.6, 1, 0.03, 6.015, IBD_Paille11E, 0.8, 0, 0.151, 0.15, TMD_PE11, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Paille12E_min, Paille12E_max, 19.6, 1, 0.03, 6.015, IBD_Paille12E, 0.8, 0, 0.151, 0.15, TMD_PE12, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Paille13E_min, Paille13E_max, 19.6, 1, 0.03, 6.015, IBD_Paille13E, 0.8, 0, 0.151, 0.15, TMD_PE13, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Paille14E_min, Paille14E_max, 19.6, 1, 0.03, 6.015, IBD_Paille14E, 0.8, 0, 0.151, 0.15, TMD_PE14, 400, 0.9, 5000,
          erSem, 1, 1, 0, 0, 0, 0, 0, Paille15E_min, Paille15E_max, 19.6, 1, 0.03, 6.015, IBD_Paille15E, 0.8, 0, 0.151, 0.15, TMD_PE15, 400, 0.9, 5000)
          
all_plants_epinards <- data.frame(Parameter = c(Prm_E), 
                   Specie = c(Sp_E), 
                   Treatment = c(Trt_E),
                   Plante = c(Plt_E), 
                   Time = c(Ti_E), 
                   Value = c(Val_E))


write.csv(all_plants_epinards, "/Users/nicoh/Desktop/Mémoire/Paramètres/all_plants_epinards.csv")

######################################################################################################

# Importation données butternuts

Bache1B <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Butternuts/Bache/BBache1.csv")
Bache2B <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Butternuts/Bache/BBache2.csv")
Bache3B <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Butternuts/Bache/BBache3.csv")

Feuilles1B <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Butternuts/Feuilles/BFeuilles1.csv")
Feuilles2B <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Butternuts/Feuilles/BFeuilles2.csv")
Feuilles3B <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Butternuts/Feuilles/BFeuilles3.csv")

Labour1B <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Butternuts/Labour/BLabour1.csv")
Labour2B <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Butternuts/Labour/BLabour2.csv")
Labour3B <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Butternuts/Labour/BLabour3.csv")

Paille1B <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Butternuts/Paille/BPaille1.csv")
Paille2B <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Butternuts/Paille/BPaille2.csv") 
Paille3B <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Butternuts/Paille/BPaille3.csv")

# Diamètres butternuts

min_DB <- function(BacheB){
  
  QT <- quantile(BacheB$diameter, prob = seq(0.1, 1, by=0.1))
  BacheB_min <- as.numeric(QT[1]*10)
  return(BacheB_min)
}
max_DB <- function(BacheB){
  
  QT <- quantile(BacheB$diameter, prob = seq(0.1, 1, by=0.1))
  BacheB_max <- as.numeric(QT[8]*10)
  return(BacheB_max)
}

Bache1B_min <- min_DB(Bache1B)
Bache2B_min <- min_DB(Bache2B)
Bache3B_min <- min_DB(Bache3B)
Bache1B_max <- max_DB(Bache1B)
Bache2B_max <- max_DB(Bache2B)
Bache3B_max <- max_DB(Bache3B)

Feuilles1B_min <- min_DB(Feuilles1B)
Feuilles2B_min <- min_DB(Feuilles2B)
Feuilles3B_min <- min_DB(Feuilles3B)
Feuilles1B_max <- max_DB(Feuilles1B)
Feuilles2B_max <- max_DB(Feuilles2B)
Feuilles3B_max <- max_DB(Feuilles3B)

Labour1B_min <- min_DB(Labour1B)
Labour2B_min <- min_DB(Labour2B)
Labour3B_min <- min_DB(Labour3B)
Labour1B_max <- max_DB(Labour1B)
Labour2B_max <- max_DB(Labour2B)
Labour3B_max <- max_DB(Labour3B)

Paille1B_min <- min_DB(Paille1B)
Paille2B_min <- min_DB(Paille2B)
Paille3B_min <- min_DB(Paille3B)
Paille1B_max <- max_DB(Paille1B)
Paille2B_max <- max_DB(Paille2B)
Paille3B_max <- max_DB(Paille3B)

# Root tissue density butternuts et IPD

IPD_B <- function(BacheB){
  IPD_BacheB <- BacheB %>%
    filter(child_density != 0)
  
  IBD_BacheB <- (1/mean(IPD_BacheB$child_density))*10
  return(IBD_BacheB)
}

IBD_Bache1B <- IPD_B(Bache1B)
IBD_Bache2B <- IPD_B(Bache2B)
IBD_Bache3B <- IPD_B(Bache3B)

IBD_Feuilles1B <- IPD_B(Feuilles1B)
IBD_Feuilles2B <- IPD_B(Feuilles2B)
IBD_Feuilles3B <- IPD_B(Feuilles3B)

IBD_Labour1B <- IPD_B(Labour1B)
IBD_Labour2B <- IPD_B(Labour2B)
IBD_Labour3B <- IPD_B(Labour3B)

IBD_Paille1B <- IPD_B(Paille1B)
IBD_Paille2B <- IPD_B(Paille2B)
IBD_Paille3B <- IPD_B(Paille3B)

masse_seche_butternuts <- read_excel("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Suivi Lauzelle.xlsx",sheet = 3)

TMD_BB1 <- masse_seche_butternuts$`MS [g]`[1] / mean(Bache1B$volume)
TMD_BB2 <- masse_seche_butternuts$`MS [g]`[2] / mean(Bache2B$volume)
TMD_BB3 <- masse_seche_butternuts$`MS [g]`[3] / mean(Bache3B$volume)

TMD_FB1 <- masse_seche_butternuts$`MS [g]`[1] / mean(Feuilles1B$volume)
TMD_FB2 <- masse_seche_butternuts$`MS [g]`[2] / mean(Feuilles2B$volume)
TMD_FB3 <- masse_seche_butternuts$`MS [g]`[3] / mean(Feuilles3B$volume)

TMD_LB1 <- masse_seche_butternuts$`MS [g]`[1] / mean(Labour1B$volume)
TMD_LB2 <- masse_seche_butternuts$`MS [g]`[2] / mean(Labour2B$volume)
TMD_LB3 <- masse_seche_butternuts$`MS [g]`[3] / mean(Labour3B$volume)

TMD_PB1 <- masse_seche_butternuts$`MS [g]`[1] / mean(Paille1B$volume)
TMD_PB2 <- masse_seche_butternuts$`MS [g]`[2] / mean(Paille2B$volume)
TMD_PB3 <- masse_seche_butternuts$`MS [g]`[3] / mean(Paille3B$volume)

# Dataframe all plants butternuts 

params_rhizos_butternuts <- read.csv("/Users/nicoh/Desktop/Mémoire/Paramètres/parametres_rhizos_butternuts.csv")
erSem_B <- 0.1071

Prm_B <- c("erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC")
Prm_B <- replicate(n = 12, Prm_B)
Sp_B <- replicate(n = 276, "Butternut")
Trt_B <- rep(c("Bache", "Feuilles", "Labour", "Paille"), each = 69)
Plt_B <- rep(c(1, 2, 3), each = 23)
Ti_B <- rep(c("T1"), each = 276)
Val_B <- c(erSem_B, 1, 1, 0, 0, 0, 0, 0, Bache1B_min, Bache1B_max, 19.1, 1, 0.03, 0.936, IBD_Bache1B, 0.8, 0, 0.287, 0.15, TMD_BB1, 400, 0.9, 5000,
         erSem_B, 1, 1, 0, 0, 0, 0, 0, Bache2B_min, Bache2B_max, 19.1, 1, 0.03, 0.936, IBD_Bache2B, 0.8, 0, 0.287, 0.15, TMD_BB2, 400, 0.9, 5000,
         erSem_B, 1, 1, 0, 0, 0, 0, 0, Bache3B_min, Bache3B_max, 19.1, 1, 0.03, 0.936, IBD_Bache3B, 0.8, 0, 0.287, 0.15, TMD_BB3, 400, 0.9, 5000,
         erSem_B, 1, 1, 0, 0, 0, 0, 0, Feuilles1B_min, Feuilles1B_max, 19.1, 1, 0.03, 0.936, IBD_Feuilles1B, 0.8, 0, 0.287, 0.15, TMD_FB1, 400, 0.9, 5000,
         erSem_B, 1, 1, 0, 0, 0, 0, 0, Feuilles2B_min, Feuilles2B_max, 19.1, 1, 0.03, 0.936, IBD_Feuilles2B, 0.8, 0, 0.287, 0.15, TMD_FB2, 400, 0.9, 5000,
         erSem_B, 1, 1, 0, 0, 0, 0, 0, Feuilles3B_min, Feuilles3B_max, 19.1, 1, 0.03, 0.936, IBD_Feuilles3B, 0.8, 0, 0.287, 0.15, TMD_FB3, 400, 0.9, 5000,
         erSem_B, 1, 1, 0, 0, 0, 0, 0, Labour1B_min, Labour1B_max, 19.1, 1, 0.03, 0.936, IBD_Labour1B, 0.8, 0, 0.287, 0.15, TMD_LB1, 400, 0.9, 5000,
         erSem_B, 1, 1, 0, 0, 0, 0, 0, Labour2B_min, Labour2B_max, 19.1, 1, 0.03, 0.936, IBD_Labour2B, 0.8, 0, 0.287, 0.15, TMD_LB2, 400, 0.9, 5000,
         erSem_B, 1, 1, 0, 0, 0, 0, 0, Labour3B_min, Labour3B_max, 19.1, 1, 0.03, 0.936, IBD_Labour3B, 0.8, 0, 0.287, 0.15, TMD_LB3, 400, 0.9, 5000,
         erSem_B, 1, 1, 0, 0, 0, 0, 0, Paille1B_min, Paille1B_max, 19.1, 1, 0.03, 0.936, IBD_Paille1B, 0.8, 0, 0.287, 0.15, TMD_PB1, 400, 0.9, 5000,
         erSem_B, 1, 1, 0, 0, 0, 0, 0, Paille2B_min, Paille2B_max, 19.1, 1, 0.03, 0.936, IBD_Paille2B, 0.8, 0, 0.287, 0.15, TMD_PB2, 400, 0.9, 5000,
         erSem_B, 1, 1, 0, 0, 0, 0, 0, Paille3B_min, Paille3B_max, 19.1, 1, 0.03, 0.936, IBD_Paille3B, 0.8, 0, 0.287, 0.15, TMD_PB3, 400, 0.9, 5000)


all_plants_butternuts <- data.frame(Parameter = c(Prm_B), 
                                    Specie = c(Sp_B), 
                                    Treatment = c(Trt_B),
                                    Plante = c(Plt_B), 
                                    Time = c(Ti_B),
                                    Value = Val_B)

write.csv(all_plants_butternuts, "/Users/nicoh/Desktop/Mémoire/Paramètres/all_plants_butternuts.csv")

######################################################################################################

# Importartion données potimarrons

Bache1P <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Potimarrons/Bache/PBache1.csv")
Bache2P <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Potimarrons/Bache/PBache2.csv")
Bache3P <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Potimarrons/Bache/PBache3.csv")

Feuilles1P <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Potimarrons/Feuilles/PFeuilles1.csv")
Feuilles2P <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Potimarrons/Feuilles/PFeuilles2.csv")
Feuilles3P <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Potimarrons/Feuilles/PFeuilles3.csv")

Labour1P <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Potimarrons/Labour/PLabour1.csv")
Labour2P <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Potimarrons/Labour/PLabour2.csv")
Labour3P <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Potimarrons/Labour/PLabour3.csv")

Temoin1P <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Potimarrons/Temoins/PTemoin1.csv")
Temoin2P <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Potimarrons/Temoins/PTemoin2.csv") 
Temoin3P <- read.csv("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Scans racines/Potimarrons/Temoins/PTemoin3.csv")

# Diamètres potimarrons

min_DP <- function(BacheP){
  
  QT <- quantile(BacheP$diameter, prob = seq(0.1, 1, by=0.1))
  BacheP_min <- as.numeric(QT[1]*10)
  return(BacheP_min)
}
max_DP <- function(BacheP){
  
  QT <- quantile(BacheP$diameter, prob = seq(0.1, 1, by=0.1))
  BacheP_max <- as.numeric(QT[8]*10)
  return(BacheP_max)
}

Bache1P_min <- min_DP(Bache1P)
Bache2P_min <- min_DP(Bache2P)
Bache3P_min <- min_DP(Bache3P)
Bache1P_max <- max_DP(Bache1P)
Bache2P_max <- max_DP(Bache2P)
Bache3P_max <- max_DP(Bache3P)

Feuilles1P_min <- min_DP(Feuilles1P)
Feuilles2P_min <- min_DP(Feuilles2P)
Feuilles3P_min <- min_DP(Feuilles3P)
Feuilles1P_max <- max_DP(Feuilles1P)
Feuilles2P_max <- max_DP(Feuilles2P)
Feuilles3P_max <- max_DP(Feuilles3P)

Labour1P_min <- min_DP(Labour1P)
Labour2P_min <- min_DP(Labour2P)
Labour3P_min <- min_DP(Labour3P)
Labour1P_max <- max_DP(Labour1P)
Labour2P_max <- max_DP(Labour2P)
Labour3P_max <- max_DP(Labour3P)

Temoin1P_min <- min_DP(Temoin1P)
Temoin2P_min <- min_DP(Temoin2P)
Temoin3P_min <- min_DP(Temoin3P)
Temoin1P_max <- max_DP(Temoin1P)
Temoin2P_max <- max_DP(Temoin2P)
Temoin3P_max <- max_DP(Temoin3P)

# Root tissue density potimarrons et IPD

IPD_P <- function(BacheP){
  IPD_BacheP <- BacheP %>%
    filter(child_density != 0)
  
  IBD_BacheP <- (1/mean(IPD_BacheP$child_density))*10
  return(IBD_BacheP)
}

IBD_Bache1P <- IPD_P(Bache1P)
IBD_Bache2P <- IPD_P(Bache2P)
IBD_Bache3P <- IPD_P(Bache3P)

IBD_Feuilles1P <- IPD_P(Feuilles1P)
IBD_Feuilles2P <- IPD_P(Feuilles2P)
IBD_Feuilles3P <- IPD_P(Feuilles3P)

IBD_Labour1P <- IPD_P(Labour1P)
IBD_Labour2P <- IPD_P(Labour2P)
IBD_Labour3P <- IPD_P(Labour3P)

IBD_Temoin1P <- IPD_P(Temoin1P)
IBD_Temoin2P <- IPD_P(Temoin2P)
IBD_Temoin3P <- IPD_P(Temoin3P)

masse_seche_potimarrons <- read_excel("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Suivi Lauzelle.xlsx",sheet = 2)

TMD_BP1 <- masse_seche_potimarrons$`MS [g]`[1] / mean(Bache1P$volume)
TMD_BP2 <- masse_seche_potimarrons$`MS [g]`[2] / mean(Bache2P$volume)
TMD_BP3 <- masse_seche_potimarrons$`MS [g]`[3] / mean(Bache3P$volume)

TMD_FP1 <- masse_seche_potimarrons$`MS [g]`[1] / mean(Feuilles1P$volume)
TMD_FP2 <- masse_seche_potimarrons$`MS [g]`[2] / mean(Feuilles2P$volume)
TMD_FP3 <- masse_seche_potimarrons$`MS [g]`[3] / mean(Feuilles3P$volume)

TMD_LP1 <- masse_seche_potimarrons$`MS [g]`[1] / mean(Labour1P$volume)
TMD_LP2 <- masse_seche_potimarrons$`MS [g]`[2] / mean(Labour2P$volume)
TMD_LP3 <- masse_seche_potimarrons$`MS [g]`[3] / mean(Labour3P$volume)

TMD_TP1 <- masse_seche_potimarrons$`MS [g]`[1] / mean(Temoin1P$volume)
TMD_TP2 <- masse_seche_potimarrons$`MS [g]`[2] / mean(Temoin2P$volume)
TMD_TP3 <- masse_seche_potimarrons$`MS [g]`[3] / mean(Temoin3P$volume)

# Dataframe all plants potimarrons 

params_rhizos_potimarrons <- read.csv("/Users/nicoh/Desktop/Mémoire/Paramètres/parametres_rhizos_potimarrons.csv")
erSem_P <- 0.1071

Prm_P <- c("erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC")
Prm_P <- replicate(n = 12, Prm_P)
Sp_P <- replicate(n = 276, "Potimarron")
Trt_P <- rep(c("Bache", "Feuilles", "Labour", "Temoin"), each = 69)
Plt_P <- rep(c(1, 2, 3), each = 23)
Ti_P <- rep(c("T1"), each = 276)
Val_P <- c(erSem_P, 1, 1, 0, 0, 0, 0, 0, Bache1P_min, Bache1P_max, 16.641, 1, 0.03, 0.936, IBD_Bache1P, 0.8, 0, 0.233, 0.15, TMD_BP1, 400, 0.9, 5000,
         erSem_P, 1, 1, 0, 0, 0, 0, 0, Bache2P_min, Bache2P_max, 16.641, 1, 0.03, 0.936, IBD_Bache2P, 0.8, 0, 0.233, 0.15, TMD_BP2, 400, 0.9, 5000,
         erSem_P, 1, 1, 0, 0, 0, 0, 0, Bache3P_min, Bache3P_max, 16.641, 1, 0.03, 0.936, IBD_Bache3P, 0.8, 0, 0.233, 0.15, TMD_BP3, 400, 0.9, 5000,
         erSem_P, 1, 1, 0, 0, 0, 0, 0, Feuilles1P_min, Feuilles1P_max, 16.641, 1, 0.03, 0.936, IBD_Feuilles1P, 0.8, 0, 0.233, 0.15, TMD_FP1, 400, 0.9, 5000,
         erSem_P, 1, 1, 0, 0, 0, 0, 0, Feuilles2P_min, Feuilles2P_max, 16.641, 1, 0.03, 0.936, IBD_Feuilles2P, 0.8, 0, 0.233, 0.15, TMD_FP2, 400, 0.9, 5000,
         erSem_P, 1, 1, 0, 0, 0, 0, 0, Feuilles3P_min, Feuilles3P_max, 16.641, 1, 0.03, 0.936, IBD_Feuilles3P, 0.8, 0, 0.233, 0.15, TMD_FP3, 400, 0.9, 5000,
         erSem_P, 1, 1, 0, 0, 0, 0, 0, Labour1P_min, Labour1P_max, 16.641, 1, 0.03, 0.936, IBD_Labour1P, 0.8, 0, 0.233, 0.15, TMD_LP1, 400, 0.9, 5000,
         erSem_P, 1, 1, 0, 0, 0, 0, 0, Labour2P_min, Labour2P_max, 16.641, 1, 0.03, 0.936, IBD_Labour2P, 0.8, 0, 0.233, 0.15, TMD_LP2, 400, 0.9, 5000,
         erSem_P, 1, 1, 0, 0, 0, 0, 0, Labour3P_min, Labour3P_max, 16.641, 1, 0.03, 0.936, IBD_Labour3P, 0.8, 0, 0.233, 0.15, TMD_LP3, 400, 0.9, 5000,
         erSem_P, 1, 1, 0, 0, 0, 0, 0, Temoin1P_min, Temoin1P_max, 16.641, 1, 0.03, 0.936, IBD_Temoin1P, 0.8, 0, 0.233, 0.15, TMD_TP1, 400, 0.9, 5000,
         erSem_P, 1, 1, 0, 0, 0, 0, 0, Temoin2P_min, Temoin2P_max, 16.641, 1, 0.03, 0.936, IBD_Temoin2P, 0.8, 0, 0.233, 0.15, TMD_TP2, 400, 0.9, 5000,
         erSem_P, 1, 1, 0, 0, 0, 0, 0, Temoin3P_min, Temoin3P_max, 16.641, 1, 0.03, 0.936, IBD_Temoin3P, 0.8, 0, 0.233, 0.15, TMD_TP3, 400, 0.9, 5000)


all_plants_potimarrons <- data.frame(Parameter = c(Prm_P), 
                                     Specie = c(Sp_P), 
                                     Treatment = c(Trt_P),
                                     Plante = c(Plt_P), 
                                     Time = c(Ti_P),
                                     Value = Val_P)

write.csv(all_plants_potimarrons, "/Users/nicoh/Desktop/Mémoire/Paramètres/all_plants_potimarrons.csv")


all_plants <- rbind(all_plants_epinards, all_plants_butternuts, all_plants_potimarrons)
write.csv(all_plants, "/Users/nicoh/Desktop/Mémoire/Paramètres/all_plants.csv")

######################################################################################################