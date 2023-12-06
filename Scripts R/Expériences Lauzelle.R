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
require(gridExtra)

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

Bache_E_T1 <- rbind(Bache1E, Bache2E, Bache3E, Bache4E, Bache5E)
Bache_E_T2 <- rbind(Bache6E, Bache7E, Bache8E, Bache9E, Bache10E)
Bache_E_T3 <- rbind(Bache11E, Bache12E, Bache13E, Bache14E, Bache15E)

Feuilles_E_T1 <- rbind(Feuilles1E, Feuilles2E, Feuilles3E, Feuilles4E, Feuilles5E)
Feuilles_E_T2 <- rbind(Feuilles6E, Feuilles7E, Feuilles8E, Feuilles9E, Feuilles10E)
Feuilles_E_T3 <- rbind(Feuilles11E, Feuilles12E, Feuilles13E, Feuilles14E, Feuilles15E)

Labour_E_T1 <- rbind(Labour1E, Labour2E, Labour3E, Labour4E, Labour5E)
Labour_E_T2 <- rbind(Labour6E, Labour7E, Labour8E, Labour9E, Labour10E)
Labour_E_T3 <- rbind(Labour11E, Labour12E, Labour13E, Labour14E, Labour15E)

Paille_E_T1 <- rbind(Paille1E, Paille2E, Paille3E, Paille4E, Paille5E)
Paille_E_T2 <- rbind(Paille6E, Paille7E, Paille8E, Paille9E, Paille10E)
Paille_E_T3 <- rbind(Paille11E, Paille12E, Paille13E, Paille14E, Paille15E)

# Paramètres diamètres épinards 

min_D <- function(Bache_E_T){
  
  QT <- quantile(Bache_E_T$diameter, prob = seq(0.1, 1, by=0.1))
  BET_min <- as.numeric(QT[1]*10)
  return(BET_min)
}
max_D <- function(Bache_E_T){
  
  QT <- quantile(Bache_E_T$diameter, prob = seq(0.1, 1, by=0.1))
  BET_max <- as.numeric(QT[8]*10)
  return(BET_max)
}


BET1_min <- min_D(Bache_E_T1)
BET1_max <- max_D(Bache_E_T1)
BET2_min <- min_D(Bache_E_T2)
BET2_max <- max_D(Bache_E_T2)
BET3_min <- min_D(Bache_E_T3)
BET3_max <- max_D(Bache_E_T3)

FET1_min <- min_D(Feuilles_E_T1)
FET1_max <- max_D(Feuilles_E_T1)
FET2_min <- min_D(Feuilles_E_T2)
FET2_max <- max_D(Feuilles_E_T2)
FET3_min <- min_D(Feuilles_E_T3)
FET3_max <- max_D(Feuilles_E_T3)

LET1_min <- min_D(Labour_E_T1)
LET1_max <- max_D(Labour_E_T1)
LET2_min <- min_D(Labour_E_T2)
LET2_max <- max_D(Labour_E_T2)
LET3_min <- min_D(Labour_E_T3)
LET3_max <- max_D(Labour_E_T3)

PET1_min <- min_D(Paille_E_T1)
PET1_max <- max_D(Paille_E_T1)
PET2_min <- min_D(Paille_E_T2)
PET2_max <- max_D(Paille_E_T2)
PET3_min <- min_D(Paille_E_T3)
PET3_max <- max_D(Paille_E_T3)

# Pente diamètre fille vs mère épinards

ratio_diameter <- function(BacheE){

Bt <- data.frame(Diameter = BacheE$diameter, Name = BacheE$root, Parent = BacheE$parent)
Bt[Bt$Parent == " SEM", "Diameter"]

Diam_parent = c(0)
for (val in Bt$Parent){
  Diam_parent <- append(Diam_parent, Bt[Bt$Name == val, "Diameter"])
}
Bt$Diam_parent <- Diam_parent
Bt <- Bt %>%
  mutate(Diam = (Diameter*10),
         DiamP = Diam_parent*10)                                                   

name <- Bt$Name
Bt <- Bt[,-2]
B <- as.data.frame(t(Bt))
colnames(B) <- name
B <- t(B)
B <- as.data.frame(B)
B$Diam <- as.numeric(paste(B$Diam))
B$DiamP <- as.numeric(paste(B$DiamP))
return(B)
}

BE1 <- ratio_diameter(Bache1E)
BE2 <- ratio_diameter(Bache2E)
BE3 <- ratio_diameter(Bache3E)
BE4 <- ratio_diameter(Bache4E)
BE5 <- ratio_diameter(Bache5E)
BE6 <- ratio_diameter(Bache6E) 
BE7 <- ratio_diameter(Bache7E)
BE8 <- ratio_diameter(Bache8E) 
BE9 <- ratio_diameter(Bache9E)
BE10 <- ratio_diameter(Bache10E)
BE11 <- ratio_diameter(Bache11E)
BE12 <- ratio_diameter(Bache12E)
BE13 <- ratio_diameter(Bache13E)
BE14 <- ratio_diameter(Bache14E)
BE15 <- ratio_diameter(Bache15E)

FE1 <- ratio_diameter(Feuilles1E)
FE2 <- ratio_diameter(Feuilles2E)
FE3 <- ratio_diameter(Feuilles3E)
FE4 <- ratio_diameter(Feuilles4E)
FE5 <- ratio_diameter(Feuilles5E)
FE6 <- ratio_diameter(Feuilles6E) 
FE7 <- ratio_diameter(Feuilles7E) 
FE8 <- ratio_diameter(Feuilles8E)
FE9 <- ratio_diameter(Feuilles9E)
FE10 <- ratio_diameter(Feuilles10E) 
FE11 <- ratio_diameter(Feuilles11E)
FE12 <- ratio_diameter(Feuilles12E)
FE13 <- ratio_diameter(Feuilles13E)
FE14 <- ratio_diameter(Feuilles14E)
FE15 <- ratio_diameter(Feuilles15E)

LE1 <- ratio_diameter(Labour1E)
LE2 <- ratio_diameter(Labour2E)
LE3 <- ratio_diameter(Labour3E)
LE4 <- ratio_diameter(Labour4E)
LE5 <- ratio_diameter(Labour5E)
LE6 <- ratio_diameter(Labour6E)
LE7 <- ratio_diameter(Labour7E) 
LE8 <- ratio_diameter(Labour8E)
LE9 <- ratio_diameter(Labour9E) 
LE10 <- ratio_diameter(Labour10E) 
LE11 <- ratio_diameter(Labour11E) 
LE12 <- ratio_diameter(Labour12E)
LE13 <- ratio_diameter(Labour13E) 
LE14 <- ratio_diameter(Labour14E) 
LE15 <- ratio_diameter(Labour15E)

PE1 <- ratio_diameter(Paille1E)
PE2 <- ratio_diameter(Paille2E)
PE3 <- ratio_diameter(Paille3E)
PE4 <- ratio_diameter(Paille4E)
PE5 <- ratio_diameter(Paille5E)
PE6 <- ratio_diameter(Paille6E) 
PE7 <- ratio_diameter(Paille7E) 
PE8 <- ratio_diameter(Paille8E)
PE9 <- ratio_diameter(Paille9E) 
PE10 <- ratio_diameter(Paille10E)
PE11 <- ratio_diameter(Paille11E) 
PE12 <- ratio_diameter(Paille12E)
PE13 <- ratio_diameter(Paille13E) 
PE14 <- ratio_diameter(Paille14E)
PE15 <- ratio_diameter(Paille15E)

RDM_Epinard <- rbind(BE1, BE2, BE3, BE4, BE5, BE6, BE7, BE8, BE9, BE10, BE11, BE12, BE13, BE14, BE15,
                     FE1, FE2, FE3, FE4, FE5, FE6, FE7, FE8, FE9, FE10, FE11, FE12, FE13, FE14, FE15,
                     LE1, LE2, LE3, LE4, LE5, LE6, LE7, LE8, LE9, LE10, LE11, LE12, LE13, LE14, LE15,
                     PE1, PE2, PE3, PE4, PE5, PE6, PE7, PE8, PE9, PE10, PE11, PE12, PE13, PE14, PE15)

RDM_Epinard <- RDM_Epinard %>%
  filter(Diam < 0.86 & DiamP < 0.86)

mod_RDM <- lm(Diam ~ DiamP, data = RDM_Epinard)
summary(mod_RDM)
RDM_E <- coef(mod_RDM)
RDM_E <- RDM_E[2]

# Autres paramètres épinards
## IBD traitements 

IPD <- function(Bache_E_T){
  IPD_BE_T <- Bache_E_T %>%
    filter(child_density != 0)
  
  IBD_BE_T <- (1/mean(IPD_BE_T$child_density))*10
  return(IBD_BE_T)
}

IBD_BE_T1 <- IPD(Bache_E_T1)
IBD_BE_T2 <- IPD(Bache_E_T2)
IBD_BE_T3 <- IPD(Bache_E_T3)
IBD_FE_T1 <- IPD(Feuilles_E_T1)
IBD_FE_T2 <- IPD(Feuilles_E_T2)
IBD_FE_T3 <- 7.53
IBD_LE_T1 <- IPD(Labour_E_T1)
IBD_LE_T2 <- IPD(Labour_E_T2)
IBD_LE_T3 <- IPD(Labour_E_T3)
IBD_PE_T1 <- IPD(Paille_E_T1)
IBD_PE_T2 <- IPD(Paille_E_T2)
IBD_PE_T3 <- IPD(Paille_E_T3)

## Root tissue density

masse_seche <- read_excel("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Suivi Lauzelle.xlsx",sheet = 1)

TMD_BE_T1 <- mean(masse_seche$`MS [g]`[16:20]) / mean(Bache_E_T1$volume)
TMD_BE_T2 <- mean(masse_seche$`MS [g]`[36:40]) / mean(Bache_E_T2$volume)
TMD_BE_T3 <- mean(masse_seche$`MS [g]`[56:60]) / mean(Bache_E_T3$volume)

TMD_FE_T1 <- mean(masse_seche$`MS [g]`[6:10]) / mean(Feuilles_E_T1$volume)
TMD_FE_T2 <- mean(masse_seche$`MS [g]`[26:30]) / mean(Feuilles_E_T2$volume)
TMD_FE_T3 <- mean(masse_seche$`MS [g]`[46:50]) / mean(Feuilles_E_T3$volume)

TMD_LE_T1 <- mean(masse_seche$`MS [g]`[11:15]) / mean(Labour_E_T1$volume)
TMD_LE_T2 <- mean(masse_seche$`MS [g]`[31:35]) / mean(Labour_E_T2$volume)
TMD_LE_T3 <- mean(masse_seche$`MS [g]`[51:55]) / mean(Labour_E_T3$volume)

TMD_PE_T1 <- mean(masse_seche$`MS [g]`[1:5]) / mean(Paille_E_T1$volume)
TMD_PE_T2 <- mean(masse_seche$`MS [g]`[21:25]) / mean(Paille_E_T2$volume)
TMD_PE_T3 <- mean(masse_seche$`MS [g]`[41:45]) / mean(Paille_E_T3$volume)

# Dataframe paramètres épinard 

params_rhizos_epinards <- read.csv("/Users/nicoh/Desktop/Mémoire/Paramètres/parametres_rhizos_epinards.csv")
erSem <- 0.12752
EL_E <- 39.2

parameters_epinards <- data.frame(Parameter = c("erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC"),
                         Specie = c("Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard", "Epinard"),
                         Treatment = c("Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille",  "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille"),
                         Time = c("T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T2", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3", "T3"),
                         Value = c(erSem, 1, 1, 0, 0, 0, 0, 0, BET1_min, BET1_max, EL_E, 1, 0.03, 6.015, IBD_BE_T1, 0.8, 0, 0.0739, 0.15, TMD_BE_T1, 400, 0.9, 5000, erSem, 1, 1, 0, 0, 0, 0, 0, BET2_min, BET2_max, EL_E, 1, 0.03, 6.015, IBD_BE_T2, 0.8, 0, 0.0739, 0.15, TMD_BE_T2, 400, 0.9, 5000, erSem, 1, 1, 0, 0, 0, 0, 0, BET3_min, BET3_max, EL_E, 1, 0.03, 6.015, IBD_BE_T3, 0.8, 0, 0.0739, 0.15, TMD_BE_T3, 400, 0.9, 5000, erSem, 1, 1, 0, 0, 0, 0, 0, FET1_min, FET1_max, EL_E, 1, 0.03, 6.015, IBD_FE_T1, 0.8, 0, 0.0739, 0.15, TMD_FE_T1, 400, 0.9, 5000, erSem, 1, 1, 0, 0, 0, 0, 0, FET2_min, FET2_max, EL_E, 1, 0.03, 6.015, IBD_FE_T2, 0.8, 0, 0.0739, 0.15, TMD_FE_T2, 400, 0.9, 5000, erSem, 1, 1, 0, 0, 0, 0, 0, FET3_min, FET3_max, EL_E, 1, 0.03, 6.015, IBD_FE_T3, 0.8, 0, 0.0739, 0.15, TMD_FE_T3, 400, 0.9, 5000, erSem, 1, 1, 0, 0, 0, 0, 0, LET1_min, LET1_max, EL_E, 1, 0.03, 6.015, IBD_LE_T1, 0.8, 0, 0.0739, 0.15, TMD_LE_T1, 400, 0.9, 5000, erSem, 1, 1, 0, 0, 0, 0, 0, LET2_min, LET2_max, EL_E, 1, 0.03, 6.015, IBD_LE_T2, 0.8, 0, 0.0739, 0.15, TMD_LE_T2, 400, 0.9, 5000, erSem, 1, 1, 0, 0, 0, 0, 0, LET3_min, LET3_max, EL_E, 1, 0.03, 6.015, IBD_LE_T3, 0.8, 0, 0.0739, 0.15, TMD_LE_T3, 400, 0.9, 5000, erSem, 1, 1, 0, 0, 0, 0, 0, PET1_min, PET1_max, EL_E, 1, 0.03, 6.015, IBD_PE_T1, 0.8, 0, 0.0739, 0.15, TMD_PE_T1, 400, 0.9, 5000, erSem, 1, 1, 0, 0, 0, 0, 0, PET2_min, PET2_max, EL_E, 1, 0.03, 6.015, IBD_PE_T2, 0.8, 0, 0.0739, 0.15, TMD_PE_T2, 400, 0.9, 5000, erSem, 1, 1, 0, 0, 0, 0, 0, PET3_min, PET3_max, EL_E, 1, 0.03, 6.015, IBD_PE_T3, 0.8, 0, 0.0739, 0.15, TMD_PE_T3, 400, 0.9, 5000))

write.csv(parameters_epinards, "/Users/nicoh/Desktop/Mémoire/Paramètres/parameters_epinards.csv")

######################################################################################################

# Importations des données butternuts

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

Bache_B <- rbind(Bache1B, Bache2B, Bache3B)
Feuilles_B <- rbind(Feuilles1B, Feuilles2B, Feuilles3B)
Labour_B <- rbind(Labour1B, Labour2B, Labour3B)
Paille_B <- rbind(Paille1B, Paille2B, Paille3B)

# Paramètres diamètres butternuts

min_D_butternuts <- function(Bache_B){
  
  QT <- quantile(Bache_B$diameter, prob = seq(0.1, 1, by=0.1))
  BB_min <- as.numeric(QT[1]*10)
  return(BB_min)
}
max_D_butternuts <- function(Bache_B){
  
  QT <- quantile(Bache_B$diameter, prob = seq(0.1, 1, by=0.1))
  BB_max <- as.numeric(QT[8]*10)
  return(BB_max)
}

BB_min <- min_D_butternuts(Bache_B)
BB_max <- max_D_butternuts(Bache_B)

FB_min <- min_D_butternuts(Feuilles_B)
FB_max <- max_D_butternuts(Feuilles_B)

LB_min <- min_D_butternuts(Labour_B)
LB_max <- max_D_butternuts(Labour_B)

PB_min <- min_D_butternuts(Paille_B)
PB_max <- max_D_butternuts(Paille_B)

# Pente diamètre fille vs mère 

ratio_diameter_butternuts <- function(BacheB){
  
  Bt <- data.frame(Diameter = BacheB$diameter, Name = BacheB$root, Parent = BacheB$parent)
  Bt[Bt$Parent == " SEM", "Diameter"]
  
  Diam_parent = c(0)
  for (val in Bt$Parent){
    Diam_parent <- append(Diam_parent, Bt[Bt$Name == val, "Diameter"])
  }
  Bt$Diam_parent <- Diam_parent
  Bt <- Bt %>%
    mutate(Diam = (Diameter*10),
           DiamP = Diam_parent*10)                                                   
  
  name <- Bt$Name
  Bt <- Bt[,-2]
  B <- as.data.frame(t(Bt))
  colnames(B) <- name
  B <- t(B)
  B <- as.data.frame(B)
  B$Diam <- as.numeric(paste(B$Diam))
  B$DiamP <- as.numeric(paste(B$DiamP))
  return(B)
}

BB1 <- ratio_diameter_butternuts(Bache1B)
BB2 <- ratio_diameter_butternuts(Bache2B)
BB3 <- ratio_diameter_butternuts(Bache3B)

FB1 <- ratio_diameter_butternuts(Feuilles1B)
FB2 <- ratio_diameter_butternuts(Feuilles2B)
FB3 <- ratio_diameter_butternuts(Feuilles3B)

LB1 <- ratio_diameter_butternuts(Labour1B)
LB2 <- ratio_diameter_butternuts(Labour2B)
LB3 <- ratio_diameter_butternuts(Labour3B)

PB1 <- ratio_diameter_butternuts(Paille1B)
PB2 <- ratio_diameter_butternuts(Paille2B)
PB3 <- ratio_diameter_butternuts(Paille3B)

RDM_Butternut <- rbind(BB1, BB2, BB3,
                       FB1, FB2, FB3,
                       LB1, LB2, LB3,
                       PB1, PB2, PB3)

RDM_Butternut <- RDM_Butternut %>% 
  filter(Diam < 3.93 & DiamP < 3.93)

mod_RDM_B <- lm(Diam ~ DiamP, data = RDM_Butternut)
summary(mod_RDM_B)
RDM_B <- coef(mod_RDM_B)
RDM_B <- RDM_B[2]

# Autres paramètres butternuts
## IBD traitements 

IPD_B <- function(Bache_B){
  IPD_B <- Bache_B %>%
    filter(child_density != 0)
  
  IBD_BB <- (1/mean(IPD_B$child_density))*10
  return(IBD_BB)
}

IBD_BB <- IPD_B(Bache_B)
IBD_FB <- IPD_B(Feuilles_B)
IBD_LB <- IPD_B(Labour_B)
IBD_PB <- IPD_B(Paille_B)

## Root tissue density

masse_seche_butternuts <- read_excel("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Suivi Lauzelle.xlsx",sheet = 3)

TMD_BB <- mean(masse_seche_butternuts$`MS [g]`) / mean(Bache_B$volume)
TMD_FB <- mean(masse_seche_butternuts$`MS [g]`) / mean(Feuilles_B$volume)
TMD_LB <- mean(masse_seche_butternuts$`MS [g]`) / mean(Labour_B$volume)
TMD_PB <- mean(masse_seche_butternuts$`MS [g]`) / mean(Paille_B$volume)

# Dataframe paramètres butternuts

params_rhizos_butternuts <- read.csv("/Users/nicoh/Desktop/Mémoire/Paramètres/parametres_rhizos_butternuts.csv")
erSem_B <- 0.1071
EL_B <- 47.2

parameters_butternuts <- data.frame(Parameter = c("erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC"),
                                  Specie = c("Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut", "Butternut"),
                                  Treatment = c("Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille", "Paille"),
                                  Time = c("T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1"),
                                  Value = c(erSem_B, 1, 1, 0, 0, 0, 0, 0, BB_min, BB_max, EL_B, 1, 0.03, 0.936, IBD_BB, 0.8, 0, 0.166, 0.15, TMD_BB, 400, 0.9, 5000, erSem_B, 1, 1, 0, 0, 0, 0, 0, FB_min, FB_max, EL_B, 1, 0.03, 0.936, IBD_FB, 0.8, 0, 0.166, 0.15, TMD_FB, 400, 0.9, 5000, erSem_B, 1, 1, 0, 0, 0, 0, 0, LB_min, LB_max, EL_B, 1, 0.03, 0.936, IBD_LB, 0.8, 0, 0.166, 0.15, TMD_LB, 400, 0.9, 5000, erSem_B, 1, 1, 0, 0, 0, 0, 0, PB_min, PB_max, EL_B, 1, 0.03, 0.936, IBD_PB, 0.8, 0, 0.166, 0.15, TMD_PB, 400, 0.9, 5000))

write.csv(parameters_butternuts, "/Users/nicoh/Desktop/Mémoire/Paramètres/parameters_butternuts.csv")

######################################################################################################

# Importations des données potimarrons

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

Bache_P <- rbind(Bache1P, Bache2P, Bache3P)
Feuilles_P <- rbind(Feuilles1P, Feuilles2P, Feuilles3P)
Labour_P <- rbind(Labour1P, Labour2P, Labour3P)
Temoin_P <- rbind(Temoin1P, Temoin2P, Temoin3P)

# Paramètres diamètres potimarrons

min_D_potimarrons <- function(Bache_P){
  
  QT <- quantile(Bache_P$diameter, prob = seq(0.1, 1, by=0.1))
  BP_min <- as.numeric(QT[1]*10)
  return(BP_min)
}
max_D_potimarrons <- function(Bache_P){
  
  QT <- quantile(Bache_P$diameter, prob = seq(0.1, 1, by=0.1))
  BP_max <- as.numeric(QT[8]*10)
  return(BP_max)
}

BP_min <- min_D_potimarrons(Bache_P)
BP_max <- max_D_potimarrons(Bache_P)

FP_min <- min_D_potimarrons(Feuilles_P)
FP_max <- max_D_potimarrons(Feuilles_P)

LP_min <- min_D_potimarrons(Labour_P)
LP_max <- max_D_potimarrons(Labour_P)

TP_min <- min_D_potimarrons(Temoin_P)
TP_max <- max_D_potimarrons(Temoin_P)

# Pente diamètre fille vs mère 

ratio_diameter_potimarrons<- function(BacheP){
  
  Bt <- data.frame(Diameter = BacheP$diameter, Name = BacheP$root, Parent = BacheP$parent)
  Bt[Bt$Parent == " SEM", "Diameter"]
  
  Diam_parent = c(0)
  for (val in Bt$Parent){
    Diam_parent <- append(Diam_parent, Bt[Bt$Name == val, "Diameter"])
  }
  Bt$Diam_parent <- Diam_parent
  Bt <- Bt %>%
    mutate(Diam = (Diameter*10),
           DiamP = Diam_parent*10)                                                   
  
  name <- Bt$Name
  Bt <- Bt[,-2]
  B <- as.data.frame(t(Bt))
  colnames(B) <- name
  B <- t(B)
  B <- as.data.frame(B)
  B$Diam <- as.numeric(paste(B$Diam))
  B$DiamP <- as.numeric(paste(B$DiamP))
  return(B)
}

BP1 <- ratio_diameter_potimarrons(Bache1P)
BP2 <- ratio_diameter_potimarrons(Bache2P)
BP3 <- ratio_diameter_potimarrons(Bache3P)

FP1 <- ratio_diameter_potimarrons(Feuilles1P)
FP2 <- ratio_diameter_potimarrons(Feuilles2P)
FP3 <- ratio_diameter_potimarrons(Feuilles3P)

LP1 <- ratio_diameter_potimarrons(Labour1P)
LP2 <- ratio_diameter_potimarrons(Labour2P)
LP3 <- ratio_diameter_potimarrons(Labour3P)

TP1 <- ratio_diameter_potimarrons(Temoin1P)
TP2 <- ratio_diameter_potimarrons(Temoin2P)
TP3 <- ratio_diameter_potimarrons(Temoin3P)

RDM_Potimmaron <- rbind(BP1, BP2, BP3, 
                        FP1, FP2, FP3,
                        LP1, LP2, LP3,
                        TP1, TP2, TP3)

RDM_Potimmaron <- RDM_Potimmaron %>%
  filter(Diam < 2.76 & DiamP < 2.76)

mod_RDM_P <- lm(Diam ~ DiamP, data = RDM_Potimmaron)
summary(mod_RDM_P)
RDM_P <- coef(mod_RDM_P)
RDM_P <- RDM_P[2]

# Autres paramètres potimarrons
## IBD traitements 

IPD_P <- function(Bache_P){
  IPD_P <- Bache_P %>%
    filter(child_density != 0)
  
  IBD_BP <- (1/mean(IPD_P$child_density))*10
  return(IBD_BP)
}

IBD_BP <- IPD_P(Bache_P)
IBD_FP <- IPD_P(Feuilles_P)
IBD_LP <- IPD_P(Labour_P)
IBD_TP <- IPD_P(Temoin_P)

## Root tissue density

masse_seche_potimarrons <- read_excel("/Users/nicoh/Desktop/Mémoire/Expériences Lauzelle/Suivi Lauzelle.xlsx",sheet = 2)

TMD_BP <- mean(masse_seche_potimarrons$`MS [g]`) / mean(Bache_P$volume)
TMD_FP <- mean(masse_seche_potimarrons$`MS [g]`) / mean(Feuilles_P$volume)
TMD_LP <- mean(masse_seche_potimarrons$`MS [g]`) / mean(Labour_P$volume)
TMD_TP <- mean(masse_seche_potimarrons$`MS [g]`) / mean(Temoin_P$volume)

# Dataframe paramètres potimarrons

params_rhizos_potimarrons <- read.csv("/Users/nicoh/Desktop/Mémoire/Paramètres/parametres_rhizos_potimarrons.csv")
erSem_P <- 0.1071
EL_P <- 60.5

parameters_potimarrons <- data.frame(Parameter = c("erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC", "erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC"),
                                    Specie = c("Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron", "Potimmaron"),
                                    Treatment = c("Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Bache", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Feuilles", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Labour", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin", "Temoin"),
                                    Time = c("T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1", "T1"),
                                    Value = c(erSem_P, 1, 1, 0, 0, 0, 0, 0, BP_min, BP_max, EL_P, 1, 0.03, 0.6571, IBD_BP, 0.8, 0, 0.214, 0.15, TMD_BP, 400, 0.9, 5000, erSem_P, 1, 1, 0, 0, 0, 0, 0, FP_min, FP_max, EL_P, 1, 0.03, 0.6571, IBD_FP, 0.8, 0, 0.214, 0.15, TMD_FP, 400, 0.9, 5000, erSem_P, 1, 1, 0, 0, 0, 0, 0, LP_min, LP_max, EL_P, 1, 0.03, 0.6571, IBD_LP, 0.8, 0, 0.214, 0.15, TMD_LP, 400, 0.9, 5000, erSem_P, 1, 1, 0, 0, 0, 0, 0, TP_min, TP_max, EL_P, 1, 0.03, 0.6571, IBD_TP, 0.8, 0, 0.214, 0.15, TMD_TP, 400, 0.9, 5000))

write.csv(parameters_potimarrons, "/Users/nicoh/Desktop/Mémoire/Paramètres/parameters_potimarrons.csv")

######################################################################################################

all_parameters <- rbind(parameters_epinards, parameters_butternuts, parameters_potimarrons)
write.csv(all_parameters, "/Users/nicoh/Desktop/Mémoire/Paramètres/all_parameters.csv")


####################

Un <- ggplot(data = RDM_Epinard, aes(x=DiamP, y=Diam)) + 
  geom_point(size = 0.3) +
  geom_smooth(method = "lm") + 
  ggtitle("Relation entre le diamètre mère et le diamètre fille de l'épinard") +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        axis.title.x = element_text(size = 20), 
        axis.text = element_text(size = 16),
        axis.title.y = element_text(size = 20)) +
  xlab("Diamètre mère [mm]") + 
  ylab("Diamètre fille [mm]") + 
  geom_abline(intercept = 0,
              slope = 1)

Deux <- ggplot(data = RDM_Butternut, aes(x=DiamP, y=Diam)) + 
  geom_point(size = 0.3) +
  geom_smooth(method = "lm") +
  ggtitle("Relation entre le diamètre mère et le diamètre fille du butternut") +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        axis.title.x = element_text(size = 20), 
        axis.text = element_text(size = 16),
        axis.title.y = element_text(size = 20)) +
  xlab("Diamètre mère [mm]") + 
  ylab("Diamètre fille [mm]") +
  geom_abline(intercept = 0,
              slope = 1)


Trois <- ggplot(data = RDM_Potimmaron, aes(x=DiamP, y=Diam)) + 
  geom_point(size = 0.3) +
  geom_smooth(method = "lm") +
  ggtitle("Relation entre le diamètre mère et le diamètre fille du potimarron") +
  theme(plot.title = element_text(size = 25, hjust = 0.5), 
        axis.title.x = element_text(size = 20), 
        axis.text = element_text(size = 16),
        axis.title.y = element_text(size = 20)) +
  xlab("Diamètre mère [mm]") + 
  ylab("Diamètre fille [mm]") +
  geom_abline(intercept = 0,
              slope = 1)

Un
Deux
Trois

grid.arrange(Un, arrangeGrob(Deux, Trois), ncol = 2)






