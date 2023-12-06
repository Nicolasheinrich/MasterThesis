# 1. Packages 

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
require(quantreg)
require(caret)
require(gridExtra)

# 2. Diamètres

E1S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Scans/NH6/NH6S.csv")
E1R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Rhizos/NH6/NH6R.csv")
E2S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Scans/NH7/NH7S.csv")
E2R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Rhizos/NH7/NH7R.csv")
E3S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Scans/NH8/NH8S.csv")
E3R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Rhizos/NH8/NH8R.csv")
E4S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Scans/NH9/NH9S.csv")
E4R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Rhizos/NH9/NH9R.csv")
E5S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Scans/NH10/NH10S.csv")
E5R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Rhizos/NH10/NH10R.csv")
E6S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Scans/NH11/NH11S.csv")
E6R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Rhizos/NH11/NH11R.csv")
E7S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Scans/NH12/NH12S.csv")
E7R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Rhizos/NH12/NH12R.csv")
#E8S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Scans/NH13/NH13S.csv")
#E8R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Rhizos/NH13/NH13R.csv")
E9S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Scans/NH14/NH14S.csv")
E9R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Rhizos/NH14/NH14R.csv")
E10S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Scans/NH15/NH15S.csv")
E10R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Rhizos/NH15/NH15R.csv")
E11S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Scans/NH16/NH16S.csv")
E11R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Rhizos/NH16/NH16R.csv")
E12S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Scans/NH17/NH17S.csv")
E12R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Rhizos/NH17/NH17R.csv")
E13S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Scans/NH18/NH18S.csv")
E13R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Rhizos/NH18/NH18R.csv")
E14S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Scans/NH19/NH19S.csv")
E14R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Rhizos/NH19/NH19R.csv")
E15S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Scans/NH20/NH20S.csv")
E15R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Epinards/Rhizos/NH20/NH20R.csv")

funct <- function(ES, ER){
  EG <- ES %>%
    dplyr::group_by(root_name) %>%
    dplyr::summarize(meanG = median(growth))
  
  E <- merge(EG, ER, by = "root_name")
  E <- E %>%
    mutate(Diam = (diameter*10),
           Croiss = (meanG*10))
return(E)  
}

E1 <- funct(E1S, E1R)
E2 <- funct(E2S, E2R)
E3 <- funct(E3S, E3R)
E4 <- funct(E4S, E4R)
E5 <- funct(E5S, E5R)
E6 <- funct(E6S, E6R)
E7 <- funct(E7S, E7R)
#E8 <- funct(E8S, E8R)
E9 <- funct(E9S, E9R)
E10 <- funct(E10S, E10R)
E11 <- funct(E11S, E11R)
E12 <- funct(E12S, E12R)
E13 <- funct(E13S, E13R)
E14 <- funct(E14S, E14R)
E15 <- funct(E15S, E15R)

min_D <- function(E){
  
  QT <- quantile(E$diameter, prob = seq(0.1, 1, by=0.1))
  E_minD <- as.numeric(QT[1]*10)
  return(E_minD)
}
max_D <- function(E){
  
  QT <- quantile(E$diameter, prob = seq(0.1, 1, by=0.1))
  E_maxD <- as.numeric(QT[9]*10)
  return(E_maxD)
}

E1_minD <- min_D(E1)
E1_maxD <- max_D(E1)
E2_minD <- min_D(E2)
E2_maxD <- max_D(E2)
E3_minD <- min_D(E3)
E3_maxD <- max_D(E3)
E4_minD <- min_D(E4)
E4_maxD <- max_D(E4)
E5_minD <- min_D(E5)
E5_maxD <- max_D(E5)
E6_minD <- min_D(E6)
E6_maxD <- max_D(E6)
E7_minD <- min_D(E7)
E7_maxD <- max_D(E7)
#E8_minD <- min_D(E8)
#E8_maxD <- max_D(E8)
E9_minD <- min_D(E9)
E9_maxD <- max_D(E9)
E10_minD <- min_D(E10)
E10_maxD <- max_D(E10)
E11_minD <- min_D(E11)
E11_maxD <- max_D(E11)
E12_minD <- min_D(E12)
E12_maxD <- max_D(E12)
E13_minD <- min_D(E13)
E13_maxD <- max_D(E13)
E14_minD <- min_D(E14)
E14_maxD <- max_D(E14)
E15_minD <- min_D(E15)
E15_maxD <- max_D(E15)

totl1 <- sum(E1$length)
totl2 <- sum(E2$length)
totl3 <- sum(E3$length)
totl4 <- sum(E4$length)
totl5 <- sum(E5$length)
totl6 <- sum(E6$length)
totl7 <- sum(E7$length)
#totl8 <- sum(E8$length)
totl9 <- sum(E9$length)
totl10 <- sum(E10$length)
totl11 <- sum(E11$length)
totl12 <- sum(E12$length)
totl13 <- sum(E13$length)
totl14 <- sum(E14$length)
totl15 <- sum(E15$length)
meantotlE <- (totl1 + totl2 + totl3 + totl4 + totl5 + totl6 + totl7 + totl9 + totl10 + totl11 + totl12 + totl13 + totl14 + totl15)/14

prof1 <- max(E1S$position)
prof2 <- max(E2S$position)
prof3 <- max(E3S$position)
prof4 <- max(E4S$position)
prof5 <- max(E5S$position)
prof6 <- max(E6S$position)
prof7 <- max(E7S$position)
#prof8 <- max(E8S$position)
prof9 <- max(E9S$position)
prof10 <- max(E10S$position)
prof11 <- max(E11S$position)
prof12 <- max(E12S$position)
prof13 <- max(E13S$position)
prof14 <- max(E14S$position)
prof15 <- max(E15S$position)
meanprofE <- (prof1 + prof2 + prof3 + prof4 + prof5 + prof6 + prof7 + prof9 + prof10 + prof11 + prof12 + prof13 + prof14 + prof15)/14
  
ratio_diameter_ER <- function(E){
  
  Et <- data.frame(Diameter = E$diameter, Name = E$root, Parent = E$parent)
  Et[Et$Parent == " SEM", "Diameter"]
  
  Diam_parent = c(0)
  for (val in Et$Parent){
    Diam_parent <- append(Diam_parent, Et[Et$Name == val, "Diameter"])
  }
  Et$Diam_parent <- Diam_parent
  Et <- Et %>%
    mutate(Diam = (Diameter*10),
           DiamP = Diam_parent*10)                                                   
  
  name <- Et$Name
  Et <- Et[,-2]
  ERDM <- as.data.frame(t(Et))
  colnames(ERDM) <- name
  ERDM <- t(ERDM)
  ERDM <- as.data.frame(ERDM)
  ERDM$Diam <- as.numeric(paste(ERDM$Diam))
  ERDM$DiamP <- as.numeric(paste(ERDM$DiamP))
  return(ERDM)
}

ERDM1 <- ratio_diameter_ER(E1)
ERDM2 <- ratio_diameter_ER(E2)
ERDM3 <- ratio_diameter_ER(E3)
ERDM4 <- ratio_diameter_ER(E4)
ERDM5 <- ratio_diameter_ER(E5)
ERDM6 <- ratio_diameter_ER(E6)
ERDM7 <- ratio_diameter_ER(E7)
#ERDM8 <- ratio_diameter_ER(E8)
ERDM9 <- ratio_diameter_ER(E9)
ERDM10 <- ratio_diameter_ER(E10)
ERDM11 <- ratio_diameter_ER(E11)
ERDM12 <- ratio_diameter_ER(E12)
ERDM13 <- ratio_diameter_ER(E13)
ERDM14 <- ratio_diameter_ER(E14)
ERDM15 <- ratio_diameter_ER(E15)

RDM_ER <- rbind(ERDM1, ERDM2, ERDM3, ERDM4, ERDM5, ERDM6, ERDM7, ERDM9, ERDM10, ERDM11, ERDM12, ERDM13, ERDM14, ERDM15)

mod_RDM_ER <- lm(Diam ~ DiamP, data = RDM_ER)
summary(mod_RDM_ER)
RDM_ER <- coef(mod_RDM_ER)
RDM_ER <- RDM_ER[2]

# 3. Elongation vs diamètre 

IPD <- function(E){
  IPD_E <- E %>%
    filter(child_density != 0)
  
  IBD_E <- (1/mean(IPD_E$child_density))*10
  return(IBD_E)
}
  
IBD_E1 <- IPD(E1)
IBD_E2 <- IPD(E2)
IBD_E3 <- IPD(E3)
IBD_E4 <- IPD(E4)
IBD_E5 <- IPD(E5)
IBD_E6 <- IPD(E6)
IBD_E7 <- IPD(E7)
#IBD_E8 <- IPD(E8)
IBD_E9 <- IPD(E9)
IBD_E10 <- IPD(E10)
IBD_E11 <- IPD(E11)
IBD_E12 <- IPD(E12)
IBD_E13 <- IPD(E13)
IBD_E14 <- IPD(E14)
IBD_E15 <- IPD(E15)
  
#IBD_E <- mean(IBD_E1, IBD_E2, IBD_E3, IBD_E4, IBD_E5, IBD_E6, IBD_E7, IBD_E8, IBD_E9, IBD_E10, IBD_E11, IBD_E12, IBD_E13, IBD_E14, IBD_E15)
IBD_RE = c(IBD_E1, IBD_E2, IBD_E3, IBD_E4, IBD_E5, IBD_E6, IBD_E7, IBD_E9, IBD_E10, IBD_E11, IBD_E12, IBD_E13, IBD_E14, IBD_E15)
rhizos_epinards <- rbind(E1, E2, E3, E4, E5, E6, E7, E9, E10, E11, E12, E13, E14, E15)

#EL_reg_E <- rq(Croiss ~ Diam, data = rhizos_epinards, tau = 0.8)
#summary(EL_reg_E)

ggplot(rhizos_epinards, aes(x = Diam, y = Croiss)) +
  geom_point(size = 0.5) + 
  geom_smooth(method=lm) +
  ggtitle("A") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22)) +
  xlab("Diamètre apical [mm]") + 
  ylab("Croissance [mm/Jour]") 

#EL_E <- coef(rq(Croiss ~ Diam, data = rhizos_epinards, tau = 0.8, ))
rhizos_epinards_1 <- rhizos_epinards %>%
  dplyr::group_by(image, root_order) %>%
  dplyr::summarize(Croiss = median(Croiss), Diam = median(Diam))
EL_E <- lm(Croiss ~ 0 + Diam, data = rhizos_epinards_1, )
EL_E <- EL_E$coefficients
#EL_E <- EL_E[2]

ELvsD_E <- ggplot(rhizos_epinards_1, aes(x = Diam, y = Croiss)) +
  geom_point(size = 0.5) + 
  geom_abline(slope = EL_E, intercept = 0) +
  geom_smooth(method=lm) +
  ggtitle("A") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22)) +
  xlab("Diamètre apical [mm]") + 
  ylab("Croissance [mm/Jour]") 

# 4. PDT (primordia developpement length) 

LAUZ_E <- c(60, 165, 2, 58, 340, 22, 55, 80, 16, 3, 70, 60, 280, 224) #Distance entre le bout de la SEM et la dernière latérale
PDT_E <- median(LAUZ_E) / median(rhizos_epinards$Croiss)


# 5. Dataframes 

Prm_E <- c("erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC")
Prm_E <- replicate(n = 14, Prm_E)
Sp_E <- replicate(n = 322, "Epinard")
Plt_E <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14), each = 23)
Val_E <- c(0.12752, 1, 1, 0, 0, 0, 0, 0, E1_minD, E1_maxD, EL_E, 1, 0.03, 6.015, IBD_RE[1], 0.8, 0, RDM_ER, 0.15, 0.1, 400, 0.9, 5000,
           0.12752, 1, 1, 0, 0, 0, 0, 0, E2_minD, E2_maxD, EL_E, 1, 0.03, 6.015, IBD_RE[2], 0.8, 0, RDM_ER, 0.15, 0.1, 400, 0.9, 5000,
           0.12752, 1, 1, 0, 0, 0, 0, 0, E3_minD, E3_maxD, EL_E, 1, 0.03, 6.015, IBD_RE[3], 0.8, 0, RDM_ER, 0.15, 0.1, 400, 0.9, 5000,
           0.12752, 1, 1, 0, 0, 0, 0, 0, E4_minD, E4_maxD, EL_E, 1, 0.03, 6.015, IBD_RE[4], 0.8, 0, RDM_ER, 0.15, 0.1, 400, 0.9, 5000,
           0.12752, 1, 1, 0, 0, 0, 0, 0, E5_minD, E5_maxD, EL_E, 1, 0.03, 6.015, IBD_RE[5], 0.8, 0, RDM_ER, 0.15, 0.1, 400, 0.9, 5000,
           0.12752, 1, 1, 0, 0, 0, 0, 0, E6_minD, E6_maxD, EL_E, 1, 0.03, 6.015, IBD_RE[6], 0.8, 0, RDM_ER, 0.15, 0.1, 400, 0.9, 5000,
           0.12752, 1, 1, 0, 0, 0, 0, 0, E7_minD, E7_maxD, EL_E, 1, 0.03, 6.015, IBD_RE[7], 0.8, 0, RDM_ER, 0.15, 0.1, 400, 0.9, 5000,
           0.12752, 1, 1, 0, 0, 0, 0, 0, E9_minD, E9_maxD, EL_E, 1, 0.03, 6.015, IBD_RE[8], 0.8, 0, RDM_ER, 0.15, 0.1, 400, 0.9, 5000,
           0.12752, 1, 1, 0, 0, 0, 0, 0, E10_minD, E10_maxD, EL_E, 1, 0.03, 6.015, IBD_RE[9], 0.8, 0, RDM_ER, 0.15, 0.1, 400, 0.9, 5000,
           0.12752, 1, 1, 0, 0, 0, 0, 0, E11_minD, E11_maxD, EL_E, 1, 0.03, 6.015, IBD_RE[10], 0.8, 0, RDM_ER, 0.15, 0.1, 400, 0.9, 5000,
           0.12752, 1, 1, 0, 0, 0, 0, 0, E12_minD, E12_maxD, EL_E, 1, 0.03, 6.015, IBD_RE[11], 0.8, 0, RDM_ER, 0.15, 0.1, 400, 0.9, 5000,
           0.12752, 1, 1, 0, 0, 0, 0, 0, E13_minD, E13_maxD, EL_E, 1, 0.03, 6.015, IBD_RE[12], 0.8, 0, RDM_ER, 0.15, 0.1, 400, 0.9, 5000,
           0.12752, 1, 1, 0, 0, 0, 0, 0, E14_minD, E14_maxD, EL_E, 1, 0.03, 6.015, IBD_RE[13], 0.8, 0, RDM_ER, 0.15, 0.1, 400, 0.9, 5000,
           0.12752, 1, 1, 0, 0, 0, 0, 0, E15_minD, E15_maxD, EL_E, 1, 0.03, 6.015, IBD_RE[14], 0.8, 0, RDM_ER, 0.15, 0.1, 400, 0.9, 5000)

params_rhizos_epinards <- data.frame(Parameter = c(Prm_E), 
                                     Specie = c(Sp_E), 
                                     Plante = c(Plt_E), 
                                     Value = Val_E)

write.csv(params_rhizos_epinards, "/Users/nicoh/Desktop/Mémoire/Paramètres/parametres_rhizos_epinards.csv")

##############################################

B1S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Butternuts/Scans/B1/B1S.csv")
B1R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Butternuts/Rhizos/B1R.csv")
B2S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Butternuts/Scans/B2/B2S.csv")
B2R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Butternuts/Rhizos/B2R.csv")
B3S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Butternuts/Scans/B3/B3S.csv")
B3R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Butternuts/Rhizos/B3R.csv")
B4S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Butternuts/Scans/B4/B4S.csv")
B4R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Butternuts/Rhizos/B4R.csv")
B5S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Butternuts/Scans/B5/B5S.csv")
B5R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Butternuts/Rhizos/B5R.csv")

B1 <- funct(B1S, B1R)
B2 <- funct(B2S, B2R)
B3 <- funct(B3S, B3R)
B4 <- funct(B4S, B4R)
B5 <- funct(B5S, B5R)

totlB1 <- sum(B1$length)
totlB2 <- sum(B2$length)
totlB3 <- sum(B3$length)
totlB4 <- sum(B4$length)
totlB5 <- sum(B5$length)
meantotlB <- (totlB1 + totlB2 + totlB3 + totlB4 + totlB5)/5

profB1 <- max(B1S$position)
profB2 <- max(B2S$position)
profB3 <- max(B3S$position)
profB4 <- max(B4S$position)
profB5 <- max(B5S$position)
meanprofB <- (profB1 + profB2 + profB3 + profB4 + profB5)/5

B1_minD <- min_D(B1)
B1_maxD <- max_D(B1)
B2_minD <- min_D(B2)
B2_maxD <- max_D(B2)
B3_minD <- min_D(B3)
B3_maxD <- max_D(B3)
B4_minD <- min_D(B4)
B4_maxD <- max_D(B4)
B5_minD <- min_D(B5)
B5_maxD <- max_D(B5)

BRDM1 <- ratio_diameter_ER(B1)
BRDM2 <- ratio_diameter_ER(B2)
BRDM3 <- ratio_diameter_ER(B3)
BRDM4 <- ratio_diameter_ER(B4)
BRDM5 <- ratio_diameter_ER(B5)

RDM_BR <- rbind(BRDM1, BRDM2, BRDM3, BRDM4, BRDM5)

mod_RDM_BR <- lm(Diam ~ DiamP, data = RDM_BR)
summary(mod_RDM_BR)
RDM_B <- coef(mod_RDM_BR)
RDM_B <- RDM_B[2]

IBD_B1 <- IPD(B1)
IBD_B2 <- IPD(B2)
IBD_B3 <- IPD(B3)
IBD_B4 <- IPD(B4)
IBD_B5 <- IPD(B5)

IBD_B <- mean(IBD_B1, IBD_B2, IBD_B3, IBD_B4, IBD_B5)
IBD_RB = c(IBD_B1, IBD_B2, IBD_B3, IBD_B4, IBD_B5)
rhizos_butternuts <- rbind(B1, B2, B3, B4, B5)

ggplot(rhizos_butternuts, aes(x = Diam, y = Croiss)) +
  geom_point(size = 0.5) + 
  geom_smooth(method=lm) +
  ggtitle("B") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22)) +
  xlab("Diamètre apical [mm]") + 
  ylab("Croissance [mm/Jour]") 

#EL_B <- coef(rq(Croiss ~ Diam, data = rhizos_butternuts, tau = 0.8))
rhizos_butternuts_1 <- rhizos_butternuts %>%
  dplyr::group_by(image, root_order) %>%
  dplyr::summarize(Croiss = median(Croiss), Diam = median(Diam))
EL_B <- lm(Croiss ~ 0 + Diam, data = rhizos_butternuts_1, )
EL_B <- EL_B$coefficients
#EL_B <- EL_B[2]

ELvsD_B <- ggplot(rhizos_butternuts_1, aes(x = Diam, y = Croiss)) +
  geom_point(size = 0.5) + 
  geom_abline(slope = EL_B, intercept = 0) +
  geom_smooth(method=lm) +
  ggtitle("B") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22)) +
  xlab("Diamètre apical [mm]") + 
  ylab("Croissance [mm/Jour]") 

# 4.PDT (primordia developpement length) 

LAUZ_B <- c(2.09, 3.5, 13.22, 20.48, 11.97) #Distance entre le bout de la SEM et la dernière latérale
#PDT <- mean(LAUZ) / mean(rhizos$Croiss)
PDT_B <- median(LAUZ_B) / median(rhizos_butternuts$Croiss)

Prm_B <- c("erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC")
Prm_B <- replicate(n = 5, Prm_B)
Sp_B <- replicate(n = 115, "Butternut")
Plt_B <- rep(c(1, 2, 3, 4, 5), each = 23)
Val_B <- c(0.1071, 1, 1, 0, 0, 0, 0, 0, B1_minD, B1_maxD, EL_B, 1, 0.03, 0.936, IBD_RB[1], 0.8, 0, RDM_B, 0.15, 0.1, 400, 0.9, 5000,
           0.1071, 1, 1, 0, 0, 0, 0, 0, B2_minD, B2_maxD, EL_B, 1, 0.03, 0.936, IBD_RB[2], 0.8, 0, RDM_B, 0.15, 0.1, 400, 0.9, 5000,
           0.1071, 1, 1, 0, 0, 0, 0, 0, B3_minD, B3_maxD, EL_B, 1, 0.03, 0.936, IBD_RB[3], 0.8, 0, RDM_B, 0.15, 0.1, 400, 0.9, 5000,
           0.1071, 1, 1, 0, 0, 0, 0, 0, B4_minD, B4_maxD, EL_B, 1, 0.03, 0.936, IBD_RB[4], 0.8, 0, RDM_B, 0.15, 0.1, 400, 0.9, 5000,
           0.1071, 1, 1, 0, 0, 0, 0, 0, B5_minD, B5_maxD, EL_B, 1, 0.03, 0.936, IBD_RB[5], 0.8, 0, RDM_B, 0.15, 0.1, 400, 0.9, 5000)
           
params_rhizos_butternuts <- data.frame(Parameter = c(Prm_B), 
                                     Specie = c(Sp_B), 
                                     Plante = c(Plt_B), 
                                     Value = Val_B)

write.csv(params_rhizos_butternuts, "/Users/nicoh/Desktop/Mémoire/Paramètres/parametres_rhizos_butternuts.csv")

##############################################

P1S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Potimarons/Scans/P1/P1S.csv")
P1R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Potimarons/Rhizos/P1R.csv")
P2S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Potimarons/Scans/P2/P2S.csv")
P2R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Potimarons/Rhizos/P2R.csv")
P3S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Potimarons/Scans/P3/P3S.csv")
P3R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Potimarons/Rhizos/P3R.csv")
P4S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Potimarons/Scans/P4/P4S.csv")
P4R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Potimarons/Rhizos/P4R.csv")
P5S <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Potimarons/Scans/P5/P5S.csv")
P5R <- read.csv("/Users/nicoh/Desktop/Mémoire/Rhizotrons/Scans rhizotrons /Potimarons/Rhizos/P5R.csv")

P1 <- funct(P1S, P1R)
P2 <- funct(P2S, P2R)
P3 <- funct(P3S, P3R)
P4 <- funct(P4S, P4R)
P5 <- funct(P5S, P5R)

totlP1 <- sum(P1$length)
totlP2 <- sum(P2$length)
totlP3 <- sum(P3$length)
totlP4 <- sum(P4$length)
totlP5 <- sum(P5$length)
meantotlP <- (totlP1 + totlP2 + totlP3 + totlP4 + totlP5)/5

profP1 <- max(P1S$position)
profP2 <- max(P2S$position)
profP3 <- max(P3S$position)
profP4 <- max(P4S$position)
profP5 <- max(P5S$position)
meanprofP <- (profP1 + profP2 + profP3 + profP4 + profP5)/5

P1_minD <- min_D(P1)
P1_maxD <- max_D(P1)
P2_minD <- min_D(P2)
P2_maxD <- max_D(P2)
P3_minD <- min_D(P3)
P3_maxD <- max_D(P3)
P4_minD <- min_D(P4)
P4_maxD <- max_D(P4)
P5_minD <- min_D(P5)
P5_maxD <- max_D(P5)

PRDM1 <- ratio_diameter_ER(P1)
PRDM2 <- ratio_diameter_ER(P2)
PRDM3 <- ratio_diameter_ER(P3)
PRDM4 <- ratio_diameter_ER(P4)
#PRDM5 <- ratio_diameter_ER(P5)

RDM_PR <- rbind(PRDM1, PRDM2, PRDM3, PRDM4)

mod_RDM_PR <- lm(Diam ~ DiamP, data = RDM_PR)
summary(mod_RDM_PR)
RDM_P <- coef(mod_RDM_PR)
RDM_P <- RDM_P[2]

IBD_P1 <- IPD(P1)
IBD_P2 <- IPD(P2)
IBD_P3 <- IPD(P3)
IBD_P4 <- IPD(P4)
IBD_P5 <- IPD(P5)

IBD_P <- mean(IBD_P1, IBD_P2, IBD_P3, IBD_P4, IBD_P5)
IBD_RP = c(IBD_P1, IBD_P2, IBD_P3, IBD_P4, IBD_P5) # A changer (données des scans des transparents)

rhizos_potimarrons <- rbind(P1, P2, P3, P4, P5)

ggplot(rhizos_potimarrons, aes(x = Diam, y = Croiss)) +
  geom_point(size = 0.5) + 
  geom_smooth(method=lm) +
  ggtitle("C") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22)) +
  xlab("Diamètre apical [mm]") + 
  ylab("Croissance [mm/Jour]") 

#EL_P <- coef(rq(Croiss ~ Diam, data = rhizos_potimarrons, tau = 0.8))
rhizos_potimarrons_1 <- rhizos_potimarrons %>%
  dplyr::group_by(image, root_order) %>%
  dplyr::summarize(Croiss = median(Croiss), Diam = median(Diam))

EL_P <- lm(Croiss ~ 0 + Diam, data = rhizos_potimarrons_1, )
EL_P <- EL_P$coefficients
#EL_P <- EL_P[2]

ELvsD_P <- ggplot(rhizos_potimarrons_1, aes(x = Diam, y = Croiss)) +
  geom_point(size = 0.5) + 
  geom_abline(slope = EL_P, intercept = 0) +
  geom_smooth(method=lm) +
  ggtitle("C") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22)) +
  xlab("Diamètre apical [mm]") + 
  ylab("Croissance [mm/Jour]") 

LAUZ_P <- c(7.2, 6.26, 3.88, 4.83, 8.49) #Distance entre le bout de la SEM et la dernière latérale
#PDT <- mean(LAUZ) / mean(rhizos$Croiss)
PDT_P <- median(LAUZ_P) / median(rhizos_potimarrons$Croiss)

Prm_P <- c("erSem", "dSem", "maxSem", "ageAdv", "distAdv", "erAdv", "dAdv", "maxAdv", "dmin", "dmax", "EL", "TrT", "Trint", "PDT", "IPD", "pdmax", "pdmin", "RDM", "CVDD", "TMD", "GDs", "SGC", "LDC")
Prm_P <- replicate(n = 5, Prm_P)
Sp_P <- replicate(n = 115, "Potimarron")
Plt_P <- rep(c(1, 2, 3, 4, 5), each = 23)
Val_P <- c(0.1071, 1, 1, 0, 0, 0, 0, 0, P1_minD, P1_maxD, EL_P, 1, 0.03, 0.657, IBD_RP[1], 0.8, 0, RDM_P, 0.15, 0.1, 400, 0.9, 5000,
           0.1071, 1, 1, 0, 0, 0, 0, 0, P2_minD, P2_maxD, EL_P, 1, 0.03, 0.657, IBD_RP[2], 0.8, 0, RDM_P, 0.15, 0.1, 400, 0.9, 5000,
           0.1071, 1, 1, 0, 0, 0, 0, 0, P3_minD, P3_maxD, EL_P, 1, 0.03, 0.657, IBD_RP[3], 0.8, 0, RDM_P, 0.15, 0.1, 400, 0.9, 5000,
           0.1071, 1, 1, 0, 0, 0, 0, 0, P4_minD, P4_maxD, EL_P, 1, 0.03, 0.657, IBD_RP[4], 0.8, 0, RDM_P, 0.15, 0.1, 400, 0.9, 5000,
           0.1071, 1, 1, 0, 0, 0, 0, 0, P5_minD, P5_maxD, EL_P, 1, 0.03, 0.657, IBD_RP[5], 0.8, 0, RDM_P, 0.15, 0.1, 400, 0.9, 5000)

params_rhizos_potimarrons <- data.frame(Parameter = c(Prm_P), 
                                       Specie = c(Sp_P), 
                                       Plante = c(Plt_P), 
                                       Value = Val_P)

write.csv(params_rhizos_potimarrons, "/Users/nicoh/Desktop/Mémoire/Paramètres/parametres_rhizos_potimarrons.csv")

all_rhizotrons <- rbind(params_rhizos_epinards, params_rhizos_butternuts, params_rhizos_potimarrons)
write.csv(all_rhizotrons, "/Users/nicoh/Desktop/Mémoire/Paramètres/all_rhizotrons.csv")

grid.arrange(ELvsD_E, arrangeGrob(ELvsD_B, ELvsD_P), ncol = 2)


#0.12752, 1, 1, 0, 0, 0, 0, 0, E8_minD, E8_maxD, EL_E, 1, 0.03, 6.015, IBD_RE[8], 0.8, 0, RDM_ER, 0.15, 0.1, 400, 0.9, 5000,
