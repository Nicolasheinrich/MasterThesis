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

all_plants <- read.csv("/Users/nicoh/Desktop/Mémoire/Paramètres/all_plants.csv")
# Jeux de données dmax

Anova_dmax <- all_plants %>%
  filter(Parameter == "dmax")

# Jeux de données dmin

Anova_dmin <- all_plants %>%
  filter(Parameter == "dmin")

# Jeux de données IPD

Anova_IPD <- all_plants %>%
  filter(Parameter == "IPD")

# Jeux de données TMD

Anova_TMD <- all_plants %>%
  filter(Parameter == "TMD")





## Comparaison traitements épinards 
# dmax 

SP_dmax <- ggboxplot(Anova_dmax, x = "Specie",
                     y = "Value",
                     color = "Specie",
                     add = "jitter",
                     notch = TRUE, 
                     outlier.color = "red",
                     outlier.shape = 8,
                     outlier.size = 4) +
  stat_compare_means(comparisons = list(c("Epinard", "Butternut"), c("Butternut", "Potimarron"), c("Epinard", "Potimarron")),
                     method = "t.test",
                     p.adjust.method = "holm") +
  ggtitle("A") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22))+
  xlab("Espèce") + 
  ylab("Diamètre maximal [mm]") 

# dmin 
SP_dmin <- ggboxplot(Anova_dmin, x = "Specie",
                     y = "Value",
                     color = "Specie",
                     add = "jitter",
                     notch = TRUE, 
                     outlier.color = "red",
                     outlier.shape = 8,
                     outlier.size = 4) +
  stat_compare_means(comparisons = list(c("Epinard", "Butternut"), c("Butternut", "Potimarron"), c("Epinard", "Potimarron")),
                     method = "t.test",
                     p.adjust.method = "holm") +
  ggtitle("B") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22)) +
  xlab("Espèce") + 
  ylab("Diamètre minimal [mm]") 

# IPD 
SP_IPD <- ggboxplot(Anova_IPD, x = "Specie",
                    y = "Value",
                    color = "Specie",
                    add = "jitter",
                    notch = TRUE, 
                    outlier.color = "red",
                    outlier.shape = 8,
                    outlier.size = 4) +
  stat_compare_means(comparisons = list(c("Epinard", "Butternut"), c("Butternut", "Potimarron"), c("Epinard", "Potimarron")),
                     method = "t.test",
                     p.adjust.method = "holm") +
  ggtitle("Boxplot de la distance inter-ramification en fonction de l'espèce") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22)) +
  xlab("Espèce") + 
  ylab("Distance inter-ramification [mm]") 

# TMD 
SP_TMD <- ggboxplot(Anova_TMD, x = "Specie",
                    y = "Value",
                    color = "Specie",
                    add = "jitter",
                    notch = TRUE, 
                    outlier.color = "red",
                    outlier.shape = 8,
                    outlier.size = 4) +
  stat_compare_means(comparisons = list(c("Epinard", "Butternut"), c("Butternut", "Potimarron"), c("Epinard", "Potimarron")),
                     method = "t.test",
                     p.adjust.method = "holm") +
  ggtitle("Boxplot de la densité des tissus racinaires en fonction de l'espèce") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22)) +
  xlab("Espèce") + 
  ylab("Densité des tissus racinaires [g/cm3]") 

SP_dmax | SP_dmin 
SP_IPD  
SP_TMD


### Comparaison de traitements 
## Epinard 
Anova_dmax_E <- Anova_dmax %>%
  filter(Specie == "Epinard")
Anova_dmin_E <- Anova_dmin %>%
  filter(Specie == "Epinard")
Anova_IPD_E <- Anova_IPD %>%
  filter(Specie == "Epinard")
Anova_TMD_E <- Anova_TMD %>%
  filter(Specie == "Epinard")

# dmax
Trt_dmax_E <- ggboxplot(Anova_dmax_E, x = "Treatment",
                        y = "Value",
                        color = "Treatment",
                        add = "jitter",
                        notch = TRUE, 
                        outlier.color = "red",
                        outlier.shape = 8,
                        outlier.size = 4) +
  stat_compare_means(comparisons = list(c("Bache", "Feuilles"), c("Bache", "Labour"), c("Bache", "Paille"), c("Feuilles", "Labour"), c("Feuilles", "Paille"), c("Labour", "Paille")),
                     method = "t.test",
                     p.adjust.method = "holm") +
  ggtitle("A") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22))+
  xlab("Traitement") + 
  ylab("Diamètre maximal [mm]") 

# dmin
Trt_dmin_E <- ggboxplot(Anova_dmin_E, x = "Treatment",
                        y = "Value",
                        color = "Treatment",
                        add = "jitter",
                        notch = TRUE, 
                        outlier.color = "red",
                        outlier.shape = 8,
                        outlier.size = 4) +
  stat_compare_means(comparisons = list(c("Bache", "Feuilles"), c("Bache", "Labour"), c("Bache", "Paille"), c("Feuilles", "Labour"), c("Feuilles", "Paille"), c("Labour", "Paille")),
                     method = "t.test",
                     p.adjust.method = "holm") +
  ggtitle("B") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22))+
  xlab("Traitement") + 
  ylab("Diamètre minimal [mm]") 

Trt_dmax_E | Trt_dmin_E

# IPD
Trt_IPD_E <- ggboxplot(Anova_IPD_E, x = "Treatment",
                       y = "Value",
                       color = "Treatment",
                       add = "jitter",
                       notch = TRUE, 
                       outlier.color = "red",
                       outlier.shape = 8,
                       outlier.size = 4) +
  stat_compare_means(comparisons = list(c("Bache", "Feuilles"), c("Bache", "Labour"), c("Bache", "Paille"), c("Feuilles", "Labour"), c("Feuilles", "Paille"), c("Labour", "Paille")),
                     method = "t.test",
                     p.adjust.method = "holm") +
  ggtitle("A") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22))+
  xlab("Traitement") + 
  ylab("Distance inter-ramification [mm]") 

Trt_IPD_E

# TMD 
Trt_TMD_E <- ggboxplot(Anova_TMD_E, x = "Treatment",
                       y = "Value",
                       color = "Treatment",
                       add = "jitter",
                       notch = TRUE, 
                       outlier.color = "red",
                       outlier.shape = 8,
                       outlier.size = 4) +
  stat_compare_means(comparisons = list(c("Bache", "Feuilles"), c("Bache", "Labour"), c("Bache", "Paille"), c("Feuilles", "Labour"), c("Feuilles", "Paille"), c("Labour", "Paille")),
                     method = "t.test",
                     p.adjust.method = "holm") +
  ggtitle("B") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22))+
  xlab("Traitement") + 
  ylab("Densité des tissus racinaires [g/cm3]") 

Trt_TMD_E

Trt_IPD_E | Trt_TMD_E
## Butternut 
Anova_dmax_B <- Anova_dmax %>%
  filter(Specie == "Butternut")
Anova_dmin_B <- Anova_dmin %>%
  filter(Specie == "Butternut")
Anova_IPD_B <- Anova_IPD %>%
  filter(Specie == "Butternut")
Anova_TMD_B <- Anova_TMD %>%
  filter(Specie == "Butternut")

# dmax
Trt_dmax_B <- ggboxplot(Anova_dmax_B, x = "Treatment",
                        y = "Value",
                        color = "Treatment",
                        add = "jitter",
                        notch = TRUE, 
                        outlier.color = "red",
                        outlier.shape = 8,
                        outlier.size = 4) +
  stat_compare_means(comparisons = list(c("Bache", "Feuilles"), c("Bache", "Labour"), c("Bache", "Paille"), c("Feuilles", "Labour"), c("Feuilles", "Paille"), c("Labour", "Paille")),
                     method = "t.test",
                     p.adjust.method = "holm") +
  ggtitle("A") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22))+
  xlab("Traitement") + 
  ylab("Diamètre maximal [mm]") 

# dmin
Trt_dmin_B <- ggboxplot(Anova_dmin_B, x = "Treatment",
                        y = "Value",
                        color = "Treatment",
                        add = "jitter",
                        notch = TRUE, 
                        outlier.color = "red",
                        outlier.shape = 8,
                        outlier.size = 4) +
  stat_compare_means(comparisons = list(c("Bache", "Feuilles"), c("Bache", "Labour"), c("Bache", "Paille"), c("Feuilles", "Labour"), c("Feuilles", "Paille"), c("Labour", "Paille")),
                     method = "t.test",
                     p.adjust.method = "holm") +
  ggtitle("B") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22))+
  xlab("Traitement") + 
  ylab("Diamètre minimal [mm]") 

Trt_dmax_B | Trt_dmin_B

# IPD
Trt_IPD_B <- ggboxplot(Anova_IPD_B, x = "Treatment",
                       y = "Value",
                       color = "Treatment",
                       add = "jitter",
                       notch = TRUE, 
                       outlier.color = "red",
                       outlier.shape = 8,
                       outlier.size = 4) +
  stat_compare_means(comparisons = list(c("Bache", "Feuilles"), c("Bache", "Labour"), c("Bache", "Paille"), c("Feuilles", "Labour"), c("Feuilles", "Paille"), c("Labour", "Paille")),
                     method = "t.test",
                     p.adjust.method = "holm") +
  ggtitle("A") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22))+
  xlab("Traitement") + 
  ylab("Distance inter-ramification [mm]") 

Trt_IPD_B

# TMD 
Trt_TMD_B <- ggboxplot(Anova_TMD_B, x = "Treatment",
                       y = "Value",
                       color = "Treatment",
                       add = "jitter",
                       notch = TRUE, 
                       outlier.color = "red",
                       outlier.shape = 8,
                       outlier.size = 4) +
  stat_compare_means(comparisons = list(c("Bache", "Feuilles"), c("Bache", "Labour"), c("Bache", "Paille"), c("Feuilles", "Labour"), c("Feuilles", "Paille"), c("Labour", "Paille")),
                     method = "t.test",
                     p.adjust.method = "holm") +
  ggtitle("B") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22))+
  xlab("Traitement") + 
  ylab("Densité des tissus racinaires [g/cm3]") 

Trt_TMD_B

Trt_IPD_B | Trt_TMD_B

## Potimarron 
Anova_dmax_P <- Anova_dmax %>%
  filter(Specie == "Potimarron")
Anova_dmin_P <- Anova_dmin %>%
  filter(Specie == "Potimarron")
Anova_IPD_P <- Anova_IPD %>%
  filter(Specie == "Potimarron")
Anova_TMD_P <- Anova_TMD %>%
  filter(Specie == "Potimarron")

# dmax
Trt_dmax_P <- ggboxplot(Anova_dmax_P, x = "Treatment",
                        y = "Value",
                        color = "Treatment",
                        add = "jitter",
                        notch = TRUE, 
                        outlier.color = "red",
                        outlier.shape = 8,
                        outlier.size = 4) +
  stat_compare_means(comparisons = list(c("Bache", "Feuilles"), c("Bache", "Labour"), c("Bache", "Temoin"), c("Feuilles", "Labour"), c("Feuilles", "Temoin"), c("Labour", "Temoin")),
                     method = "t.test",
                     p.adjust.method = "holm") +
  ggtitle("A") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22))+
  xlab("Traitement") + 
  ylab("Diamètre maximal [mm]") 

# dmin
Trt_dmin_P <- ggboxplot(Anova_dmin_P, x = "Treatment",
                        y = "Value",
                        color = "Treatment",
                        add = "jitter",
                        notch = TRUE, 
                        outlier.color = "red",
                        outlier.shape = 8,
                        outlier.size = 4) +
  stat_compare_means(comparisons = list(c("Bache", "Feuilles"), c("Bache", "Labour"), c("Bache", "Temoin"), c("Feuilles", "Labour"), c("Feuilles", "Temoin"), c("Labour", "Temoin")),
                     method = "t.test",
                     p.adjust.method = "holm") +
  ggtitle("B") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22))+
  xlab("Traitement") + 
  ylab("Diamètre minimal [mm]") 

Trt_dmax_P | Trt_dmin_P

# IPD
Trt_IPD_P <- ggboxplot(Anova_IPD_P, x = "Treatment",
                       y = "Value",
                       color = "Treatment",
                       add = "jitter",
                       notch = TRUE, 
                       outlier.color = "red",
                       outlier.shape = 8,
                       outlier.size = 4) +
  stat_compare_means(comparisons = list(c("Bache", "Feuilles"), c("Bache", "Labour"), c("Bache", "Temoin"), c("Feuilles", "Labour"), c("Feuilles", "Temoin"), c("Labour", "Temoin")),
                     method = "t.test",
                     p.adjust.method = "holm") +
  ggtitle("A") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22))+
  xlab("Traitement") + 
  ylab("Distance inter-ramification [mm]") 

Trt_IPD_P

# TMD 
Trt_TMD_P <- ggboxplot(Anova_TMD_P, x = "Treatment",
                       y = "Value",
                       color = "Treatment",
                       add = "jitter",
                       notch = TRUE, 
                       outlier.color = "red",
                       outlier.shape = 8,
                       outlier.size = 4) +
  stat_compare_means(comparisons = list(c("Bache", "Feuilles"), c("Bache", "Labour"), c("Bache", "Temoin"), c("Feuilles", "Labour"), c("Feuilles", "Temoin"), c("Labour", "Temoin")),
                     method = "t.test",
                     p.adjust.method = "holm") +
  ggtitle("B") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22))+
  xlab("Traitement") + 
  ylab("Densité des tissus racinaires [g/cm3]") 

Trt_TMD_P
Trt_IPD_P | Trt_TMD_P

######################################################################################################
## Distribution des diamètres 

dmax <- all_plants %>%
  filter(all_plants$Parameter == "dmax")


dmax_freq <- ggplot(dmax, aes(Value, fill = Specie))  + 
  geom_histogram(aes(y = ..density..), bins = 84, colour = "black") + 
  stat_function(fun = dnorm, args = list(mean = mean(dmax$Value), sd = sd(dmax$Value))) +
  ggtitle("Distribution des diamètres maximums") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        axis.text = element_text(size = 16),
        legend.position = "none")+
  xlab("Diamètre maximum [mm]") + 
  ylab("Fréquence") 


dmin <- all_plants %>%
  filter(all_plants$Parameter == "dmin")

dmin_freq <- ggplot(dmin, aes(Value, fill = Specie))  + 
  geom_histogram(aes(y = ..density..), bins = 84, colour = "black") + 
  stat_function(fun = dnorm, args = list(mean = mean(dmin$Value), sd = sd(dmin$Value))) + 
  ggtitle("Distribution des diamètres minimums") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        legend.title = element_text(size=24),
        axis.text = element_text(size = 16),
        legend.text = element_text(size=22))+
  xlab("Diamètre minimum [mm]") + 
  labs(fill='Espèce') +
  ylab("Fréquence") 

dmax_freq | dmin_freq

