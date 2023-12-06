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

all_rhizos <- read.csv("/Users/nicoh/Desktop/Mémoire/Paramètres/all_rhizotrons.csv")

Anova_dmax <- all_rhizos %>%
  filter(Parameter == "dmax")

# Jeux de données dmin

Anova_dmin <- all_rhizos %>%
  filter(Parameter == "dmin")

# Jeux de données IPD

Anova_IPD <- all_rhizos %>%
  filter(Parameter == "IPD")

# Jeux de données RDM

Anova_RDM <- all_rhizos %>%
  filter(Parameter == "RDM")

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

grid.arrange(SP_dmax, SP_dmin, ncol = 2)

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

SP_IPD

dmax_freq <- ggplot(Anova_dmax, aes(Value, fill = Specie))  + 
  geom_histogram(aes(y = ..density..), bins = 84, colour = "black") + 
  stat_function(fun = dnorm, args = list(mean = mean(Anova_dmax$Value), sd = sd(Anova_dmax$Value))) +
  ggtitle("Distribution des diamètres maximums") +
  theme(plot.title = element_text(size = 24, hjust = 0.5), 
        axis.title.x = element_text(size = 22), 
        axis.title.y = element_text(size = 22),
        axis.text = element_text(size = 16),
        legend.position = "none")+
  xlab("Diamètre maximum [mm]") + 
  ylab("Fréquence")

dmin_freq <- ggplot(Anova_dmin, aes(Value, fill = Specie))  + 
  geom_histogram(aes(y = ..density..), bins = 84, colour = "black") + 
  stat_function(fun = dnorm, args = list(mean = mean(Anova_dmin$Value), sd = sd(Anova_dmin$Value))) + 
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

grid.arrange(dmax_freq, dmin_freq, ncol = 2)

