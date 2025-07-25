# packages
library(lubridate)
library(ggplot2)
library(scales)
library(ggpubr)
library(ggcorrplot)
library(RColorBrewer)
library(dplyr)
library(tidyverse)
library(car)
library(MASS)
library(GGally)
library(vegan)
library(broom)
library(tidyr)
library(ggplot2)
library(patchwork)



# read in file
DBCdata <- read.csv(file.choose())



# convert date
DBCdata$Sample.Collection.Date <- 
  as.Date(DBCdata$Sample.Collection.Date)



# subset data
sub.2022 <- subset(DBCdata, 
                   DBCdata$Year == "2022")
sub.2023 <- subset(DBCdata, 
                   DBCdata$Year == "2023")
sub.2022.snow.only <- subset(sub.2022, 
                             sub.2022$Sample.Type == "snow")
sub.all.snow <- subset(DBCdata, 
                       DBCdata$Sample.Type == "snow")
sub.not.snow <- subset(DBCdata, 
                       DBCdata$Sample.Type %in% 
                         c("supraglacial melt", "river", "stream"))



# variables shorthand
date <- DBCdata$Sample.Collection.Date
DBC.Dittmar.ug <- DBCdata$Dittmar..DBC..µg.C.L
Y <- DBCdata$Year
DOC <- DBCdata$X.DOC..mg.L
DBC.Dittmar.mg <- DBCdata$Dittmar..DBC..mg.C.L
B6CA.uM <- DBCdata$X.B6CA..uM
B5CA.uM <- DBCdata$X.B5CA..uM
B4CA.uM <- DBCdata$X.B4CA..uM
B3CA.uM <- DBCdata$X.B3CA..uM
B6CA <- DBCdata$B6CA
B5CA <- DBCdata$B5CA
B4CA <- DBCdata$B4CA
B3CA <- DBCdata$B3CA
ratio.2022 <- sub.2022$Ratio.B3.B4.B5.B6
ratio.2022.snow <- sub.2022.snow.only$Ratio.B3.B4.B5.B6
ratio.2023 <- sub.2023$Ratio.B3.B4.B5.B6
ratio <- DBCdata$Ratio.B3.B4.B5.B6



# summary stats


## DOC - all
DOC.nan <- na.omit(DOC)
mean(DOC.nan)
sd(DOC.nan)


## DOC - snow
DOC.snow <- na.omit(sub.all.snow$X.DOC..mg.L)
mean(DOC.snow)
sd(DOC.snow)


## DOC - not snow
DOC.non.snow <- na.omit(sub.not.snow$X.DOC..mg.L)
mean(DOC.non.snow)
sd(DOC.non.snow)


## DBC - snow
DBC.snow <- na.omit(sub.all.snow$Dittmar..DBC..µg.C.L)
mean(DBC.snow)
sd(DBC.snow)


## DBC - not snow
DBC.non.snow <- na.omit(sub.not.snow$Dittmar..DBC..µg.C.L)
mean(DBC.non.snow)
sd(DBC.non.snow)


## DBC:DOC ratio
DOC.DBC.nan <- na.omit(DBCdata$DBC.DOC.......Dittmar..2008.)
mean(DOC.DBC.nan)
sd(DOC.DBC.nan)


## 2022 snow DBC
Ditt.snow.2022.nan <- na.omit(sub.2022.snow.only$Dittmar..DBC..µg.C.L)
mean(Ditt.snow.2022.nan)
sd(Ditt.snow.2022.nan)


## 2023 snow DBC
Ditt.2023.nan <- na.omit(sub.2023$Dittmar..DBC..µg.C.L)
mean(Ditt.2023.nan)
sd(Ditt.2023.nan)


## 2022 early season snow
early.2022 <- subset(sub.2022.snow.only, 
                     sub.2022.snow.only$
                       Early.vs..Late.Season..March.July.vs..August.October. 
                     == "Early")
early.2022.Ditt.nan <- na.omit(early.2022$Dittmar..DBC..µg.C.L)
mean(early.2022.Ditt.nan)
sd(early.2022.Ditt.nan)


## 2022 late season snow
late.2022 <- subset(sub.2022.snow.only, sub.2022.snow.only$
                      Early.vs..Late.Season..March.July.vs..August.October. 
                    == "Late")
late.2022.Ditt.nan <- na.omit(late.2022$Dittmar..DBC..µg.C.L)
mean(late.2022.Ditt.nan)
sd(late.2022.Ditt.nan)


## 2023 early season snow
early.2023 <- subset(sub.2023, 
                     sub.2023$
                       Early.vs..Late.Season..March.July.vs..August.October. 
                     == "Early")
early.2023.Ditt.nan <- na.omit(early.2023$Dittmar..DBC..µg.C.L)
mean(early.2023.Ditt.nan)
sd(early.2023.Ditt.nan)


## 2023 late season snow
late.2023 <- subset(sub.2023, sub.2023$
                      Early.vs..Late.Season..March.July.vs..August.October. 
                    == "Late")
late.2023.Ditt.nan <- na.omit(late.2023$Dittmar..DBC..µg.C.L)
mean(late.2023.Ditt.nan)
sd(late.2023.Ditt.nan)


## BPCA ratio
ratio.nan <- na.omit(ratio)
summary(ratio.nan)
mean(ratio.nan)
sd(ratio.nan)


## BPCA ratio snow
ratio.snow.nan <- na.omit(sub.all.snow$Ratio.B3.B4.B5.B6)
summary(ratio.snow.nan)
mean(ratio.snow.nan)
sd(ratio.snow.nan)


## BPCA ratio snow 2022
ratio.2022.snow.nan <- na.omit(ratio.2022.snow)
mean(ratio.2022.snow.nan)
sd(ratio.2022.snow.nan)


## BPCA ratio snow 2023
ratio.2023.nan <- na.omit(ratio.2023)
mean(ratio.2023.nan)
sd(ratio.2023.nan)


## BPCA % snow
B6CA.snow.nan <- na.omit(sub.all.snow$B6CA)
print(summary(B6CA.snow.nan), digits = 6)
sd(B6CA.snow.nan)


## B6CA % not snow
B6CA.not.snow.nan <- na.omit(sub.not.snow$B6CA)
print(summary(B6CA.not.snow.nan), digits = 6)
sd(B6CA.not.snow.nan)


## B5CA % snow
B5CA.snow.nan <- na.omit(sub.all.snow$B5CA)
print(summary(B5CA.snow.nan), digits = 6)
sd(B5CA.snow.nan)


## B5CA % not snow
B5CA.not.snow.nan <- na.omit(sub.not.snow$B5CA)
print(summary(B5CA.not.snow.nan), digits = 6)
sd(B5CA.not.snow.nan)


## B4CA % snow
B4CA.snow.nan <- na.omit(sub.all.snow$B4CA)
print(summary(B4CA.snow.nan), digits = 6)
sd(B4CA.snow.nan)


## B4CA % not snow
B4CA.not.snow.nan <- na.omit(sub.not.snow$B4CA)
print(summary(B4CA.not.snow.nan), digits = 6)
sd(B4CA.not.snow.nan)


## B3CA % snow
B3CA.snow.nan <- na.omit(sub.all.snow$B3CA)
print(summary(B3CA.snow.nan), digits = 6)
sd(B3CA.snow.nan)


## B3CA % not snow
B3CA.not.snow.nan <- na.omit(sub.not.snow$B3CA)
print(summary(B3CA.not.snow.nan), digits = 6)
sd(B3CA.not.snow.nan)


## B6CA % 2022 snow
B6CA.nan.2022 <- na.omit(sub.2022.snow.only$B6CA)
print(summary(B6CA.nan.2022), digits = 6)
sd(B6CA.nan.2022)


## B5CA % 2022 snow
B5CA.nan.2022 <- na.omit(sub.2022.snow.only$B5CA)
print(summary(B5CA.nan.2022), digits = 6)
sd(B5CA.nan.2022)


## B4CA % 2022 snow
B4CA.nan.2022 <- na.omit(sub.2022.snow.only$B4CA)
print(summary(B4CA.nan.2022), digits = 6)
sd(B4CA.nan.2022)


## B3CA % 2022 snow
B3CA.nan.2022 <- na.omit(sub.2022.snow.only$B3CA)
print(summary(B3CA.nan.2022), digits = 6)
sd(B3CA.nan.2022)


## B6CA % 2023 snow
B6CA.nan.2023 <- na.omit(sub.2023$B6CA)
print(summary(B6CA.nan.2023), digits = 6)
sd(B6CA.nan.2023)


## B5CA % 2023 snow
B5CA.nan.2023 <- na.omit(sub.2023$B5CA)
print(summary(B5CA.nan.2023), digits = 6)
sd(B5CA.nan.2023)


## B4CA % 2023 snow
B4CA.nan.2023 <- na.omit(sub.2023$B4CA)
print(summary(B4CA.nan.2023), digits = 6)
sd(B4CA.nan.2023)


## B3CA % 2023 snow
B3CA.nan.2023 <- na.omit(sub.2023$B3CA)
print(summary(B3CA.nan.2023), digits = 6)
sd(B3CA.nan.2023)


## [B6CA]
B6CA.con.nan <- na.omit(DBCdata$X.B6CA..uM)
mean(B6CA.con.nan)
sd(B6CA.con.nan)


## [B5CA]
B5CA.con.nan <- na.omit(DBCdata$X.B5CA..uM)
mean(B5CA.con.nan)
sd(B5CA.con.nan)


## [B4CA]
B4CA.con.nan <- na.omit(DBCdata$X.B4CA..uM)
mean(B4CA.con.nan)
sd(B4CA.con.nan)


## [B3CA]
B3CA.con.nan <- na.omit(DBCdata$X.B3CA..uM)
mean(B3CA.con.nan)
sd(B3CA.con.nan)



# hypothesis testing


## normality
p.density <- ggdensity(DBC.Dittmar.ug, 
                       main = "Density plot of DBC (µg/L)",
                       xlab = "DBC (Dittmar) (µg/L)")
p.density
P.QQ <- ggqqplot(DBC.Dittmar.ug)
P.QQ
shapiro.test(sub.2022$Dittmar..DBC..µg.C.L)
shapiro.test(sub.2023$Dittmar..DBC..µg.C.L)
shapiro.test(DBC.Dittmar.ug)
shapiro.test(DOC.nan)


## variance
leveneTest(sub.2022.snow.only$Dittmar..DBC..µg.C.L 
           ~ sub.2022.snow.only$
             Early.vs..Late.Season..March.July.vs..August.October., 
           sub.2022.snow.only)

leveneTest(sub.2023$Dittmar..DBC..µg.C.L 
           ~ sub.2023$
             Early.vs..Late.Season..March.July.vs..August.October., 
           sub.2023)



# stats tests - differences between early vs. late season samples
kruskal.test(sub.2022.snow.only$Dittmar..DBC..µg.C.L 
             ~ sub.2022.snow.only$
               Early.vs..Late.Season..March.July.vs..August.October., 
             sub.2022.snow.only)

kruskal.test(sub.2023$Dittmar..DBC..µg.C.L 
             ~ sub.2023$
               Early.vs..Late.Season..March.July.vs..August.October., 
             sub.2023)
kruskal.test(sub.2022.snow.only$Ratio.B3.B4.B5.B6
             ~ sub.2022.snow.only$
               Early.vs..Late.Season..March.July.vs..August.October., 
             sub.2022.snow.only)
kruskal.test(sub.2023$Ratio.B3.B4.B5.B6
             ~ sub.2023$
               Early.vs..Late.Season..March.July.vs..August.October., 
             sub.2023)



# barplot - BPCA relative proportions (B3+B4:B5+B6)


## reformat data so that each BPCA % is its own row (2022)
data.long.2022 <- sub.2022.snow.only %>%
  pivot_longer(cols = starts_with("B"), 
               names_to = "BPCA", 
               values_to = "Percentage")
data.long.2023 <- sub.2023 %>%
  pivot_longer(cols = starts_with("B"), 
               names_to = "BPCA", 
               values_to = "Percentage")


## plot (2022)
order.2022 <- c("16-May-2022", 
                "15-Jul-2022", 
                "05-Aug-2022", 
                "06-Aug-2022", 
                "11-Aug-2022", 
                "15-Aug-2022", 
                "15-Sept-2022 (PR)", 
                "15-Sept-2022 (SH)", 
                "25-Sept-2022", 
                "14-Oct-2022")
p.BPCA.bar.2022 <- ggplot(data.long.2022, 
                          aes(x = Axis.Label, 
                              y = Percentage, 
                              fill = BPCA)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = 
                      c("B3CA" = "#CC79A7", 
                        "B4CA" = "#009E73", 
                        "B5CA" = "#56B4E9", 
                        "B6CA" = "#E69F00")) +
  labs(y = "Percentage of Total DBC", 
       x = "Collection Date", 
       fill = "BPCA",
       title = "a.) 2022") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, 
                                   size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18)) +
  scale_x_discrete(limits = order.2022)
p.BPCA.bar.2022
order.2023 <- c("02-Mar-2023", 
                "06-May-2023", 
                "20-May-2023 (AP)", 
                "20-May-2023 (BB)", 
                "08-Jun-2023", 
                "26-Jun-2023", 
                "10-Jul-2023", 
                "17-Jul-2023", 
                "24-Jul-2023", 
                "06-Aug-2023", 
                "23-Aug-2023")
p.BPCA.bar.2023 <- ggplot(data.long.2023, 
                          aes(x = Axis.Label, 
                              y = Percentage, 
                              fill = BPCA)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("B3CA" = "#CC79A7", 
                               "B4CA" = "#009E73", 
                               "B5CA" = "#56B4E9", 
                               "B6CA" = "#E69F00")) +
  labs(y = "Percentage of Total DBC", 
       x = "Collection Date", 
       fill = "BPCA",
       title = "b.) 2023") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, 
                                   size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 18)) +
  scale_x_discrete(limits = order.2023)
p.BPCA.bar.2023



# BPCA ratio


## stats - early season vs. late season

### 2022
p.density.1 <- ggdensity(sub.2022.snow.only$B3.B4.B5.B6, 
                         main = "Density plot of BPCA Ratio",
                         xlab = "(B3CA + B4CA) / (B5CA + B6CA)")
p.density.1
P.QQ.1 <- ggqqplot(sub.2022.snow.only$B3.B4.B5.B6)
P.QQ.1
shapiro.test(ratio.2022.snow.nan)
leveneTest(ratio.2022.snow.nan ~ 
             sub.2022.snow.only$
             Early.vs..Late.Season..March.July.vs..August.October., 
           sub.2022.snow.only)
kruskal.test(ratio.2022.snow.nan ~ 
               sub.2022.snow.only$
               Early.vs..Late.Season..March.July.vs..August.October., 
             sub.2022.snow.only)

### 2023
p.density.2 <- ggdensity(sub.2023$B3.B4.B5.B6, 
                         main = "Density plot of BPCA Ratio",
                         xlab = "(B3CA + B4CA) / (B5CA + B6CA)")
p.density.2
P.QQ.2 <- ggqqplot(sub.2023$B3.B4.B5.B6)
P.QQ.2
shapiro.test(ratio.2023.nan)
leveneTest(ratio.2023 ~ 
             sub.2023$Early.vs..Late.Season..March.July.vs..August.October., 
           sub.2023)
kruskal.test(ratio.2023 ~ 
               sub.2023$Early.vs..Late.Season..March.July.vs..August.October., 
             sub.2023)



# multi-variate analysis - PCA


## prep data for PCA
pca.data <- sub.all.snow[, c("B6CA", 
                             "B5CA", 
                             "B4CA", 
                             "B3CA")]
pca.data <- na.omit(pca.data)


## run PCA
pca.result <- prcomp(pca.data, 
                     scale. = FALSE)
summary(pca.result)

### extract loadings from PCA result
loadings <- pca.result$rotation[, 1:2]

### create dataframe for plotting loadings
loadings.df <- data.frame(BPCA = c("B6CA", 
                                   "B5CA", 
                                   "B4CA", 
                                   "B3CA"),
                          PC1 = loadings[, 1],
                          PC2 = loadings[, 2],
                          vjust = c(2, 
                                    -1, 
                                    2, 
                                    2),
                          hjust = c(-0.2, 
                                    1, 
                                    0.5, 
                                    -0.2))

### plot loadings
p.loadings <- ggplot(loadings.df, 
                     aes(x = PC1, 
                         y = PC2, 
                         label = BPCA)) +
  geom_point(size = 5, 
             aes(color = BPCA)) + 
  geom_label(aes(label = BPCA, 
                 color = BPCA, 
                 vjust = vjust, 
                 hjust = hjust), 
             label.size = 0.5, 
             label.padding = unit(0.2, 
                                  "lines")) + 
  scale_color_manual(values = c("B6CA" = "#E69F00", 
                                "B5CA" = "#56B4E9", 
                                "B4CA" = "#009E73", 
                                "B3CA" = "#CC79A7")) +
  labs(title = "PCA Loadings Plot", 
       x = "PC1 (96.97%)", 
       y = "PC2 (2.79%)") +
  theme_minimal() + 
  theme(legend.position = "none")
p.loadings

### add PCA scores to original dataframe
pca.data <- sub.all.snow[, 
                         c("Sample.ID", 
                           "Sample.Location", 
                           "Sample.Type", "Year",
                           "Early.vs..Late.Season..March.July.vs..August.October.",
                           "B6CA", 
                           "B5CA", 
                           "B4CA", 
                           "B3CA")]
pca.data.nan <- na.omit(pca.data)
pca.data.nan$PC1 <- pca.result$x[, 1]  # Score for PC1
pca.data.nan$PC2 <- pca.result$x[, 2]  # Score for PC2

### scaling factor for loadings
scaling.factor <- max(abs(pca.data.nan$PC1), 
                      abs(pca.data.nan$PC2)) / max(abs(loadings))

### update loadings df with scaling factor
loadings.df <- data.frame(BPCA = c("B6CA", 
                                   "B5CA", 
                                   "B4CA", 
                                   "B3CA"),
                          PC1 = loadings[, 1] * scaling.factor,
                          PC2 = loadings[, 2] * scaling.factor,
                          vjust = c(0, -1, 0, 0.25),
                          hjust = c(-0.25, 0, 1.25, -0.5))

### plot PCA scores by sample location and early/late season
p.scores.location <- ggplot(pca.data.nan, 
                            aes(x = PC1, 
                                y = PC2, 
                                color = Sample.Location,
                                shape = Early.vs..Late.Season..March.July.vs..August.October.)) +
  geom_point(size = 3) +
  labs(x = "PC1 (95.21%)", 
       y = "PC2 (5.88%)") +
  theme_minimal() + 
  scale_color_manual(values = c("Artist Point" = "#CC6677", 
                                "Bagley Basin" = "#332288",
                                "Camp Kiser" = "#DDCC77", 
                                "Easton Glacier" = "#117733",
                                "Muir Snowfield" = "#88CCEE", 
                                "Ptarmigan Ridge" = "#882255", 
                                "Rainbow Glacier" = "#44AA99", 
                                "Sholes Glacier" = "#999933")) + 
  labs(color = "Sampling Location", 
       shape = "Early vs. Late Season") +
  geom_segment(data = loadings.df, 
               aes(x = 0, 
                   y = 0, 
                   xend = PC1, 
                   yend = PC2), 
               arrow = arrow(length = unit(0.3, 
                                           "cm")), 
               color = "black",
               inherit.aes = FALSE) +
  geom_text(data = loadings.df, 
            aes(x = PC1, 
                y = PC2, 
                label = BPCA, 
                vjust = vjust, 
                hjust = hjust), 
            color = "black", 
            size = 3.5, 
            inherit.aes = FALSE)
p.scores.location



# SNICAR


## data prep
library(reshape2)
SNICAR.2022 <- read.csv(file.choose())
SNICARmelt.2022 <- melt(SNICAR.2022, 
                        id.vars = "Wavelength")
SNICAR.2023 <- read.csv(file.choose())
SNICARmelt.2023 <- melt(SNICAR.2023, 
                        id.vars = "Wavelength")


## plot 2022
p.SNICAR.2022 <- ggplot(SNICARmelt.2022, 
                        aes(Wavelength, 
                            value,
                            col = variable)) + 
  geom_line() + 
  scale_x_continuous(limits = c(300, 1500), 
                     expand = c(0, 0)) +  
  scale_y_continuous(limits = c(0, 1), 
                     expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme_minimal() +
  xlab("Wavelength (nm)") + 
  ylab("Albedo") + 
  labs(color = "Sample ID", 
       title = "a.) 2022") +
  scale_color_manual(labels = c("16-May-2022", 
                                "15-Jul-2022", 
                                "05-Aug-2022", 
                                "06-Aug-2022", 
                                "11-Aug-2022", 
                                "15-Aug-2022", 
                                "15-Sept-2022 (PR)", 
                                "15-Sept-2022 (SH)", 
                                "25-Sept-2022", 
                                "14-Oct-2022"),
                     values = c("#332288", 
                                "#117733", 
                                "#88CCEE", 
                                "#882255",
                                "#44AA99", 
                                "#999933", 
                                "#AA4499", 
                                "#EE8866", 
                                "#CC3311", 
                                "#222255")) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 22),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(margin = margin(t = 8)),
        axis.text.y = element_text(margin = margin(r = 8)))
p.SNICAR.2022


## plot 2023
p.SNICAR.2023 <- ggplot(SNICARmelt.2023, 
                        aes(Wavelength, 
                            value, 
                            col = variable)) + 
  geom_line() + scale_x_continuous(limits = c(300, 1500), 
                                   expand = c(0, 0)) +  
  scale_y_continuous(limits = c(0, 1), 
                     expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  theme_minimal() +
  xlab("Wavelength (nm)") + 
  ylab("Albedo") + 
  labs(color = "Sample ID",
       title = "b.) 2023") +
  scale_color_manual(labels = c("2-Mar-2023",
                                "6-May-2023", 
                                "20-May-2023 (AP)", 
                                "20-May-2023 (BB)", 
                                "8-Jun-2023", 
                                "26-Jun-2023", 
                                "10-Jul-2023", 
                                "17-Jul-2023", 
                                "24-Jul-2023", 
                                "6-Aug-2023", 
                                "23-Aug-2023"),
                     values = c("#332288", 
                                "#DDCC77", 
                                "#117733", 
                                "#88CCEE", 
                                "#882255", 
                                "#44AA99", 
                                "#999933", 
                                "#AA4499", 
                                "#EE8866", 
                                "#CC3311", 
                                "#222255")) + 
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 22),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(margin = margin(t = 8)),
        axis.text.y = element_text(margin = margin(r = 8)))
p.SNICAR.2023



# NAAPS deposition & precip. 


## data prep
naaps.baker <- read.csv(file.choose())
naaps.baker$date <- 
  as.Date(naaps.baker$date)
naaps.precip <- read.csv(file.choose())


## stats tests
naaps.test <- read.csv(file.choose())
naaps.test$Date <- as.Date(naaps.test$Date)
naaps.test.2022 <- naaps.test %>%
  filter(format(Date, "%Y") == "2022")
naaps.test.2023 <- naaps.test %>%
  filter(format(Date, "%Y") == "2023")
naaps.test.long.2022 <- naaps.test.2022 %>%
  pivot_longer(cols = c(smoke.tot, 
                        dust.tot, 
                        abf.tot),
               names_to = "Variable", 
               values_to = "Value")
naaps.test.long.2023 <- naaps.test.2023 %>%
  pivot_longer(cols = c(smoke.tot, 
                        dust.tot, 
                        abf.tot),
               names_to = "Variable", 
               values_to = "Value")
early.2022 <- naaps.test.2022 %>%
  filter(month(Date) %in% 5:7)
late.2022 <- naaps.test.2022 %>%
  filter(month(Date) %in% 8:10)
early.2023 <- naaps.test.2023 %>%
  filter(month(Date) %in% 5:7)
late.2023 <- naaps.test.2023 %>%
  filter(month(Date) %in% 8:10)

### normality
shapiro.test(early.2022$smoke.tot)
shapiro.test(early.2023$smoke.tot)
shapiro.test(late.2022$smoke.tot)
shapiro.test(late.2023$smoke.tot)

### stats tests - differences between early vs. late season deposition
early.2022$season <- "early"
late.2022$season <- "late"
combined.smoke.2022 <- rbind(early.2022, 
                             late.2022)
kruskal.test(smoke.tot ~ season, 
             data = combined.smoke.2022)
early.2023$season <- "early"
late.2023$season <- "late"
combined.smoke.2023 <- rbind(early.2023, 
                             late.2023)
kruskal.test(smoke.tot ~ season, 
             data = combined.smoke.2023)


## 2022
naaps.baker.2022 <- naaps.baker %>%
  filter(format(date, "%Y") == "2022")
naaps.baker.long.2022 <- naaps.baker.2022 %>%
  pivot_longer(cols = c(smoke.tot, 
                        dust.tot, 
                        abf.tot),
               names_to = "Variable", 
               values_to = "Value")

### set 2022 markers
marker.baker.dates.2022 <- as.Date(c("2022-05-16", 
                                     "2022-06-30", 
                                     "2022-07-31", 
                                     "2022-08-05", 
                                     "2022-08-06", 
                                     "2022-08-11", 
                                     "2022-09-15",
                                     "2022-09-25", 
                                     "2022-10-14"))
markers.baker.2022 <- data.frame(
  Date = marker.baker.dates.2022,
  ymin = 0,
  ymax = 5)

### Baker 2022 plot
naaps.baker.2022.smoke <- subset(naaps.baker.long.2022, 
                                 naaps.baker.long.2022$Variable == "smoke.tot")
markers.baker.2022$color <- c("black", 
                              "black", 
                              "black", 
                              "black", 
                              "black",
                              "black", 
                              "darkorange", 
                              "black", 
                              "black")
naaps.baker.smoke.2022.p <- ggplot(naaps.baker.2022.smoke, 
                                   aes(x = date, 
                                       y = Value)) +
  geom_line(color = "deepskyblue2") + 
  theme_minimal() + 
  labs(title = "a.) Mount Baker, 2022", 
       x = "Date", 
       y = expression("Smoke Deposition Flux (mg/m"^2*"/day)")) +
  geom_segment(data = markers.baker.2022, 
               aes(x = Date, 
                   xend = Date, 
                   y = ymin, yend = ymax), 
               linetype = "solid", 
               color = markers.baker.2022$color, 
               show.legend = FALSE) +
  scale_x_date(limits = c(as.Date("2022-05-01"), 
                          as.Date("2022-10-31")),
               breaks = seq(as.Date("2022-05-01"), 
                            as.Date("2022-10-31"), 
                            by = "1 month"), 
               date_labels = "%b %d") +
  scale_y_continuous(limits = c(0, 80)) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 22),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
naaps.baker.smoke.2022.p

naaps.precip.2022.p <- ggplot(naaps.baker.2022.smoke, 
                              aes(x = date, 
                                  y = total.precip)) +
  geom_line(color = "royalblue4") +
  geom_segment(data = markers.baker.2022, 
               aes(x = Date, 
                   xend = Date, 
                   y = ymin, yend = ymax), 
               linetype = "solid", 
               color = markers.baker.2022$color, 
               show.legend = FALSE) +
  theme_minimal() +
  labs(title = NULL,
       x = "Date",
       y = "Precipitation (mm/day)") +
  scale_x_date(limits = c(as.Date("2022-05-01"), 
                          as.Date("2022-10-31")),
               breaks = seq(as.Date("2022-05-01"), 
                            as.Date("2022-10-31"), 
                            by = "1 month"),
               date_labels = "%b %d") +
  scale_y_continuous(limits = c(0, 80)) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 22),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
naaps.precip.2022.p

### 2022 stacked plots
combined.plot.2022 <- naaps.baker.smoke.2022.p / naaps.precip.2022.p
combined.plot.2022


## 2023
naaps.baker.2023 <- naaps.baker %>%
  filter(format(date, "%Y") == "2023")
naaps.baker.long.2023 <- naaps.baker.2023 %>%
  pivot_longer(cols = c(smoke.tot, 
                        dust.tot, 
                        abf.tot),
               names_to = "Variable", 
               values_to = "Value")

### set 2023 markers
marker.baker.dates.2023 <- as.Date(c("2023-05-06", 
                                     "2022-05-20", 
                                     "2023-06-08",
                                     "2023-06-26", 
                                     "2023-07-10", 
                                     "2023-07-17", 
                                     "2023-07-24",
                                     "2023-08-06",
                                     "2023-08-23"))
markers.baker.2023 <- data.frame(Date = marker.baker.dates.2023,
                                 ymin = 0,
                                 ymax = 5)

### Baker 2023 plot
naaps.baker.2023.smoke <- subset(naaps.baker.long.2023, 
                                 naaps.baker.long.2023$Variable == "smoke.tot")
markers.baker.2023$color <- c("black",
                              "black",
                              "black",
                              "black",
                              "black",
                              "black", 
                              "black", 
                              "black", 
                              "darkorange1")
naaps.baker.smoke.2023.p <- ggplot(naaps.baker.2023.smoke, 
                                 aes(x = date, 
                                     y = Value)) +
  geom_line(color = "deepskyblue2") + 
  theme_minimal() + 
  labs(title = "b.) Mount Baker, 2023", 
       x = "Date", 
       y = expression("Smoke Deposition Flux (mg/m"^2*"/day)")) +
  geom_segment(data = markers.baker.2023, 
               aes(x = Date, 
                   xend = Date, 
                   y = ymin, 
                   yend = ymax), 
               linetype = "solid", 
               color = markers.baker.2023$color, 
               show.legend = FALSE) +
  scale_x_date(limits = c(as.Date("2023-05-01"), 
                          as.Date("2023-10-31")),
               breaks = seq(as.Date("2023-05-01"), 
                            as.Date("2023-10-31"), 
                            by = "1 month"), 
               date_labels = "%b %d") +
  scale_y_continuous(limits = c(0, 80)) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 22),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
naaps.baker.smoke.2023.p

naaps.precip.2023.p <- ggplot(naaps.baker.2023.smoke, 
                              aes(x = date, 
                                  y = total.precip)) +
  geom_line(color = "royalblue4") +
  geom_segment(data = markers.baker.2023, 
               aes(x = Date, 
                   xend = Date, 
                   y = ymin, yend = ymax), 
               linetype = "solid", 
               color = markers.baker.2023$color, 
               show.legend = FALSE) +
  theme_minimal() +
  labs(title = NULL,
       x = "Date",
       y = "Precipitation (mm/day)") +
  scale_x_date(limits = c(as.Date("2023-05-01"), 
                          as.Date("2023-10-31")),
               breaks = seq(as.Date("2023-05-01"), 
                            as.Date("2023-10-31"), 
                            by = "1 month"),
               date_labels = "%b %d") +
  scale_y_continuous(limits = c(0, 80)) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 22),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
naaps.precip.2023.p

### 2023 stacked plots
combined.plot.2023 <- naaps.baker.smoke.2023.p / naaps.precip.2023.p
combined.plot.2023



# NAAPS AOT


## data prep
naaps.aot <- read.csv(file.choose())
naaps.aot.long <- pivot_longer(naaps.aot, 
                               cols = c("smoke_aot", 
                                        "total_aot"), 
                               names_to = "type", 
                               values_to = "aot")
naaps.aot.long$date <- as.Date(naaps.aot.long$date)

### 2022 markers
marker.baker.dates.2022.aot <- as.Date(c("2022-05-16", 
                                         "2022-06-30", 
                                         "2022-07-31", 
                                         "2022-08-05", 
                                         "2022-08-06", 
                                         "2022-08-11", 
                                         "2022-09-15", 
                                         "2022-09-25", 
                                         "2022-10-14"))
markers.baker.2022.aot <- data.frame(Date = marker.baker.dates.2022.aot,
                                     ymin = 0,
                                     ymax = 0.1)
markers.baker.2022.aot$color <- c("black", 
                                  "black", 
                                  "black", 
                                  "black", 
                                  "black",
                                  "black", 
                                  "black", 
                                  "black", 
                                  "black")

### 2023 markers
marker.baker.dates.2023.aot <- as.Date(c("2023-05-06", 
                                         "2022-05-20", 
                                         "2023-06-08",
                                         "2023-06-26", 
                                         "2023-07-10", 
                                         "2023-07-17", 
                                         "2023-07-24",
                                         "2023-08-06",
                                         "2023-08-23"))
markers.baker.2023.aot <- data.frame(Date = marker.baker.dates.2023.aot,
                                     ymin = 0,
                                     ymax = 0.1)
markers.baker.2023.aot$color <- c("black",
                                  "black",
                                  "black",
                                  "black",
                                  "black",
                                  "black", 
                                  "black", 
                                  "black", 
                                  "black")


## plot - AOT - 2022
naaps.aot.2022.shade <- ggplot(naaps.aot.long, 
                               aes(x = date, 
                                   y = aot)) + 
  geom_area(data = subset(naaps.aot.long, 
                          type == "total_aot"),
            fill = "orange", 
            alpha = 0.5) +
  geom_line(data = subset(naaps.aot.long, 
                          type == "total_aot"),
            aes(color = type), 
            linewidth = 0.75) +
  geom_line(data = subset(naaps.aot.long, 
                          type == "smoke_aot"),
            aes(color = type), 
            linewidth = 0.75) +
  geom_segment(data = markers.baker.2022.aot, 
               aes(x = Date, 
                   xend = Date, 
                   y = ymin, 
                   yend = ymax), 
               linetype = "solid", 
               color = markers.baker.2022.aot$color, 
               show.legend = FALSE) +
  scale_color_manual(values = c("total_aot" = "orange", 
                                "smoke_aot" = "gray35"),
                     labels = c("total_aot" = "Total", 
                                "smoke_aot" = "Smoke")) +
  theme_minimal() + 
  labs(title = "a.) Mount Baker, 2022", 
       x = "Date", 
       y = "Aerosol Optical Thickness",
       color = "AOT") +
  scale_x_date(limits = c(as.Date("2022-05-01"), 
                          as.Date("2022-10-31")),
               breaks = seq(as.Date("2022-05-01"), 
                            as.Date("2022-10-31"), 
                            by = "1 month"), 
               date_labels = "%b %d") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 22),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))
naaps.aot.2022.shade


## plot - AOT - 2023
naaps.aot.2023.shade <- ggplot(naaps.aot.long, 
                               aes(x = date, 
                                   y = aot)) + 
  geom_area(data = subset(naaps.aot.long, 
                          type == "total_aot"),
            fill = "orange", 
            alpha = 0.5) +
  geom_line(data = subset(naaps.aot.long, 
                          type == "total_aot"),
            aes(color = type), 
            linewidth = 0.75) +
  geom_line(data = subset(naaps.aot.long, 
                          type == "smoke_aot"),
            aes(color = type), 
            linewidth = 0.75) +
  geom_segment(data = markers.baker.2023.aot, 
               aes(x = Date, 
                   xend = Date, 
                   y = ymin, 
                   yend = ymax), 
               linetype = "solid", 
               color = markers.baker.2023.aot$color, 
               show.legend = FALSE) +
  scale_color_manual(values = c("total_aot" = "orange", 
                                "smoke_aot" = "gray35"),
                     labels = c("total_aot" = "Total", 
                                "smoke_aot" = "Smoke")) +
  theme_minimal() + 
  labs(title = "b.) Mount Baker, 2023", 
       x = "Date", 
       y = "Aerosol Optical Thickness",
       color = "AOT") +
  scale_x_date(limits = c(as.Date("2023-05-01"), 
                          as.Date("2023-10-31")),
               breaks = seq(as.Date("2023-05-01"), 
                            as.Date("2023-10-31"), 
                            by = "1 month"), 
               date_labels = "%b %d") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 22),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))
naaps.aot.2023.shade