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


# read in file
DBCdata <- read.csv(file.choose())


# convert date
DBCdata$Sample.Collection.Date <- 
  as.Date(DBCdata$Sample.Collection.Date)


# subset data
sub.2022 <- subset(DBCdata, 
                   DBCdata$Year
                   == "2022")
sub.2023 <- subset(DBCdata, 
                   DBCdata$Year 
                   == "2023")
sub.2022.snow.only <- subset(sub.2022, sub.2022$Sample.Type == "snow")
sub.all.snow <- subset(DBCdata, DBCdata$Sample.Type == "snow")
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
DOC.nan <- na.omit(DOC)
mean(DOC.nan)
sd(DOC.nan)

DOC.snow <- na.omit(sub.all.snow$X.DOC..mg.L)
mean(DOC.snow)
sd(DOC.snow)

DOC.non.snow <- na.omit(sub.not.snow$X.DOC..mg.L)
mean(DOC.non.snow)
sd(DOC.non.snow)

DBC.snow <- na.omit(sub.all.snow$Dittmar..DBC..µg.C.L)
mean(DBC.snow)
sd(DBC.snow)

DBC.non.snow <- na.omit(sub.not.snow$Dittmar..DBC..µg.C.L)
mean(DBC.non.snow)
sd(DBC.non.snow)

DOC.DBC.nan <- na.omit(DBCdata$DBC.DOC.......Dittmar..2008.)
mean(DOC.DBC.nan)
sd(DOC.DBC.nan)

Ditt.snow.2022.nan <- na.omit(sub.2022.snow.only$Dittmar..DBC..µg.C.L)
mean(Ditt.snow.2022.nan)
sd(Ditt.snow.2022.nan)

Ditt.2023.nan <- na.omit(sub.2023$Dittmar..DBC..µg.C.L)
mean(Ditt.2023.nan)
sd(Ditt.2023.nan)

early.2022 <- subset(sub.2022.snow.only, 
                     sub.2022.snow.only$
                       Early.vs..Late.Season..March.July.vs..August.October. 
                     == "Early")
early.2022.Ditt.nan <- na.omit(early.2022$Dittmar..DBC..µg.C.L)
mean(early.2022.Ditt.nan)
sd(early.2022.Ditt.nan)

late.2022 <- subset(sub.2022.snow.only, sub.2022.snow.only$
                      Early.vs..Late.Season..March.July.vs..August.October. 
                    == "Late")
late.2022.Ditt.nan <- na.omit(late.2022$Dittmar..DBC..µg.C.L)
mean(late.2022.Ditt.nan)
sd(late.2022.Ditt.nan)

early.2023 <- subset(sub.2023, 
                     sub.2023$
                       Early.vs..Late.Season..March.July.vs..August.October. 
                     == "Early")
early.2023.Ditt.nan <- na.omit(early.2023$Dittmar..DBC..µg.C.L)
mean(early.2023.Ditt.nan)
sd(early.2023.Ditt.nan)

late.2023 <- subset(sub.2023, sub.2023$
                      Early.vs..Late.Season..March.July.vs..August.October. 
                    == "Late")
late.2023.Ditt.nan <- na.omit(late.2023$Dittmar..DBC..µg.C.L)
mean(late.2023.Ditt.nan)
sd(late.2023.Ditt.nan)

ratio.nan <- na.omit(ratio)
summary(ratio.nan)
mean(ratio.nan)
sd(ratio.nan)

ratio.snow.nan <- na.omit(sub.all.snow$Ratio.B3.B4.B5.B6)
summary(ratio.snow.nan)
mean(ratio.snow.nan)
sd(ratio.snow.nan)

ratio.2022.snow.nan <- na.omit(ratio.2022.snow)
mean(ratio.2022.snow.nan)
sd(ratio.2022.snow.nan)

ratio.2023.nan <- na.omit(ratio.2023)
mean(ratio.2023.nan)
sd(ratio.2023.nan)

B6CA.snow.nan <- na.omit(sub.all.snow$B6CA)
print(summary(B6CA.snow.nan), digits = 6)
sd(B6CA.snow.nan)

B6CA.not.snow.nan <- na.omit(sub.not.snow$B6CA)
print(summary(B6CA.not.snow.nan), digits = 6)
sd(B6CA.not.snow.nan)

B5CA.snow.nan <- na.omit(sub.all.snow$B5CA)
print(summary(B5CA.snow.nan), digits = 6)
sd(B5CA.snow.nan)

B5CA.not.snow.nan <- na.omit(sub.not.snow$B5CA)
print(summary(B5CA.not.snow.nan), digits = 6)
sd(B5CA.not.snow.nan)

B4CA.snow.nan <- na.omit(sub.all.snow$B4CA)
print(summary(B4CA.snow.nan), digits = 6)
sd(B4CA.snow.nan)

B4CA.not.snow.nan <- na.omit(sub.not.snow$B4CA)
print(summary(B4CA.not.snow.nan), digits = 6)
sd(B4CA.not.snow.nan)

B3CA.snow.nan <- na.omit(sub.all.snow$B3CA)
print(summary(B3CA.snow.nan), digits = 6)
sd(B3CA.snow.nan)

B3CA.not.snow.nan <- na.omit(sub.not.snow$B3CA)
print(summary(B3CA.not.snow.nan), digits = 6)
sd(B3CA.not.snow.nan)

B6CA.nan.2022 <- na.omit(sub.2022.snow.only$B6CA)
print(summary(B6CA.nan.2022), digits = 6)
sd(B6CA.nan.2022)

B5CA.nan.2022 <- na.omit(sub.2022.snow.only$B5CA)
print(summary(B5CA.nan.2022), digits = 6)
sd(B5CA.nan.2022)

B4CA.nan.2022 <- na.omit(sub.2022.snow.only$B4CA)
print(summary(B4CA.nan.2022), digits = 6)
sd(B4CA.nan.2022)

B3CA.nan.2022 <- na.omit(sub.2022.snow.only$B3CA)
print(summary(B3CA.nan.2022), digits = 6)
sd(B3CA.nan.2022)

B6CA.nan.2023 <- na.omit(sub.2023$B6CA)
print(summary(B6CA.nan.2023), digits = 6)
sd(B6CA.nan.2023)

B5CA.nan.2023 <- na.omit(sub.2023$B5CA)
print(summary(B5CA.nan.2023), digits = 6)
sd(B5CA.nan.2023)

B4CA.nan.2023 <- na.omit(sub.2023$B4CA)
print(summary(B4CA.nan.2023), digits = 6)
sd(B4CA.nan.2023)

B3CA.nan.2023 <- na.omit(sub.2023$B3CA)
print(summary(B3CA.nan.2023), digits = 6)
sd(B3CA.nan.2023)

B6CA.con.nan <- na.omit(DBCdata$X.B6CA..uM)
mean(B6CA.con.nan)
sd(B6CA.con.nan)

B5CA.con.nan <- na.omit(DBCdata$X.B5CA..uM)
mean(B5CA.con.nan)
sd(B5CA.con.nan)

B4CA.con.nan <- na.omit(DBCdata$X.B4CA..uM)
mean(B4CA.con.nan)
sd(B4CA.con.nan)

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
shapiro.test(sub.2022.snow.only$Dittmar..DBC..µg.C.L)

shapiro.test(sub.2023$Dittmar..DBC..µg.C.L)

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

## boxplot
box.season.1 <- ggplot(sub.2022.snow.only, 
                       aes(x = sub.2022.snow.only$
                             Early.vs..Late.Season..March.July.vs..August.October., 
                           y = sub.2022.snow.only$Dittmar..DBC..µg.C.L, 
                           color = sub.2022.snow.only$
                             Early.vs..Late.Season..March.July.vs..August.October.)) + 
  geom_boxplot() + theme_minimal() + xlab("Collection Season") + ylab("DBC (μg/L)") +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  labs(color = "Early vs. Late Season", 
       title = 
         "a.) 2022 DBC samples") + 
  stat_summary(fun = "mean", geom = "point", shape = 4, size = 3, color = "black") +
  theme(plot.title = element_text(size = 12))
box.season.1

box.season.3 <- ggplot(sub.2023, 
                       aes(x = sub.2023$
                             Early.vs..Late.Season..March.July.vs..August.October., 
                           y = sub.2023$Dittmar..DBC..µg.C.L, 
                           color = sub.2023$
                             Early.vs..Late.Season..March.July.vs..August.October.)) + 
  geom_boxplot() + theme_minimal() + xlab("Collection Season") + ylab("DBC (μg/L)") +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  labs(color = "Early vs. Late Season", 
       title = 
         "b.) 2023 DBC samples") + 
  stat_summary(fun = "mean", geom = "point", shape = 4, size = 3, color = "black") +
  theme(plot.title = element_text(size = 12))
box.season.3


# linear regression - DBC ~ DOC

## default linear regression
###linearity assumption
coefficient <- cor.test(DBC.Dittmar.mg, DOC)
coefficient$estimate
### normality
shapiro.test(DBCdata$X.DOC..mg.L)
shapiro.test(DBCdata$Dittmar..DBC..µg.C.L)
### outliers
p.outliers.1 <- DBCdata %>%
  ggplot(aes(DBC.Dittmar.mg, DBC.Dittmar.mg)) +
  geom_boxplot()
p.outliers.1
p.outliers.DOC <- DBCdata %>%
  ggplot(aes(DOC, DOC, na.rm = TRUE)) +
  geom_boxplot()
p.outliers.DOC
### linear regression
lm.1 <- lm(DBC.Dittmar.mg ~ DOC, DBCdata)
summary(lm.1)
qqPlot(lm.1)
### extract and check residuals
residuals <- residuals(lm.1)
lm.1.hist <- hist(residuals, breaks = 20, main = "Histogram of Residuals", 
                  xlab = "Residuals")
lm.1.qqnorm <- qqnorm(residuals)
lm.1.qqline <- qqline(residuals, col = "red")
shapiro.test(residuals)
print(shapiro_test)
### plot lm.1
p.lm.1 <- ggscatter(DBCdata, x = 'X.DOC..mg.L', y = 'Dittmar..DBC..mg.C.L', 
                    shape = 1, add = "reg.line", conf.int = TRUE, 
                    cor.method = "pearson") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., 
                                          sep = "~~~")), label.x = 0.1, 
                        label.y = 0.05, size = 10) +
  stat_cor(aes(label = paste("..corr.label..", "..p.label..", sep = "~~~")),
           method = "pearson", label.x = 0.1, label.y = 0.045, size = 10) +
  theme_minimal() +
  labs(x = "DOC (mg/L)", y = "DBC (mg/L)") + 
  theme(plot.title = element_text(size=10)) + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 22))
p.lm.1

## log-transform the variables
DBCdata$log.DBC <- log(DBCdata$Dittmar..DBC..mg.C.L)
DBCdata$log.DOC <- log(DBCdata$X.DOC..mg.L)
### run the linear regression with transformed variables
lm.2 <- lm(log.DBC ~ log.DOC, data = DBCdata)
summary(lm.2)
### extract and check residuals
residuals.lm.2 <- residuals(lm.2)
lm.2.hist <- hist(residuals.lm.2, breaks = 20, 
                  main = "Histogram of Log-Transformed Residuals", 
                  xlab = "Residuals")
lm.2.qqnorm <- qqnorm(residuals.lm.2)
lm.2.qqline <- qqline(residuals.lm.2, col = "red")
shapiro.test(residuals.lm.2)
### plot lm.2
p.lm.2 <- ggscatter(DBCdata, x = 'log.DOC', y = 'log.DBC', 
                    shape = 1, add = "reg.line", conf.int = TRUE, 
                    cor.method = "pearson") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., 
                                          sep = "~~~")), label.x = -2, 
                        label.y = -1, size = 10) +
  stat_cor(aes(label = paste("..corr.label..", "..p.label..", sep = "~~~")),
    method = "pearson", label.x = -2, label.y = -2, size = 10) +
  theme_minimal() +
  labs(x = "DOC (mg/L)", y = "DBC (mg/L)") + 
  theme(plot.title = element_text(size=10)) + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 22))
p.lm.2
#### snow only DBC~DOC regression
p.lm.2 <- ggscatter(sub.all.snow, x = 'X.DOC..mg.L', 
                    y = 'Dittmar..DBC..mg.C.L', shape = 1,
                    add = "reg.line", conf.int = TRUE, 
                    cor.method = "pearson") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., 
                                          sep = "~~~")),
                        label.x = 0.1, label.y = 0.05, size = 10) +
  stat_cor(aes(label = paste("..corr.label..", "..p.label..", sep = "~~~")),
    method = "pearson", label.x = 0.1, label.y = 0.045, size = 10) +
  theme_minimal() + labs(x = "DOC (mg/L)", y = "DBC (mg/L)") + 
  theme(axis.title = element_text(size = 20), 
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 22))
p.lm.2
#### regression w/ not-snow samples
p.lm.3 <- ggscatter(sub.not.snow, x = 'X.DOC..mg.L', 
                    y = 'Dittmar..DBC..mg.C.L', 
                    shape = 1, add = "reg.line", conf.int = TRUE, 
                    cor.method = "pearson") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., 
                                          sep = "~~~")),
                        label.x = 0.1,label.y = 0.05) +
  stat_cor(aes(label = paste("..corr.label..", "..p.label..", sep = "~~~")),
    method = "pearson", label.x = 0.1, label.y = 0.045) +
  theme_minimal() + labs(x = "DOC (mg/L)", y = "DBC (mg/L)",
                         title = "b.) Non-snow samples") + 
  theme(plot.title = element_text(size=10))
p.lm.3


## linear regression (robust)
rlm <- rlm(Dittmar..DBC..mg.C.L ~ X.DOC..mg.L, data = DBCdata)
summary(rlm)
### rlm stats
DBCdata.rlm <- DBCdata %>%
  filter(!is.na(DOC) & !is.na(Dittmar..DBC..mg.C.L))
cor.coeff <- cor(x = DBCdata.rlm$X.DOC..mg.L, 
                 DBCdata.rlm$Dittmar..DBC..mg.C.L, method = "spearman")
cor.label <- paste0("Spearman's ρ = ", sprintf("%.2f", cor.coeff))
coefficients <- coef(rlm)
intercept <- coefficients[2]
slope <- coefficients[2]
equation <- paste0("y = ", round(slope, 2), "x + ", round(intercept, 2))
### linear trend + confidence interval plot (robust)
p.rlm.1 <- ggplot(DBCdata, aes(x=X.DOC..mg.L, y=Dittmar..DBC..mg.C.L)) +
  geom_point(shape = 1) +
  geom_smooth(method='rlm', color = "black") + 
  annotate("text", x = Inf, y = Inf, label = paste0(cor.label), 
           hjust = 5, vjust = 6, size = 6.5) + theme_minimal() + 
  labs(x = "DOC (mg/L)", y = "DBC (mg/L)") + 
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14))
p.rlm.1

## square-root transform data
DBCdata$sqrt.DBC <- sqrt(DBCdata$Dittmar..DBC..mg.C.L)
DBCdata$sqrt.DOC <- sqrt(DBCdata$X.DOC..mg.L)
### linear regression with square root-transformed data
lm.3 <- lm(sqrt.DBC ~ sqrt.DOC, data = DBCdata)
summary(lm.3)
### extract and check residuals
residuals.lm.3 <- residuals(lm.3)
lm.3.hist <- hist(residuals.lm.3, breaks = 20, 
                  main = "Histogram of SQRT-Transformed Residuals", 
                  xlab = "Residuals")
lm.3.qqnorm <- qqnorm(residuals.lm.3)
lm.3.qqline <- qqline(residuals.lm.3, col = "red")
shapiro.test(residuals.lm.2)
### plot lm.3
p.lm.3 <- ggscatter(DBCdata, x = 'sqrt.DOC', y = 'sqrt.DBC', 
                    add = "reg.line", conf.int = TRUE, 
                    cor.method = "pearson",
                    shape = "Sample.Type") +
  stat_regline_equation(aes(label = ..eq.label..), 
                        label.x = 0.3, label.y = 0.275, size = 6) +
  stat_regline_equation(aes(label = ..rr.label..), 
                        label.x = 0.3, label.y = 0.25, size = 6) + 
  theme_minimal() +
  labs(x = "Square Root(DOC (mg/L))", y = "Square Root(DBC (mg/L))", 
       shape = "Sample Type") + 
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 14)) + 
  scale_shape_manual(values = c(1, 2, 0, 3),
                     labels = c("River", "Snow", "Stream", 
                                "Supraglacial Melt"))
p.lm.3


# scatter plot - DOC and DBC over time
p.DBC.scatter.2022.1 <- ggplot(sub.2022, aes(x=sub.2022$Sample.Collection.Date, 
                                        y=sub.2022$Dittmar..DBC..µg.C.L, 
                                        color = sub.2022$Sample.Type)) +
  geom_point(na.rm=TRUE, shape = 19) + xlab("Collection Date") + ylab("DBC (μg/L)") +
  labs(color = "Sample Type", 
       title = "a.) 2022 DBC samples") +
  theme_minimal() + theme(panel.background = element_rect(fill = "white"), 
                          plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size=10)) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#CC79A7")) +
  scale_x_date(labels = date_format("%m-%Y"))
p.DBC.scatter.2022.1 
p.DBC.scatter.2022.2 <- ggplot(sub.2022, aes(x=sub.2022$Sample.Collection.Date, 
                                             y=sub.2022$Stubbins..DBC..µg.C.L, 
                                             color = sub.2022$Sample.Type)) +
  geom_point(na.rm=TRUE, shape = 19) + xlab("Collection Date") + ylab("DBC (μg/L)") +
  labs(color = "Sample Type", 
       title = "a.) 2022 DBC samples") +
  theme_minimal() + theme(panel.background = element_rect(fill = "white"), 
                          plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size=10)) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#CC79A7")) +
  scale_x_date(labels = date_format("%m-%Y"))
p.DBC.scatter.2022.2
p.DBC.scatter.2023.1 <- ggplot(sub.2023, aes(x=sub.2023$Sample.Collection.Date, 
                                             y=sub.2023$Dittmar..DBC..µg.C.L, 
                                             color = sub.2023$Sample.Type)) +
  geom_point(na.rm=TRUE, shape = 19) + xlab("Collection Date") + ylab("DBC (μg/L)") +
  labs(color = "Sample Type", 
       title = "b.) 2023 DBC samples") +
  theme_minimal() + theme(panel.background = element_rect(fill = "white"), 
                          plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size=10)) +
  scale_color_manual(values=c("#56B4E9")) +
  scale_x_date(labels = date_format("%m-%Y"))
p.DBC.scatter.2023.1 
p.DBC.scatter.2023.2 <- ggplot(sub.2023, aes(x=sub.2023$Sample.Collection.Date, 
                                             y=sub.2023$Stubbins..DBC..µg.C.L, 
                                             color = sub.2023$Sample.Type)) +
  geom_point(na.rm=TRUE, shape = 19) + xlab("Collection Date") + ylab("DBC (μg/L)") +
  labs(color = "Sample Type", 
       title = "b.) 2023 DBC samples") +
  theme_minimal() + theme(panel.background = element_rect(fill = "white"), 
                          plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size=10)) +
  scale_color_manual(values=c("#56B4E9")) +
  scale_x_date(labels = date_format("%m-%Y"))
p.DBC.scatter.2023.2


# barplot - BPCA relative proportions (B3+B4:B5+B6)

## reformat data so that each BPCA % is its own row (2022)
data.long.2022 <- sub.2022.snow.only %>%
  pivot_longer(cols = starts_with("B"), 
               names_to = "BPCA", values_to = "Percentage")
data.long.2023 <- sub.2023 %>%
  pivot_longer(cols = starts_with("B"), 
               names_to = "BPCA", values_to = "Percentage")

## plot (2022)
order.2022 <- c("16-May-2022", "Jun-2022", "Jul-2022", "15-Jul-2022", 
                "05-Aug-2022", "06-Aug-2022", "11-Aug-2022", "15-Aug-2022", 
                "15-Sept-2022 (PR)", "15-Sept-2022 (SH)", "25-Sept-2022", "14-Oct-2022")
p.BPCA.bar.2022 <- ggplot(data.long.2022, aes(x = Axis.Label, 
                                         y = Percentage, fill = BPCA)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = 
                      c("B3CA" = "#CC79A7", "B4CA" = "#009E73", 
                        "B5CA" = "#56B4E9", "B6CA" = "#E69F00")) +
  labs(y = "Percentage of Total DBC", x = "Collection Date", fill = "BPCA",
       title = "a.) 2022") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 18)) +
  scale_x_discrete(limits = order.2022)
p.BPCA.bar.2022
order.2023 <- c("02-Mar-2023", "06-May-2023", "20-May-2023 (AP)", 
                "20-May-2023 (BB)", "08-Jun-2023", "26-Jun-2023", 
                "10-Jul-2023", "17-Jul-2023", "24-Jul-2023", 
                "06-Aug-2023", "23-Aug-2023")
p.BPCA.bar.2023 <- ggplot(data.long.2023, aes(x = Axis.Label, 
                                         y = Percentage, fill = BPCA)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = 
                      c("B3CA" = "#CC79A7", "B4CA" = "#009E73", 
                        "B5CA" = "#56B4E9", "B6CA" = "#E69F00")) +
  labs(y = "Percentage of Total DBC", x = "Collection Date", fill = "BPCA",
       title = "b.) 2023") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, size = 14),
                          axis.text.y = element_text(size = 14),
                          axis.title.x = element_text(size = 16),
                          axis.title.y = element_text(size = 16),
                          legend.title = element_text(size = 16),
                          legend.text = element_text(size = 14),
                          plot.title = element_text(size = 18)) +
  scale_x_discrete(limits = order.2023)
p.BPCA.bar.2023

## scatter plot by year
p.ratio.2022 <- ggplot(sub.2022.snow.only, 
                       aes(x=sub.2022.snow.only$Sample.Collection.Date, 
                                             y=sub.2022.snow.only$B3.B4.B5.B6, 
                                             color = sub.2022.snow.only$Sample.Type)) +
  geom_point(na.rm=TRUE, shape = 19) + xlab("Collection Date") + 
  ylab("(B3CA + B4CA) / (B5CA + B6CA)") +
  labs(color = "Sample Type") +
  theme_minimal() + theme(panel.background = element_rect(fill = "white"), 
                          plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size=10)) +
  scale_color_manual(values=c("#56B4E9")) +
  scale_x_date(labels = date_format("%m-%Y"))
p.ratio.2022
p.ratio.2023 <- ggplot(sub.2023, aes(x=sub.2023$Sample.Collection.Date, 
                                         y=sub.2023$B3.B4.B5.B6, 
                                         color = sub.2023$Sample.Type)) +
  geom_point(na.rm=TRUE, shape = 19) + xlab("Collection Date") + 
  ylab("(B3CA + B4CA) / (B5CA + B6CA)") +
  labs(color = "Sample Type") +
  theme_minimal() + theme(panel.background = element_rect(fill = "white"), 
                          plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size=10)) +
  scale_color_manual(values=c("#56B4E9")) +
  scale_x_date(labels = date_format("%m-%Y"))
p.ratio.2023

## boxplot comparing 2022 vs. 2023
snow.only <- subset(DBCdata, Sample.Type == "snow")
p.ratio.yr <- ggplot(snow.only, 
                     aes(x = as.factor(snow.only$Year), y = snow.only$B3.B4.B5.B6,
                         color = as.factor(snow.only$Year))) + 
  geom_boxplot() + scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  stat_summary(fun = "mean", geom = "point", shape = 4, size = 3, color = "black") +
  theme_minimal() + xlab("Collection Year") + 
  ylab("(B3CA + B4CA) / (B5CA + B6CA)") + labs(color = "Year")
p.ratio.yr

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
             sub.2022.snow.only$Early.vs..Late.Season..March.July.vs..August.October., 
           sub.2022.snow.only)
kruskal.test(ratio.2022.snow.nan ~ 
               sub.2022.snow.only$Early.vs..Late.Season..March.July.vs..August.October., 
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


# boxplot BPCAs
bpca.comp.2022 <- ggplot(data_long, 
                         aes(x = BPCA, y = Percentage, color = BPCA)) +
  geom_boxplot() + scale_color_manual(values = 
                                       c("B3CA" = "#CC79A7", "B4CA" = "#009E73", 
                                         "B5CA" = "#56B4E9", "B6CA" = "#E69F00")) +
  theme_minimal() + 
  stat_summary(fun = "mean", geom = "point", shape = 4, size = 2, color = "black") +
  ylim(0, 100) + theme(legend.position="none") + 
  labs(title = "2022 BPCA Relative Percentages") + 
  theme(plot.title = element_text(size = 12))
bpca.comp.2022

bpca.comp.2023 <- ggplot(data_long.2023, 
                         aes(x = BPCA, y = Percentage, color = BPCA)) +
  geom_boxplot() + scale_color_manual(values = 
                                        c("B3CA" = "#CC79A7", "B4CA" = "#009E73", 
                                          "B5CA" = "#56B4E9", "B6CA" = "#E69F00")) +
  theme_minimal() + 
  stat_summary(fun = "mean", geom = "point", shape = 4, size = 3, color = "black") +
  ylim(0, 100) + theme(legend.position="none") + 
  labs(title = "2023 BPCA Relative Percentages") + 
  theme(plot.title = element_text(size = 12))
bpca.comp.2023


# multi-variate analysis - PCA
dbc.nan <- na.omit(DBCdata)
bpca.per <- DBCdata[, c("Sample.ID", "B6CA", "B5CA", "B4CA", "B3CA")]
bpca.per.nan <- na.omit(bpca.per)
bpca_columns <- c("B6CA", "B5CA", "B4CA", "B3CA")
bpca.per.nan.for.pca <- bpca.per.nan[, bpca_columns]

## correlation with [BPCAs]
p.bpca.corr <- ggpairs(bpcas, aes(color = as.factor(Y))) + 
  theme_minimal()
p.bpca.corr

## correlation with BPCA %s
p.bpca.per.corr <- ggpairs(bpca.per, aes(color = as.factor(Y))) + 
  theme_minimal()
p.bpca.per.corr

## prep data for PCA
pca.data <- sub.all.snow[, c("B6CA", "B5CA", "B4CA", "B3CA")]
pca.data$B3CA[is.na(pca.data$B3CA)] <- 0
### run PCA
pca.result <- prcomp(pca.data, scale. = FALSE)
summary(pca.result)
### extract loadings from PCA result
loadings <- pca.result$rotation[, 1:2]  # Extract loadings for PC1 and PC2
### create dataframe for plotting loadings
loadings.df <- data.frame(
  BPCA = c("B6CA", "B5CA", "B4CA", "B3CA"),
  PC1 = loadings[, 1],
  PC2 = loadings[, 2],
  vjust = c(2, -1, 2, 2),
  hjust = c(-0.2, 1, 0.5, -0.2))
### plot loadings
p.loadings <- ggplot(loadings.df, aes(x = PC1, y = PC2, label = BPCA)) +
  geom_point(size = 5, aes(color = BPCA)) + 
  geom_label(aes(label = BPCA, color = BPCA, vjust = vjust, hjust = hjust), 
             label.size = 0.5, 
             label.padding = unit(0.2, "lines")) + 
  scale_color_manual(values = 
                       c("B6CA" = "#E69F00", "B5CA" = "#56B4E9", 
                         "B4CA" = "#009E73", "B3CA" = "#CC79A7")) +
  labs(title = "PCA Loadings Plot", x = "PC1 (96.97%)", y = "PC2 (2.79%)") +
  theme_minimal() + theme(legend.position = "none")
p.loadings
### add PCA scores to original dataframe
pca.data <- sub.all.snow[,
                         c("Sample.ID", "Sample.Location", 
                           "Sample.Type", "Year",
                       "Early.vs..Late.Season..March.July.vs..August.October.",
                        "B6CA", "B5CA", "B4CA", "B3CA")]
pca.data$B3CA[is.na(pca.data$B3CA)] <- 0
#pca.data.nan <- na.omit(pca.data)
#pca.data.nan$PC1 <- pca.result$x[, 1]  # Score for PC1
#pca.data.nan$PC2 <- pca.result$x[, 2]  # Score for PC2
pca.data$PC1 <-pca.result$x[, 1]
pca.data$PC2 <- pca.result$x[, 2]

# updates

# Determine a scaling factor for the loadings
scaling.factor <- max(abs(pca.data$PC1), 
                      abs(pca.data$PC2)) / max(abs(loadings))

# Create a data frame for the scaled loadings
loadings.df <- data.frame(
  BPCA = c("B6CA", "B5CA", "B4CA", "B3CA"),
  PC1 = loadings[, 1] * scaling.factor,
  PC2 = loadings[, 2] * scaling.factor,
  vjust = c(0, -1, 0, 0.25),
  hjust = c(-0.25, 0, 1.25, -0.5))


## plot PCA scores by sample location and early/late season
p.scores.location <- 
  ggplot(pca.data, 
         aes(x = PC1, y = PC2, color = Sample.Location,
             shape = Early.vs..Late.Season..March.July.vs..August.October.)) +
  geom_point(size = 3) +
  labs(x = "PC1 (95.89%)", y = "PC2 (3.86%)") +
  theme_minimal() + 
  scale_color_manual(values = 
                      c("Artist Point" = 
                          "#CC6677", "Bagley Basin" = "#332288", 
                        "Camp Kiser" = "#DDCC77", "Easton Glacier" = "#117733",
                        "Muir Snowfield" = "#88CCEE", "Ptarmigan Ridge" =
                          "#882255", "Rainbow Glacier" = "#44AA99", 
                        "Sholes Glacier" = "#999933")) + 
  labs(color = "Sampling Location", shape = "Early vs. Late Season") +
  geom_segment(data = loadings.df, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.3, "cm")), color = "black",
               inherit.aes = FALSE) +
  geom_text(data = loadings.df, aes(x = PC1, y = PC2, label = BPCA, vjust = vjust, hjust = hjust), 
            color = "black", size = 3.5, inherit.aes = FALSE)

p.scores.location



## plot PCA scores by Sample ID
pca.data.nan.2022 <- subset(pca.data.nan, pca.data.nan$Year == "2022")
p.scores.2022 <- ggplot(pca.data.nan.2022, 
                          aes(x = PC1, 
                              y = PC2,
                              color = Sample.ID)) +
  geom_point(size = 3) +
  labs(title = "PCA Scores by Sample ID (2022)", 
       x = "PC1 (96.97%)", y = "PC2 (2.79%)") + labs(color = "Sample ID") +
  theme_minimal()
p.scores.2022


pca.data.nan.2023 <- subset(pca.data.nan, pca.data.nan$Year == "2023")
p.scores.2023 <- ggplot(pca.data.nan.2023, 
                        aes(x = PC1, 
                            y = PC2,
                            color = Sample.ID)) +
  geom_point(size = 3) +
  labs(title = "PCA Scores by Sample ID (2023)", 
       x = "PC1 (96.97%)", y = "PC2 (2.79%)") + labs(color = "Sample ID") +
  theme_minimal()
p.scores.2023


# SNICAR
library(reshape2)
SNICAR.2022 <- read.csv(file.choose())
SNICARmelt.2022 <- melt(SNICAR.2022, id.vars="Wavelength")
SNICAR.2023 <- read.csv(file.choose())
SNICARmelt.2023 <- melt(SNICAR.2023, id.vars="Wavelength")

## plot 2022
p.SNICAR.2022 <- ggplot(SNICARmelt.2022, aes(Wavelength,value, 
                                             col=variable)) + 
  geom_line() + scale_x_continuous(limits = c(300, 1500), 
                                   expand = c(0, 0)) +  
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Wavelength (nm)") + ylab("Albedo") + 
  labs(color = "Sample ID", title = "a.) 2022") +
  scale_color_manual(labels = c("Clean Snow", "16-May-2022", "Jun-2022", 
                                "Jul-2022", "15-Jul-2022", "05-Aug-2022", 
                                "06-Aug-2022", "11-Aug-2022", "15-Aug-2022", 
                                "15-Sept-2022 (PR)", "15-Sept-2022 (SH)", 
                                "25-Sept-2022", "14-Oct-2022"),
                     values = c("#CC6677", "#332288", "#DDCC77", "#117733", 
                                "#88CCEE", "#882255", "#44AA99", 
                                "#999933", "#AA4499", "#EE8866", "#DDDDDD", 
                                "#CC3311", "#222255")) +
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
                        aes(Wavelength,value, col=variable)) + 
  geom_line() + scale_x_continuous(limits = c(300, 1500), 
                                   expand = c(0, 0)) +  
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  xlab("Wavelength (nm)") + ylab("Albedo") + 
  labs(color = "Sample ID", title = "b.) 2023") +
  scale_color_manual(labels = c("Clean Snow", "2-Mar-2023", "6-May-2023", 
                                "20-May-2023 (AP)", 
                                "20-May-2023 (BB)", "8-Jun-2023", 
                                "26-Jun-2023", 
                                "10-Jul-2023", "17-Jul-2023", "24-Jul-2023", 
                                "6-Aug-2023", "23-Aug-2023"),
                     values = c("#CC6677", "#332288", "#DDCC77", 
                                "#117733", "#88CCEE", "#882255", "#44AA99", 
                                "#999933", "#AA4499", "#EE8866", "#CC3311", 
                                "#222255")) + 
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 22),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(margin = margin(t = 8)),
        axis.text.y = element_text(margin = margin(r = 8)))
p.SNICAR.2023


# snowmelt and hourly IRF
hourly.IRF <- read.csv(file.choose())
hourly.melt <- read.csv(file.choose())

IRF.long <- hourly.IRF %>%
  gather(key = "Study", value = "RadiativeForcing", -Hour.of.Day)
IRF.long$Study <- 
  factor(IRF.long$Study, 
         levels = 
           c("X2022.8.5.SHOLES", "X2022.9.15.SHOLES", 
             "X2022.9.25.SHOLES", "X2022.10.14.SHOLES"))

IRF.hr.plot <- ggplot(IRF.long, aes(x = Hour.of.Day, 
                                    y = RadiativeForcing, 
                                    color = Study, group = Study)) +
  geom_line() +
  labs(x = "Hour of the Day", 
       y = expression("Radiative Forcing (W/m"^2*")"),
       color = "Study Date") +
  theme_minimal() + scale_x_continuous(breaks = seq(6, 18, by = 2)) +
  scale_color_manual(values = c("X2022.8.5.SHOLES" = "#E69F00", 
  "X2022.9.15.SHOLES" = "#56B4E9", "X2022.9.25.SHOLES" = "#009E73",
  "X2022.10.14.SHOLES" = "#CC79A7"),
  labels = c("X2022.8.5.SHOLES" = "05 Aug. 2022", 
             "X2022.9.15.SHOLES" = "15 Sept. 2022",
             "X2022.9.25.SHOLES" = "25 Sept. 2022",
             "X2022.10.14.SHOLES" = "14 Oct. 2022")) + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))
IRF.hr.plot

melt.long <- hourly.melt %>%
  gather(key = "Study", value = "Snowmelt", -Hour.of.Day)
melt.hr.plot <- ggplot(melt.long, aes(x = Hour.of.Day, 
                                    y = Snowmelt, 
                                    color = Study, group = Study)) +
  geom_line() +
  labs(x = "Hour of the Day", 
       y = "Snow Water Equivalent (mm)", 
       title = "b.) Hourly Snow Water Equivalent") +
  theme_minimal()
melt.hr.plot


# naaps - Baker
naaps.baker <- read.csv(file.choose())
naaps.baker$Date <- 
  as.Date(naaps.baker$Date)

## 2022
naaps.baker.2022 <- naaps.baker %>%
  filter(format(Date, "%Y") == "2022")
naaps.baker.long.2022 <- naaps.baker.2022 %>%
  pivot_longer(cols = c(smoke_totsink, dust_totsink, abf_totsink),
               names_to = "Variable", values_to = "Value")
### marker info
marker.baker.dates.2022 <- as.Date(c("2022-05-16", "2022-06-30", "2022-07-31", 
                               "2022-08-05", "2022-08-06", "2022-08-11", 
                               "2022-09-15", "2022-09-25", "2022-10-14"))
markers.baker.2022 <- data.frame(
  Date = marker.baker.dates.2022,
  ymin = 0,   # Lower limit of the segment
  ymax = 5     # Upper limit of the segment
)
### plot
naaps.baker.2022 <- ggplot(naaps.baker.long.2022, 
                     aes(x = Date, y = Value, color = Variable)) +
  geom_line() + 
  geom_segment(data = markers.baker.2022, aes(x = Date, xend = Date, 
                                        y = ymin, yend = ymax), 
               linetype = "solid", color = "black", show.legend = FALSE) +
  labs(title = "a.) Mount Baker, 2022", 
       x = "Date", 
       y = expression("Deposition Flux (mg/m"^2*"/day)"), 
       color = "Aerosol Type") +
  theme_minimal() + scale_color_manual(values = 
                                         c("abf_totsink" = "#E69F00", 
                                "dust_totsink" = "#56B4E9",
                                "smoke_totsink" = "#CC79A7"),
                     labels = c("abf_totsink" = "ABF", "dust_totsink" = 
                                  "Dust", "smoke_totsink" = "Smoke")) +
  scale_x_date(limits = c(as.Date("2022-05-01"), 
                          as.Date("2022-10-31")),
    breaks = seq(as.Date("2022-05-01"), as.Date("2022-10-31"), 
                 by = "1 month"), date_labels = "%b %d")
naaps.baker.2022

## 2023
naaps.baker.2023 <- naaps.baker %>%
  filter(format(Date, "%Y") == "2023")
naaps.baker.long.2023 <- naaps.baker.2023 %>%
  pivot_longer(cols = c(smoke_totsink, dust_totsink, abf_totsink),
               names_to = "Variable", values_to = "Value")
### marker info
marker.baker.dates.2023 <- as.Date(c("2023-05-06", "2022-05-20", 
                               "2023-06-08", "2023-06-26", "2023-07-10", 
                               "2023-07-17", "2023-07-24", "2023-08-06",
                               "2023-08-23"))
markers.baker.2023 <- data.frame(
  Date = marker.baker.dates.2023,
  ymin = 0,   # Lower limit of the segment
  ymax = 5     # Upper limit of the segment
)
### plot
naaps.baker.2023 <- ggplot(naaps.baker.long.2023, 
                     aes(x = Date, y = Value, color = Variable)) +
  geom_line() + 
  geom_segment(data = markers.baker.2023, aes(x = Date, xend = Date, 
                                        y = ymin, yend = ymax), 
               linetype = "solid", color = "black", show.legend = FALSE) +
  labs(title = "b.) Mount Baker, 2023", x = "Date", 
       y = expression("Deposition Flux (mg/m"^2*"/day)"), 
       color = "Aerosol Type") +
  theme_minimal() + scale_color_manual(values = 
                                         c("abf_totsink" = "#E69F00", 
                                           "dust_totsink" = "#56B4E9",
                                           "smoke_totsink" = "#CC79A7"),
                                       labels = c("abf_totsink" = "ABF",
                                                  "dust_totsink" = "Dust",
                                                  "smoke_totsink" = "Smoke")) +
  scale_x_date(limits = c(as.Date("2023-05-01"), 
                          as.Date("2023-10-31")),
               breaks = seq(as.Date("2023-05-01"), as.Date("2023-10-31"), 
                            by = "1 month"), date_labels = "%b %d")
naaps.baker.2023

# naaps - Rainier
naaps.rainier <- read.csv(file.choose())
naaps.rainier$Date <- 
  as.Date(naaps.rainier$Date)

## 2022
naaps.rainier.2022 <- naaps.rainier %>%
  filter(format(Date, "%Y") == "2022")
naaps.rainier.long.2022 <- naaps.rainier.2022 %>%
  pivot_longer(cols = c(smoke_totsink, dust_totsink, abf_totsink),
               names_to = "Variable", values_to = "Value")
### marker info
marker.rainier.dates.2022 <- 
  as.Date(c("2022-07-15", "2022-08-15"))
markers.rainier.2022 <- data.frame(
  Date = marker.rainier.dates.2022,
  ymin = 0,   # Lower limit of the segment
  ymax = 5     # Upper limit of the segment
)
### plot
naaps.rainier.2022 <- ggplot(naaps.rainier.long.2022, 
                     aes(x = Date, y = Value, color = Variable)) +
  geom_line() + 
  geom_segment(data = markers.rainier.2022, aes(x = Date, xend = Date, 
                                        y = ymin, yend = ymax), 
               linetype = "solid", color = "black", show.legend = FALSE) +
  labs(title = "Mount Rainier, 2022", 
       x = "Date", 
       y = expression("Deposition Flux (mg/m"^2*"/day)"), 
       color = "Aerosol Type") +
  theme_minimal() + scale_color_manual(values = 
                                         c("abf_totsink" = "#E69F00", 
                                           "dust_totsink" = "#56B4E9",
                                           "smoke_totsink" = "#CC79A7"),
                                       labels = c("abf_totsink" = "ABF",
                                                  "dust_totsink" = 
                                                    "Dust", 
                                                  "smoke_totsink" = "Smoke")) +
  scale_x_date(limits = c(as.Date("2022-05-01"), 
                          as.Date("2022-10-31")),
               breaks = seq(as.Date("2022-05-01"), as.Date("2022-10-31"), 
                            by = "1 month"), date_labels = "%b %d")
naaps.rainier.2022

### plot smoke only
#### baker 2022
naaps.baker.2022.smoke <- subset(naaps.baker.long.2022, 
                                 naaps.baker.long.2022$Variable
                  == "smoke_totsink")
markers.baker.2022$color <- c("black", "black", "black", "black", "black",
                              "black", "darkorange1", "black", "black")
naaps.baker.smoke.2022 <- ggplot(naaps.baker.2022.smoke, 
                                 aes(x = Date, y = Value)) +
  geom_line(color = "deepskyblue2") + theme_minimal() + 
  labs(title = "a.) Mount Baker, 2022", x = "Date", 
       y = expression("Smoke Deposition Flux (mg/m"^2*"/day)")) +
  geom_segment(data = markers.baker.2022, aes(x = Date, xend = Date, 
                                                y = ymin, yend = ymax), 
               linetype = "solid", color = markers.baker.2022$color, 
               show.legend = FALSE) +
  scale_x_date(limits = c(as.Date("2022-05-01"), as.Date("2022-10-31")),
               breaks = seq(as.Date("2022-05-01"), as.Date("2022-10-31"), 
                            by = "1 month"), date_labels = "%b %d") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 22))
naaps.baker.smoke.2022
#### baker 2023
naaps.baker.2023.smoke <- subset(naaps.baker.long.2023, 
                                 naaps.baker.long.2023$Variable
                                 == "smoke_totsink")
markers.baker.2023$color <- c("black", "black", "black", "black", "black",
                              "black", "black", "black", "darkorange1")
naaps.baker.smoke.2023 <- ggplot(naaps.baker.2023.smoke, 
                                 aes(x = Date, y = Value)) +
  geom_line(color = "deepskyblue2") + theme_minimal() + 
  labs(title = "b.) Mount Baker, 2023", x = "Date", 
       y = expression("Smoke Deposition Flux (mg/m"^2*"/day)")) +
  geom_segment(data = markers.baker.2023, aes(x = Date, xend = Date, 
                                              y = ymin, yend = ymax), 
               linetype = "solid", color = markers.baker.2023$color, 
               show.legend = FALSE) +
  scale_x_date(limits = c(as.Date("2023-05-01"), as.Date("2023-10-31")),
               breaks = seq(as.Date("2023-05-01"), as.Date("2023-10-31"), 
                            by = "1 month"), date_labels = "%b %d") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 22))
naaps.baker.smoke.2023
#### rainier 2022
naaps.rainier.2022.smoke <- subset(naaps.rainier.long.2022, 
                                 naaps.rainier.long.2022$Variable
                                 == "smoke_totsink")
naaps.rainier.smoke.2022 <- ggplot(naaps.rainier.2022.smoke, 
                                 aes(x = Date, y = Value)) +
  geom_line(color = "deepskyblue2") + theme_minimal() + 
  labs(title = "Mount Rainier, 2022", x = "Date", 
       y = expression("Smoke Deposition Flux (mg/m"^2*"/day)")) +
  geom_segment(data = markers.rainier.2022, aes(x = Date, xend = Date, 
                                              y = ymin, yend = ymax), 
               linetype = "solid", color = "black", show.legend = FALSE) +
  scale_x_date(limits = c(as.Date("2022-05-01"), as.Date("2022-10-31")),
               breaks = seq(as.Date("2022-05-01"), as.Date("2022-10-31"), 
                            by = "1 month"), date_labels = "%b %d") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        plot.title = element_text(size = 22))
naaps.rainier.smoke.2022



# USGS discharge data
install.packages("dataRetrieval")
library(dataRetrieval)
install.packages("remotes")
library(remotes)

## access data
site.ID = "12205000"
startDate = "2022-08-01"
endDate = "2022-11-30"
pCode = "00060" #discharge
NF.discharge <- readNWISdv(site.ID, pCode, startDate, 
                           endDate)

## rename discharge variable
names(NF.discharge)[names(NF.discharge) == "X_00060_00003"] <- 'Discharge'

## add month column to get monthly discharge totals
NF.discharge$Month <- month(NF.discharge$Date, label = TRUE, abbr = FALSE)

## monthly stats
# Summarize the data
monthly.summary <- NF.discharge %>%
  group_by(Month) %>% 
  summarise(
    Avg.Discharge = mean(Discharge, na.rm = TRUE),    # Average discharge
    SD.Discharge = sd(Discharge, na.rm = TRUE),       # Standard deviation
    Total.Discharge = sum(Discharge, na.rm = TRUE)    # Total discharge
  ) %>%
  arrange(match(Month, month.abb))
