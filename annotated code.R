# 1. Load data
load('E:/Academic/UW/504/project/New_data/dat_all.RData')
head(dat)

## Log-data
dat$Region <- as.factor(dat$Region)
dat$LogDiffMSP <- log(dat$Dec2014MSP) - log(dat$Jan2010MSP)
dat$LogDiffPopulation <- log(dat$PopulationTotal2014) - log(dat$PopulationTotal2010)
dat$LogDiffMR <- log(dat$Dec2014MR) - log(dat$Nov2010MR)
dat$LogDiffInvt <- log(dat$Dec2014Invt) - log(dat$Jan2010Invt)
dat$LogDiffIncome <- log(dat$MedianIncome2014) - log(dat$MedianIncome2010)
dat$LogDiffUnemploy <- log(dat$UnemploymentRate2014) - log(dat$UnemploymentRate2010)

## Percentage change-data
dat$PctChngMSP <- (dat$Dec2014MSP - dat$Jan2010MSP)/dat$Jan2010MSP
dat$PctChngPopulation <- (dat$PopulationTotal2014 - dat$PopulationTotal2010)/dat$PopulationTotal2010
dat$PctChngMR <- (dat$Dec2014MR - dat$Nov2010MR)/dat$Nov2010MR
dat$PctChngInvt <- (dat$Dec2014Invt - dat$Jan2010Invt)/dat$Jan2010Invt
dat$PctChngIncome <- (dat$MedianIncome2014 - dat$MedianIncome2010)/dat$MedianIncome2010
dat$PctChngUnemploy <- dat$UnemploymentRate2014 - dat$UnemploymentRate2010

attach(dat)

# 2. Model selection
log_diff <- lm(LogDiffMSP ~ LogDiffPopulation + LogDiffMR + LogDiffInvt + LogDiffIncome +                LogDiffUnemploy + Region, dat)
precentage <- lm(PctChngMSP ~ PctChngPopulation + PctChngMR + PctChngInvt + PctChngIncome                + PctChngUnemploy + Region, dat)
summary(log_diff)
summary(precentage)

## Comparison of two Y
id <- c(1:131)
y <- data.frame(id=factor(c(1:131),c(1:131)), y1=LogDiffMSP,y2=PctChngMSP)

library(reshape2)  # reshape the dataset
y_long <- melt(y, id="id", variable.name = "ChangeType", value.name = "SalePriceChange")

library(ggplot2)
gg1 <- ggplot(data=y_long, aes(x=id, y=SalePriceChange, group=ChangeType)) + 
  geom_line(size = 0.78, aes(color = ChangeType)) + 
  xlab("Observation ID") + ylab("Sale Price Change") + 
  ggtitle("Fig 1: Comparison of Two Sale Price Change") +
  scale_x_discrete(breaks = seq(1, 131, 10)) +
  scale_color_manual(values=c("#de2d26","#67a9cf"),labels = c("Log-Difference", "Percentage Change")) + 
  theme(legend.position = c(0.92, 0.92), legend.title = element_blank(),
        legend.key = element_blank()) +
  theme(plot.title = element_text(face = "bold"))

## Comparison of two predictions
id <- c(1:131)
pred <- data.frame(id=factor(c(1:131),c(1:131)), pred1=predict(log_diff),  
                   pred2=predict(precentage))
pred_long <- melt(pred, id="id", variable.name = "model", value.name = "prediction")

gg2 <- ggplot(data=pred_long, aes(x=id, y=prediction, group=model)) + 
  geom_line(aes(color = model),size = 0.78) + 
  xlab("Observation ID") + ylab("Predictions") +
  ggtitle("Fig 2: Comparison of Two Predictions") +
  scale_x_discrete(breaks = seq(1, 131, 10)) +
  scale_color_manual(values=c("#fc8d62","#66c2a5"),labels = c("Log-Difference", "Percentage Change")) + 
  theme(legend.position = c(0.92, 0.92), legend.title = element_blank(),
        legend.key = element_blank()) +
  theme(plot.title = element_text(face = "bold"))

## Combine two ggplot together
library(gridExtra)  
gg3 <- grid.arrange(gg1, gg2, nrow = 2)
gg3

## AIC & BIC results of two models
res <- data.frame(AIC(log_diff,precentage), BIC(log_diff,precentage))
res[,-3]


# 3. Scatterplot matrix
library(ggplot2)
library(GGally)
attach(dat)
logDat <- data.frame(LogDiffMSP, LogDiffPopulation, LogDiffMR, LogDiffInvt, LogDiffIncome
                     ,LogDiffUnemploy, Region)
gg <- ggpairs(data = logDat[, c("LogDiffMSP", "LogDiffMR", "LogDiffInvt",  
                                "LogDiffIncome", "LogDiffUnemploy", "LogDiffPopulation", 
                                "Region")], 
              columnLabels = c("SalePriceChange", "RentalChange", "InvtChange",      
                               "SalaryChange", "UnemployChange", "PopChange", "Region"), 
              upper = list(continuous = "smooth", combo = "box"),
              lower = list(continuous = wrap("cor", size = 7), combo = "denstrip"),
              diag = list(continuous = "densityDiag", discrete = "barDiag")) 

# Put in colored boxplot
for (i in 1:7){
  gg[i,7] <- gg[i,7] + aes(fill = Region)
}

gg


# 4. Test of collinearity
## calculate condition number of design matrix
kappa(logDat[,2:6])

# 5. Initial model
m1 <- lm(LogDiffMSP ~ LogDiffPopulation + LogDiffMR + LogDiffInvt + LogDiffIncome + 
           LogDiffUnemploy + Region, dat)
summary(m1)

# 6. Interactions
m1.2 <- lm(LogDiffMSP ~ LogDiffPopulation + LogDiffMR + LogDiffInvt + LogDiffUnemploy +
             LogDiffIncome * Region, dat)
summary(m1.2)
table(Region)

# 7. Variable Selection and ANOVA
## stepwise based on AIC 
lm.step=step(m1) 
summary(lm.step)

## ANOVA
m1.3 <- lm(LogDiffMSP ~ LogDiffInvt + Region + LogDiffIncome + LogDiffPopulation
           + LogDiffMR + LogDiffUnemploy, dat)
anova(m1.3)

# 8. Diagnostics
## Diagnostics Plots
m2 <- lm(LogDiffMSP ~ LogDiffInvt + LogDiffIncome + LogDiffPopulation + Region, dat)

library(ggfortify)
new_dat <- dat
rownames(new_dat) <- new_dat$RegionName
autoplot(lm(LogDiffMSP ~ LogDiffPopulation + LogDiffInvt + LogDiffIncome + Region, 
            data = new_dat), colour = 'steelblue',  smooth.colour = 'black', 
            label.size = 4, label.n = 3)

## Outliers Test
library(car)  
outlierTest(m2)   ## The point with maximum absolute residual is not significant

##  Regression without leverage points
m3 <- lm(LogDiffMSP ~ LogDiffInvt + LogDiffIncome + LogDiffPopulation + 
           Region, dat[-103,])
summary(m3)
