#part 1 & 2
library(datasets)
data(ToothGrowth)
head(ToothGrowth)
str(ToothGrowth)
summary(ToothGrowth)
table(ToothGrowth$supp, ToothGrowth$dose)
library(ggplot2)
library(gridExtra)
qplot(dose, len, data = ToothGrowth, color = supp)
#boxplot(len ~ supp * dose, data = ToothGrowth)
plot2 <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, fill = supp))
plot2 <- plot2 + geom_boxplot()
plot2 <- plot2 + labs(title = "Effect of Dose and Supplement on Length of Tooth", x = "Dose", y = "Length")
plot2
# coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
#        xlab = "ToothGrowth data: length vs dose, given type of supplement")
plot2.1 <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, fill = dose))
plot2.1 <- plot2.1 + geom_boxplot(aes(fill = as.factor(dose)))
plot2.1 <- plot2.1 + labs(x = "Dose", y = "Length")
plot2.2 <- ggplot(ToothGrowth, aes(x = supp, y = len, fill = supp))
plot2.2 <- plot2.2 + geom_boxplot(aes(fill = supp))
plot2.2 <- plot2.2 + labs(x = "Supplement", y = "Length")
grid.arrange(plot2.1, plot2.2, ncol = 2)

#part 3
library(dplyr)
#len ~ dose
toothDose0.5 <- filter(ToothGrowth, dose == 0.5)
toothDose1 <- filter(ToothGrowth, dose == 1)
toothDose2 <- filter(ToothGrowth, dose == 2)
#diff <- toothDose0.5$len - toothDose1$len
t.test(toothDose0.5$len, toothDose1$len, paired = T)
t.test(toothDose0.5$len, toothDose2$len, paired = T)
t.test(toothDose1$len, toothDose2$len, paired = T)

#len ~ supp
t.test(len ~ supp, data = ToothGrowth, paired = T)