### analysis for O'Connor and Stegen


library(lme4)
data <- data2

#### analysis of NPP and ER

## basic plotting to examine patterns
hist(data$NPP)
hist(log(data$NPP+ 0.1))
plot(log(data$NPP+ 0.1)~data$Week) # shows a trajectory through time; initial conditions were high (week 1), then lower, and began to increase at the end.
plot(logNPP~invT, data = data[(data$Week > '2'),], pch = 19)
plot(logNPP~TempO2NPP, data = data[(data$Week > '2'),])

# analysis of NPP
NPP1 <- lmer(logNPP~I(invT - mean(invT))  + (1 + I(invT - mean(invT)) | Week), REML = FALSE, data=data[(data$Week > '2'),], na.action=na.omit)
NPP2 <- lmer(logNPP~I(invT - mean(invT))  + (1 | Week), REML = FALSE, data=data[(data$Week > '2'),], na.action=na.omit)
anova(NPP1, NPP2)
summary(NPP2)

# analysis of ER
ER1 <- lmer(logER~I(invT - mean(invT))  + (1 + I(invT - mean(invT)) | Week), REML = FALSE, data=data[(data$Week > '2'),], na.action=na.omit)
ER2 <- lmer(logER~I(invT - mean(invT))  + (1 | Week), REML = FALSE, data=data[(data$Week > '2'),], na.action=na.omit)
anova(ER1, ER2)

summary(ER2)
