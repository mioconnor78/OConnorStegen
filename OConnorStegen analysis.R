### analysis for O'Connor and Stegen


library(lme4)
library(MuMIn)
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
model.sel(NPP1, NPP2)
summary(NPP2)
confint(NPP2)

# analysis of ER
ER1 <- lmer(logER~I(invT - mean(invT))  + (1 + I(invT - mean(invT)) | Week), REML = FALSE, data=data[(data$Week > '2'),], na.action=na.omit)
ER2 <- lmer(logER~I(invT - mean(invT))  + (1 | Week), REML = FALSE, data=data[(data$Week > '2'),], na.action=na.omit)
anova(ER1, ER2)
model.sel(ER1, ER2)
summary(ER2)
confint(ER2)

# analysis of chla
Chla1 <- lmer(logChla~I(invT - mean(invT))  + (1 + I(invT - mean(invT)) | Week), REML = FALSE, data=data[(data$Week > '2'),], na.action=na.omit)
Chla2 <- lmer(logChla~I(invT - mean(invT))  + (1 | Week), REML = FALSE, data=data[(data$Week > '2'),], na.action=na.omit)
anova(Chla1, Chla2)
model.sel(Chla1, Chla2)
summary(Chla2)
confint(Chla2)


# backswimmers
plot((3-backswimmers) ~ TempO2NPP, main = 'Number of dead or missing Notonectids', data=data[(data$Week > '2'),])
