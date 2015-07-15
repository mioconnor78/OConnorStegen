### week 8 (or 9) analysis ####


data <- data2[(data2$Week=='9'),]
head(data)

### plotting to explore patterns
par(mfrow = c(2,2))
plot(logNPP~invT, data = data, pch = 19, xlim = c(38, 40), ylim = c(-2.5, -1.5))
plot(logER~invT, data = data, pch = 19, xlim = c(38, 40), ylim = c(-2.5, -1.5))
plot(logChla~invT, data = data, pch = 19, xlim = c(38, 40), ylim = c(0,4))
plot(peri.mg ~ invT, data = data, pch = 19, xlim = c(38, 40), ylim = c(0,60))
plot(tot.alg ~ invT, data = data, pch = 19, xlim = c(38, 40))

NPP9 <- lm(logNPP~invT, data = data)
summary(NPP9)

ER9 <- lm(logER~invT, data = data)
summary(ER9)

chla9 <- lm(logChla~invT, data = data)
summary(chla9)

peri9 <- lm(log(peri.mg)~invT, data = data)
summary(peri9)

tot.alg9 <- lm(log(tot.alg)~invT, data = data)
summary(tot.alg9)



