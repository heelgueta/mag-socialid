#aquí configuro el primer archivo

##setup and load data
#rm(list = ls())
options(scipen=999) #disables scientific notation
options(max.print=1000000) #enable long outputs
dfidm5p <- read.csv("idm5p.csv")
dfidm7p <- read.csv("idm7p.csv")
dfidc7p <- read.csv("idc7p.csv")

##cfa model testing
#1 factor
mod1f <- 'gen =~ idm1 + idm2 + idm3 + idm4 + idm5 + idm6 + idm7 + idm8 + idm9'
fit1f <- lavaan::cfa(mod1f, data=dfidm5p,estimator="MLR")
#lavaan::summary(fit1f,standardized=TRUE)
lavaan::fitMeasures(fit1f, c("chisq.scaled","df","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled"))
lavaan::standardizedSolution(fit1f)

#3 factors
mod3f <- 'es1 =~ idm1 + idm2 + idm3
          es2 =~ idm4 + idm5 + idm6
          es3 =~ idm7 + idm8 + idm9'
fit3f <- lavaan::cfa(mod3f, data=dfidm5p,estimator="MLR")
#lavaan::summary(fit3f,standardized=TRUE)
lavaan::fitMeasures(fit3f, c("chisq.scaled","df","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled"))
lavaan::standardizedSolution(fit3f)

#bifactor
modbf <- 'gen =~ 1*idm1 + 1*idm2 + 1*idm3 + 1*idm4 + 1*idm5 + 1*idm6 + 1*idm7 + 1*idm8 + 1*idm9
          es1 =~ idm1 + idm2 + idm3
          es2 =~ idm4 + idm5 + idm6
          es3 =~ idm7 + idm8 + idm9
          gen ~~ 0*es1
          gen ~~ 0*es2
          gen ~~ 0*es3'
fitbf <- lavaan::cfa(modbf, data=dfidm5p,estimator="MLR")
#lavaan::summary(fitbf,standardized=TRUE)
lavaan::fitMeasures(fitbf, c("chisq.scaled","df","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled"))
lavaan::standardizedSolution(fitbf)

lavaan::anova(fit1f,fit3f,fitbf)


dftemp <- as.data.frame(cbind(lavaan::inspect(fit1f, "case.idx"),lavaan::lavPredict(fit1f)))
colnames(dftemp) <- c("pid","idm")

merge(df5p00, dftemp[, c("pid", "idm")], by="pid", all.x=TRUE)
df5p01 <- merge(df5p00, dftemp[, c("pid", "idm")], by="pid", all=TRUE)

boxplot(df5p01$idm~df5p01$sex)
boxplot(df5p01$idm~df5p01$age)
plot(df5p01$idm~df5p01$age)
plot(jitter(df5p01$age),jitter(df5p01$idm))
plot(jitter(df5p01$sex),jitter(df5p01$idm))

hist(lavaan::lavPredict(fit1f))

round(polycor::hetcor((dfidm5p[4:12]))$correlations,2)
qgraph::qgraph(round(polycor::hetcor((dfidm5p[4:12]))$correlations,2),layout = "spring")
