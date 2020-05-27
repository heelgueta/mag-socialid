##setup and load data
options(scipen=999) #disables scientific notation
options(max.print=1000000) #enable long outputs
df5p00 <- read.csv("ide5p.csv")

##cfa model testing
#1 factor
mod1f <- 'gen =~ id1 + id2 + id3 + id4 + id5 + id6 + id7 + id8 + id9'
fit1f <- lavaan::cfa(mod1f, data=df5p00,estimator="MLR")
#lavaan::summary(fit1f,standardized=TRUE)
lavaan::fitMeasures(fit1f, c("chisq.scaled","df","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled"))
lavaan::standardizedSolution(fit1f)

#3 factors
mod3f <- 'es1 =~ id1 + id2 + id3
          es2 =~ id4 + id5 + id6
          es3 =~ id7 + id8 + id9'
fit3f <- lavaan::cfa(mod3f, data=df5p00,estimator="MLR")
#lavaan::summary(fit3f,standardized=TRUE)
lavaan::fitMeasures(fit3f, c("chisq.scaled","df","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled"))
lavaan::standardizedSolution(fit3f)

#bifactor
modbf <- 'gen =~ 1*id1 + 1*id2 + 1*id3 + 1*id4 + 1*id5 + 1*id6 + 1*id7 + 1*id8 + 1*id9
          es1 =~ id1 + id2 + id3
          es2 =~ id4 + id5 + id6
          es3 =~ id7 + id8 + id9
          gen ~~ 0*es1
          gen ~~ 0*es2
          gen ~~ 0*es3'
fitbf <- lavaan::cfa(modbf, data=df5p00,estimator="MLR")
#lavaan::summary(fitbf,standardized=TRUE)
lavaan::fitMeasures(fitbf, c("chisq.scaled","df","pvalue.scaled","cfi.scaled","tli.scaled","rmsea.scaled"))
lavaan::standardizedSolution(fitbf)

lavaan::anova(fit1f,fit3f,fitbf)

hist(lavaan::lavPredict(fit1f))

round(polycor::hetcor((df5p00[4:12]))$correlations,2)
qgraph::qgraph(round(polycor::hetcor((df5p00[4:12]))$correlations,2),layout = "spring")
