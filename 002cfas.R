#####cfa model testing

###1 factor
mod1f <- 'gen =~ id1 + id2 + id3 + id4 + id5 + id6 + id7 + id8 + id9'
###1 second orderfactor 
mod2o <- 'es1 =~ id1 + id2 + id3
          es2 =~ id4 + id5 + id6
          es3 =~ id7 + id8 + id9
          gen =~ 1*es1 + 1*es2 + 1*es3'
###3 factors
mod3f <- 'es1 =~ id1 + id2 + id3
          es2 =~ id4 + id5 + id6
          es3 =~ id7 + id8 + id9'
###bifactor
modbf <- 'gen =~ 1*id1 + 1*id2 + 1*id3 + 1*id4 + 1*id5 + 1*id6 + 1*id7 + 1*id8 + 1*id9
          es1 =~ id1 + id2 + id3
          es2 =~ id4 + id5 + id6
          es3 =~ id7 + id8 + id9
          gen ~~ 0*es1
          gen ~~ 0*es2
          gen ~~ 0*es3'
#modbf <- 'gen =~ 1*id1 + 1*id2 + 1*id3 + 1*id4 + 1*id5 + 1*id6 + 1*id7 + 1*id8 + 1*id9
#          es1 =~ id1 + id2 + id3
#          es2 =~ id4 + id5 + id6
#          es3 =~ id7 + id8 + id9
#          gen ~~ 0*es1
#          gen ~~ 0*es2
#          gen ~~ 0*es3
#          es1 ~~ 0*es1
#          es2 ~~ 0*es2
#          es3 ~~ 0*es3'
#modbf <- 'gen =~ 1*id1 + 1*id2 + 1*id3 + 1*id4 + 1*id5 + 1*id6 + 1*id7 + 1*id8 + 1*id9
#          es1 =~ id1 + id2 + id3
#          es2 =~ id4 + id5 + id6
#          es3 =~ id7 + id8 + id9
#          gen ~~ 0*es1
#          gen ~~ 0*es2
#          gen ~~ 0*es3
#          es1 ~~ 0*es2
#          es1 ~~ 0*es3
#          es2 ~~ 0*es3'


###choose dataset?
df <- dfm5
df <- dfm7
df <- dfc5
df <- dfc7


#run models
fit1f <- lavaan::cfa(mod1f, data=df,estimator="MLR")
fit2o <- lavaan::cfa(mod2o, data=df,estimator="MLR")
fit3f <- lavaan::cfa(mod3f, data=df,estimator="MLR")
fitbf <- lavaan::cfa(modbf, data=df,estimator="MLR")

#resultados
paste(round(lavaan::fitMeasures(fit1f, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fit2o, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fit3f, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitbf, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))


#comparación
lavaan::anova(fit1f,fit3f,fitbf,fit2o)

#cargas factoriales y correlaciones
lavaan::standardizedSolution(fit1f)
lavaan::standardizedSolution(fit2o)
lavaan::standardizedSolution(fit3f)
lavaan::standardizedSolution(fitbf)
