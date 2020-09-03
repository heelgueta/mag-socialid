###multigroup cfas

###choose dataset?
df <- dfm5
df <- dfm7
df <- dfc5
df <- dfc7


#group cfa sex
fitconfig <- lavaan::cfa(mod2o, data=df,estimator="MLR", group="sex")
fitmetric <- lavaan::cfa(mod2o, data=df,estimator="MLR", group="sex", group.equal = c("loadings"))
fitscalar <- lavaan::cfa(mod2o, data=df,estimator="MLR", group="sex", group.equal = c("loadings","intercepts"))
fitresidu <- lavaan::cfa(mod2o, data=df,estimator="MLR", group="sex", group.equal = c("loadings","intercepts","residuals"))

#group cfa age
fitconfig <- lavaan::cfa(mod2o, data=df,estimator="MLR", group="ag2")
fitmetric <- lavaan::cfa(mod2o, data=df,estimator="MLR", group="ag2", group.equal = c("loadings"))
fitscalar <- lavaan::cfa(mod2o, data=df,estimator="MLR", group="ag2", group.equal = c("loadings","intercepts"))
fitresidu <- lavaan::cfa(mod2o, data=df,estimator="MLR", group="ag2", group.equal = c("loadings","intercepts","residuals"))



paste(round(lavaan::fitMeasures(fitconfig, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitmetric, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitscalar, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitresidu, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))

lavaan::anova(fitconfig,fitmetric,fitscalar,fitresidu)

lavaan::standardizedSolution(fitconfig)
lavaan::standardizedSolution(fitmetric)
lavaan::standardizedSolution(fitscalar)
lavaan::standardizedSolution(fitresidu)