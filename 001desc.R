###choose dataset
dfm5 <- read.csv("mag5p.csv")
dfm7 <- read.csv("mag7p.csv")
dfc5 <- read.csv("chi5p.csv")
dfc7 <- read.csv("chi7p.csv")
#dfa5 <- read.csv("ant5p.csv")

#excluir casos incompletos
dfm5 <- na.omit(dfm5)
dfm7 <- na.omit(dfm7)
dfc5 <- na.omit(dfc5)
dfc7 <- na.omit(dfc7)

#para dfs de 5p, excluir valores fuera de rango
dfm5 <- dfm5[which(dfm5$sex<2), ]
dfm5 <- dfm5[which(dfm5$age>18), ]
dfm5 <- dfm5[which(dfm5$id1>0 & dfm5$id1 < 6), ]
dfm5 <- dfm5[which(dfm5$id2>0 & dfm5$id2 < 6), ]
dfm5 <- dfm5[which(dfm5$id3>0 & dfm5$id3 < 6), ]
dfm5 <- dfm5[which(dfm5$id4>0 & dfm5$id4 < 6), ]
dfm5 <- dfm5[which(dfm5$id5>0 & dfm5$id5 < 6), ]
dfm5 <- dfm5[which(dfm5$id6>0 & dfm5$id6 < 6), ]
dfm5 <- dfm5[which(dfm5$id7>0 & dfm5$id7 < 6), ]
dfm5 <- dfm5[which(dfm5$id8>0 & dfm5$id8 < 6), ]
dfm5 <- dfm5[which(dfm5$id9>0 & dfm5$id9 < 6), ]
dfc5 <- dfc5[which(dfc5$sex<2), ]
dfc5 <- dfc5[which(dfc5$age>18), ]
dfc5 <- dfc5[which(dfc5$id1>0 & dfc5$id1 < 6), ]
dfc5 <- dfc5[which(dfc5$id2>0 & dfc5$id2 < 6), ]
dfc5 <- dfc5[which(dfc5$id3>0 & dfc5$id3 < 6), ]
dfc5 <- dfc5[which(dfc5$id4>0 & dfc5$id4 < 6), ]
dfc5 <- dfc5[which(dfc5$id5>0 & dfc5$id5 < 6), ]
dfc5 <- dfc5[which(dfc5$id6>0 & dfc5$id6 < 6), ]
dfc5 <- dfc5[which(dfc5$id7>0 & dfc5$id7 < 6), ]
dfc5 <- dfc5[which(dfc5$id8>0 & dfc5$id8 < 6), ]
dfc5 <- dfc5[which(dfc5$id9>0 & dfc5$id9 < 6), ]

#para dfs de 7p, excluir valores fuera de rango
dfm7$sex[dfm7$sex==2] <- 0
dfm7 <- dfm7[which(dfm7$sex<2), ]
dfm7 <- dfm7[which(dfm7$age>18), ]
dfm7 <- dfm7[which(dfm7$id1>0 & dfm7$id1 < 8), ]
dfm7 <- dfm7[which(dfm7$id2>0 & dfm7$id2 < 8), ]
dfm7 <- dfm7[which(dfm7$id3>0 & dfm7$id3 < 8), ]
dfm7 <- dfm7[which(dfm7$id4>0 & dfm7$id4 < 8), ]
dfm7 <- dfm7[which(dfm7$id5>0 & dfm7$id5 < 8), ]
dfm7 <- dfm7[which(dfm7$id6>0 & dfm7$id6 < 8), ]
dfm7 <- dfm7[which(dfm7$id7>0 & dfm7$id7 < 8), ]
dfm7 <- dfm7[which(dfm7$id8>0 & dfm7$id8 < 8), ]
dfm7 <- dfm7[which(dfm7$id9>0 & dfm7$id9 < 8), ]
dfc7$sex[dfc7$sex==2] <- 0
dfc7 <- dfc7[which(dfc7$sex<2), ]
dfc7 <- dfc7[which(dfc7$age>18), ]
dfc7 <- dfc7[which(dfc7$id1>0 & dfc7$id1 < 8), ]
dfc7 <- dfc7[which(dfc7$id2>0 & dfc7$id2 < 8), ]
dfc7 <- dfc7[which(dfc7$id3>0 & dfc7$id3 < 8), ]
dfc7 <- dfc7[which(dfc7$id4>0 & dfc7$id4 < 8), ]
dfc7 <- dfc7[which(dfc7$id5>0 & dfc7$id5 < 8), ]
dfc7 <- dfc7[which(dfc7$id6>0 & dfc7$id6 < 8), ]
dfc7 <- dfc7[which(dfc7$id7>0 & dfc7$id7 < 8), ]
dfc7 <- dfc7[which(dfc7$id8>0 & dfc7$id8 < 8), ]
dfc7 <- dfc7[which(dfc7$id9>0 & dfc7$id9 < 8), ]

#divide age
dfm5$ag2 <- sjmisc::dicho(dfm5$age)
dfm7$ag2 <- sjmisc::dicho(dfm7$age)
dfc5$ag2 <- sjmisc::dicho(dfc5$age)
dfc7$ag2 <- sjmisc::dicho(dfc7$age)

###choose dataset?
df <- dfm5
df <- dfm7
df <- dfc5
df <- dfc7

###descriptives
table(df$sex)
round(mean(df$age),2);round(sd(df$age),2)

#reliability
summary(psych::alpha(df[06:08])) #factor 1
summary(psych::alpha(df[09:11])) #factor 2
summary(psych::alpha(df[12:14])) #factor 3
summary(psych::alpha(df[06:14])) #total