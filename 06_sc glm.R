
# import package
require(tidyverse) # correlation plot
require(GGally) # correlation plot

################################################################################

# social cognition
# p9 - p18
# nt(39)+aut(28)
sc.mis <- numeric(0)
for(i in 9:18){
  sc.mis <- c(sc.mis, row.mis.ls[[i]])
}
sc.mis <- as.numeric(dimnames(table(sc.mis))[[1]])
dat07.sc <- dat07[-sc.mis,] 
dim(dat07.sc)
length(which(dat07.sc[,4] == "0")) # Neurotypical
length(which(dat07.sc[,4] == "1")) # Autistic

################################################################################

# social cognition
ggcorr(dat07.sc[,c(12,13,14,15,18)], label = TRUE, 
      label_size = 3, label_round = 2, label_alpha = TRUE)
plot(dat07.sc[,c(12,13,14,15,18)])

################################################################################

# response variable
# candidate variables are p12-p18
# before the generation of the response variable for the social cognition,
# we should check the accuracy of the variable

################################################################################

# p18: et_falsebelief_testtrial_preference_score
# Dwell time[(correct trial) - (incorrect trial)]
# comparing the dwelling time for correct and incorrect response
plot(y=abs(dat07.sc[,16]-dat07.sc[,17]-dat07.sc[,18]),
     x=dat07.sc[,18], xlab="preference score",
     ylab="difference", ylim=c(-1000,6000), pch=4)
abline(h=0, col="red")
dat07.sc[,18] <- (dat07.sc[,16]-dat07.sc[,17])
points(y=abs(dat07.sc[,16]-dat07.sc[,17]-dat07.sc[,18]),
       x=dat07.sc[,18], col="blue", pch = 1)
legend(legend=c("before adjustment","after adjustment"),
       col=c("black", "blue"), pch=c(4,1), cex=0.8,
       x=3000, y=5000)

# result for false belief task - correct and incorrect
ind.cor <- which(dat07.sc[,18] > 0) # 43
ind.incor <- which(dat07.sc[,18] < 0) # 24
fal.task <- numeric(0)
fal.task[ind.cor] <- "correct"
fal.task[ind.incor] <- "incorect"

################################################################################

# p14 - interacting figures
# p15 - non-interacting figures
# figure task - measure participants' preference 
# by comparing dwelling time to interacting and non-interacting figures
ind.pref.int <- which(dat07.sc[,14] > dat07.sc[,15]) # 42
ind.pref.non <- which(dat07.sc[,14] < dat07.sc[,15]) # 25
fig.task <- numeric(0)
fig.task[ind.pref.int] <- "interacting"
fig.task[ind.pref.non] <- "non-interacting"

################################################################################

boxplot(dat07.sc[,12]~fig.task, 
        main="ToMI composite mean", ylab="")
boxplot(dat07.sc[,13]~fig.task, 
        main="ToMI Task battery Total score", ylab="")

boxplot(dat07.sc[,12]~fal.task, 
        main="ToMI composite mean", ylab="")
boxplot(dat07.sc[,13]~fal.task, 
        main="ToMI Task battery Total score", ylab="")

boxplot(dat07.sc[,12]~dat07.sc[,4], 
        main="ToMI composite mean", ylab="")
boxplot(dat07.sc[,13]~dat07.sc[,4], 
        main="ToMI Task battery Total score", ylab="")

################################################################################

# decide response variables
sc1 <- log(dat07.sc[,12]*dat07.sc[,13])
plot(density(sc1))

sc2 <- dat07.sc[,12]+dat07.sc[,13]+dat07.sc[,18]+
  (dat07.sc[,14]-dat07.sc[,15])
plot(density(sc2))

# we assign different proportion according to the results 
# from false belief task and figure task
sc.prop <- numeric(0)
for (i in 1:nrow(dat07.sc)){
  if (fal.task[i] == "correct"){
    if (fig.task[i] == "interacting"){
      sc.prop[i] <- 1
    }else{
      sc.prop[i] <- 0.5
    }
  } else{
    if(fig.task[i] == "interacting"){
      sc.prop[i] <- 0.5
    } else{
      sc.prop[i] <- 0.25
    }
  }
}
sc3 <- (dat07.sc[,12]+dat07.sc[,13])*sc.prop
plot(density(sc3)) 
# it looks like bipolar gaussian
boxplot(sc3~sc.prop)

################################################################################

glm.sc00 <- glm(sc1~1, family=Gamma(link="inverse"))
summary(glm.sc00)
# plot(glm.sc00)

################################################################################

glm.sc01 <- glm(sc1~factor(p2)*p3+factor(p4), 
                family=Gamma(link="identity"), data=dat07.sc)
summary(glm.sc01)
# plot(glm.sc01)

fit.res1 <- lm(residuals(glm.sc01)~sc1)
summary(fit.res1)
plot(x=sc1, y=residuals(glm.sc01))
abline(a=coefficients(fit.res1)[[1]], 
       b=coefficients(fit.res1)[[2]], col="red")

################################################################################

glm.sc02 <- glm(sc1~factor(p2)*p3+factor(p4)+p23+p24, 
                family=Gamma(link="identity"), data=dat07.sc)
summary(glm.sc02)
# plot(glm.sc02)

fit.res2 <- lm(residuals(glm.sc02)~sc1)
summary(fit.res2)
plot(x=sc1, y=residuals(glm.sc02))
abline(a=coefficients(fit.res2)[[1]], b=coefficients(fit.res2)[[2]], col="red")

################################################################################

glm.sc03 <- glm(sc1~factor(p2)*p3+factor(p4)+p23+p24+p25+factor(p26), 
                family=Gamma(link="identity"), data=dat07.sc)
summary(glm.sc03)
# plot(glm.sc03)

fit.res3 <- lm(residuals(glm.sc03)~sc1)
summary(fit.res3)
plot(x=sc1, y=residuals(glm.sc03))
abline(a=coefficients(fit.res3)[[1]], b=coefficients(fit.res3)[[2]], col="red")

################################################################################

glm.sc04 <- glm(sc1~factor(p2)*p3+factor(p4)+p25+factor(p26), 
                family=Gamma(link="identity"), data=dat07.sc)
summary(glm.sc04)
# plot(glm.sc04)

fit.res4 <- lm(residuals(glm.sc04)~sc1)
summary(fit.res4)
plot(x=sc1, y=residuals(glm.sc04))
abline(a=coefficients(fit.res4)[[1]], b=coefficients(fit.res4)[[2]], col="red")

################################################################################

anova(glm.sc00, glm.sc01, glm.sc02, glm.sc03, glm.sc04, test="LRT")

anova(glm.sc01, glm.sc03, test="LRT")

################################################################################

AIC(glm.sc01)
AIC(glm.sc03)

BIC(glm.sc01)
BIC(glm.sc03)
