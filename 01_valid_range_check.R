
# Valid range check
dat03 <- dat02

################################################################################

# age_m (3) - 72+
# 9 -> 70, 71 = around the valid range but not included
# include these rows as we do not consider these as significant violations
row.iv.3 <- which(dat03[,3] < 72)
length(row.iv.3)

################################################################################

# bpvs_raw (6) - 0-155
# 2: 166 (72), 162 (74) -> 155 (72, 74)
# family = poisson() -> bpvs_raw = the number of correct answers
# language (vocabulary)
# direct assessment
row.iv.6 <- which(dat03[,6] < 0 | dat03[,6] > 155)
# length(row.iv.6)
glm.bpvs <- glm(p6 ~ log(p7)+log(p8), 
                family = poisson(), data = dat03[-row.iv.6,])
# plot(glm.bpvs)
# summary(glm.bpvs)
# plot(residuals(glm.bpvs))

# to check the goodness of fit -> chisq test
qchisq(.95, glm.bpvs$df.null-glm.bpvs$df.residual)
1-pchisq(glm.bpvs$null.deviance-glm.bpvs$deviance, df=2)

exp(coefficients(glm.bpvs)[[1]]
    +coefficients(glm.bpvs)[[2]]*log(dat03$p7[row.iv.6])
    +coefficients(glm.bpvs)[[3]]*log(dat03$p8[row.iv.6]))

# considering the relationship with equivalent language abilities variables,
# the invalid values are still over the valid range
# -> we could just assign the maximum value in an acceptable range.
dat03[row.iv.6,6] <- 155

################################################################################

# tomi_compmean (12) - 0-20
# none
# social cognition (theory of mind)
# parent report
# ind.tom.cm1: 68 - not identical between dat03[,12] and the calculated composite mean with dat03[,9:11]
# ind.tom.cm2: 17 - identical mean value between the calculated mean
# ind.tom.cm3: 4 - missing values for TOMI
# they are 2 methods for calculating composite mean 
# -> unit weighted, regression-weighted
ind.tom.cm1 <- which(round((dat03[,9]+dat03[,10]+dat03[,11])/3, 1) != dat03[,12])
ind.tom.cm2 <- which(round((dat03[,9]+dat03[,10]+dat03[,11])/3, 1) == dat03[,12])
ind.tom.cm3 <- which(is.na(dat03[,12]))

# comp.mean.df: data frame with original values 
# from the raw data and unit weighted mean
comp.mean.df <- data.frame(dat03[, 9:12],
                           comp = round((dat03[,9]+dat03[,10]+dat03[,11])/3, 1))

# unit mean vs. given data - before adjustment
plot(dat03$p12, comp.mean.df$comp, 
     ylim = c(6,20), xlim = c(6,20),
     xlab = "given composite mean", ylab = "calculated unit mean")
abline(a=0, b=1, col="red")
boxplot(comp.mean.df$comp-comp.mean.df$p12) # outliers

# there are 68 different values which are different from the calculalted mean
# to minimize the data adjustment, we assign unit mean for only these 2 variables.
# row - 27, 29 : outliers
(comp.mean.df$comp-comp.mean.df$p12)[c(27,29)]
dat03$p12[c(27,29)] <- comp.mean.df$comp[c(27,29)]

# unit mean vs. given data - after adjustment
# comparison given composite mean and calculated unit mean
plot(dat03$p12, comp.mean.df$comp, 
     ylim = c(6,20), xlim = c(6,20),
     xlab = "given composite mean", ylab = "unit mean")
abline(a=0, b=1, col="red")

plot(dat03$p12,
     xlab = "row index", ylab = "mean",
     main = "composite mean from the raw data")
abline(v=51, col = "red")

################################################################################

# et_falsebelief_testtrial_preference_score (18) - 0+
# 29 (negative values)
# social cognition (theory of mind)
# to keep the scale of the data, we would assign the glm values
row.iv.18 <- which(dat03[,18] < 0)
length(row.iv.18)

# dat03.18: exclude rows with negative values for the 18th variables
dat03.18 <- dat03[-row.iv.18,]
hist(dat03.18[,18], xlab="", main="", 
     probability = TRUE, nclass = 20)
ind.18.na <- which(is.na(dat03.18[,18]))

# glm.fit(family=Gamma) <- skewed continuous positive respond variables
glm.18 <- glm(p18 ~ log(p16)+log(1+p17), 
              family=Gamma(link="inverse"),data = dat03.18)
summary(glm.18)

# plot to compare the result to compare the given data and approximate values
plot(log(dat03.18$p16)[-ind.18.na], 
     dat03.18$p18[-ind.18.na], ylim=c(0,10000),
     xlab="p16", ylab="p18")
points(log(dat03.18$p16)[-ind.18.na],
       fitted.values(glm.18), col="blue")
legend("topleft", c("given data","fitted values"), fill=c("black","blue"))

# check the predict effect of the models
# we could certify the significance of the model we set up.
qchisq(1-.05,glm.18$df.null-glm.18$df.residual)
glm.18$null.deviance-glm.18$deviance

dat03[row.iv.18,18] <- 1/(glm.18$coefficients[[1]]+
                            log(dat03[row.iv.18,16])*glm.18$coefficients[[2]]+
                            log(dat03[row.iv.18,17]+1)*glm.18$coefficients[[3]])

################################################################################
