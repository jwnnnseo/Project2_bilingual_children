
# missing data mechanism
# 1. MCAR = Missing Completely At Random
# 2. MAR = Missing At Random
# 3. (MNAR) MDUP = Missing that Depends on Unobserved Predictors
# 4. (MNAR) MDM = Missing that Depends on the Missing value itself

dat01[ind.mis.7,]
dat01[ind.mis.7, predictor.ind]
dat01[ind.mis.8,]
dat01[ind.mis.8, predictor.ind]

dat01[ind.mis.9,] # the whole variables of tomi result is missing
dat01[ind.mis.13,]
dat01[ind.mis.14,]
dat01[ind.mis.16,]
dat01[ind.mis.27,]
dat01[ind.mis.36,]
dat01[ind.mis.40,]
dat01[ind.mis.41,]
dat01[ind.mis.42,]
dat01[ind.mis.43,]

################################################################################

# Let's find a way to impute for each variable

# import library needed to plot correlation plot
require(GGally)

dat02 <- dat01[all.row.ful,]

# Language
ggcorr(dat02[,6:8], label = TRUE)
fit.wasi.log <- glm(dat02[,8] ~ log(dat02[,6]))
summary(fit.wasi.log)

plot(dat02[,8])
points(fitted.values(fit.wasi.log),col="red")

################################################################################


################################################################################

fit.wasi <- glm(dat02[,8] ~ dat02[,6])
summary(fit.wasi)
plot(fit.wasi)

plot(log(dat02[,6]), dat02[,8])
plot(residuals(fit.wasi))
