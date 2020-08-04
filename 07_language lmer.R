
require("lme4") # for linear mixed effect
# install.packages("arm") # for display function
require(arm)

################################################################################

# let's consider random effect
# start by box plot

# gender varaible does not show big difference for other variables
# boxplot(dat01$age_m~dat01$gender)
# boxplot(dat01$bilec_total_input~dat01$gender)
# boxplot(dat01$bilec_total_output~dat01$gender)
# boxplot(dat01$age_acquisition~dat01$gender)

boxplot(dat01$age_m~dat01$diagnosis, 
        xlab="diagnosis", ylab="age in months")
# boxplot(dat01$bilec_total_input~dat01$diagnosis)
boxplot(dat01$bilec_total_output~dat01$diagnosis,
        xlab="diagnosis", ylab="total output")
boxplot(dat01$age_acquisition~dat01$diagnosis,
        xlab="diagnosis", ylab="when child learned 2nd language")

# boxplot(dat01$age_m~dat01$where_english)
# boxplot(dat01$bilec_total_input~dat01$where_english)
# boxplot(dat01$bilec_total_output~dat01$where_english)
# boxplot(dat01$age_acquisition~dat01$where_english)

################################################################################

lmer.lang1 <- glmer(lang2 ~ (1|p26)+factor(p2)*p3+factor(p4)+p24+p25,
                   data=dat07.lang, family=Gamma(link="inverse"))
summary(lmer.lang1)
plot(lmer.lang1)

plot(dat07.lang[,24],lang2)
points(dat07.lang[,24],fitted.values(lmer.lang1), col="blue")

plot(lang2, type = "l")
lines(fitted.values(lmer.lang1), col="blue")

################################################################################

lmer.lang2 <- glmer(lang2 ~ (1|p26)+factor(p4)+p24+p25,
                   data=dat07.lang, family=Gamma(link="inverse"))
summary(lmer.lang2)
plot(lmer.lang2)

plot(dat07.lang[,24],lang2)
points(dat07.lang[,24],fitted.values(lmer.lang2), col="blue")

plot(lang2, type = "l")
lines(fitted.values(lmer.lang2), col="red")

################################################################################

lmer.lang3 <- glmer(lang2 ~ factor(p2)*p3+factor(p4)+
                     (1|when.learned/fam.back), data=dat07.lang, family=Gamma(link="inverse"))
summary(lmer.lang3)
plot(lmer.lang3)

plot(dat07.lang[,24],lang2)
points(dat07.lang[,24],fitted.values(lmer.lang3), col="blue")

plot(lang2, type = "l")
lines(fitted.values(lmer.lang3), col="red")

################################################################################

lmer.lang4 <- glmer(lang2 ~ factor(p2)*p3+factor(p4)+
                     (1|when.learned), data=dat07.lang,
                    family=Gamma(link="inverse"))
summary(lmer.lang4)
plot(lmer.lang4)

plot(dat07.lang[,24],lang2)
points(dat07.lang[,24],fitted.values(lmer.lang4), col="blue")

plot(lang2, type = "l")
lines(fitted.values(lmer.lang4), col="red")

################################################################################

lmer.lang5 <- glmer(lang2 ~ factor(p2)*p3+factor(p4)+p23+p24+
                     (1|when.learned), data=dat07.lang,
                    family=Gamma(link="inverse"))
summary(lmer.lang5)
plot(lmer.lang5)

plot(dat07.lang[,24],lang2)
points(dat07.lang[,24],fitted.values(lmer.lang5), col="blue")

plot(lang2, type = "l")
lines(fitted.values(lmer.lang5), col="red")

################################################################################

lmer.lang6 <- glmer(lang2 ~ factor(p2)*p3+factor(p4)+
                     (1|fam.back), data=dat07.lang,
                    family=Gamma(link="inverse"))
summary(lmer.lang6)
plot(lmer.lang6)

plot(dat07.lang[,24],lang2)
points(dat07.lang[,24],fitted.values(lmer.lang6), col="blue")

plot(lang2, type = "l")
lines(fitted.values(lmer.lang6), col="red")

################################################################################

anova(lmer.lang1, lmer.lang2, lmer.lang3, 
      lmer.lang4, lmer.lang5, lmer.lang6)

anova(lmer.lang1, lmer.lang2, lmer.lang3, lmer.lang4, 
      lmer.lang5, lmer.lang6, refit = FALSE)

anova(lmer.lang4, lmer.lang5, test="Chisq")

################################################################################

anova(glm.lang01, lmer.lang4, test="Chisq")

# AIC
# glm.lang01 has the smaller value of AIC
AIC(glm.lang01)
AIC(lmer.lang4)

# BIC
# glm.lang01 has the smaller value of BIC
BIC(glm.lang01)
BIC(lmer.lang4)

confint(glm.lang01)

################################################################################
