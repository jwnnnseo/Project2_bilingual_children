
require("lme4") # for linear mixed effect
# install.packages("arm") # for display function
require(arm)

################################################################################

lmer.sc1 <- glmer(sc1 ~ (1|when.learned/fam.back)+factor(p2)*p3+factor(p4),
                  data=dat07.sc, family=Gamma(link="identity"))
summary(lmer.lang1)
summ(lmer.lang1)
plot(lmer.sc1)

plot(x=sc1, y=residuals(lmer.sc1))

################################################################################

lmer.sc2 <- glmer(sc1 ~ (1|when.learned)+factor(p2)*p3+factor(p4), 
                  data=dat07.sc, family=Gamma(link="identity"))
summary(lmer.sc2)
plot(lmer.sc2)

plot(x=sc1, y=residuals(lmer.sc2))

################################################################################

lmer.sc3 <- glmer(sc1 ~ (1|fam.back)+factor(p2)*p3+factor(p4), 
                  data=dat07.sc, family=Gamma(link="identity"))
summary(lmer.sc3)
plot(lmer.sc3)

plot(x=sc1, y=residuals(lmer.sc3))

################################################################################

lmer.sc4 <- glmer(sc1 ~ (1|when.learned/fam.back)+factor(p2)*p3+factor(p4)+p23+p24, 
                  data=dat07.sc, family=Gamma(link="identity"))
summary(lmer.sc4)
plot(lmer.sc4)

plot(x=sc1, y=residuals(lmer.sc4))

################################################################################

lmer.sc5 <- glmer(sc1 ~ (1|when.learned)+factor(p2)*p3+factor(p4)+p23+p24, 
                  data=dat07.sc, family=Gamma(link="identity"))
summary(lmer.sc5)
plot(lmer.sc5)

plot(x=sc1, y=residuals(lmer.sc5))

################################################################################

lmer.sc6 <- glmer(sc1 ~ (1|fam.back)+factor(p2)*p3+factor(p4)+p23+p24, 
                  data=dat07.sc, family=Gamma(link="identity"))
summary(lmer.sc6)
plot(lmer.sc6)

plot(x=sc1, y=residuals(lmer.sc6))

################################################################################

anova(lmer.sc1, lmer.sc2, lmer.sc3, 
      lmer.sc4, lmer.sc5, lmer.sc6, test="LRT")
anova(lmer.sc3, lmer.sc6, test="LRT")

AIC(lmer.sc3)
AIC(lmer.sc6)

BIC(lmer.sc3)
BIC(lmer.sc6)

confint(lmer.sc3)
confint(lmer.sc6)