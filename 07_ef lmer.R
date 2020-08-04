
require("lme4") # for linear mixed effect
# install.packages("arm") # for display function
require(arm)

################################################################################

lmer.ef1 <- glmer(ef1 ~ (1|when.learned/fam.back/p4)+factor(p2)*p3,
                  data=dat07.ef1, family=poisson(link="log"))
summary(lmer.ef1)
summ(lmer.ef1)
plot(lmer.ef1)

plot(x=ef1, y=residuals(lmer.ef1),
     pch=2*as.numeric(dat07.ef1[,4]), 
     col=c(rep("blue",48), rep("red",34)))
legend(x=60, y=5, c("TD","ASD"), 
       col=c("blue","red"), pch=c(0,2))

################################################################################

lmer.ef1.1 <- glmer(ef1 ~ (1|when.learned/fam.back/p4)+factor(p2)*p3+p23+p24,
                  data=dat07.ef1, family=poisson(link="log"))
summary(lmer.ef1.1)
plot(lmer.ef1.1)

plot(x=ef1, y=residuals(lmer.ef1.1),
     pch=2*as.numeric(dat07.ef1[,4]), 
     col=c(rep("blue",48), rep("red",34)))
legend(x=60, y=5, c("TD","ASD"), 
       col=c("blue","red"), pch=c(0,2))

################################################################################

lmer.ef2 <- glmer(ef1 ~ (1|when.learned/fam.back)+factor(p2)*p3+factor(p4), 
                  data=dat07.ef1, family=poisson(link="log"))
summary(lmer.ef2)
plot(lmer.ef2)

plot(x=ef1, y=residuals(lmer.ef2))

################################################################################

lmer.ef2.1 <- glmer(ef1 ~ (1|when.learned/fam.back)+factor(p2)*p3+factor(p4)+
                      p23+p24, data=dat07.ef1, family=poisson(link="log"))
summary(lmer.ef2.1)
plot(lmer.ef2.1)

plot(x=ef1, y=residuals(lmer.ef2.1))

################################################################################

lmer.ef3 <- glmer(ef1 ~ (1|when.learned/p4)+factor(p2)*p3, 
                  data=dat07.ef1, family=poisson(link="log"))
summary(lmer.ef3)
plot(lmer.ef3)

plot(x=ef1, y=residuals(lmer.ef3))

################################################################################

lmer.ef3.1 <- glmer(ef1 ~ (1|when.learned/p4)+factor(p2)*p3+p23+p24, 
                  data=dat07.ef1, family=poisson(link="log"))
summary(lmer.ef3.1)
plot(lmer.ef3.1)

plot(x=ef1, y=residuals(lmer.ef3.1))

################################################################################

lmer.ef4 <- glmer(ef1 ~ (1|p4/fam.back)+factor(p2)*p3, 
                  data=dat07.ef1, family=poisson(link="log"))
summary(lmer.ef4)
plot(lmer.ef4)

plot(x=ef1, y=residuals(lmer.ef4))

################################################################################

lmer.ef4.1 <- glmer(ef1 ~ (1|p4/fam.back)+factor(p2)*p3+p23+p24, 
                  data=dat07.ef1, family=poisson(link="log"))
summary(lmer.ef4.1)
plot(lmer.ef4.1)

plot(x=ef1, y=residuals(lmer.ef4.1))

################################################################################

lmer.ef5 <- glmer(ef1 ~ (1|when.learned)+factor(p2)*p3+factor(p4), 
                  data=dat07.ef1, family=poisson(link="log"))
summary(lmer.ef5)
plot(lmer.ef5)

plot(x=ef1, y=residuals(lmer.ef5))

################################################################################

lmer.ef5.1 <- glmer(ef1 ~ (1|when.learned)+factor(p2)*p3+factor(p4)+p23+p24, 
                    data=dat07.ef1, family=poisson(link="log"))
summary(lmer.ef5.1)
plot(lmer.ef5.1)

plot(x=ef1, y=residuals(lmer.ef5.1))

################################################################################

lmer.ef6 <- glmer(ef1 ~ (1|fam.back)+factor(p2)*p3+factor(p4), 
                  data=dat07.ef1, family=poisson(link="log"))
summary(lmer.ef6)
plot(lmer.ef6)

plot(x=ef1, y=residuals(lmer.ef6))

################################################################################

lmer.ef6.1 <- glmer(ef1 ~ (1|fam.back)+factor(p2)*p3+factor(p4)+p23+p24,
                    data=dat07.ef1, family=poisson(link="log"))
summary(lmer.ef6.1)
plot(lmer.ef6.1)

plot(x=ef1, y=residuals(lmer.ef6.1))

################################################################################

lmer.ef7 <- glmer(ef1 ~ (1|p4)+factor(p2)*p3,
                    data=dat07.ef1, family=poisson(link="log"))
summary(lmer.ef7)
plot(lmer.ef7)

plot(x=ef1, y=residuals(lmer.ef7))

################################################################################

lmer.ef7.1 <- glmer(ef1 ~ (1|p4)+factor(p2)*p3+p23+p24,
                    data=dat07.ef1, family=poisson(link="log"))
summary(lmer.ef7.1)
plot(lmer.ef7.1)

plot(x=ef1, y=residuals(lmer.ef7.1))

################################################################################

anova(lmer.ef1, lmer.ef1.1, 
      lmer.ef2, lmer.ef2.1,
      lmer.ef3, lmer.ef3.1,
      lmer.ef4, lmer.ef4.1,
      lmer.ef5, lmer.ef5.1,
      lmer.ef6, lmer.ef6.1,
      lmer.ef7, lmer.ef7.1, test="LRT")

anova(lmer.ef2, lmer.ef2.1, 
      lmer.ef4, lmer.ef4.1,
      lmer.ef5, lmer.ef5.1,
      test="LRT")

anova(lmer.ef5, lmer.ef5.1, test="LRT")

AIC(lmer.ef5, lmer.ef5.1)
BIC(lmer.ef5, lmer.ef5.1)

################################################################################

anova(lmer.ef5, glm.ef01, test="LRT")
AIC(lmer.ef5, glm.ef01)
BIC(lmer.ef5, glm.ef01)
confint(glm.ef01)
