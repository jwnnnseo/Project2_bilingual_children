
# include factor variables
dat07 <- data.frame(dat05, when.learned, fam.back)
dat07[,2] <- as.character(dat07[,2])
dat07[,4] <- as.character(dat07[,4])
dat07[,26] <- as.character(dat07[,26])

################################################################################

# import needed package
require(jtools) # for summ() function
require(interactions) # for interact_plot() function
require(ggplot2)

################################################################################

# language
# p6 - bpvs_raw
# p7 - vocabprocess_processing_sped_target
# p8 - wasi_sum_rawscores
# run glm without missing data for language skill test
# nt(45) + aut(33)
lang.mis <- numeric(0)
for (i in 6:8){
  lang.mis <- c(lang.mis, row.mis.ls[[i]])
}

length(which(dat07.lang[,4] == "1"))
# number of autistic subjects used for language analysis
length(which(dat07.lang[,4] == "0"))
# number of neurotypical subjects used for language analysis

# dat07.lang: df without missing on dat05
dat07.lang <- dat07[-lang.mis,]

# result variable to be used for glm
lang1 <- dat07.lang[,6]+dat07.lang[,7]+dat07.lang[,8]
log.lang1 <- log(lang1)-min(log(lang1))
lang2 <- dat07.lang[,6]*dat07.lang[,8]/dat07.lang[,7]
log.lang2 <- log(lang2)

################################################################################

# mean
glm.lang00 <- glm(lang2 ~ 1, family=Gamma(link="inverse"))
summary(glm.lang00)
summ(glm.lang00)

################################################################################

# fixed effect
# with factor variable
# age(p3), diagnosis(p4)
glm.lang01 <- glm(lang2 ~ factor(p2)*p3+factor(p4),
                  data=dat07.lang, family=Gamma(link="inverse"))
summary(glm.lang01)
summ(glm.lang01)
# plot(glm.lang01)

plot(lang2, type = "l")
lines(fitted.values(glm.lang01), col="red")

plot(dat07.lang[,3],lang2, 
     xlab="age", ylab="language skill index",
     main="Fixed effect approximation \nwith age and diagnosis",
     ylim=c(0,5))
points(dat07.lang[,3],fitted.values(glm.lang01), col="blue")
legend("topleft", c("given data","fitted values"), fill=c("black","blue"))

plot(fitted.values(glm.lang01), residuals(glm.lang01),
     xlab="fitted values", ylab="residuals")
abline(h=0, col="red")

################################################################################

# fixed effect model
# with basic subject information + input, output
glm.lang02 <- glm(lang2 ~ factor(p2)*p3+factor(p4)+p23+p24,
                  data=dat07.lang, family = Gamma(link="inverse"))
summary(glm.lang02)
summ(glm.lang02)
# plot(glm.lang02)

plot(lang2, type = "l")
lines(fitted.values(glm.lang02), col="red")

plot(dat07.lang[,3],lang2, 
     xlab="age", ylab="language skill index",
     main="Fixed effect approximation \nwith basic subject information + input&output")
points(dat07.lang[,3],fitted.values(glm.lang02), col="blue")
legend("topleft", c("given data","fitted values"), fill=c("black","blue"))

plot(fitted.values(glm.lang02), residuals(glm.lang02),
     xlab="fitted values", ylab="residuals")
abline(h=0, col="red")

################################################################################

# fixed with every variable
glm.lang03 <- glm(lang2 ~ factor(p2)*p3+factor(p4)+p23+p24+p25+factor(p26),
                  data=dat07.lang, family = Gamma(link="inverse"))
summary(glm.lang03)
summ(glm.lang03)

################################################################################

# fixed effect model
# with basic subject information + where_english and when they learned
glm.lang04 <- glm(lang2 ~ factor(p2)*p3+factor(p4)+p25+factor(p26),
                  data=dat07.lang, family = Gamma(link="inverse"))
summary(glm.lang04)
summ(glm.lang04)
# plot(glm.lang02)

plot(lang2, type = "l")
lines(fitted.values(glm.lang04), col="red")

plot(dat07.lang[,3],lang2, 
     xlab="age", ylab="language skill index",
     main="Fixed effect approximation \nwith basic subject information + input&output")
points(dat07.lang[,3],fitted.values(glm.lang04), col="blue")
legend("topleft", c("given data","fitted values"), fill=c("black","blue"))

plot(fitted.values(glm.lang04), residuals(glm.lang04),
     xlab="fitted values", ylab="residuals")
abline(h=0, col="red")

################################################################################

# lang.ls <- list(fitted1=as.numeric(fitted.values(glm.lang01)),
#                 fitted2=as.numeric(fitted.values(glm.lang02)),
#                 fitted3=as.numeric(fitted.values(glm.lang03)),
#                 fitted4=as.numeric(fitted.values(glm.lang04)),
#                 raw_data=lang2,
#                 resid1=lang2-lang.ls$fitted1,
#                 resid2=lang2-lang.ls$fitted2,
#                 resid3=lang2-lang.ls$fitted3,
#                 resid4=lang2-lang.ls$fitted4)

anova(glm.lang00, glm.lang01, glm.lang02, glm.lang03, glm.lang04, test = "LRT")

################################################################################

# we run the anova test to compare the goodness of fits of models.
# it turns out to be more effective to use models without exploratory variables 
# about bilingualism.
# finally we could make conclusion that whether the children are bilingual or not
# does not really influence the level of language ability of each entities.

################################################################################

# interaction plot - p2 and p3
interact_plot(glm.lang01, pred=p3, modx=p2)

