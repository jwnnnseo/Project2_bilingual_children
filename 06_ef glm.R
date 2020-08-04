
# import package
require(tidyverse) # correlation plot
require(GGally) # correlation plot
require(ggplot2)

################################################################################

# Executvie function
# p27 - p43
# nt(40) + aut(23)
# sample from autistic group is apparently smaller 
# than typically developing (TD) group
mis.func <- function(mis, range1){
  mis <- numeric(0)
  for (i in range1){
    mis <- c(mis, row.mis.ls[[i]])
  }
  mis <- as.numeric(dimnames(table(mis))[[1]])
  return(mis)
}
ef.mis <- mis.func(ef.mis, 27:43)
dat07.ef <- dat07[-ef.mis,]
dim(dat07.ef)
length(which(dat07.ef[,4] == "0")) # Neurotypical
length(which(dat07.ef[,4] == "1")) # Autistic

################################################################################

# However, we could see the significant difference 
# in absence between different tests.
# ef.mis = ef.mis1 (breif) U ef.mis2 (flanker) U ef.mis3 (pvt)

################################################################################

# ef.mis1 (brief)
# nt(48) + aut(34)
ef.mis1 <- mis.func(ef.mis1, 27:35)
dat07.ef1 <- dat07[-ef.mis1,]

# ef.mis2 (flanker)
# nt(48) + aut(28)
ef.mis2 <- mis.func(ef.mis2, 36:39)
dat07.ef2 <- dat07[-ef.mis2,]

# ef.mis3 (pvt)
# nt(44) + aut(26)
ef.mis3 <- mis.func(ef.mis3, 40:43)
dat07.ef3 <- dat07[-ef.mis3,]

################################################################################

# correlation plot
ggcorr(dat07.ef[,27:43], label = TRUE, 
       label_size = 3, label_round = 1, label_alpha = TRUE)

# 9 sub scales of BRIEF
# inhibit/ self monitor/ shift/ emotional control (Behavioural Regulation)
# initiate/ working memory/ plan/ task-monitor/ organization of materials (Metacognition)
ef1 <- (dat07.ef1[,27]+dat07.ef1[,28]+dat07.ef1[,29]+dat07.ef1[,30]+
  dat07.ef1[,31]+dat07.ef1[,32]+dat07.ef1[,33]+dat07.ef1[,34]+dat07.ef1[,35])
hist(ef1, nclass=20)
plot(density(ef1))

ef2 <- log(dat07.ef1[,27]*dat07.ef1[,28]*dat07.ef1[,29]*dat07.ef1[,30]*
  dat07.ef1[,31]*dat07.ef1[,32]*dat07.ef1[,33]*dat07.ef1[,34]*dat07.ef1[,35])
hist(ef1, nclass=20)
plot(density(ef2))

################################################################################

glm.ef00 <- glm(ef1 ~ 1)
summary(glm.ef00)
# plot(glm.ef00)
plot(x=ef1, y=residuals(glm.ef00))

################################################################################

glm.ef01 <- glm(ef1~factor(p2)*p3+factor(p4), 
                family=poisson(link="log"), data=dat07.ef1)
summary(glm.ef01)
# plot(glm.ef01)
plot(x=ef1, y=residuals(glm.ef01))
# we can detect 2 different layers of residuals 
# from the result of glm.ef01 -> ???

plot(x=ef1, y=residuals(glm.ef01), 
     pch=2*as.numeric(dat07.ef1[,4]), 
     col=c(rep("blue",48), rep("red",34)))
legend(x=60, y=5, c("TD","ASD"), 
       col=c("blue","red"), pch=c(0,2))


################################################################################

glm.ef02 <- glm(ef1~factor(p2)*p3+factor(p4)+p23+p24,
                family=poisson(link="log"), data=dat07.ef1)
summary(glm.ef02)
plot(glm.ef02)
plot(x=ef1, y=residuals(glm.ef02))

################################################################################

glm.ef03 <- glm(ef1~factor(p2)*p3+factor(p4)+p23+p24+p25+factor(p26), 
                family=poisson(link="log"), data=dat07.ef1)
summary(glm.ef03)
plot(glm.ef02)
plot(x=ef1, y=residuals(glm.ef03))

################################################################################

glm.ef04 <- glm(ef1~factor(p2)*p3+factor(p4)+p25+factor(p26), 
                family=poisson(link="inverse"), data=dat07.ef1)
summary(glm.ef04)
plot(glm.ef04)

plot(x=ef1, y=residuals(glm.ef04), 
     pch=2*as.numeric(dat07.ef1[,4]), 
     col=c(rep("blue",48), rep("red",34)))
legend(x=60, y=5, c("TD","ASD"), 
       col=c("blue","red"), pch=c(0,2))

################################################################################

anova(glm.ef00, glm.ef01, glm.ef02, glm.ef03, glm.ef04, test="LRT")
anova(glm.ef01, glm.ef02, test="LRT") 
# With 5% of significance level, we cannot be sure 
# that the Model2 shows better goodness of fit that Model 1 

AIC(glm.ef01, glm.ef02)
BIC(glm.ef01, glm.ef02)
