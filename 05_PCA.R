
# Principal component analysis (PCA)

# import package
require(ggfortify) # for PCA

################################################################################

# define the column index vector for each area of cognition
col.lang <- 6:8
col.sc <- 9:18
col.ef <- 27:43

################################################################################

# PCA for language (with S - covariance matrix)
dat06.lang <- dat06[,col.lang]
s.lang <- cov(dat06.lang)
lang.eigen <- eigen(s.lang)
for (e in lang.eigen$values) {
  print(e/ sum(lang.eigen$values))
}
lang.eigen$vectors
plot(lang.eigen$values, xlab = 'Eigenvalue Number', 
     ylab = 'Eigenvalue Size', main = 'Scree Graph\n with covariance matrix')
lines(lang.eigen$values)

################################################################################

# PCA for social cognition (with S - covariance matrix)
variable.names(dat06.sc)
dat06.sc <- dat06[,col.sc]
s.sc <- cov(dat06.sc)
sc.eigen <- eigen(s.sc)
for (e in sc.eigen$values) {
  print(e/ sum(sc.eigen$values))
}
sc.eigen$vectors
plot(sc.eigen$values, xlab = 'Eigenvalue Number', 
     ylab = 'Eigenvalue Size', main = 'Scree Graph\n with covariance matrix')
lines(sc.eigen$values)

################################################################################

# PCA for EF (with S - covariance matrix)
variable.names(dat06.ef)
dat06.ef <- dat06[ ,col.ef]
s.ef <- cov(dat06.ef)
ef.eigen <- eigen(s.ef)
for (e in ef.eigen$values) {
  print(e/ sum(ef.eigen$values))
}
ef.eigen$vectors
plot(ef.eigen$values, xlab = 'Eigenvalue Number', 
     ylab = 'Eigenvalue Size', main = 'Scree Graph\n with covariance matrix')
lines(ef.eigen$values)

################################################################################

# PCA with the correlation matrix, R
# https://aaronschlegel.me/principal-component-analysis-r-example.html
# Cases where using R may be preferable to S include data 
# that is measured in different units or has wide variances. 

# let's perform pca with correlation matrix

################################################################################

# PCA for language (with R - correlation matrix)
r.lang <- cor(dat06.lang)
r.eigen.lang <- eigen(r.lang)
for (r in r.eigen.lang$values) {
  print(r/ sum(r.eigen.lang$values))
}
r.eigen.lang$vectors
pplot(r.eigen.lang$values, xlab = 'Eigenvalue Number', 
     ylab = 'Eigenvalue Size', main = 'Scree Graph\n with correlation matrix')
lines(r.eigen.lang$values)

################################################################################

# PCA for SC (with R - correlation matrix)
r.sc <- cor(dat06.sc)
r.eigen.sc <- eigen(r.sc)
for (r in r.eigen.sc$values) {
  print(r/ sum(r.eigen.sc$values))
}
r.eigen.sc$vectors
plot(r.eigen.sc$values, xlab = 'Eigenvalue Number', 
     ylab = 'Eigenvalue Size', main = 'Scree Graph\n with correlation matrix')
lines(r.eigen.sc$values)

################################################################################

# PCA for EF (with R - correlation matrix)
r.ef <- cor(dat06.ef)
r.eigen.ef <- eigen(r.ef)
for (r in r.eigen.ef$values) {
  print(r/ sum(r.eigen.ef$values))
}
r.eigen.ef$vectors
plot(r.eigen.ef$values, xlab = 'Eigenvalue Number', 
     ylab = 'Eigenvalue Size', main = 'Scree Graph\n with correlation matrix')
lines(r.eigen.ef$values)

################################################################################
