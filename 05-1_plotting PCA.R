
# import packages needed

library(devtools) # visualize the contribution of each variables
devtools::install_github("vqv/ggbiplot", force = TRUE)
library(ggbiplot) # visualize pca
require(ggfortify)

################################################################################

# PCA for language with correlation
# compute principal components
lang.pca.scaled <- prcomp(dat06.lang, center=TRUE, scale.=TRUE)
summary(lang.pca.scaled)
str(lang.pca.scaled)

# plotting PCA - biplot
dat06.diag <- numeric(nrow(dat06))
dat06.diag[dat06[,4]==0] <- "nt"
dat06.diag[dat06[,4]==1] <- "aut"
ggbiplot(lang.pca.scaled)
ggbiplot(lang.pca.scaled, 
         labels=rownames(dat06.lang)) # with row number

# interpreting the results
ggbiplot(lang.pca.scaled, ellipse=TRUE, 
         labels=rownames(dat06.lang),
         groups=dat06.diag,
         obs.scale=1, var.scale=1)

################################################################################

# PCA for social cognition with correlation
# compute principal components
sc.pca.scaled <- prcomp(dat06.sc, center=TRUE, scale.=TRUE)
summary(sc.pca.scaled)
str(sc.pca.scaled)

# plotting PCA - biplot
ggbiplot(sc.pca.scaled, ellipse=TRUE, 
         labels=rownames(dat06.lang),
         groups=dat06.diag,
         obs.scale=1, var.scale=1)

################################################################################

# PCA for EF (Executive Function) with correlation
# compute principal components
ef.pca.scaled <- prcomp(dat06.ef, center=TRUE, scale.=TRUE)
summary(ef.pca.scaled)
str(ef.pca.scaled)

# plotting PCA - biplot
ggbiplot(ef.pca.scaled, ellipse=TRUE, 
         labels=rownames(dat06.lang),
         groups=dat06.diag,
         obs.scale=1, var.scale=1)

################################################################################

# To perform PCA with covariance, 
# run the code with center and scale argument