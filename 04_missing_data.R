
# missing data
dat05 <- dat04

# to detect the difference of bilingual level between the group of where_english
boxplot((dat05[,23]+dat05[,24])~dat05[,26], 
        xlab = "where english learned",
        ylab = "bilingual level")

# to detect the difference of age_acquisition between where_english learned
boxplot(dat05[,25]~dat05[,26],
        xlab = "where english learned",
        ylab = "when english learned")

# ind.eng.late: from english speaking family background but learned english late
# reconsider whether to use the where_english or not to guess the family background
ind.eng.late <- which(dat05[,25] > 0 & dat05[,26] == 1)

################################################################################

# From the result to detect missing data on script 01_file_import
# we could get row.mis.len (number of missing for each variable)
# and row.mis.ls (list of index for missing data)

# As the scale of the given data set is not big enough to ignore the omission
# of each variables, we should compute imputation for each missing.

# For the missing on the SCQ(5th, 38) variables, we could ignore them.
# SCQ is used to measure autistic traits.
# diagnosis can be appropriate substitute for the absence.

################################################################################

# let's see how many rows we would miss if we delete all rows with missing
# except the SCQ which was used to measure the extent of autism and can be replaced with diagnosis
# the total number of rows with missing data are 43
# neurotypical -> 51-22 = 29
# autistic -> 38-21 = 18

# all.row.mis (43): vector of rows with at least one missing values
# all.row.ful (46): vector of rows with full values for every variable
all.row.mis <- numeric(0)
for (i in 7:n.col){
  all.row.mis <- c(all.row.mis, row.mis.ls[[i]])
}
all.row.mis <- as.numeric(dimnames(table(all.row.mis))[[1]])
y <- 1:n.row
all.row.ful <- y[!y %in% all.row.mis]

# all.row.ful.aut: index vector without missing for autism
# all.row.ful.nt: index vector without missing for nt
all.row.ful.aut <- all.row.ful[all.row.ful > 51]
all.row.ful.nt <- all.row.ful[all.row.ful <= 51]

################################################################################

# dat06: without missing data except for variable p5
dat06 <- dat05[all.row.ful,]
dat06.nt <- dat05[all.row.ful.nt,] # 29
dat06.aut <- dat05[all.row.ful.aut,] # 17

################################################################################
