
# import file & detect missing

################################################################################

# import raw data
rm(list = ls())
require(xlsx)
dat00 <- read.xlsx(file = "D:/1920/Dissertation/Session 2/Data/Raw Data.xlsx", 
                   sheetName = "All Participants")
# head(dat00)
# variable.names(dat00)
n.col <- dim(dat00)[2]
n.row <- dim(dat00)[1]
# summary(dat00)
# str(dat00)

# dat00[54,40] = 11.40.83333 ???
# -> 1140.83333333 = 1140+5/6
dat00[54,40] <- (1140 + 5/6)
dat00$pvt_mean_rt <- as.numeric(dat00$pvt_mean_rt)

################################################################################

# detect missing data for each col
# variables with missing data
# 1:7, 9:38 -> 888
# 8 -> 888, 0

# row.mis.ls: row index with missing for each col
row.mis.ls <- vector(mode = "list", length = n.col)
# row.mis.len: number of missing for each col
row.mis.len <- numeric(n.col)
for (i in 1:n.col){
  if (i == 8){
    row.mis.ls[[i]] <- which(dat00[,i] == 888 | dat00[,i] == 0)
  }else{
    row.mis.ls[[i]] <- which(dat00[,i] == 888)
  }
  row.mis.len[i] <- length(row.mis.ls[[i]])
}

################################################################################

# Assign NA for the missing values
# new data frame = dat01
dat01 <- dat00
for (i in 1:n.col){
  dat01[row.mis.ls[[i]], i] <- NA
}

################################################################################

# as the name for every variable is too long and complicated, 
# we decide to assign p1 - p43: dat02
dat02 <- dat01
var.names <- numeric(n.col)
for (i in 1:n.col){
  var.names[i] <- paste("p",i,sep="")
}
names(dat02) <- var.names

################################################################################
