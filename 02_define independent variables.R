
# define independent variables
dat04 <- dat03

################################################################################

# what if the sum of first and second languages input/ exposure != 100?
# We define the input(enlgish+non) = output(english+non) = 100.
# As the rows showing the same errors, we could expect these to be
# "non-sampling error"
# https://www.displayr.com/what-is-non-sampling-error/
# we add half of the deficit to each english and non-english values.

################################################################################

# input
# input.dif : if elements in this vector are not equal to 0,
# 100 - (sum of input of eng + non-eng) != 0
input.dif <- (rep(100,n.row)-(dat04[,19]+dat04[,20]))
ind.in.dif <- which(input.dif != 0)
# sum of input of home and english != 100
# just run once
dat04[ind.in.dif,19:20] <- dat04[ind.in.dif,19:20]+input.dif[ind.in.dif]/2

################################################################################

# output
output.dif <- (rep(100,n.row)-(dat04[,21]+dat04[,22]))
ind.out.dif <- which(output.dif != 0)
# 100 - (sum of output of home and english) != 100
# just run once
dat04[ind.out.dif,21:22] <- dat04[ind.out.dif,21:22]+output.dif[ind.out.dif]/2

################################################################################

# with the values adjusted, 
# we calculate the total_input, total_output again.

################################################################################

# input
tot.in <- numeric(n.row)
for (i in 1:n.row){
  tot.in[i] <- min(dat04[i,19:20])*2
}
dat04[,23] <- tot.in

# output
tot.out <- numeric(n.row)
for (i in 1:n.row){
  tot.out[i] <- min(dat04[i,21:22])*2
}
dat04[,24] <- tot.out

################################################################################

# the subjects are all exposed to second language, which is not english.
# however, some children just hear but do not speak second language.

row.0.23 <- which(dat04[,23] == 0) # none
row.0.24 <- which(dat04[,24] == 0) # 9 - 36 37 52 57 58 65 79 80 86
# english only speaker exposed to second language hearing

################################################################################

# Box plot
# gender, diagnosis, age_acquisition, where_english

# To see the overall shape of the total_input by factor variables
# boxplot(dat04[,23]~dat04[,2])
# boxplot(dat04[,23]~dat04[,4])
boxplot(dat04[,23]~dat04[,25],
        xlab="when child learned 2nd language", ylab="total input")
boxplot(dat04[,23]~dat04[,26],
        xlab="where child learned 2nd language", ylab="total input")

# To see the overall shape of the total_ouput by factor variables
# boxplot(dat04[,24]~dat04[,2])
# boxplot(dat04[,24]~dat04[,4])
boxplot(dat04[,24]~dat04[,25],
        xlab="when child learned 2nd language", ylab="total output")
boxplot(dat04[,24]~dat04[,26],
        xlab="where child learned 2nd language", ylab="total output")

################################################################################

# Bilingual level

# Bilingualism could be divided into 2 categories
# simultaneous bilinguals will be more proficient in their non-dominant language
# 1. simultaneous: acquired second language before 3yrs
# 85 = 50 (nt) + 35(asd)
# 2. sequential: acquired second language after 3yrs
# 4 = 1 (nt) + 3 (asd)
ind.sim <- which(dat04[,25] <= 3)
ind.seq <- which(dat04[,25] > 3)
when.learned <- numeric(n.row)
when.learned[ind.sim] <- "simultaneous"
when.learned[-ind.sim] <- "sequential"

## input (hearing)
## [0,40): low likely to be bilingual - 28
## [40, 80): slow pace bilingual - 40
## [80, 100): more fluent for non-dominant language - 21
# ind.input.40 <- which(dat04[,23] < 40)
# ind.input.80 <- which(dat04[,23] < 80 & dat04[,23] >= 40)
# ind.input.100 <- which(dat04[,23] >= 80)
# spd.lang <- numeric(n.row)
# spd.lang[ind.input.40] <- "low likely"
# spd.lang[ind.input.80] <- "slow"
# spd.lang[ind.input.100] <- "fast"

# where english (first language was taught)
# ind.eng: english speaking family = 64
# ind.non.eng: non-english speaking family = 25
ind.eng <- which(dat04[,26] == 1) 
ind.non.eng <- which(dat04[,26] != 1) 
fam.back <- numeric(n.row) # family background
fam.back[ind.eng] <- "English"
fam.back[ind.non.eng] <- "non-English"

################################################################################

# we should consider 
# total_input, total_output, age_acquisition, where_english

# fam.back <- where_english: dominant language for family

# spd.lang <- total_input: from the reference (Myriam et al)
# categorize the speed of 2nd language acquisition (fast, slow, low-likely)

# when.learned <- age_acquisition: from the reference (Myriam et al)
# categorize how simultaneously children learned english and 2nd language

################################################################################
