
# EDA (exploratory data analysis)

################################################################################

# by autism - diagnosis (4)

# index for autism and neurotypical
# neurotypical : 1-51 (0)
# autistic : 52-89 (1)
ind.nt <- which(dat04[,4] == 0)
ind.aut <- which(dat04[,4] == 1)

gr.aut <- numeric(n.row)
gr.aut[ind.aut] <- "Autistic"
gr.aut[ind.nt] <- "Neurotypical"

# import packages
require(ggplot2) # boxplot + correlation plot
require(tidyverse) # correlation plot
require(GGally) # correlation plot

# boxplot

# create a data frame for boxplot
aut.box.df <- data.frame(diag = gr.aut, 
                         dat04[,c(6:8, 9:18, 27:43)])

################################################################################

# diag - bpvs_raw
ggplot(aut.box.df, aes(x=diag, y=p6))+geom_boxplot()

# diag - vocabprocess_processing_speed_target
ggplot(aut.box.df, aes(x=diag, y=p7))+geom_boxplot()

# diag - wasi_sum_rawscores
ggplot(aut.box.df, aes(x=diag, y=p8))+geom_boxplot()

# diag - tomi_early
ggplot(aut.box.df, aes(x=diag, y=p9))+geom_boxplot()

# diag - tomi_basic
ggplot(aut.box.df, aes(x=diag, y=p10))+geom_boxplot()

# diag - tomi_advanced
ggplot(aut.box.df, aes(x=diag, y=p11))+geom_boxplot()

# diag - tomi_compmean
ggplot(aut.box.df, aes(x=diag, y=p12))+geom_boxplot()

# diag - tom_tb_totalscore
ggplot(aut.box.df, aes(x=diag, y=p13))+geom_boxplot()

# diag - et_figurestask_dwell_time_interacting
ggplot(aut.box.df, aes(x=diag, y=p14))+geom_boxplot()

# diag - et_figurestask_dwell_time_not_interacting
ggplot(aut.box.df, aes(x=diag, y=p15))+geom_boxplot()

# diag - et_falsebelief_Testtrial_dwell_time_to_correct
ggplot(aut.box.df, aes(x=diag, y=p16))+geom_boxplot()

# diag - et_falsebelief_testtrial_dwell_time_to_incorrect
ggplot(aut.box.df, aes(x=diag, y=p17))+geom_boxplot()

# diag - et_falsebelief_testtrial_preference_score
ggplot(aut.box.df, aes(x=diag, y=p18))+geom_boxplot()

# diag - brief_raw_inhibit
ggplot(aut.box.df, aes(x=diag, y=p27))+geom_boxplot()

# diag - brief_raw_self.monitor
ggplot(aut.box.df, aes(x=diag, y=p28))+geom_boxplot()

# diag - brief_raw_shift
ggplot(aut.box.df, aes(x=diag, y=p29))+geom_boxplot()

# diag - brief_raw_emotional_control
ggplot(aut.box.df, aes(x=diag, y=p30))+geom_boxplot()

# diag - brief_raw_initiate
ggplot(aut.box.df, aes(x=diag, y=p31))+geom_boxplot()

# diag - brief_raw_working_memory
ggplot(aut.box.df, aes(x=diag, y=p32))+geom_boxplot()

# diag - brief_raw_plan_organise
ggplot(aut.box.df, aes(x=diag, y=p33))+geom_boxplot()

# diag - brief_raw_task_monitor
ggplot(aut.box.df, aes(x=diag, y=p34))+geom_boxplot()

# diag - brief_raw_organisation_of_materials
ggplot(aut.box.df, aes(x=diag, y=p35))+geom_boxplot()

# diag - flanker_percenterrors_congruent
ggplot(aut.box.df, aes(x=diag, y=p36))+geom_boxplot()

# diag - flanker_percenterrors_incongruent
ggplot(aut.box.df, aes(x=diag, y=p37))+geom_boxplot()

# diag - flanker_mean_rt_congruent
ggplot(aut.box.df, aes(x=diag, y=p38))+geom_boxplot()

# diag - flanker_mean_rt_incongruent
ggplot(aut.box.df, aes(x=diag, y=p39))+geom_boxplot()

# diag - pvt_mean_rt
ggplot(aut.box.df, aes(x=diag, y=p40))+geom_boxplot()

# diag - pvt_number_of_lapses
ggplot(aut.box.df, aes(x=diag, y=p41))+geom_boxplot()

# diag - pvt_mean_lapse_rt
ggplot(aut.box.df, aes(x=diag, y=p42))+geom_boxplot()

# diag - pvt_count_falsestarts
ggplot(aut.box.df, aes(x=diag, y=p43))+geom_boxplot()

################################################################################

# correlation plot
# https://briatte.github.io/ggcorr/

# Cognitive outcomes

# Language - 6:8
ggcorr(dat04[,6:8], label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE)
ggcorr(dat04[,6:8], label = TRUE)

# Social cognition - 9:18
ggcorr(dat04[,9:18], label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE)
ggcorr(dat04[,9:18], label = TRUE)

# Executive function (EF) - 27:43 
ggcorr(dat04[,27:43], label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE)
ggcorr(dat04[,27:43], label = TRUE)

################################################################################

# correlation plot

# Assessment type (Codebook)

# eye tracking (direct but non-verbal) - 7, 14:18
col.eye <- c(7,14:18)
ggcorr(dat04[,col.eye], label = TRUE)

# parents' report (indirect verbal) - 9:12, 19:35
col.par.rep <- c(9:12,19:35)
ggcorr(dat04[,col.par.rep], label = TRUE, label_size = 2, label_round = 1, label_alpha = TRUE)

# experimenter (direct verbal) - 6, 8, 13
# computer - 36:43
col.exp <- c(6,8,13,36:43)
ggcorr(dat04[,col.exp], label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE)

################################################################################

# Measurement type (KO Slides)

# Direct assessment - 6, 8, 36:43
col.dir <- c(6,8,36:43)
ggcorr(dat04[,col.dir], label = TRUE)

# Eye-tracking - 7, 14:18
ggcorr(dat04[,col.eye], label = TRUE)

# Parent reports - 9:12, 27:35
col.par.rep2 <- c(9:12, 27:35)
ggcorr(dat04[,col.par.rep2], label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE)

################################################################################

# Area of cognition

# language+IQ - 6:8
col.lang <- 6:8
ggcorr(dat04[,col.lang], label = TRUE, label_round = 2)

# EF - 27:43
col.ef <- 27:43
ggcorr(dat04[,col.ef], label = TRUE, label_size = 2, label_round = 2, label_alpha = TRUE)

# Social cognition - 9:18
col.sc <- 9:18
ggcorr(dat04[,col.sc], label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE)


