library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(rstatix)

#load data
PSdat <- read_csv("Exp1_raw.csv")
#PSdat <- read_csv("Exp2_raw.csv")
summary(PSdat)

#subset columns of interest
psclean <- PSdat[c(1,4,5,11,13:17,27,28,30,31,33,34,38)]   #use this for Exp1
#psclean <- PSdat[c(1,4,5,11,13:17,27,28,30:33,37)]  #use this for Exp2


#rename columns (*s in original column names are problematic)
colnames(psclean) <- c("Username","Session", "Trial", "Condition", "StimPainting",
                 "PainterAnswer", "StimContext", "ContextAnswer",
                 "ContextCategory", "ExpPhase", "ArtistNum",
                 "Schedule", "ShuffleBlock", "RT", "Response", "Accuracy")

#remove unnecessary rows (instruction trials, etc)
psclean <- subset(psclean, StimPainting!="NA")

#subset different tests
training <- subset(psclean, ExpPhase=="Study")
gentest <- subset(psclean, ExpPhase=="Generalization")
rectest <- subset(psclean, ExpPhase=="ContextRec")
genrectest <- subset(psclean, ExpPhase=="ArtistRec")


#####CREATE FILTER LOG#####
#Create Filter Log
filterLog <- psclean %>%
  group_by(Username, Condition, ExpPhase, Session) %>%
  summarize(totalAcc = mean(Accuracy))
filterLog <- subset(filterLog, Session ==1)
filterLog <- filterLog[c(1,2,3,5)]
filter_wide <- spread(filterLog[c(1,2,3,4)], ExpPhase, totalAcc)
filter_wide$GenFil <-ifelse(filter_wide$Generalization < .15, 1, 0)
filter_wide$RecFil <-ifelse(filter_wide$ContextRec < 0.27, 1, 0)
filter_wide$GenRecFil <- ifelse(filter_wide$ArtistRec < 0.17, 1, 0)
colnames(filter_wide) <- c("Username","Condition", "S1GenRec", "S1Rec", "S1Gen",
                       "Study", "GenFilter", "RecFilter",
                       "GenRecFilter")
write.table(filter_wide, "FilterLog.csv", row.names=F, sep=",")

filter_widews <- read_csv("FilterLog.csv")

#####All tests together#####
all_avg <- psclean %>%
  group_by(Username, ExpPhase, Session, Schedule) %>%
  summarise(acc_avg = mean(Accuracy))

all_avg$ExpPhase <- gsub("ContextRec", "Detailed Recognition",all_avg$ExpPhase)
all_avg$ExpPhase <- gsub("ArtistRec", "General Recognition",all_avg$ExpPhase)
all_avg$ExpPhase <- gsub("Study", "Training",all_avg$ExpPhase)

#Remove training accuracy
full_results <- subset(all_avg, ExpPhase!="Training")
#add filter indicator columns
full_results <- merge(full_results, filter_widews[,c(1,7:9)], by='Username', all.x=TRUE, all.y=TRUE)
wide_results <- spread(full_results, ExpPhase, acc_avg)


##Replace excluded data points with "NA"
wide_results$Generalization <-ifelse(wide_results$GenFilter == 1, "NA", wide_results$Generalization)
wide_results$`General Recognition` <-ifelse(wide_results$GenRecFilter == 1, "NA", wide_results$`General Recognition`)
wide_results$`Detailed Recognition` <-ifelse(wide_results$RecFilter == 1, "NA", wide_results$`Detailed Recognition`)
filt_results <- gather(wide_results, ExpPhase, acc_avg, `Detailed Recognition`:`Generalization`)
filt_results <- subset(filt_results, acc_avg!="NA")
filt_results$acc_avg <- as.numeric(filt_results$acc_avg)

#####Check Plot#####

summdat <- filt_results %>%
  group_by(ExpPhase, Session, Schedule) %>%
  summarise(acc_avg = mean(acc_avg))

filt_results$ExpPhase<-factor(filt_results$ExpPhase, levels = c("Generalization","General Recognition","Detailed Recognition"))

filt_results %>% 
  ggplot(aes(fill=Schedule, x=as.factor(Session), y=acc_avg, width=.9)) + facet_wrap(~ExpPhase) +
  stat_summary(fun=mean,position=position_dodge(preserve = 'single'),geom="bar",size=3,aes(fill=Schedule), alpha =1) + 
  geom_dotplot(binaxis = "y", stackdir = "center", stackratio=0.7,position = position_dodge(width=.9),dotsize = 0.5, alpha=1, colour="black") + 
  stat_summary(fun.data = "mean_cl_boot", position=position_dodge(width=.9), geom="errorbar", width=0,size=1) +
  scale_x_discrete(labels=c("1"="Session 1", "2"="Session 2")) +
  labs(y="Accuracy", x=NULL) +
  scale_y_continuous(expand=c(0,0), limits=c(0, .95), breaks=c(0.0, 0.2, 0.4, 0.6, 0.8)) +
  theme(legend.position=c(.99,.99), legend.justification=c("right","top"), legend.box.just="right", legend.margin=margin(6,6,6,6),
        text=element_text(size=12, family="HelveticaNeueLT Com 45 Lt"),
        strip.background=element_rect(colour="black", fill=NA),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background=element_rect(fill=NA),
        axis.line=element_line(size=0),
        strip.text.x = element_text(size=12)
  )
  


#####Summary Stats#####

##Generalization by Session x Schedule
gen_avg <- gentest %>%
  group_by(Username, Condition, Session, Schedule) %>%
  summarise(acc_avg = mean(Accuracy))
gen_full <- merge(gen_avg, filter_widews[,c(1,7)], by='Username', all.x=TRUE, all.y=TRUE)
gen_filt <- gen_full[gen_full$GenFilter==0,]

##Recognition by Session x Schedule
rec_avg <- rectest %>%
  group_by(Username, Condition, Session, Schedule) %>%
  summarise(acc_avg = mean(Accuracy))
rec_full <- merge(rec_avg, filter_widews[,c(1,8)], by='Username', all.x=TRUE, all.y=TRUE)
rec_filt <- rec_full[rec_full$RecFilter==0,]

##General Recognition by Session x Schedule
genrec_avg <- genrectest %>%
  group_by(Username, Condition, Session, Schedule) %>%
  summarise(acc_avg = mean(Accuracy))
genrec_full <- merge(genrec_avg, filter_widews[,c(1,9)], by='Username', all.x=TRUE, all.y=TRUE)
genrec_filt <- genrec_full[genrec_full$GenRecFilter==0,]



#####ANOVA#####

#generalization test
gen.aov <- anova_test(data=gen_filt, dv=acc_avg, wid=Username, within=c(Schedule, Session))
get_anova_table(gen.aov)

#recognition test
rec.aov <- anova_test(data=rec_filt, dv=acc_avg, wid=Username, within=c(Schedule, Session))
get_anova_table(rec.aov)

#general recognition test
genrec.aov <- anova_test(data=genrec_filt, dv=acc_avg, wid=Username, within=c(Schedule, Session))
get_anova_table(genrec.aov)
