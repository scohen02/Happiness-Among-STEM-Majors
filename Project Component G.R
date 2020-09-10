load("/Volumes/qac/qac201/Studies and Codebooks/GSS/DATA/GSS_all.RData")

library(descr)

gss14 <- gss[gss$YEAR == 2014, c("ID", "HAPPY", "MAJOR1", "MAJOR2", "AGE", "SEX", "RACE", "CLASS")]

library(car)

gss14$HAPPY <- recode(gss14$HAPPY, "1=3; 3=1; 8 = NA; 9 = NA")

gss14$AGE <- recode(gss14$AGE, "98 = NA; 99 = NA")

gss14$CLASS <- recode(gss14$CLASS, "8 = NA; 9 = NA")

stem <- list(41, 11, 8, 33, 69, 14, 42, 4, 15, 18, 24, 27, 34, 36, 38, 48, 51, 58, 68, 69, 71, 72, 76, 80)

gss14$MAJOR1 <- recode(gss14$MAJOR1, "0 = NA")
gss14$MAJOR2 <- recode(gss14$MAJOR2, "0 = NA")

gss14$STEM <- ifelse(gss14$MAJOR1 %in% stem | gss14$MAJOR2 %in% stem, "yes", "no")

library(ggplot2)

ggplot(data=gss14)+
  stat_summary(aes(x=STEM, y=HAPPY),
               fun.y="mean", geom="bar", fill="green", 
               color="black")+
  ylab("Proportion of Happiness")+
  xlab("STEM Major")+
  ggtitle("Proportion of Happiness Among STEM and Non-STEM Majors")

gss14_new <- gss14[, c("ID", "HAPPY", "STEM")]
gss14_new <- na.omit(gss14_new)

ggplot(data=gss14_new)+
  geom_bar(aes(x=STEM, fill=factor(HAPPY)), position='fill', color ='black', width=0.25)+
  ylab("Proportion of Respondents")+
  xlab("STEM Major")+
  ggtitle("Happiness Among STEM vs. Non-STEM Majors")+
  scale_fill_brewer("Level of Happiness", labels = c("1 - Not Too Happy", "2 - Pretty Happy", "3 - Very Happy"))+theme_grey(base_size=14)

gss14$SEX <- ifelse(gss14$SEX == 1, "Male", "Female")

ggplot(data=gss14)+
  stat_summary(aes(x=STEM, fill=factor(SEX), y=HAPPY),
               fun.y="mean", geom="bar", position="dodge", alpha=0.8)+
  ylab("Proportion Happy")+
  xlab("STEM Major")+
  scale_fill_manual("Sex", values=c("magenta","blue"))

gss14_gender <- gss14[, c("ID", "HAPPY", "STEM", "SEX")]
gss14_gender <- na.omit(gss14_gender)

ggplot(data=gss14_gender)+
  geom_bar(aes(x=STEM, fill=factor(HAPPY)), position="fill",color='black')+
             facet_wrap(~SEX)+
  scale_fill_brewer(name = "Happiness Level", labels = c("1 - Not Too Happy", "2 - Pretty Happy", "3 - Very Happy"), palette="RdPu")+
  xlab("STEM Major")+
  ylab("Proportion of Respondents")+
  ggtitle("Happiness Among STEM vs. Non-STEM Majors 
  Based on Sex")+theme_grey(base_size=14)


gss14_class <- gss14[, c("ID", "HAPPY", "STEM", "CLASS")]
gss14_class <- na.omit(gss14_class)
gss14_class$CLASS[gss14_class$CLASS == 1] <- "Lower"
gss14_class$CLASS[gss14_class$CLASS == 2] <- "Working"
gss14_class$CLASS[gss14_class$CLASS == 3] <- "Middle"
gss14_class$CLASS[gss14_class$CLASS == 4] <- "Upper"

ggplot(data=gss14_class)+
  geom_bar(aes(x=STEM, fill=factor(HAPPY)), position="fill")+
  facet_wrap(~CLASS)+
  scale_fill_brewer(name = "Happiness Level", labels = c("1 - Not Too Happy", "2 - Pretty Happy", "3 - Very Happy"), palette="RdPu")+
  xlab("STEM Major")+
  ylab("Proportion of Respondents")+
  ggtitle("Happiness Among STEM vs. Non-STEM Majors Based on Class")


ggplot(data=gss14)+
  geom_bar(aes(x=STEM, fill=factor(HAPPY)), position="fill")+
  facet_wrap(~RACE)+
  scale_fill_brewer(name = "Happiness Level", labels = c("1 - Not Too Happy", "2 - Pretty Happy", "3 - Very Happy"), palette="RdPu")+
  xlab("STEM Major")+
  ylab("Proportion of Respondents")+
  ggtitle("Happiness Among STEM vs. Non-STEM Majors Based on Class")
