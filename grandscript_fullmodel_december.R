#grandscript_full neg data
install.packages("pacman")
pacman::p_load(vioplot, metap, ggplot2, MASS, Hmisc, plyr, ltm, epiDisplay,VennDiagram,psych,performance,pscl)

library(vioplot) #used to create violin plot 
library(metap) # used to calculate fishers exact (sumlog)
library(plyr) #used for revalue
library(ltm)#used to calculate cronbach alpha
library(epiDisplay) #used for mhor, cc, tabpct for odds ratio calculations
library(VennDiagram) #create venn diagrams
library(psych) #tetrachoric correlation
library(performance) #r2_mcfadden McFadden's pseudo R2


## re-creating files each time script is run
if (file.exists("table1.csv")) {
file.remove("table1.csv")
}

if (file.exists("table2.csv")) {
file.remove("table2.csv")
}

## loading in data file
data1 <- read.csv(file = "survey_data_recoded_mine_fullneg.csv", header=TRUE)
colnames(data1)[11:13] <- c("Hype","Trust", "GM_benefits" )

## renaming variables
tr <- as.numeric(as.character(data1$Trust))
gm <- as.numeric(as.character(data1$GM_benefits))
hy <- as.numeric(as.character(data1$Hype))
edu <- as.numeric(as.character(data1$education_level))
data1$CovVac_recoded <- as.numeric(as.character(data1$CovVac_recoded))
vac <- data1$CovVac_recoded
pol <- data1$politics
religion <- data1$religion
sex <- as.factor(data1$RS_Sex)
ethnic <- as.factor(data1$RS_EthnicGroup)
know <- data1$sci_know
su <- data1$scigen_self_under

## Creating dummy variables for polychotomous variables
rel2 <- ifelse(religion==1 ,1,0)
rel3 <- ifelse(religion==2 ,1,0)

edu2 <- ifelse(edu==1 ,1,0)
edu3 <- ifelse(edu==2 ,1,0)

## Extra tests in results section 
# tetrachoric correlation

df1 <- data.frame (tr,hy)
df2 <- data.frame (tr,gm)
df3 <- data.frame (tr,vac)
df4 <- data.frame (hy,gm)
df5 <- data.frame (hy,vac)
df6 <- data.frame (gm,vac)

corr1 <- tetrachoric(df1, na.rm= TRUE)
corr2 <- tetrachoric(df2, na.rm= TRUE)
corr3 <- tetrachoric(df3, na.rm= TRUE)
corr4 <- tetrachoric(df4, na.rm= TRUE)
corr5 <- tetrachoric(df5, na.rm= TRUE)
corr6 <- tetrachoric(df6, na.rm= TRUE)



## METHOD : characteristics of data

## How many people did not answer 1 or more relevant questions
sexrecoded<- as.numeric(as.character(revalue(sex, c("Female"=1, "Male"=0 ))))
ethnicrecoded <- as.numeric(as.character(revalue(ethnic, c("White British"=0, "Other"=1 ))))
df.data1 <-data.frame(tr,hy,gm,vac,know,su,rel2,rel3,pol,ethnicrecoded,age=data1$Age,edu2,edu3,sexrecoded)
df.data.sum <- rowSums(df.data1)

## Chronbach alpha on knowledge data
rows <- c(17:20,22,24:28)
know_all<- data1[,rows]
CronAlpha_know_all <- cronbach.alpha(know_all, na.rm=TRUE, CI=TRUE)
## removing each variable to see if any are particuarly influential 
## example - removing row 25
rows <- c(17:20,22,27,25,24:28)
know_all_minus<- data1[,rows]
CronAlpha_know_all_minus<- cronbach.alpha(know_all, na.rm=TRUE, CI=TRUE)

## Chronbach alpha on knowledge data
rows <- c(1:6)
know_all<- data1[,rows]
CronAlpha_know_all <- cronbach.alpha(know_all, na.rm=TRUE, CI=TRUE)
## removing each variable to see if any are particuarly influential 

#1
rows <- c(2:6)
know_all_minus<- data1[,rows]
CronAlpha_know_all_minus<- cronbach.alpha(know_all, na.rm=TRUE, CI=TRUE)
#2
rows <- c(1:6,2)
know_all_minus<- data1[,rows]
CronAlpha_know_all_minus<- cronbach.alpha(know_all, na.rm=TRUE, CI=TRUE)
rows <- c(1:6,3)
know_all_minus<- data1[,rows]
CronAlpha_know_all_minus<- cronbach.alpha(know_all, na.rm=TRUE, CI=TRUE)
rows <- c(1:6,4)
know_all_minus<- data1[,rows]
CronAlpha_know_all_minus<- cronbach.alpha(know_all, na.rm=TRUE, CI=TRUE)
rows <- c(1:5)
know_all_minus<- data1[,rows]
CronAlpha_know_all_minus<- cronbach.alpha(know_all, na.rm=TRUE, CI=TRUE)

## Figure 1- venn diagram of the number with negative attitudes in the four questions
df.venn <- data.frame (gm,hy,tr,vac)
tr.neg<- which(df.venn$tr ==1)
hy.neg <- which(df.venn$hy ==1)
gm.neg<- which(df.venn$gm ==1)
vac.neg <-which(df.venn$vac ==1)

all.neg <- list(tr.neg, hy.neg, gm.neg, vac.neg)

venn.diagram(
	x=all.neg,
	na= "remove",
	category.names = c("Trust", "Hype", "GM", "Vaccine"),
	filename= 'figure_venn.tiff',
	output=TRUE
)

## Table 1 - characteristics of data
#kn, su, age - calculate mean and sd
#all other variables calculate number in one catergory (as these were dichotomous)
  
## pos and neg totals (all)
tr.pos.total <- length(na.omit(tr[tr==0]))
tr.neg.total <- length(na.omit(tr[tr==1]))
hy.pos.total <- length(na.omit(hy[hy==0]))
hy.neg.total <- length(na.omit(hy[hy==1]))
gm.pos.total <- length(na.omit(gm[gm==0]))
gm.neg.total <- length(na.omit(gm[gm==1]))
vac.pos.total <- length(na.omit(vac[vac==0]))
vac.neg.total <- length(na.omit(vac[vac==1]))

## TRUST POSITIVE
#knowledge
tr.pos.know.a <- mean(know[tr==0], na.rm=TRUE)
tr.pos.know.b <- sd(know[tr==0], na.rm=TRUE)
tr.pos.know <- paste(round(tr.pos.know.a,2), round(tr.pos.know.b,2), sep="(")
#su
tr.pos.su.a <- mean(su[tr==0], na.rm=TRUE)
tr.pos.su.b <- sd(su[tr==0], na.rm=TRUE)
tr.pos.su <- paste(round(tr.pos.su.a,2), round(tr.pos.su.b,2), sep="(")
#female =1
tr.sex<- na.omit(sexrecoded[tr==0])
tr.pos.sex.female <- length(tr.sex[tr.sex==1])
tr.pos.sex.female.percent <- round((length(tr.sex[tr.sex==1])/length(tr.sex))*100,2)
tr.pos.sex.female <- paste (tr.pos.sex.female,tr.pos.sex.female.percent,sep="(")
#0= white british
tr.eth<- na.omit(ethnicrecoded [tr==0])
tr.pos.eth.wb <- length(tr.eth[tr.eth==0])
tr.pos.eth.wb.percent <- round((length(tr.eth[tr.eth==0])/length(tr.eth))*100,2)
tr.pos.eth.wb <- paste(tr.pos.eth.wb,tr.pos.eth.wb.percent,sep="(")
#no religion
tr.rel<- na.omit(religion [tr==0])
tr.pos.rel.0 <- length(tr.rel[tr.rel==0])
tr.pos.rel.0.percent <- round((tr.pos.rel.0/length(tr.rel))*100,2)
tr.pos.rel.0 <- paste(tr.pos.rel.0,tr.pos.rel.0.percent,sep="(")
#religious non practising
tr.pos.rel.1 <- length(tr.rel[tr.rel==1])
tr.pos.rel.1.percent <- round((tr.pos.rel.1/length(tr.rel))*100,2)
tr.pos.rel.1 <- paste(tr.pos.rel.1,tr.pos.rel.1.percent,sep="(")
#religious activley practising
tr.pos.rel.2 <- length(tr.rel[tr.rel==2])
tr.pos.rel.2.percent <- round((tr.pos.rel.2/length(tr.rel))*100,2)
tr.pos.rel.2 <- paste(tr.pos.rel.2,tr.pos.rel.2.percent,sep="(")
#pol
tr.pos.pol.a <- mean(pol[tr==0], na.rm=TRUE)
tr.pos.pol.b <- sd(pol[tr==0], na.rm=TRUE)
tr.pos.pol <- paste(round(tr.pos.pol.a,2), round(tr.pos.pol.b,2), sep="(")
#age
tr.pos.age.a <- mean(data1$Age[tr==0], na.rm=TRUE)
tr.pos.age.b <- sd(data1$Age[tr==0], na.rm=TRUE)
tr.pos.age <- paste(round(tr.pos.age.a,2), round(tr.pos.age.b,2), sep="(")
#no qualifications
tr.edu<- edu [tr==0]
tr.edu<- na.omit(tr.edu)
tr.pos.edu.0 <- length(tr.edu[tr.edu==0])
tr.pos.edu.0.percent <- round((tr.pos.edu.0/length(tr.edu))*100,2) 
tr.pos.edu.0 <- paste(tr.pos.edu.0,tr.pos.edu.0.percent,sep="(")
#edu2
tr.pos.edu.1 <- length(tr.edu[tr.edu==1])
tr.pos.edu.1.percent <- round((tr.pos.edu.1/length(tr.edu))*100,2) 
tr.pos.edu.1 <- paste(tr.pos.edu.1,tr.pos.edu.1.percent,sep="(")
#edu 3
tr.pos.edu.2 <- length(tr.edu[tr.edu==2])
tr.pos.edu.2.percent <- round((tr.pos.edu.2/length(tr.edu))*100,2) 
tr.pos.edu.2 <- paste(tr.pos.edu.2,tr.pos.edu.2.percent,sep="(")

## TRUST NEGATIVE 
#knowledge
tr.neg.know.a <- mean(know[tr==1], na.rm=TRUE)
tr.neg.know.b <- sd(know[tr==1], na.rm=TRUE)
tr.neg.know <- paste(round(tr.neg.know.a,2), round(tr.neg.know.b,2), sep="(")
#su
tr.neg.su.a <- mean(su[tr==1], na.rm=TRUE)
tr.neg.su.b <- sd(su[tr==1], na.rm=TRUE)
tr.neg.su <- paste(round(tr.neg.su.a,2), round(tr.neg.su.b,2), sep="(")
#female =1
tr.sex<- na.omit(sexrecoded[tr==1])
tr.neg.sex.female <- length(tr.sex[tr.sex==1])
tr.neg.sex.female.percent <- round((length(tr.sex[tr.sex==1])/length(tr.sex))*100,2)
tr.neg.sex.female <- paste (tr.neg.sex.female,tr.neg.sex.female.percent,sep="(")
#0= white british
tr.eth<- na.omit(ethnicrecoded [tr==1])
tr.neg.eth.wb <- length(tr.eth[tr.eth==0])
tr.neg.eth.wb.percent <- round((length(tr.eth[tr.eth==0])/length(tr.eth))*100,2)
tr.neg.eth.wb <- paste(tr.neg.eth.wb,tr.neg.eth.wb.percent,sep="(")
#no religion
tr.rel<- na.omit(religion [tr==1])
tr.neg.rel.0 <- length(tr.rel[tr.rel==0])
tr.neg.rel.0.percent <- round((tr.neg.rel.0/length(tr.rel))*100,2)
tr.neg.rel.0 <- paste(tr.neg.rel.0,tr.neg.rel.0.percent,sep="(")
#religious non practising
tr.neg.rel.1 <- length(tr.rel[tr.rel==1])
tr.neg.rel.1.percent <- round((tr.neg.rel.1/length(tr.rel))*100,2)
tr.neg.rel.1 <- paste(tr.neg.rel.1,tr.neg.rel.1.percent,sep="(")
#religious activley practising
tr.neg.rel.2 <- length(tr.rel[tr.rel==2])
tr.neg.rel.2.percent <- round((tr.neg.rel.2/length(tr.rel))*100,2)
tr.neg.rel.2 <- paste(tr.neg.rel.2,tr.neg.rel.2.percent,sep="(")
#pol
tr.neg.pol.a <- mean(pol[tr==1], na.rm=TRUE)
tr.neg.pol.b <- sd(pol[tr==1], na.rm=TRUE)
tr.neg.pol <- paste(round(tr.neg.pol.a,2), round(tr.neg.pol.b,2), sep="(")
#age
tr.neg.age.a <- mean(data1$Age[tr==1], na.rm=TRUE)
tr.neg.age.b <- sd(data1$Age[tr==1], na.rm=TRUE)
tr.neg.age <- paste(round(tr.neg.age.a,2), round(tr.neg.age.b,2), sep="(")
#no qualifications
tr.edu<- edu [tr==1]
tr.edu<- na.omit(tr.edu)
tr.neg.edu.0 <- length(tr.edu[tr.edu==0])
tr.neg.edu.0.percent <- round((tr.neg.edu.0/length(tr.edu))*100,2) 
tr.neg.edu.0 <- paste(tr.neg.edu.0,tr.neg.edu.0.percent,sep="(")
#edu2
tr.neg.edu.1 <- length(tr.edu[tr.edu==1])
tr.neg.edu.1.percent <- round((tr.neg.edu.1/length(tr.edu))*100,2) 
tr.neg.edu.1 <- paste(tr.neg.edu.1,tr.neg.edu.1.percent,sep="(")
#edu 3
tr.neg.edu.2 <- length(tr.edu[tr.edu==2])
tr.neg.edu.2.percent <- round((tr.neg.edu.2/length(tr.edu))*100,2) 
tr.neg.edu.2 <- paste(tr.neg.edu.2,tr.neg.edu.2.percent,sep="(")


## TRUST DATA.FRAME
character.trust <- data.frame(Factor = c("","Knowledge","Self-assessed Understanding", "Female", "White British", "Not religious", "Relgious(not practising","Religious(activley practising)","Political ideology","Age","No qualifications","Non-degree level","Degree level"),
	Trust_Positive =c(tr.pos.total,tr.pos.know ,tr.pos.su,tr.pos.sex.female,tr.pos.eth.wb,tr.pos.rel.0,tr.pos.rel.1,tr.pos.rel.2,tr.pos.pol,tr.pos.age,tr.pos.edu.0,tr.pos.edu.1,tr.pos.edu.2),
	Trust_Negative=c(tr.neg.total,tr.neg.know ,tr.neg.su,tr.neg.sex.female,tr.neg.eth.wb,tr.neg.rel.0,tr.neg.rel.1,tr.neg.rel.2,tr.neg.pol,tr.neg.age,tr.neg.edu.0,tr.neg.edu.1,tr.neg.edu.2))
 
 
## HYPE POSITIVE 
#knowledge
hy.pos.know.a <- mean(know[hy==0], na.rm=TRUE)
hy.pos.know.b <- sd(know[hy==0], na.rm=TRUE)
hy.pos.know <- paste(round(hy.pos.know.a,2), round(hy.pos.know.b,2), sep="(")
#su
hy.pos.su.a <- mean(su[hy==0], na.rm=TRUE)
hy.pos.su.b <- sd(su[hy==0], na.rm=TRUE)
hy.pos.su <- paste(round(hy.pos.su.a,2), round(hy.pos.su.b,2), sep="(")
#female =1
hy.sex<- na.omit(sexrecoded[hy==0])
hy.pos.sex.female <- length(hy.sex[hy.sex==1])
hy.pos.sex.female.percent <- round((length(hy.sex[hy.sex==1])/length(hy.sex))*100,2)
hy.pos.sex.female <- paste (hy.pos.sex.female,hy.pos.sex.female.percent,sep="(")
#0= white british
hy.eth<- na.omit(ethnicrecoded [hy==0])
hy.pos.eth.wb <- length(hy.eth[hy.eth==0])
hy.pos.eth.wb.percent <- round((length(hy.eth[hy.eth==0])/length(hy.eth))*100,2)
hy.pos.eth.wb <- paste(hy.pos.eth.wb,hy.pos.eth.wb.percent,sep="(")
#no religion
hy.rel<- na.omit(religion [hy==0])
hy.pos.rel.0 <- length(hy.rel[hy.rel==0])
hy.pos.rel.0.percent <- round((hy.pos.rel.0/length(hy.rel))*100,2)
hy.pos.rel.0 <- paste(hy.pos.rel.0,hy.pos.rel.0.percent,sep="(")
#religious non practising
hy.pos.rel.1 <- length(hy.rel[hy.rel==1])
hy.pos.rel.1.percent <- round((hy.pos.rel.1/length(hy.rel))*100,2)
hy.pos.rel.1 <- paste(hy.pos.rel.1,hy.pos.rel.1.percent,sep="(")
#religious activley practising
hy.pos.rel.2 <- length(hy.rel[hy.rel==2])
hy.pos.rel.2.percent <- round((hy.pos.rel.2/length(hy.rel))*100,2)
hy.pos.rel.2 <- paste(hy.pos.rel.2,hy.pos.rel.2.percent,sep="(")
#pol
hy.pos.pol.a <- mean(pol[hy==0], na.rm=TRUE)
hy.pos.pol.b <- sd(pol[hy==0], na.rm=TRUE)
hy.pos.pol <- paste(round(hy.pos.pol.a,2), round(hy.pos.pol.b,2), sep="(")
#age
hy.pos.age.a <- mean(data1$Age[hy==0], na.rm=TRUE)
hy.pos.age.b <- sd(data1$Age[hy==0], na.rm=TRUE)
hy.pos.age <- paste(round(hy.pos.age.a,2), round(hy.pos.age.b,2), sep="(")
#no qualifications
hy.edu<- edu [hy==0]
hy.edu<- na.omit(hy.edu)
hy.pos.edu.0 <- length(hy.edu[hy.edu==0])
hy.pos.edu.0.percent <- round((hy.pos.edu.0/length(hy.edu))*100,2) 
hy.pos.edu.0 <- paste(hy.pos.edu.0,hy.pos.edu.0.percent,sep="(")
#edu2
hy.pos.edu.1 <- length(hy.edu[hy.edu==1])
hy.pos.edu.1.percent <- round((hy.pos.edu.1/length(hy.edu))*100,2) 
hy.pos.edu.1 <- paste(hy.pos.edu.1,hy.pos.edu.1.percent,sep="(")
#edu 3
hy.pos.edu.2 <- length(hy.edu[hy.edu==2])
hy.pos.edu.2.percent <- round((hy.pos.edu.2/length(hy.edu))*100,2) 
hy.pos.edu.2 <- paste(hy.pos.edu.2,hy.pos.edu.2.percent,sep="(")

#HYPE NEGATIVE
#knowledge
hy.neg.know.a <- mean(know[hy==1], na.rm=TRUE)
hy.neg.know.b <- sd(know[hy==1], na.rm=TRUE)
hy.neg.know <- paste(round(hy.neg.know.a,2), round(hy.neg.know.b,2), sep="(")
#su
hy.neg.su.a <- mean(su[hy==1], na.rm=TRUE)
hy.neg.su.b <- sd(su[hy==1], na.rm=TRUE)
hy.neg.su <- paste(round(hy.neg.su.a,2), round(hy.neg.su.b,2), sep="(")
#female =1
hy.sex<- na.omit(sexrecoded[hy==1])
hy.neg.sex.female <- length(hy.sex[hy.sex==1])
hy.neg.sex.female.percent <- round((length(hy.sex[hy.sex==1])/length(hy.sex))*100,2)
hy.neg.sex.female <- paste (hy.neg.sex.female,hy.neg.sex.female.percent,sep="(")
#0= white british
hy.eth<- na.omit(ethnicrecoded [hy==1])
hy.neg.eth.wb <- length(hy.eth[hy.eth==0])
hy.neg.eth.wb.percent <- round((length(hy.eth[hy.eth==0])/length(hy.eth))*100,2)
hy.neg.eth.wb <- paste(hy.neg.eth.wb,hy.neg.eth.wb.percent,sep="(")
#no religion
hy.rel<- na.omit(religion [hy==1])
hy.neg.rel.0 <- length(hy.rel[hy.rel==0])
hy.neg.rel.0.percent <- round((hy.neg.rel.0/length(hy.rel))*100,2)
hy.neg.rel.0 <- paste(hy.neg.rel.0,hy.neg.rel.0.percent,sep="(")
#religious non practising
hy.neg.rel.1 <- length(hy.rel[hy.rel==1])
hy.neg.rel.1.percent <- round((hy.neg.rel.1/length(hy.rel))*100,2)
hy.neg.rel.1 <- paste(hy.neg.rel.1,hy.neg.rel.1.percent,sep="(")
#religious activley practising
hy.neg.rel.2 <- length(hy.rel[hy.rel==2])
hy.neg.rel.2.percent <- round((hy.neg.rel.2/length(hy.rel))*100,2)
hy.neg.rel.2 <- paste(hy.neg.rel.2,hy.neg.rel.2.percent,sep="(")
#pol
hy.neg.pol.a <- mean(pol[hy==1], na.rm=TRUE)
hy.neg.pol.b <- sd(pol[hy==1], na.rm=TRUE)
hy.neg.pol <- paste(round(hy.neg.pol.a,2), round(hy.neg.pol.b,2), sep="(")
#age
hy.neg.age.a <- mean(data1$Age[hy==1], na.rm=TRUE)
hy.neg.age.b <- sd(data1$Age[hy==1], na.rm=TRUE)
hy.neg.age <- paste(round(hy.neg.age.a,2), round(hy.neg.age.b,2), sep="(")
#no qualifications
hy.edu<- edu [hy==1]
hy.edu<- na.omit(hy.edu)
hy.neg.edu.0 <- length(hy.edu[hy.edu==0])
hy.neg.edu.0.percent <- round((hy.neg.edu.0/length(hy.edu))*100,2) 
hy.neg.edu.0 <- paste(hy.neg.edu.0,hy.neg.edu.0.percent,sep="(")
#edu2
hy.neg.edu.1 <- length(hy.edu[hy.edu==1])
hy.neg.edu.1.percent <- round((hy.neg.edu.1/length(hy.edu))*100,2) 
hy.neg.edu.1 <- paste(hy.neg.edu.1,hy.neg.edu.1.percent,sep="(")
#edu 3
hy.neg.edu.2 <- length(hy.edu[hy.edu==2])
hy.neg.edu.2.percent <- round((hy.neg.edu.2/length(hy.edu))*100,2) 
hy.neg.edu.2 <- paste(hy.neg.edu.2,hy.neg.edu.2.percent,sep="(")


## HYPE DATA.FRAME
character.hype <- data.frame(
	hype_Positive =c(hy.pos.total,hy.pos.know ,hy.pos.su,hy.pos.sex.female,hy.pos.eth.wb,hy.pos.rel.0,hy.pos.rel.1,hy.pos.rel.2,hy.pos.pol,hy.pos.age,hy.pos.edu.0,hy.pos.edu.1,hy.pos.edu.2),
	hype_Negative=c(hy.neg.total,hy.neg.know ,hy.neg.su,hy.neg.sex.female,hy.neg.eth.wb,hy.neg.rel.0,hy.neg.rel.1,hy.neg.rel.2,hy.neg.pol,hy.neg.age,hy.neg.edu.0,hy.neg.edu.1,hy.neg.edu.2))
	
## GM POSITIVE 
#knowledge
gm.pos.know.a <- mean(know[gm==0], na.rm=TRUE)
gm.pos.know.b <- sd(know[gm==0], na.rm=TRUE)
gm.pos.know <- paste(round(gm.pos.know.a,2), round(gm.pos.know.b,2), sep="(")
#su
gm.pos.su.a <- mean(su[gm==0], na.rm=TRUE)
gm.pos.su.b <- sd(su[gm==0], na.rm=TRUE)
gm.pos.su <- paste(round(gm.pos.su.a,2), round(gm.pos.su.b,2), sep="(")
#female =1
gm.sex<- na.omit(sexrecoded[gm==0])
gm.pos.sex.female <- length(gm.sex[gm.sex==1])
gm.pos.sex.female.percent <- round((length(gm.sex[gm.sex==1])/length(gm.sex))*100,2)
gm.pos.sex.female <- paste (gm.pos.sex.female,gm.pos.sex.female.percent,sep="(")
#0= white british
gm.eth<- na.omit(ethnicrecoded [gm==0])
gm.pos.eth.wb <- length(gm.eth[gm.eth==0])
gm.pos.eth.wb.percent <- round((length(gm.eth[gm.eth==0])/length(gm.eth))*100,2)
gm.pos.eth.wb <- paste(gm.pos.eth.wb,gm.pos.eth.wb.percent,sep="(")
#no religion
gm.rel<- na.omit(religion [gm==0])
gm.pos.rel.0 <- length(gm.rel[gm.rel==0])
gm.pos.rel.0.percent <- round((gm.pos.rel.0/length(gm.rel))*100,2)
gm.pos.rel.0 <- paste(gm.pos.rel.0,gm.pos.rel.0.percent,sep="(")
#religious non practising
gm.pos.rel.1 <- length(gm.rel[gm.rel==1])
gm.pos.rel.1.percent <- round((gm.pos.rel.1/length(gm.rel))*100,2)
gm.pos.rel.1 <- paste(gm.pos.rel.1,gm.pos.rel.1.percent,sep="(")
#religious activley practising
gm.pos.rel.2 <- length(gm.rel[gm.rel==2])
gm.pos.rel.2.percent <- round((gm.pos.rel.2/length(gm.rel))*100,2)
gm.pos.rel.2 <- paste(gm.pos.rel.2,gm.pos.rel.2.percent,sep="(")
#pol
gm.pos.pol.a <- mean(pol[gm==0], na.rm=TRUE)
gm.pos.pol.b <- sd(pol[gm==0], na.rm=TRUE)
gm.pos.pol <- paste(round(gm.pos.pol.a,2), round(gm.pos.pol.b,2), sep="(")
#age
gm.pos.age.a <- mean(data1$Age[gm==0], na.rm=TRUE)
gm.pos.age.b <- sd(data1$Age[gm==0], na.rm=TRUE)
gm.pos.age <- paste(round(gm.pos.age.a,2), round(gm.pos.age.b,2), sep="(")
#no qualifications
gm.edu<- edu [gm==0]
gm.edu<- na.omit(gm.edu)
gm.pos.edu.0 <- length(gm.edu[gm.edu==0])
gm.pos.edu.0.percent <- round((gm.pos.edu.0/length(gm.edu))*100,2) 
gm.pos.edu.0 <- paste(gm.pos.edu.0,gm.pos.edu.0.percent,sep="(")
#edu2
gm.pos.edu.1 <- length(gm.edu[gm.edu==1])
gm.pos.edu.1.percent <- round((gm.pos.edu.1/length(gm.edu))*100,2) 
gm.pos.edu.1 <- paste(gm.pos.edu.1,gm.pos.edu.1.percent,sep="(")
#edu 3
gm.pos.edu.2 <- length(gm.edu[gm.edu==2])
gm.pos.edu.2.percent <- round((gm.pos.edu.2/length(gm.edu))*100,2) 
gm.pos.edu.2 <- paste(gm.pos.edu.2,gm.pos.edu.2.percent,sep="(")

#GM NEGATIVE 
#knowledge
gm.neg.know.a <- mean(know[gm==1], na.rm=TRUE)
gm.neg.know.b <- sd(know[gm==1], na.rm=TRUE)
gm.neg.know <- paste(round(gm.neg.know.a,2), round(gm.neg.know.b,2), sep="(")
#su
gm.neg.su.a <- mean(su[gm==1], na.rm=TRUE)
gm.neg.su.b <- sd(su[gm==1], na.rm=TRUE)
gm.neg.su <- paste(round(gm.neg.su.a,2), round(gm.neg.su.b,2), sep="(")
#female =1
gm.sex<- na.omit(sexrecoded[gm==1])
gm.neg.sex.female <- length(gm.sex[gm.sex==1])
gm.neg.sex.female.percent <- round((length(gm.sex[gm.sex==1])/length(gm.sex))*100,2)
gm.neg.sex.female <- paste (gm.neg.sex.female,gm.neg.sex.female.percent,sep="(")
#0= white british
gm.eth<- na.omit(ethnicrecoded [gm==1])
gm.neg.eth.wb <- length(gm.eth[gm.eth==0])
gm.neg.eth.wb.percent <- round((length(gm.eth[gm.eth==0])/length(gm.eth))*100,2)
gm.neg.eth.wb <- paste(gm.neg.eth.wb,gm.neg.eth.wb.percent,sep="(")
#no religion
gm.rel<- na.omit(religion [gm==1])
gm.neg.rel.0 <- length(gm.rel[gm.rel==0])
gm.neg.rel.0.percent <- round((gm.neg.rel.0/length(gm.rel))*100,2)
gm.neg.rel.0 <- paste(gm.neg.rel.0,gm.neg.rel.0.percent,sep="(")
#religious non practising
gm.neg.rel.1 <- length(gm.rel[gm.rel==1])
gm.neg.rel.1.percent <- round((gm.neg.rel.1/length(gm.rel))*100,2)
gm.neg.rel.1 <- paste(gm.neg.rel.1,gm.neg.rel.1.percent,sep="(")
#religious activley practising
gm.neg.rel.2 <- length(gm.rel[gm.rel==2])
gm.neg.rel.2.percent <- round((gm.neg.rel.2/length(gm.rel))*100,2)
gm.neg.rel.2 <- paste(gm.neg.rel.2,gm.neg.rel.2.percent,sep="(")
#pol
gm.neg.pol.a <- mean(pol[gm==1], na.rm=TRUE)
gm.neg.pol.b <- sd(pol[gm==1], na.rm=TRUE)
gm.neg.pol <- paste(round(gm.neg.pol.a,2), round(gm.neg.pol.b,2), sep="(")
#age
gm.neg.age.a <- mean(data1$Age[gm==1], na.rm=TRUE)
gm.neg.age.b <- sd(data1$Age[gm==1], na.rm=TRUE)
gm.neg.age <- paste(round(gm.neg.age.a,2), round(gm.neg.age.b,2), sep="(")
#no qualifications
gm.edu<- edu [gm==1]
gm.edu<- na.omit(gm.edu)
gm.neg.edu.0 <- length(gm.edu[gm.edu==0])
gm.neg.edu.0.percent <- round((gm.neg.edu.0/length(gm.edu))*100,2) 
gm.neg.edu.0 <- paste(gm.neg.edu.0,gm.neg.edu.0.percent,sep="(")
#edu2
gm.neg.edu.1 <- length(gm.edu[gm.edu==1])
gm.neg.edu.1.percent <- round((gm.neg.edu.1/length(gm.edu))*100,2) 
gm.neg.edu.1 <- paste(gm.neg.edu.1,gm.neg.edu.1.percent,sep="(")
#edu 3
gm.neg.edu.2 <- length(gm.edu[gm.edu==2])
gm.neg.edu.2.percent <- round((gm.neg.edu.2/length(gm.edu))*100,2) 
gm.neg.edu.2 <- paste(gm.neg.edu.2,gm.neg.edu.2.percent,sep="(")


## GM DATA.FRAME
character.gm <- data.frame(
	gm_Positive =c(gm.pos.total,gm.pos.know ,gm.pos.su,gm.pos.sex.female,gm.pos.eth.wb,gm.pos.rel.0,gm.pos.rel.1,gm.pos.rel.2,gm.pos.pol,gm.pos.age,gm.pos.edu.0,gm.pos.edu.1,gm.pos.edu.2),
	gm_Negative=c(gm.neg.total,gm.neg.know ,gm.neg.su,gm.neg.sex.female,gm.neg.eth.wb,gm.neg.rel.0,gm.neg.rel.1,gm.neg.rel.2,gm.neg.pol,gm.neg.age,gm.neg.edu.0,gm.neg.edu.1,gm.neg.edu.2))
	
#VACCINE POSITIVE
#knowledge
vac.pos.know.a <- mean(know[vac==0], na.rm=TRUE)
vac.pos.know.b <- sd(know[vac==0], na.rm=TRUE)
vac.pos.know <- paste(round(vac.pos.know.a,2), round(vac.pos.know.b,2), sep="(")
#su
vac.pos.su.a <- mean(su[vac==0], na.rm=TRUE)
vac.pos.su.b <- sd(su[vac==0], na.rm=TRUE)
vac.pos.su <- paste(round(vac.pos.su.a,2), round(vac.pos.su.b,2), sep="(")
#female =1
vac.sex<- na.omit(sexrecoded[vac==0])
vac.pos.sex.female <- length(vac.sex[vac.sex==1])
vac.pos.sex.female.percent <- round((length(vac.sex[vac.sex==1])/length(vac.sex))*100,2)
vac.pos.sex.female <- paste (vac.pos.sex.female,vac.pos.sex.female.percent,sep="(")
#0= white british
vac.eth<- na.omit(ethnicrecoded [vac==0])
vac.pos.eth.wb <- length(vac.eth[vac.eth==0])
vac.pos.eth.wb.percent <- round((length(vac.eth[vac.eth==0])/length(vac.eth))*100,2)
vac.pos.eth.wb <- paste(vac.pos.eth.wb,vac.pos.eth.wb.percent,sep="(")
#no religion
vac.rel<- na.omit(religion [vac==0])
vac.pos.rel.0 <- length(vac.rel[vac.rel==0])
vac.pos.rel.0.percent <- round((vac.pos.rel.0/length(vac.rel))*100,2)
vac.pos.rel.0 <- paste(vac.pos.rel.0,vac.pos.rel.0.percent,sep="(")
#religious non practising
vac.pos.rel.1 <- length(vac.rel[vac.rel==1])
vac.pos.rel.1.percent <- round((vac.pos.rel.1/length(vac.rel))*100,2)
vac.pos.rel.1 <- paste(vac.pos.rel.1,vac.pos.rel.1.percent,sep="(")
#religious activley practising
vac.pos.rel.2 <- length(vac.rel[vac.rel==2])
vac.pos.rel.2.percent <- round((vac.pos.rel.2/length(vac.rel))*100,2)
vac.pos.rel.2 <- paste(vac.pos.rel.2,vac.pos.rel.2.percent,sep="(")
#pol
vac.pos.pol.a <- mean(pol[vac==0], na.rm=TRUE)
vac.pos.pol.b <- sd(pol[vac==0], na.rm=TRUE)
vac.pos.pol <- paste(round(vac.pos.pol.a,2), round(vac.pos.pol.b,2), sep="(")
#age
vac.pos.age.a <- mean(data1$Age[vac==0], na.rm=TRUE)
vac.pos.age.b <- sd(data1$Age[vac==0], na.rm=TRUE)
vac.pos.age <- paste(round(vac.pos.age.a,2), round(vac.pos.age.b,2), sep="(")
#no qualifications
vac.edu<- edu [vac==0]
vac.edu<- na.omit(vac.edu)
vac.pos.edu.0 <- length(vac.edu[vac.edu==0])
vac.pos.edu.0.percent <- round((vac.pos.edu.0/length(vac.edu))*100,2) 
vac.pos.edu.0 <- paste(vac.pos.edu.0,vac.pos.edu.0.percent,sep="(")
#edu2
vac.pos.edu.1 <- length(vac.edu[vac.edu==1])
vac.pos.edu.1.percent <- round((vac.pos.edu.1/length(vac.edu))*100,2) 
vac.pos.edu.1 <- paste(vac.pos.edu.1,vac.pos.edu.1.percent,sep="(")
#edu 3
vac.pos.edu.2 <- length(vac.edu[vac.edu==2])
vac.pos.edu.2.percent <- round((vac.pos.edu.2/length(vac.edu))*100,2) 
vac.pos.edu.2 <- paste(vac.pos.edu.2,vac.pos.edu.2.percent,sep="(")

#VACCINE NEGATIVE 
#knowledge
vac.neg.know.a <- mean(know[vac==1], na.rm=TRUE)
vac.neg.know.b <- sd(know[vac==1], na.rm=TRUE)
vac.neg.know <- paste(round(vac.neg.know.a,2), round(vac.neg.know.b,2), sep="(")
#su
vac.neg.su.a <- mean(su[vac==1], na.rm=TRUE)
vac.neg.su.b <- sd(su[vac==1], na.rm=TRUE)
vac.neg.su <- paste(round(vac.neg.su.a,2), round(vac.neg.su.b,2), sep="(")
#female =1
vac.sex<- na.omit(sexrecoded[vac==1])
vac.neg.sex.female <- length(vac.sex[vac.sex==1])
vac.neg.sex.female.percent <- round((length(vac.sex[vac.sex==1])/length(vac.sex))*100,2)
vac.neg.sex.female <- paste (vac.neg.sex.female,vac.neg.sex.female.percent,sep="(")
#0= white british
vac.eth<- na.omit(ethnicrecoded [vac==1])
vac.neg.eth.wb <- length(vac.eth[vac.eth==0])
vac.neg.eth.wb.percent <- round((length(vac.eth[vac.eth==0])/length(vac.eth))*100,2)
vac.neg.eth.wb <- paste(vac.neg.eth.wb,vac.neg.eth.wb.percent,sep="(")
#no religion
vac.rel<- na.omit(religion [vac==1])
vac.neg.rel.0 <- length(vac.rel[vac.rel==0])
vac.neg.rel.0.percent <- round((vac.neg.rel.0/length(vac.rel))*100,2)
vac.neg.rel.0 <- paste(vac.neg.rel.0,vac.neg.rel.0.percent,sep="(")
#religious non practising
vac.neg.rel.1 <- length(vac.rel[vac.rel==1])
vac.neg.rel.1.percent <- round((vac.neg.rel.1/length(vac.rel))*100,2)
vac.neg.rel.1 <- paste(vac.neg.rel.1,vac.neg.rel.1.percent,sep="(")
#religious activley practising
vac.neg.rel.2 <- length(vac.rel[vac.rel==2])
vac.neg.rel.2.percent <- round((vac.neg.rel.2/length(vac.rel))*100,2)
vac.neg.rel.2 <- paste(vac.neg.rel.2,vac.neg.rel.2.percent,sep="(")
#pol
vac.neg.pol.a <- mean(pol[vac==1], na.rm=TRUE)
vac.neg.pol.b <- sd(pol[vac==1], na.rm=TRUE)
vac.neg.pol <- paste(round(vac.neg.pol.a,2), round(vac.neg.pol.b,2), sep="(")
#age
vac.neg.age.a <- mean(data1$Age[vac==1], na.rm=TRUE)
vac.neg.age.b <- sd(data1$Age[vac==1], na.rm=TRUE)
vac.neg.age <- paste(round(vac.neg.age.a,2), round(vac.neg.age.b,2), sep="(")
#no qualifications
vac.edu<- edu [vac==1]
vac.edu<- na.omit(vac.edu)
vac.neg.edu.0 <- length(vac.edu[vac.edu==0])
vac.neg.edu.0.percent <- round((vac.neg.edu.0/length(vac.edu))*100,2) 
vac.neg.edu.0 <- paste(vac.neg.edu.0,vac.neg.edu.0.percent,sep="(")
#edu2
vac.neg.edu.1 <- length(vac.edu[vac.edu==1])
vac.neg.edu.1.percent <- round((vac.neg.edu.1/length(vac.edu))*100,2) 
vac.neg.edu.1 <- paste(vac.neg.edu.1,vac.neg.edu.1.percent,sep="(")
#edu 3
vac.neg.edu.2 <- length(vac.edu[vac.edu==2])
vac.neg.edu.2.percent <- round((vac.neg.edu.2/length(vac.edu))*100,2) 
vac.neg.edu.2 <- paste(vac.neg.edu.2,vac.neg.edu.2.percent,sep="(")

## VACCINE DATA.FRAME
character.vac <- data.frame(
	vac_Positive =c(vac.pos.total,vac.pos.know ,vac.pos.su,vac.pos.sex.female,vac.pos.eth.wb,vac.pos.rel.0,vac.pos.rel.1,vac.pos.rel.2,vac.pos.pol,vac.pos.age,vac.pos.edu.0,vac.pos.edu.1,vac.pos.edu.2),
	vac_Negative=c(vac.neg.total,vac.neg.know ,vac.neg.su,vac.neg.sex.female,vac.neg.eth.wb,vac.neg.rel.0,vac.neg.rel.1,vac.neg.rel.2,vac.neg.pol,vac.neg.age,vac.neg.edu.0,vac.neg.edu.1,vac.neg.edu.2))
	
	
## Table 1 (overall table for all questions)
data.character.all <- cbind(character.trust,character.hype,character.gm,character.vac)
write.csv(data.character.all, "characteristics_of_data.csv")


## STATISTICAL ANALYSIS ##
## QUESTIONS 1 and 2 : 
#how much do the most antipathetic to science, know and think they know about science?
#demographically what characterises the most antipathetic to science?



## Figure 2
## The relationship between strongly negative positions on Hype, Trust, GM and vaccines and A) objective scientific knowledge and B) self-assessed understanding

## creating clean dataframes
df.know.su.tr <-data.frame(tr,know,su)
df.know.su.tr.1 <-na.omit(df.know.su.tr)
df.know.su.hy <-data.frame(hy,know,su)
df.know.su.hy.1 <-na.omit(df.know.su.hy)
df.know.su.gm <-data.frame(gm,know,su)
df.know.su.gm.1 <-na.omit(df.know.su.gm)
df.know.su.vac <-data.frame(vac,know,su)
df.know.su.vac.1 <-na.omit(df.know.su.vac)

df.gm.pol <- data.frame (gm,pol)
df.gm.pol.1 <- na.omit (df.gm.pol)

vioplot(df.gm.pol.1$pol~df.gm.pol.1$gm,xlab="GM",ylab="Political ideology", names=c("Other","Negative"),
	col="gold", cex.axis=1.2, cex.lab=0.8)
	
df.gm.age <- data.frame (gm,age=data1$Age)
df.gm.age.1 <- na.omit (df.gm.age)
vioplot(df.gm.age.1$age~df.gm.age.1$gm,xlab="GM",ylab="Age", names=c("Other","Negative"),
	col="gold", cex.axis=1.2, cex.lab=0.8)

pdf("fig1.pdf")
par(mfrow=c(3,2))

#vioplots for trust
vioplot(df.know.su.tr.1$know~df.know.su.tr.1$tr,xlab="Trust",ylab="Scientific Knowledge", names=c("Other","Negative"),
	col="gold", cex.axis=1.2, cex.lab=0.8)
vioplot(df.know.su.tr.1$su~df.know.su.tr.1$tr,xlab="Trust",ylab="Self-assessed Understanding", names=c("Other","Negative"),
	col="gold", cex.axis=1.2, cex.lab=0.8)
#vioplots for hype
vioplot(df.know.su.hy.1$know~df.know.su.hy.1$hy,xlab="Hype",ylab="Scientific Knowledge",names=c("Other","Negative"),
	col="gold", cex.axis=1.2, cex.lab=0.8) 
title("A")
vioplot(df.know.su.hy.1$su~df.know.su.hy.1$hy,xlab="Hype",ylab="Self-assessed Understanding", names=c("Other","Negative"),
	col="gold", cex.axis=1.2, cex.lab=0.8)
title("B")
#vioplot for GM
vioplot(df.know.su.gm.1$know~df.know.su.gm.1$gm,xlab="GM",ylab="Scientific Knowledge", names=c("Other","Negative"),
	col="gold", cex.axis=1.2, cex.lab=0.8)
vioplot(df.know.su.gm.1$su~df.know.su.gm.1$gm,xlab="GM",ylab="Self-assessed Understanding", names=c("Other","Negative"),
	col="gold", cex.axis=1.2, cex.lab=0.8)
#Vioplot for Vaccine
vioplot(df.know.su.vac.1$know~df.know.su.vac.1$vac,xlab="Vaccine",ylab="Scientific Knowledge", names=c("Other","Negative"),
	col="gold", cex.axis=1.2, cex.lab=0.8)
vioplot(df.know.su.vac.1$su~df.know.su.vac.1$vac,xlab="Vaccine",ylab="Self-assessed Understanding", names=c("Other","Negative"),
	col="gold", cex.axis=1.2, cex.lab=0.8)
dev.off()



#QUESTION 1 & 2 : 
## Running univariate logistic regressions

#KNOWLEDGE logistic regressions (univariate)
## Knowledge and trust
## creating clean data frame
df.know.tr <-data.frame(trust=tr,know=data1$sci_know) 
df.know.tr.1 <-na.omit(df.know.tr)
## running logistic regression
logistic.know.tr <- glm(trust ~ know, data=df.know.tr, family="binomial")
## summarising and identifying coefficients
sum.log.know.tr <- summary(logistic.know.tr)
coeff.log.know.tr <- sum.log.know.tr$coefficients

## Knowledge and hype
df.know.hy <-data.frame(hy=hy,know=data1$sci_know)
df.know.hy.1 <-na.omit(df.know.hy)
logistic.know.hy <- glm(hy ~ know, data=df.know.hy, family="binomial")
sum.log.know.hy <- summary(logistic.know.hy)
coeff.log.know.hy <- sum.log.know.hy$coefficients

## Knowledge and GM
df.know.gm <-data.frame(gm,know=data1$sci_know)
df.know.gm.1 <-na.omit(df.know.gm)
logistic.know.gm <- glm(gm ~ know, data=df.know.gm, family="binomial")
sum.log.know.gm <- summary(logistic.know.gm)
coeff.log.know.gm <- sum.log.know.gm$coefficients

## Knowledge and vaccines
df.know.vac <-data.frame(vac,know=data1$sci_know)
df.know.vac.1 <-na.omit(df.know.vac)
logistic.know.vac <- glm(vac ~ know, data=df.know.vac, family="binomial")
sum.log.know.vac <- summary(logistic.know.vac)
coeff.log.know.vac <- sum.log.know.vac$coefficients

## identifying estimate and P-values for knowledge
e.know.tr.1 <- coeff.log.know.tr[2,1]	# estimate values
e.know.hy.1 <- coeff.log.know.hy[2,1]
e.know.gm.1 <- coeff.log.know.gm[2,1]
e.know.vac.1 <- coeff.log.know.vac[2,1]
p.know.tr.1 <- coeff.log.know.tr[2,4]	#P-values
p.know.hy.1 <- coeff.log.know.hy[2,4]
p.know.gm.1 <- coeff.log.know.gm[2,4]
p.know.vac.1 <- coeff.log.know.vac[2,4]

## Calculating the 95% confidence interval for odds ratio (exp(estimate)) rounded to two decimals
ci.know.tr.u.all <- confint(logistic.know.tr, level = 0.95, trace=FALSE)
ci.know.tr.u.a <-(round(exp(ci.know.tr.u.all[2,1]),2))
ci.know.tr.u.b <-(round(exp(ci.know.tr.u.all[2,2]),2))
ci.know.tr.u <- paste(ci.know.tr.u.a, ci.know.tr.u.b, sep="-")
ci.know.hy.u.all <- confint(logistic.know.hy, level = 0.95, trace=FALSE)
ci.know.hy.u.a <-(round(exp(ci.know.hy.u.all[2,1]),2))
ci.know.hy.u.b <-(round(exp(ci.know.hy.u.all[2,2]),2))
ci.know.hy.u <- paste(ci.know.hy.u.a, ci.know.hy.u.b, sep="-")
ci.know.gm.u.all <- confint(logistic.know.gm, level = 0.95, trace=FALSE)
ci.know.gm.u.a <-(round(exp(ci.know.gm.u.all[2,1]),2))
ci.know.gm.u.b <-(round(exp(ci.know.gm.u.all[2,2]),2))
ci.know.gm.u <- paste(ci.know.gm.u.a, ci.know.gm.u.b, sep="-")
ci.know.vac.u.all <- confint(logistic.know.vac, level = 0.95, trace=FALSE)
ci.know.vac.u.a <-(round(exp(ci.know.vac.u.all[2,1]),2))
ci.know.vac.u.b <-(round(exp(ci.know.vac.u.all[2,2]),2))
ci.know.vac.u <- paste(ci.know.vac.u.a, ci.know.vac.u.b, sep="-")


#Self assessed understanding univariate logistic regressions
df.su.tr <-data.frame(trust=tr,su=data1$scigen_self_under) 
df.su.tr.1 <-na.omit(df.su.tr)
logistic.su.tr <- glm(trust ~ su, data=df.su.tr, family="binomial")
sum.log.su.tr <- summary(logistic.su.tr)
coeff.log.su.tr <- sum.log.su.tr$coefficients
df.su.hy <-data.frame(hy=hy,su=data1$scigen_self_under)
df.su.hy.1 <-na.omit(df.su.hy)
logistic.su.hy <- glm(hy ~ su, data=df.su.hy, family="binomial")
sum.log.su.hy <- summary(logistic.su.hy)
coeff.log.su.hy <- sum.log.su.hy$coefficients
df.su.gm <-data.frame(gm,su=data1$scigen_self_under)
df.su.gm.1 <-na.omit(df.su.gm)
logistic.su.gm <- glm(gm ~ su, data=df.su.gm, family="binomial")
sum.log.su.gm <- summary(logistic.su.gm)
coeff.log.su.gm <- sum.log.su.gm$coefficients
df.su.vac <-data.frame(vac,su=data1$scigen_self_under)
df.su.vac.1 <-na.omit(df.su.vac)
logistic.su.vac <- glm(vac ~ su, data=df.su.vac, family="binomial")
sum.log.su.vac <- summary(logistic.su.vac)
coeff.log.su.vac <- sum.log.su.vac$coefficients
e.su.tr.1 <- coeff.log.su.tr[2,1]
e.su.hy.1 <- coeff.log.su.hy[2,1]
e.su.gm.1 <- coeff.log.su.gm[2,1]
e.su.vac.1 <- coeff.log.su.vac[2,1]
p.su.tr.1 <- coeff.log.su.tr[2,4]
p.su.hy.1 <- coeff.log.su.hy[2,4]
p.su.gm.1 <- coeff.log.su.gm[2,4]
p.su.vac.1 <- coeff.log.su.vac[2,4]
p_vec_su <- c(p.su.tr.1,p.su.hy.1,p.su.gm.1,p.su.vac.1)
p_vec_holm_su <- p.adjust((p_vec_su), "holm") <= 0.05
ci.su.tr.u.all <- confint(logistic.su.tr, level = 0.95, trace=FALSE)
ci.su.tr.u.a <-(round(exp(ci.su.tr.u.all[2,1]),2))
ci.su.tr.u.b <-(round(exp(ci.su.tr.u.all[2,2]),2))
ci.su.tr.u <- paste(ci.su.tr.u.a, ci.su.tr.u.b, sep="-")
ci.su.hy.u.all <- confint(logistic.su.hy, level = 0.95, trace=FALSE)
ci.su.hy.u.a <-(round(exp(ci.su.hy.u.all[2,1]),2))
ci.su.hy.u.b <-(round(exp(ci.su.hy.u.all[2,2]),2))
ci.su.hy.u <- paste(ci.su.hy.u.a, ci.su.hy.u.b, sep="-")
ci.su.gm.u.all <- confint(logistic.su.gm, level = 0.95, trace=FALSE)
ci.su.gm.u.a <-(round(exp(ci.su.gm.u.all[2,1]),2))
ci.su.gm.u.b <-(round(exp(ci.su.gm.u.all[2,2]),2))
ci.su.gm.u <- paste(ci.su.gm.u.a, ci.su.gm.u.b, sep="-")
ci.su.vac.u.all <- confint(logistic.su.vac, level = 0.95, trace=FALSE)
ci.su.vac.u.a <-(round(exp(ci.su.vac.u.all[2,1]),2))
ci.su.vac.u.b <-(round(exp(ci.su.vac.u.all[2,2]),2))
ci.su.vac.u <- paste(ci.su.vac.u.a, ci.su.vac.u.b, sep="-")

#religion_2 univariate logistic regressions
df.rel2.tr <-data.frame(rel2, trust=tr) 
df.rel2.tr <-na.omit(df.rel2.tr)
logistic.rel2.tr <- glm(trust ~ rel2, data=df.rel2.tr, family="binomial")
sum.log.rel2.tr <- summary(logistic.rel2.tr)
coeff.log.rel2.tr <- sum.log.rel2.tr$coefficients
df.rel2.hy <-data.frame(hy=hy,rel2)
df.rel2.hy <-na.omit(df.rel2.hy)
logistic.rel2.hy <- glm(hy ~ rel2, data=df.rel2.hy, family="binomial")
sum.log.rel2.hy <- summary(logistic.rel2.hy)
coeff.log.rel2.hy <- sum.log.rel2.hy$coefficients
df.rel2.gm <-data.frame(gm,rel2) 
df.rel2.gm.1 <-na.omit(df.rel2.gm)
logistic.rel2.gm <- glm(gm ~ rel2, data=df.rel2.gm, family="binomial")
sum.log.rel2.gm <- summary(logistic.rel2.gm)
coeff.log.rel2.gm <- sum.log.rel2.gm$coefficients
df.rel2.vac <-data.frame(vac,rel2)
df.rel2.vac.1 <-na.omit(df.rel2.vac)
logistic.rel2.vac <- glm(vac ~ rel2, data=df.rel2.vac, family="binomial")
sum.log.rel2.vac <- summary(logistic.rel2.vac)
coeff.log.rel2.vac <- sum.log.rel2.vac$coefficients
e.rel2.tr.1 <- coeff.log.rel2.tr[2,1]
e.rel2.hy.1 <- coeff.log.rel2.hy[2,1]
e.rel2.gm.1 <- coeff.log.rel2.gm[2,1]
e.rel2.vac.1 <- coeff.log.rel2.vac[2,1]
p.rel2.tr.1 <- coeff.log.rel2.tr[2,4] 
p.rel2.hy.1 <- coeff.log.rel2.hy[2,4]
p.rel2.gm.1 <- coeff.log.rel2.gm[2,4]
p.rel2.vac.1 <- coeff.log.rel2.vac[2,4]
p_vec_rel2 <- c(p.rel2.tr.1, p.rel2.hy.1, p.rel2.gm.1, p.rel2.vac.1)
p_vec_holm_rel2 <- p.adjust((p_vec_rel2), "holm") <= 0.05
ci.rel2.tr.u.all <- confint(logistic.rel2.tr, level = 0.95, trace=FALSE)
ci.rel2.tr.u.a <-(round(exp(ci.rel2.tr.u.all[2,1]),2))
ci.rel2.tr.u.b <-(round(exp(ci.rel2.tr.u.all[2,2]),2))
ci.rel2.tr.u <- paste(ci.rel2.tr.u.a, ci.rel2.tr.u.b, sep="-")
ci.rel2.hy.u.all <- confint(logistic.rel2.hy, level = 0.95, trace=FALSE)
ci.rel2.hy.u.a <-(round(exp(ci.rel2.hy.u.all[2,1]),2))
ci.rel2.hy.u.b <-(round(exp(ci.rel2.hy.u.all[2,2]),2))
ci.rel2.hy.u <- paste(ci.rel2.hy.u.a, ci.rel2.hy.u.b, sep="-")
ci.rel2.gm.u.all <- confint(logistic.rel2.gm, level = 0.95, trace=FALSE)
ci.rel2.gm.u.a <-(round(exp(ci.rel2.gm.u.all[2,1]),2))
ci.rel2.gm.u.b <-(round(exp(ci.rel2.gm.u.all[2,2]),2))
ci.rel2.gm.u <- paste(ci.rel2.gm.u.a, ci.rel2.gm.u.b, sep="-")
ci.rel2.vac.u.all <- confint(logistic.rel2.vac, level = 0.95, trace=FALSE)
ci.rel2.vac.u.a <-(round(exp(ci.rel2.vac.u.all[2,1]),2))
ci.rel2.vac.u.b <-(round(exp(ci.rel2.vac.u.all[2,2]),2))
ci.rel2.vac.u <- paste(ci.rel2.vac.u.a, ci.rel2.vac.u.b, sep="-")

#religion_3 univariate logistic regression
df.rel3.tr <-data.frame(rel3, trust=tr) 
df.rel3.tr <-na.omit(df.rel3.tr)
logistic.rel3.tr <- glm(trust ~ rel3, data=df.rel3.tr, family="binomial")
sum.log.rel3.tr <- summary(logistic.rel3.tr)
coeff.log.rel3.tr <- sum.log.rel3.tr$coefficients
df.rel3.hy <-data.frame(hy=hy,rel3)
df.rel3.hy <-na.omit(df.rel3.hy)
logistic.rel3.hy <- glm(hy ~ rel3, data=df.rel3.hy, family="binomial")
sum.log.rel3.hy <- summary(logistic.rel3.hy)
coeff.log.rel3.hy <- sum.log.rel3.hy$coefficients
df.rel3.gm <-data.frame(gm,rel3) 
df.rel3.gm.1 <-na.omit(df.rel3.gm)
logistic.rel3.gm <- glm(gm ~ rel3, data=df.rel3.gm, family="binomial")
sum.log.rel3.gm <- summary(logistic.rel3.gm)
coeff.log.rel3.gm <- sum.log.rel3.gm$coefficients
df.rel3.vac <-data.frame(vac,rel3)
df.rel3.vac.1 <-na.omit(df.rel3.vac)
logistic.rel3.vac <- glm(vac ~ rel3, data=df.rel3.vac, family="binomial")
sum.log.rel3.vac <- summary(logistic.rel3.vac)
coeff.log.rel3.vac <- sum.log.rel3.vac$coefficients
e.rel3.tr.1 <- coeff.log.rel3.tr[2,1]
e.rel3.hy.1 <- coeff.log.rel3.hy[2,1]
e.rel3.gm.1 <- coeff.log.rel3.gm[2,1]
e.rel3.vac.1 <- coeff.log.rel3.vac[2,1]
p.rel3.tr.1 <- coeff.log.rel3.tr[2,4] 
p.rel3.hy.1 <- coeff.log.rel3.hy[2,4]
p.rel3.gm.1 <- coeff.log.rel3.gm[2,4]
p.rel3.vac.1 <- coeff.log.rel3.vac[2,4]
p_vec_rel3 <- c(p.rel3.tr.1, p.rel3.hy.1, p.rel3.gm.1, p.rel3.vac.1)
p_vec_holm_rel3 <- p.adjust((p_vec_rel3), "holm") <= 0.05
ci.rel3.tr.u.all <- confint(logistic.rel3.tr, level = 0.95, trace=FALSE)
ci.rel3.tr.u.a <-(round(exp(ci.rel3.tr.u.all[2,1]),2))
ci.rel3.tr.u.b <-(round(exp(ci.rel3.tr.u.all[2,2]),2))
ci.rel3.tr.u <- paste(ci.rel3.tr.u.a, ci.rel3.tr.u.b, sep="-")
ci.rel3.hy.u.all <- confint(logistic.rel3.hy, level = 0.95, trace=FALSE)
ci.rel3.hy.u.a <-(round(exp(ci.rel3.hy.u.all[2,1]),2))
ci.rel3.hy.u.b <-(round(exp(ci.rel3.hy.u.all[2,2]),2))
ci.rel3.hy.u <- paste(ci.rel3.hy.u.a, ci.rel3.hy.u.b, sep="-")
ci.rel3.gm.u.all <- confint(logistic.rel3.gm, level = 0.95, trace=FALSE)
ci.rel3.gm.u.a <-(round(exp(ci.rel3.gm.u.all[2,1]),2))
ci.rel3.gm.u.b <-(round(exp(ci.rel3.gm.u.all[2,2]),2))
ci.rel3.gm.u <- paste(ci.rel3.gm.u.a, ci.rel3.gm.u.b, sep="-")
ci.rel3.vac.u.all <- confint(logistic.rel3.vac, level = 0.95, trace=FALSE)
ci.rel3.vac.u.a <-(round(exp(ci.rel3.vac.u.all[2,1]),2))
ci.rel3.vac.u.b <-(round(exp(ci.rel3.vac.u.all[2,2]),2))
ci.rel3.vac.u <- paste(ci.rel3.vac.u.a, ci.rel3.vac.u.b, sep="-")

#POLITICS univariate logistic regression
df.pol.tr <-data.frame(trust=tr,pol)
df.pol.tr.1 <-na.omit(df.pol.tr)
logistic.pol.tr <- glm(trust ~ pol, data=df.pol.tr, family="binomial")
sum.log.pol.tr <- summary(logistic.pol.tr)
coeff.log.pol.tr <- sum.log.pol.tr$coefficients
df.pol.hy <-data.frame(hy=hy,pol) 
df.pol.hy.1 <-na.omit(df.pol.hy)
logistic.pol.hy <- glm(hy ~ pol, data=df.pol.hy, family="binomial")
sum.log.pol.hy <- summary(logistic.pol.hy)
coeff.log.pol.hy <- sum.log.pol.hy$coefficients
df.pol.gm <-data.frame(gm,pol)
df.pol.gm.1 <-na.omit(df.pol.gm)
logistic.pol.gm <- glm(gm ~ pol, data=df.pol.gm, family="binomial")
sum.log.pol.gm <- summary(logistic.pol.gm)
coeff.log.pol.gm <- sum.log.pol.gm$coefficients
df.pol.vac <-data.frame(vac,pol) 
df.pol.vac.1 <-na.omit(df.pol.vac)
logistic.pol.vac <- glm(vac ~ pol, data=df.pol.vac, family="binomial")
sum.log.pol.vac <- summary(logistic.pol.vac)
coeff.log.pol.vac <- sum.log.pol.vac$coefficients
e.pol.tr.1 <- coeff.log.pol.tr[2,1]
e.pol.hy.1 <- coeff.log.pol.hy[2,1]
e.pol.gm.1 <- coeff.log.pol.gm[2,1]
e.pol.vac.1 <- coeff.log.pol.vac[2,1]
p.pol.tr.1 <- coeff.log.pol.tr[2,4]
p.pol.hy.1 <- coeff.log.pol.hy[2,4]
p.pol.gm.1 <- coeff.log.pol.gm[2,4]
p.pol.vac.1 <- coeff.log.pol.vac[2,4]
p_vec_pol <- c(p.pol.tr.1, p.pol.hy.1, p.pol.gm.1, p.pol.vac.1)
p_vec_holm_pol <- p.adjust((p_vec_pol), "holm") <= 0.05
ci.pol.tr.u.all <- confint(logistic.pol.tr, level = 0.95, trace=FALSE)
ci.pol.tr.u.a <-(round(exp(ci.pol.tr.u.all[2,1]),2))
ci.pol.tr.u.b <-(round(exp(ci.pol.tr.u.all[2,2]),2))
ci.pol.tr.u <- paste(ci.pol.tr.u.a, ci.pol.tr.u.b, sep="-")
ci.pol.hy.u.all <- confint(logistic.pol.hy, level = 0.95, trace=FALSE)
ci.pol.hy.u.a <-(round(exp(ci.pol.hy.u.all[2,1]),2))
ci.pol.hy.u.b <-(round(exp(ci.pol.hy.u.all[2,2]),2))
ci.pol.hy.u <- paste(ci.pol.hy.u.a, ci.pol.hy.u.b, sep="-")
ci.pol.gm.u.all <- confint(logistic.pol.gm, level = 0.95, trace=FALSE)
ci.pol.gm.u.a <-(round(exp(ci.pol.gm.u.all[2,1]),2))
ci.pol.gm.u.b <-(round(exp(ci.pol.gm.u.all[2,2]),2))
ci.pol.gm.u <- paste(ci.pol.gm.u.a, ci.pol.gm.u.b, sep="-")
ci.pol.vac.u.all <- confint(logistic.pol.vac, level = 0.95, trace=FALSE)
ci.pol.vac.u.a <-(round(exp(ci.pol.vac.u.all[2,1]),2))
ci.pol.vac.u.b <-(round(exp(ci.pol.vac.u.all[2,2]),2))
ci.pol.vac.u <- paste(ci.pol.vac.u.a, ci.pol.vac.u.b, sep="-")

#SEX univariate logistic regression
df.sex.tr <-data.frame(sex, trust=tr)
df.sex.tr.1 <-na.omit(df.sex.tr)
logistic.sex.tr <- glm(trust ~ sex, data=df.sex.tr, family="binomial")
sum.log.sex.tr <- summary(logistic.sex.tr)
coeff.log.sex.tr <- sum.log.sex.tr$coefficients

df.sex.hy <-data.frame(hy=hy,sex)
df.sex.hy.1 <-na.omit(df.sex.hy)
logistic.sex.hy <- glm(hy ~ sex, data=df.sex.hy, family="binomial")
sum.log.sex.hy <- summary(logistic.sex.hy)
coeff.log.sex.hy <- sum.log.sex.hy$coefficients
df.sex.gm <-data.frame(gm,sex) 
df.sex.gm.1 <-na.omit(df.sex.gm)
logistic.sex.gm <- glm(gm ~ sex, data=df.sex.gm, family="binomial")
sum.log.sex.gm <- summary(logistic.sex.gm)
coeff.log.sex.gm <- sum.log.sex.gm$coefficients
df.sex.vac <-data.frame(vac,sex)
df.sex.vac.1 <-na.omit(df.sex.vac)
logistic.sex.vac <- glm(vac ~ sex, data=df.sex.vac, family="binomial")
sum.log.sex.vac <- summary(logistic.sex.vac)
sum.log.sex.vac.coeff <- sum.log.sex.vac$coefficients
coeff.log.sex.vac <- sum.log.sex.vac$coefficients
e.sex.tr.1 <- coeff.log.sex.tr[2,1]
e.sex.hy.1 <- coeff.log.sex.hy[2,1]
e.sex.gm.1 <- coeff.log.sex.gm[2,1]
e.sex.vac.1 <- coeff.log.sex.vac[2,1]
p.sex.tr.1 <- coeff.log.sex.tr[2,4]
p.sex.hy.1 <- coeff.log.sex.hy[2,4]
p.sex.gm.1 <- coeff.log.sex.gm[2,4]
p.sex.vac.1 <- coeff.log.sex.vac[2,4]
p_vec_sex <- c(p.sex.tr.1,p.sex.hy.1,p.sex.gm.1,p.sex.vac.1)
p_vec_holm_sex <- p.adjust((p_vec_sex), "holm") <= 0.05
ci.sex.tr.u.all <- confint(logistic.sex.tr, level = 0.95, trace=FALSE)
ci.sex.tr.u.a <-(round(exp(ci.sex.tr.u.all[2,1]),2))
ci.sex.tr.u.b <-(round(exp(ci.sex.tr.u.all[2,2]),2))
ci.sex.hy.u.all <- confint(logistic.sex.hy, level = 0.95, trace=FALSE)
ci.sex.hy.u.a <-(round(exp(ci.sex.hy.u.all[2,1]),2))
ci.sex.hy.u.b <-(round(exp(ci.sex.hy.u.all[2,2]),2))
ci.sex.gm.u.all <- confint(logistic.sex.gm, level = 0.95, trace=FALSE)
ci.sex.gm.u.a <-(round(exp(ci.sex.gm.u.all[2,1]),2))
ci.sex.gm.u.b <-(round(exp(ci.sex.gm.u.all[2,2]),2))
ci.sex.vac.u.all <- confint(logistic.sex.vac, level = 0.95, trace=FALSE)
ci.sex.vac.u.a <-(round(exp(ci.sex.vac.u.all[2,1]),2))
ci.sex.vac.u.b <-(round(exp(ci.sex.vac.u.all[2,2]),2))
ci.sex.tr.u <- paste(ci.sex.tr.u.a, ci.sex.tr.u.b, sep="-")
ci.sex.hy.u <- paste(ci.sex.hy.u.a, ci.sex.hy.u.b, sep="-")
ci.sex.gm.u <- paste(ci.sex.gm.u.a, ci.sex.gm.u.b, sep="-")
ci.sex.vac.u <- paste(ci.sex.vac.u.a, ci.sex.vac.u.b, sep="-")

#ETHNICITY univariate logistic regression
df.eth.tr <-data.frame(eth=ethnic, trust=tr)
df.eth.tr.1 <-na.omit(df.eth.tr)
logistic.eth.tr <- glm(trust ~ eth, data=df.eth.tr, family="binomial")
sum.log.eth.tr <- summary(logistic.eth.tr)
coeff.log.eth.tr <- sum.log.eth.tr$coefficients
df.eth.hy <-data.frame(hy=hy,eth=ethnic)
df.eth.hy.1 <-na.omit(df.eth.hy)
logistic.eth.hy <- glm(hy ~ eth, data=df.eth.hy, family="binomial")
sum.log.eth.hy <- summary(logistic.eth.hy)
coeff.log.eth.hy <- sum.log.eth.hy$coefficients
df.eth.gm <-data.frame(gm,eth=ethnic) 
df.eth.gm.1 <-na.omit(df.eth.gm)
logistic.eth.gm <- glm(gm ~ eth, data=df.eth.gm, family="binomial")
sum.log.eth.gm <- summary(logistic.eth.gm)
coeff.log.eth.gm <- sum.log.eth.gm$coefficients
df.eth.vac <-data.frame(vac,eth=ethnic)
df.eth.vac.1 <-na.omit(df.eth.vac)
logistic.eth.vac <- glm(vac ~ eth, data=df.eth.vac, family="binomial")
sum.log.eth.vac <- summary(logistic.eth.vac)
sum.log.eth.vac.coeff <- sum.log.eth.vac$coefficients
coeff.log.eth.vac <- sum.log.eth.vac$coefficients
e.eth.tr.1 <- coeff.log.eth.tr[2,1]
e.eth.hy.1 <- coeff.log.eth.hy[2,1]
e.eth.gm.1 <- coeff.log.eth.gm[2,1]
e.eth.vac.1 <- coeff.log.eth.vac[2,1]
p.eth.tr.1 <- coeff.log.eth.tr[2,4]
p.eth.hy.1 <- coeff.log.eth.hy[2,4]
p.eth.gm.1 <- coeff.log.eth.gm[2,4]
p.eth.vac.1 <- coeff.log.eth.vac[2,4]
p_vec_eth <- c(p.eth.tr.1,p.eth.hy.1,p.eth.gm.1,p.eth.vac.1)
p_vec_holm_eth <- p.adjust((p_vec_eth), "holm") <= 0.05
ci.eth.tr.u.all <- confint(logistic.eth.tr, level = 0.95, trace=FALSE)
ci.eth.tr.u.a <-(round(exp(ci.eth.tr.u.all[2,1]),2))
ci.eth.tr.u.b <-(round(exp(ci.eth.tr.u.all[2,2]),2))
ci.eth.tr.u <- paste(ci.eth.tr.u.a, ci.eth.tr.u.b, sep="-")
ci.eth.hy.u.all <- confint(logistic.eth.hy, level = 0.95, trace=FALSE)
ci.eth.hy.u.a <-(round(exp(ci.eth.hy.u.all[2,1]),2))
ci.eth.hy.u.b <-(round(exp(ci.eth.hy.u.all[2,2]),2))
ci.eth.hy.u <- paste(ci.eth.hy.u.a, ci.eth.hy.u.b, sep="-")
ci.eth.gm.u.all <- confint(logistic.eth.gm, level = 0.95, trace=FALSE)
ci.eth.gm.u.a <-(round(exp(ci.eth.gm.u.all[2,1]),2))
ci.eth.gm.u.b <-(round(exp(ci.eth.gm.u.all[2,2]),2))
ci.eth.gm.u <- paste(ci.eth.gm.u.a, ci.eth.gm.u.b, sep="-")
ci.eth.vac.u.all <- confint(logistic.eth.vac, level = 0.95, trace=FALSE)
ci.eth.vac.u.a <-(round(exp(ci.eth.vac.u.all[2,1]),2))
ci.eth.vac.u.b <-(round(exp(ci.eth.vac.u.all[2,2]),2))
ci.eth.vac.u <- paste(ci.eth.vac.u.a, ci.eth.vac.u.b, sep="-")

#AGE univariate logistic regression
df.age.tr <-data.frame(trust=tr,age=data1$Age)
df.age.tr.1 <-na.omit(df.age.tr)
logistic.age.tr <- glm(trust ~ age, data=df.age.tr, family="binomial")
sum.log.age.tr <- summary(logistic.age.tr)
coeff.log.age.tr <- sum.log.age.tr$coefficients
df.age.hy <-data.frame(hy=hy,age=data1$Age)
df.age.hy.1 <-na.omit(df.age.hy)
logistic.age.hy <- glm(hy ~ age, data=df.age.hy, family="binomial")
sum.log.age.hy <- summary(logistic.age.hy)
coeff.log.age.hy <- sum.log.age.hy$coefficients
df.age.gm <-data.frame(gm,age=data1$Age)
df.age.gm.1 <-na.omit(df.age.gm)
logistic.age.gm <- glm(gm ~ age, data=df.age.gm, family="binomial")
sum.log.age.gm <- summary(logistic.age.gm)
coeff.log.age.gm <- sum.log.age.gm$coefficients
df.age.vac <-data.frame(vac,age=data1$Age) 
df.age.vac.1 <-na.omit(df.age.vac)
logistic.age.vac <- glm(vac ~ age, data=df.age.vac, family="binomial")
sum.log.age.vac <- summary(logistic.age.vac)
coeff.log.age.vac <- sum.log.age.vac$coefficients
e.age.tr.1 <- coeff.log.age.tr[2,1]
e.age.hy.1 <- coeff.log.age.hy[2,1]
e.age.gm.1 <- coeff.log.age.gm[2,1]
e.age.vac.1 <- coeff.log.age.vac[2,1]
p.age.tr.1 <- coeff.log.age.tr[2,4]
p.age.hy.1 <- coeff.log.age.hy[2,4]
p.age.gm.1 <- coeff.log.age.gm[2,4]
p.age.vac.1 <- coeff.log.age.vac[2,4]
p_vec_age <- c(p.age.tr.1,p.age.hy.1,p.age.gm.1,p.age.vac.1)
p_vec_holm_age <- p.adjust((p_vec_age), "holm") <= 0.05
ci.age.tr.u.all <- confint(logistic.age.tr, level = 0.95, trace=FALSE)
ci.age.tr.u.a <-(round(exp(ci.age.tr.u.all[2,1]),2))
ci.age.tr.u.b <-(round(exp(ci.age.tr.u.all[2,2]),2))
ci.age.tr.u <- paste(ci.age.tr.u.a, ci.age.tr.u.b, sep="-")
ci.age.hy.u.all <- confint(logistic.age.hy, level = 0.95, trace=FALSE)
ci.age.hy.u.a <-(round(exp(ci.age.hy.u.all[2,1]),2))
ci.age.hy.u.b <-(round(exp(ci.age.hy.u.all[2,2]),2))
ci.age.hy.u <- paste(ci.age.hy.u.a, ci.age.hy.u.b, sep="-")
ci.age.gm.u.all <- confint(logistic.age.gm, level = 0.95, trace=FALSE)
ci.age.gm.u.a <-(round(exp(ci.age.gm.u.all[2,1]),2))
ci.age.gm.u.b <-(round(exp(ci.age.gm.u.all[2,2]),2))
ci.age.gm.u <- paste(ci.age.gm.u.a, ci.age.gm.u.b, sep="-")
ci.age.vac.u.all <- confint(logistic.age.vac, level = 0.95, trace=FALSE)
ci.age.vac.u.a <-(round(exp(ci.age.vac.u.all[2,1]),2))
ci.age.vac.u.b <-(round(exp(ci.age.vac.u.all[2,2]),2))
ci.age.vac.u <- paste(ci.age.vac.u.a, ci.age.vac.u.b, sep="-")

#educational attainment 2 univariate logistic regression
df.edu2.tr <-data.frame(trust=tr,edu2) 
df.edu2.tr.1 <-na.omit(df.edu2.tr)
logistic.edu2.tr <- glm(trust ~ edu2, data=df.edu2.tr, family="binomial")
sum.log.edu2.tr <- summary(logistic.edu2.tr)
coeff.log.edu2.tr <- sum.log.edu2.tr$coefficients
df.edu2.hy <-data.frame(hy=hy,edu2)
df.edu2.hy.1 <-na.omit(df.edu2.hy)
logistic.edu2.hy <- glm(hy ~ edu2, data=df.edu2.hy, family="binomial")
sum.log.edu2.hy <- summary(logistic.edu2.hy)
coeff.log.edu2.hy <- sum.log.edu2.hy$coefficients
df.edu2.gm <-data.frame(gm,edu2)
df.edu2.gm.1 <-na.omit(df.edu2.gm)
logistic.edu2.gm <- glm(gm ~ edu2, data=df.edu2.gm, family="binomial")
sum.log.edu2.gm <- summary(logistic.edu2.gm)
coeff.log.edu2.gm <- sum.log.edu2.gm$coefficients
df.edu2.vac <-data.frame(vac,edu2)
df.edu2.vac.1 <-na.omit(df.edu2.vac)
logistic.edu2.vac <- glm(vac ~ edu2, data=df.edu2.vac, family="binomial")
sum.log.edu2.vac <- summary(logistic.edu2.vac)
coeff.log.edu2.vac <- sum.log.edu2.vac$coefficients
e.edu2.tr.1 <- coeff.log.edu2.tr[2,1]
e.edu2.hy.1 <- coeff.log.edu2.hy[2,1]
e.edu2.gm.1 <- coeff.log.edu2.gm[2,1]
e.edu2.vac.1 <- coeff.log.edu2.vac[2,1]
p.edu2.tr.1 <- coeff.log.edu2.tr[2,4]
p.edu2.hy.1 <- coeff.log.edu2.hy[2,4]
p.edu2.gm.1 <- coeff.log.edu2.gm[2,4]
p.edu2.vac.1 <- coeff.log.edu2.vac[2,4]
p_vec_edu2 <- c(p.edu2.tr.1,p.edu2.hy.1,p.edu2.gm.1,p.edu2.vac.1)
p_vec_holm_edu2 <- p.adjust((p_vec_edu2), "holm") <= 0.05
ci.edu2.tr.u.all <- confint(logistic.edu2.tr, level = 0.95, trace=FALSE)
ci.edu2.tr.u.a <-(round(exp(ci.edu2.tr.u.all[2,1]),2))
ci.edu2.tr.u.b <-(round(exp(ci.edu2.tr.u.all[2,2]),2))
ci.edu2.tr.u <- paste(ci.edu2.tr.u.a, ci.edu2.tr.u.b, sep="-")
ci.edu2.hy.u.all <- confint(logistic.edu2.hy, level = 0.95, trace=FALSE)
ci.edu2.hy.u.a <-(round(exp(ci.edu2.hy.u.all[2,1]),2))
ci.edu2.hy.u.b <-(round(exp(ci.edu2.hy.u.all[2,2]),2))
ci.edu2.hy.u <- paste(ci.edu2.hy.u.a, ci.edu2.hy.u.b, sep="-")
ci.edu2.gm.u.all <- confint(logistic.edu2.gm, level = 0.95, trace=FALSE)
ci.edu2.gm.u.a <-(round(exp(ci.edu2.gm.u.all[2,1]),2))
ci.edu2.gm.u.b <-(round(exp(ci.edu2.gm.u.all[2,2]),2))
ci.edu2.gm.u <- paste(ci.edu2.gm.u.a, ci.edu2.gm.u.b, sep="-")
ci.edu2.vac.u.all <- confint(logistic.edu2.vac, level = 0.95, trace=FALSE)
ci.edu2.vac.u.a <-(round(exp(ci.edu2.vac.u.all[2,1]),2))
ci.edu2.vac.u.b <-(round(exp(ci.edu2.vac.u.all[2,2]),2))
ci.edu2.vac.u <- paste(ci.edu2.vac.u.a, ci.edu2.vac.u.b, sep="-")

#educational attainment 3 logistic regressions
df.edu3.tr <-data.frame(trust=tr,edu3) 
df.edu3.tr.1 <-na.omit(df.edu3.tr)
logistic.edu3.tr <- glm(trust ~ edu3, data=df.edu3.tr, family="binomial")
sum.log.edu3.tr <- summary(logistic.edu3.tr)
coeff.log.edu3.tr <- sum.log.edu3.tr$coefficients
df.edu3.hy <-data.frame(hy=hy,edu3)
df.edu3.hy.1 <-na.omit(df.edu3.hy)
logistic.edu3.hy <- glm(hy ~ edu3, data=df.edu3.hy, family="binomial")
sum.log.edu3.hy <- summary(logistic.edu3.hy)
coeff.log.edu3.hy <- sum.log.edu3.hy$coefficients
df.edu3.gm <-data.frame(gm,edu3)
df.edu3.gm.1 <-na.omit(df.edu3.gm)
logistic.edu3.gm <- glm(gm ~ edu3, data=df.edu3.gm, family="binomial")
sum.log.edu3.gm <- summary(logistic.edu3.gm)
coeff.log.edu3.gm <- sum.log.edu3.gm$coefficients
df.edu3.vac <-data.frame(vac,edu3)
df.edu3.vac.1 <-na.omit(df.edu3.vac)
logistic.edu3.vac <- glm(vac ~ edu3, data=df.edu3.vac, family="binomial")
sum.log.edu3.vac <- summary(logistic.edu3.vac)
coeff.log.edu3.vac <- sum.log.edu3.vac$coefficients
e.edu3.tr.1 <- coeff.log.edu3.tr[2,1]
e.edu3.hy.1 <- coeff.log.edu3.hy[2,1]
e.edu3.gm.1 <- coeff.log.edu3.gm[2,1]
e.edu3.vac.1 <- coeff.log.edu3.vac[2,1]
p.edu3.tr.1 <- coeff.log.edu3.tr[2,4]
p.edu3.hy.1 <- coeff.log.edu3.hy[2,4]
p.edu3.gm.1 <- coeff.log.edu3.gm[2,4]
p.edu3.vac.1 <- coeff.log.edu3.vac[2,4]
p_vec_edu3 <- c(p.edu3.tr.1,p.edu3.hy.1,p.edu3.gm.1,p.edu3.vac.1)
p_vec_holm_edu3 <- p.adjust((p_vec_edu3), "holm") <= 0.05
ci.edu3.tr.u.all <- confint(logistic.edu3.tr, level = 0.95, trace=FALSE)
ci.edu3.tr.u.a <-(round(exp(ci.edu3.tr.u.all[2,1]),2))
ci.edu3.tr.u.b <-(round(exp(ci.edu3.tr.u.all[2,2]),2))
ci.edu3.tr.u <- paste(ci.edu3.tr.u.a, ci.edu3.tr.u.b, sep="-")
ci.edu3.hy.u.all <- confint(logistic.edu3.hy, level = 0.95, trace=FALSE)
ci.edu3.hy.u.a <-(round(exp(ci.edu3.hy.u.all[2,1]),2))
ci.edu3.hy.u.b <-(round(exp(ci.edu3.hy.u.all[2,2]),2))
ci.edu3.hy.u <- paste(ci.edu3.hy.u.a, ci.edu3.hy.u.b, sep="-")
ci.edu3.gm.u.all <- confint(logistic.edu3.gm, level = 0.95, trace=FALSE)
ci.edu3.gm.u.a <-(round(exp(ci.edu3.gm.u.all[2,1]),2))
ci.edu3.gm.u.b <-(round(exp(ci.edu3.gm.u.all[2,2]),2))
ci.edu3.gm.u <- paste(ci.edu3.gm.u.a, ci.edu3.gm.u.b, sep="-")
ci.edu3.vac.u.all <- confint(logistic.edu3.vac, level = 0.95, trace=FALSE)
ci.edu3.vac.u.a <-(round(exp(ci.edu3.vac.u.all[2,1]),2))
ci.edu3.vac.u.b <-(round(exp(ci.edu3.vac.u.all[2,2]),2))
ci.edu3.vac.u <- paste(ci.edu3.vac.u.a, ci.edu3.vac.u.b, sep="-")


## Output table : univariate logistic regression

#Odds Ratio Vector (trust)
odds.tr.u <- c(round(exp(e.know.tr.1),2),round(exp(e.su.tr.1),2),round(exp(e.sex.tr.1),2),round(exp(e.eth.tr.1),2),"",round(exp(e.rel2.tr.1),2),round(exp(e.rel3.tr.1),2),round(exp(e.pol.tr.1),2),round(exp(e.age.tr.1),2),"",round(exp(e.edu2.tr.1),2),round(exp(e.edu3.tr.1),2))
#95% confidence interval vector (trust)
ci.tr.u <-c(ci.know.tr.u,ci.su.tr.u,ci.sex.tr.u,ci.eth.tr.u,"",ci.rel2.tr.u,ci.rel3.tr.u,ci.pol.tr.u,ci.age.tr.u,"",ci.edu2.tr.u,ci.edu3.tr.u)
#P value (trust)
p.tr.u <- c((round(p.know.tr.1,3)),(round(p.su.tr.1,3)),(round(p.sex.tr.1,3)),(round(p.eth.tr.1,3)),"",(round(p.rel2.tr.1,3)),(round(p.rel3.tr.1,3)),(round(p.pol.tr.1,3)),(round(p.age.tr.1,3)),"",(round(p.edu2.tr.1,3)),(round(p.edu3.tr.1,3)))
# Bonferroni correction
p.tr.u.bon <- ifelse(p.tr.u> .0125 & p.tr.u< .05 , "!", "")
# Paste together
p.tr.u.all <- paste(p.tr.u,p.tr.u.bon,sep="")

#Hype
odds.hy.u <- c(round(exp(e.know.hy.1),2),round(exp(e.su.hy.1),2),round(exp(e.sex.hy.1),2),round(exp(e.eth.hy.1),2),"",round(exp(e.rel2.hy.1),2),round(exp(e.rel3.hy.1),2),round(exp(e.pol.hy.1),2),round(exp(e.age.hy.1),2),"",round(exp(e.edu2.hy.1),2),round(exp(e.edu3.hy.1),2))
ci.hy.u <-c(ci.know.hy.u,ci.su.hy.u,ci.sex.hy.u,ci.eth.hy.u,"",ci.rel2.hy.u,ci.rel3.hy.u,ci.pol.hy.u,ci.age.hy.u,"",ci.edu2.hy.u,ci.edu3.hy.u)
p.hy.u <- c((round(p.know.hy.1,3)),(round(p.su.hy.1,3)),(round(p.sex.hy.1,3)),(round(p.eth.hy.1,3)),"",(round(p.rel2.hy.1,3)),(round(p.rel3.hy.1,3)),(round(p.pol.hy.1,3)),(round(p.age.hy.1,3)),"",(round(p.edu2.hy.1,3)),(round(p.edu3.hy.1,3)))
p.hy.u.bon <- ifelse(p.hy.u> .0125 & p.hy.u< .05 , "!", "")
p.hy.u.all <- paste(p.hy.u,p.hy.u.bon,sep="")

#GM
odds.gm.u <- c(round(exp(e.know.gm.1),2),round(exp(e.su.gm.1),2),round(exp(e.sex.gm.1),2),round(exp(e.eth.gm.1),2),"",round(exp(e.rel2.gm.1),2),round(exp(e.rel3.gm.1),2),round(exp(e.pol.gm.1),2),round(exp(e.age.gm.1),2),"",round(exp(e.edu2.gm.1),2),round(exp(e.edu3.gm.1),2))
ci.gm.u <-c(ci.know.gm.u,ci.su.gm.u,ci.sex.gm.u,ci.eth.gm.u,"",ci.rel2.gm.u,ci.rel3.gm.u,ci.pol.gm.u,ci.age.gm.u,"",ci.edu2.gm.u,ci.edu3.gm.u)
p.gm.u <- c((round(p.know.gm.1,3)),(round(p.su.gm.1,3)),(round(p.sex.gm.1,3)),(round(p.eth.gm.1,3)),"",(round(p.rel2.gm.1,3)),(round(p.rel3.gm.1,3)),(round(p.pol.gm.1,3)),(round(p.age.gm.1,3)),"",(round(p.edu2.gm.1,3)),(round(p.edu3.gm.1,3)))
p.gm.u.bon <- ifelse(p.gm.u> .0125 & p.gm.u< .05 , "!", "")
p.gm.u.all <- paste(p.gm.u,p.gm.u.bon,sep="")

#Vaccine 
odds.vac.u <- c(round(exp(e.know.vac.1),2),round(exp(e.su.vac.1),2),round(exp(e.sex.vac.1),2),round(exp(e.eth.vac.1),2),"",round(exp(e.rel2.vac.1),2),round(exp(e.rel3.vac.1),2),round(exp(e.pol.vac.1),2),round(exp(e.age.vac.1),2),"",round(exp(e.edu2.vac.1),2),round(exp(e.edu3.vac.1),2))
ci.vac.u <-c(ci.know.vac.u,ci.su.vac.u,ci.sex.vac.u,ci.eth.vac.u,"",ci.rel2.vac.u,ci.rel3.vac.u,ci.pol.vac.u,ci.age.vac.u,"",ci.edu2.vac.u,ci.edu3.vac.u)
p.vac.u <- c((round(p.know.vac.1,3)),(round(p.su.vac.1,3)),(round(p.sex.vac.1,3)),(round(p.eth.vac.1,3)),"",(round(p.rel2.vac.1,3)),(round(p.rel3.vac.1,3)),(round(p.pol.vac.1,3)),(round(p.age.vac.1,3)),"",(round(p.edu2.vac.1,3)),(round(p.edu3.vac.1,3)))
p.vac.u.bon <- ifelse(p.vac.u> .0125 & p.vac.u< .05 , "!", "")
p.vac.u.all <- paste(p.vac.u,p.vac.u.bon,sep="")

## creating dataframe for table
data.frame.factors <- data.frame (Factor=c("Knowledge","Self-Understanding","Sex","Ethnicity","Not Religious(Reference category)","Relgious not practising","Religious practising","Political View","Age","No qualifications (reference catergory)","Non-degree level qualifications","Degree level qualifications"))
data.frame.tr.u <- data.frame (OR = (odds.tr.u), CI = (ci.tr.u), P= (p.tr.u.all))
data.frame.hy.u <- data.frame (OR = (odds.hy.u), CI = (ci.hy.u), P= (p.hy.u.all))
data.frame.gm.u <- data.frame (OR = (odds.gm.u), CI = (ci.gm.u), P= (p.gm.u.all))
data.frame.vac.u <- data.frame (OR = (odds.vac.u), CI = (ci.vac.u), P= (p.vac.u.all))

## bind together individual dataframes
overall.u <- cbind (data.frame.factors,data.frame.tr.u,data.frame.hy.u,data.frame.gm.u,data.frame.vac.u)
## write CSV
write.csv (overall.u, "Unadjusted_oddsratio_univariate.csv", row.names=FALSE)



## Multivariate logistic regression

## Trust
## creating a clean dataframe 
df.tr.demographic <-data.frame(trust=tr,know,su,rel2,rel3,pol,ethnic,age=data1$Age,edu2,edu3,sex)
df.tr.demographic.1 <- na.omit(df.tr.demographic)
## create full model logistic regression
logistic.tr.demographic <- glm(trust ~ . , data=df.tr.demographic.1, family="binomial")
sum.log.tr.demographic <- summary(logistic.tr.demographic)
## remove sex, rel, pol, su and edu (non-significant variables)
logistic.tr.demographic.2 <- glm(trust ~ know+ethnic+age, data=df.tr.demographic.1, family="binomial")
sum.log.tr.demographic.2 <- summary(logistic.tr.demographic.2)


## comparing full model and model 2
anova (logistic.tr.demographic.2,logistic.tr.demographic, test="LRT")

## calculating coefficients of optimised model
coeff.log.demo.tr <- sum.log.tr.demographic.2$coefficients

## calculating odds ratio from estimate values
odd.know.demo.tr <- (round(exp(coeff.log.demo.tr[2,1]),2))# adjusted odds ratio
odd.eth.demo.tr <- round(exp(coeff.log.demo.tr[3,1]), 2)
odd.age.demo.tr <- round(exp(coeff.log.demo.tr[4,1]), 2)

p.know.demo.tr <- coeff.log.demo.tr[2,4]	#p-value
p.eth.demo.tr <- coeff.log.demo.tr[3,4]
p.age.demo.tr <- coeff.log.demo.tr[4,4]

#calculating confidence intervals
ci.tr.demo.m <- confint(logistic.tr.demographic.2, level = 0.95, trace=FALSE)
ci.tr.demo.m.know.a <-(round(exp(ci.tr.demo.m[2,1]),2))
ci.tr.demo.m.know.b <-(round(exp(ci.tr.demo.m[2,2]),2))
ci.tr.demo.m.know <- paste(ci.tr.demo.m.know.a,ci.tr.demo.m.know.b, sep="-")
ci.tr.demo.m.eth.a <-(round(exp(ci.tr.demo.m[3,1]),2))
ci.tr.demo.m.eth.b <-(round(exp(ci.tr.demo.m[3,2]),2))
ci.tr.demo.m.eth <- paste(ci.tr.demo.m.eth.a,ci.tr.demo.m.eth.b, sep="-")
ci.tr.demo.m.age.a <-(round(exp(ci.tr.demo.m[4,1]),2))
ci.tr.demo.m.age.b <-(round(exp(ci.tr.demo.m[4,2]),2))
ci.tr.demo.m.age <- paste(ci.tr.demo.m.age.a,ci.tr.demo.m.age.b, sep="-")


## HYPE
df.hy.demographic <-data.frame(hy,know,su,rel2,rel3,pol,ethnic,age=data1$Age,edu2,edu3,sex)
df.hy.demographic.1 <- na.omit(df.hy.demographic)
logistic.hy.demographic <- glm(hy ~ . , data=df.hy.demographic.1, family="binomial")
sum.log.hy.demographic <- summary(logistic.hy.demographic)

## remove sex, age, pol
logistic.hy.demographic.2 <- glm(hy ~ su+know+rel2+rel3,data=df.hy.demographic.1, family="binomial")
sum.log.hy.demographic.2 <- summary(logistic.hy.demographic.2)

## comparing full model and model 2
anova (logistic.hy.demographic.2,logistic.hy.demographic, test="LRT")

## calculating coefficients of optimised model
coeff.log.demo.hy <- sum.log.hy.demographic.2$coefficients

## calculating odds ratio from estimate values
odd.know.demo.hy <- round(exp(coeff.log.demo.hy[2,1]), 2)# adjusted odds ratio
odd.su.demo.hy <- round(exp(coeff.log.demo.hy[3,1]), 2)
odd.rel2.demo.hy <- round(exp(coeff.log.demo.hy[4,1]), 2)
odd.rel3.demo.hy <- round(exp(coeff.log.demo.hy[5,1]), 2)

p.know.demo.hy <- coeff.log.demo.hy[2,4]	#p-value
p.su.demo.hy <- coeff.log.demo.hy[3,4]
p.rel2.demo.hy <- coeff.log.demo.hy[4,4]
p.rel3.demo.hy <- coeff.log.demo.hy[5,4]

#calculating confidence intervals
ci.hy.demo.m <- confint(logistic.hy.demographic.2, level = 0.95, trace=FALSE)
ci.hy.demo.m.know.a <-round(exp(ci.hy.demo.m[2,1]),2)
ci.hy.demo.m.know.b <-round(exp(ci.hy.demo.m[2,2]),2)
ci.hy.demo.m.know <- paste(ci.hy.demo.m.know.a,ci.hy.demo.m.know.b, sep="-")
ci.hy.demo.m.su.a <-round(exp(ci.hy.demo.m[3,1]),2)
ci.hy.demo.m.su.b <-round(exp(ci.hy.demo.m[3,2]),2)
ci.hy.demo.m.su <- paste(ci.hy.demo.m.su.a,ci.hy.demo.m.su.b, sep="-")
ci.hy.demo.m.rel2.a <-round(exp(ci.hy.demo.m[4,1]),2)
ci.hy.demo.m.rel2.b <-round(exp(ci.hy.demo.m[4,2]),2)
ci.hy.demo.m.rel2 <- paste(ci.hy.demo.m.rel2.a,ci.hy.demo.m.rel2.b, sep="-")
ci.hy.demo.m.rel3.a <-round(exp(ci.hy.demo.m[4,1]),2)
ci.hy.demo.m.rel3.b <-round(exp(ci.hy.demo.m[4,2]),2)
ci.hy.demo.m.rel3 <- paste(ci.hy.demo.m.rel3.a,ci.hy.demo.m.rel3.b, sep="-")

## GM
df.gm.demographic <-data.frame(gm,know,su,rel2,rel3,pol,ethnic,age=data1$Age,edu2,edu3,sex)
df.gm.demographic.1 <- na.omit(df.gm.demographic)
logistic.gm.demographic <- glm(gm ~ . , data=df.gm.demographic.1, family="binomial")
sum.log.gm.demographic <- summary(logistic.gm.demographic)

#remove su, rel, pol?, edu
logistic.gm.demographic.2 <- glm(gm ~ sex+age+know,data=df.gm.demographic.1, family="binomial")
sum.log.gm.demographic.2 <- summary(logistic.gm.demographic.2)

logistic.gm.demographic.3 <- glm(gm ~ sex+age,data=df.gm.demographic.1, family="binomial")
sum.log.gm.demographic.3 <- summary(logistic.gm.demographic.3)
anova (logistic.gm.demographic.3,logistic.gm.demographic.2, test="LRT")

## calculating coefficients of optimised model
coeff.log.demo.gm <- sum.log.gm.demographic.2$coefficients

## calculating odds ratio from estimate values
odd.sex.demo.gm <- round(exp(coeff.log.demo.gm[2,1]), 2)	#odds ratio
odd.age.demo.gm <- round(exp(coeff.log.demo.gm[3,1]), 2)
p.sex.demo.gm <- coeff.log.demo.gm[2,4]	#p-value
p.age.demo.gm <- coeff.log.demo.gm[3,4]

#calculating confidence intervals
ci.gm.demo.m <- confint(logistic.gm.demographic.2, level = 0.95, trace=FALSE)
ci.gm.demo.m.sex.a <-(round(exp(ci.gm.demo.m[2,1]),2))
ci.gm.demo.m.sex.b <-(round(exp(ci.gm.demo.m[2,2]),2))
ci.gm.demo.m.sex <- paste(ci.gm.demo.m.sex.a,ci.gm.demo.m.sex.b, sep="-")
ci.gm.demo.m.age.a <-(round(exp(ci.gm.demo.m[3,1]),2))
ci.gm.demo.m.age.b <-(round(exp(ci.gm.demo.m[3,2]),2))
ci.gm.demo.m.age <- paste(ci.gm.demo.m.age.a,ci.gm.demo.m.age.b, sep="-")


## vac
df.vac.demographic <-data.frame(vac,know,su,rel2,rel3,pol,ethnic,age=data1$Age,edu2,edu3,sex)
df.vac.demographic.1 <- na.omit(df.vac.demographic)
logistic.vac.demographic <- glm(vac ~ . , data=df.vac.demographic.1, family="binomial")
sum.log.vac.demographic <- summary(logistic.vac.demographic)

#remove su,rel,ethnic
logistic.vac.demographic.2 <- glm(vac ~ know+age+edu2+edu3+sex,data=df.vac.demographic.1, family="binomial")
sum.log.vac.demographic.2 <- summary(logistic.vac.demographic.2)


anova (logistic.vac.demographic.2,logistic.vac.demographic, test="LRT")

## calculating odds ratio from estimate values
coeff.log.demo.vac <- sum.log.vac.demographic.2$coefficients
odd.know.demo.vac <- (round(exp(coeff.log.demo.vac[2,1]), 2))# adjusted odds ratio
odd.age.demo.vac <- (round(exp(coeff.log.demo.vac[3,1]), 2))
odd.edu2.demo.vac <- (round(exp(coeff.log.demo.vac[4,1]), 2))
odd.edu3.demo.vac <- (round(exp(coeff.log.demo.vac[5,1]), 2))
odd.sex.demo.vac <- (round(exp(coeff.log.demo.vac[6,1]), 2))

p.know.demo.vac <- coeff.log.demo.vac[2,4]	#p-value
p.age.demo.vac <- coeff.log.demo.vac[3,4]
p.edu2.demo.vac <- coeff.log.demo.vac[4,4]
p.edu3.demo.vac <- coeff.log.demo.vac[5,4]
p.sex.demo.vac <- coeff.log.demo.vac[6,4]

#calculating confidence intervals
ci.vac.demo.m <- confint(logistic.vac.demographic.2, level = 0.95, vacace=FALSE)
ci.vac.demo.m.know.a <-(round(exp(ci.vac.demo.m[2,1]),2))
ci.vac.demo.m.know.b <-(round(exp(ci.vac.demo.m[2,2]),2))
ci.vac.demo.m.know <- paste(ci.vac.demo.m.know.a,ci.vac.demo.m.know.b, sep="-")
ci.vac.demo.m.age.a <-(round(exp(ci.vac.demo.m[3,1]),2))
ci.vac.demo.m.age.b <-(round(exp(ci.vac.demo.m[3,2]),2))
ci.vac.demo.m.age <- paste(ci.vac.demo.m.age.a,ci.vac.demo.m.age.b, sep="-")
ci.vac.demo.m.edu2.a <-(round(exp(ci.vac.demo.m[4,1]),2))
ci.vac.demo.m.edu2.b <-(round(exp(ci.vac.demo.m[4,2]),2))
ci.vac.demo.m.edu2 <- paste(ci.vac.demo.m.edu2.a,ci.vac.demo.m.edu2.b, sep="-")
ci.vac.demo.m.edu3.a <-(round(exp(ci.vac.demo.m[5,1]),2))
ci.vac.demo.m.edu3.b <-(round(exp(ci.vac.demo.m[5,2]),2))
ci.vac.demo.m.edu3 <- paste(ci.vac.demo.m.edu3.a,ci.vac.demo.m.edu3.b, sep="-")
ci.vac.demo.m.sex.a <-(round(exp(ci.vac.demo.m[6,1]),2))
ci.vac.demo.m.sex.b <-(round(exp(ci.vac.demo.m[6,2]),2))
ci.vac.demo.m.sex <- paste(ci.vac.demo.m.sex.a,ci.vac.demo.m.sex.b, sep="-")

## creating output tables multivariate logistic regression
## creating vectors for output trust significant

## TRUST
## creating a data.frame
df.out.su.kn <- data.frame(Factor=c("Knowledge","Ethnic","Age"),
	Adjusted_OR=c(odd.know.demo.tr,odd.eth.demo.tr ,odd.age.demo.tr),
	CI = c(ci.tr.demo.m.know, ci.tr.demo.m.eth, ci.tr.demo.m.age),
	P = c(round(p.know.demo.tr,3),round(p.eth.demo.tr,3),round( p.age.demo.tr,3)),
	Unadjusted_OR = c(round(exp(e.know.tr.1),2), round(exp(e.eth.tr.1),2), round(exp(e.age.tr.1))),
	CI2 = c(ci.know.tr.u, ci.eth.tr.u, ci.age.tr.u),
	P2 = c(round(p.know.tr.1,3),round(p.eth.tr.1,3),round(p.age.tr.1,3)))
	
write.csv(df.out.su.kn, "table1_multivariate_trust.csv",row.names=FALSE)

## output table HYPE significant 
df.multi.hype <- data.frame(Factor=c("Knowledge","Self-Understanding","Not Religious(Reference category)","Relgious not practising","Religious practising"),
	Adjusted_OR=c(odd.know.demo.hy,odd.su.demo.hy ,"",odd.rel2.demo.hy,odd.rel3.demo.hy),
	CI = c(ci.hy.demo.m.know,ci.hy.demo.m.su,"",ci.hy.demo.m.rel2,ci.hy.demo.m.rel3),
	P = c(round(p.know.demo.hy,3) ,round(p.su.demo.hy,3),"",round(p.rel2.demo.hy,3),round(p.rel3.demo.hy,3)),
	Unadjusted_OR = c(round(exp(e.know.hy.1),2), round(exp(e.su.hy.1),2),"", round(exp(e.rel2.hy.1)),round(exp(e.rel3.hy.1))),
	CI2 = c(ci.know.hy.u, ci.su.hy.u, "",ci.rel2.hy.u, ci.rel3.hy.u),
	P2 = c(round(p.know.hy.1,3),round(p.su.hy.1,3),"",round(p.rel2.hy.1,3),round(p.rel3.hy.1,3)))
	
write.csv(df.multi.hype, "table1_multivariate_hype.csv",row.names=FALSE)


## output table GM significant 
df.multi.gm <- data.frame(Factor=c("Sex","Age"),
	Adjusted_OR=c(odd.sex.demo.gm,odd.age.demo.gm),
	CI = c(ci.gm.demo.m.sex,ci.gm.demo.m.age),
	P = c(round(p.sex.demo.gm,3) ,round(p.age.demo.gm,3)),
	Unadjusted_OR = c(round(exp(e.sex.gm.1),2), round(exp(e.age.gm.1),2)),
	CI2 = c(ci.sex.gm.u, ci.age.gm.u),
	P2 = c(round(p.sex.gm.1,3),round(p.age.gm.1,3)))
	
write.csv(df.multi.gm, "table1_multivariate_gm.csv",row.names=FALSE)

## output table Vac significant 
df.multi.vac <- data.frame(Factor=c("Knowledge","Age", "No qualifications (reference catergory)","Non-degree level qualifications","Degree level qualifications","Sex"),
	Adjusted_OR=c(odd.know.demo.vac, odd.age.demo.vac ,"",odd.edu2.demo.vac,odd.edu3.demo.vac,odd.sex.demo.vac),
	CI = c(ci.vac.demo.m.know,ci.vac.demo.m.age,"",ci.vac.demo.m.edu2,ci.vac.demo.m.edu3,ci.vac.demo.m.sex),
	P = c(round(p.know.demo.vac,3) ,round(p.age.demo.vac,3),"",round(p.edu2.demo.vac,3),round(p.edu3.demo.vac,3),round(p.sex.demo.vac,3)),
	Unadjusted_OR = c(round(exp(e.know.vac.1),2),round(exp(e.age.vac.1),2), "", round(exp(e.edu2.vac.1)),round(exp(e.edu3.vac.1)),round(exp(e.sex.vac.1),2)),
	CI2 = c(ci.know.vac.u,  ci.age.vac.u, "",ci.edu2.vac.u, ci.edu3.vac.u, ci.sex.vac.u),
	P2 = c(round(p.know.vac.1,3),round(p.age.vac.1,3),"",round(p.edu2.vac.1,3),round(p.edu3.vac.1,3),round(p.sex.vac.1,3)))
	
write.csv(df.multi.vac, "table1_multivariate_vac.csv",row.names=FALSE)

## Mc Fadden's pseudo R2
## how good is the fit of the multivariate model? 
r2_mcfadden(logistic.tr.demographic.2) #0.012
r2_mcfadden(logistic.hy.demographic.2) #0.032
r2_mcfadden(logistic.gm.demographic.2) #0.017
r2_mcfadden(logistic.vac.demographic.2) #0.091


#QUESTION 3 : what sources do the most antipathetic to science trust and use

## Question 3a - are those with negative attitudes more/less trusting than others
#note need to encode no trust as 0 metric. 

## encoding a rust metric where each person scores between 0 and 1 (0 being no trust)
data1$Who2Trust<- na.omit(rowSums( data1[,38:46] )/9.0)


## Running a T test for who trust
Who2Trustneg <- data1$Who2Trust[tr==1]
Who2Trustpos <- data1$Who2Trust[tr==0]
## test for variance
var.test(Who2Trustneg,Who2Trustpos)	#p=0.0005, variance is false
## run two tailed T-test
Who2TrustTtr <- t.test(Who2Trustneg,Who2Trustpos, var.equal=FALSE)
Who2TrustTtr$statistic # -8.59246
Who2TrustTtr$estimate # mean X = 0.358 mean Y = 0.4429
Who2TrustTtr$p.value # 1.65e-16

## HYPE 
Who2Trustneg.hy <- data1$Who2Trust[hy==1]
Who2Trustpos.hy <- data1$Who2Trust[hy==0] 
Who2TrustThy <- t.test(Who2Trustneg.hy,Who2Trustpos.hy, var.equal=FALSE)


## GM
Who2Trustneg.gm <- data1$Who2Trust[gm==1]
Who2Trustpos.gm <- data1$Who2Trust[gm==0]
Who2TrustTgm<- t.test(Who2Trustneg.gm,Who2Trustpos.gm, var.equal=FALSE)

## Vac
Who2Trustneg.vac <- data1$Who2Trust[vac==1]
Who2Trustpos.vac <- data1$Who2Trust[vac==0] 
Who2TrustTvac <- t.test(Who2Trustneg.vac,Who2Trustpos.vac, var.equal=FALSE)


## Creating a vector of T statistic
t.values <- c(round(Who2TrustTtr$statistic,2),round(Who2TrustThy$statistic,2),round(Who2TrustTgm$statistic,2),round(Who2TrustTvac$statistic,2))

## Creating a vector of P-values
t.test.P.values <- c(Who2TrustTtr$p.value, Who2TrustThy$p.value, Who2TrustTgm$p.value, Who2TrustTvac$p.value)
t.test.P.values <-round(t.test.P.values )

#printing out T value and significane levels and Holm test from P.value
t.test.all.df <- data.frame(Attitude=c("Trust","Hype","GM","Vac"), t_value =(t.values),P_Value=(t.test.P.values))
write.csv(t.test.all.df, "t_test_trust.csv",row.names=FALSE)


## Question 3b

## who do they trust?

## part 1- who the public trust overall

wt1 <-mean(data1$WhoTrust01, na.rm=TRUE)*100
wt2 <-mean(data1$WhoTrust02, na.rm=TRUE)*100
wt3 <-mean(data1$WhoTrust03, na.rm=TRUE)*100
wt4 <-mean(data1$WhoTrust04, na.rm=TRUE)*100
wt5 <-mean(data1$WhoTrust05, na.rm=TRUE)*100
wt6 <-mean(data1$WhoTrust06, na.rm=TRUE)*100
wt7 <-mean(data1$WhoTrust07, na.rm=TRUE)*100
wt8 <-mean(data1$WhoTrust08, na.rm=TRUE)*100


## printing mosaic diagrams
#TRUST 
pdf("fig3.pdf")
par(mfrow=c(4,2))

tabpct(tr,data1$WhoTrust01, graph= TRUE, main= "Work colleagues", ylab = "Trust01", xlab = "Trust")
tabpct(tr,data1$WhoTrust02, graph= TRUE, main= "The Government's Scientific advisers", ylab = "Trust02", xlab = "Trust")
tabpct(tr,data1$WhoTrust03, graph= TRUE, main= "The Government", ylab = "Trust03", xlab = "Trust")
tabpct(tr,data1$WhoTrust04, graph= TRUE, main= "Celebrities and public figures", ylab = "Trust04", xlab = "Trust")
tabpct(tr,data1$WhoTrust05, graph= TRUE, main= "Not-for-profit organisations", ylab = "Trust05", xlab = "Trust")
tabpct(tr,data1$WhoTrust06, graph= TRUE, main= "NHS spokesperson", ylab = "Trust06", xlab = "Trust")
tabpct(tr,data1$WhoTrust07, graph= TRUE, main= "Research Scientists/ Universities", ylab = "Trust07", xlab = "Trust")
tabpct(tr,data1$WhoTrust08, graph= TRUE, main= "Family/Friends", ylab = "Trust08", xlab = "Trust")

dev.off()


pdf("fig4.pdf")
par(mfrow=c(4,2))

#HYPE
tabpct(hy,data1$WhoTrust01, graph= TRUE, main= "Work colleagues", ylab = "Trust01", xlab = "Hype")
tabpct(hy,data1$WhoTrust02, graph= TRUE, main= "The Government's Scientific advisers", ylab = "Trust02", xlab = "Hype")
tabpct(hy,data1$WhoTrust03, graph= TRUE, main= "The Government", ylab = "Trust03", xlab = "Hype")
tabpct(hy,data1$WhoTrust04, graph= TRUE, main= "Celebrities and public figures", ylab = "Trust04", xlab = "Hype")
tabpct(hy,data1$WhoTrust05, graph= TRUE, main= "Not-for-profit organisations", ylab = "Trust05", xlab = "Hype")
tabpct(hy,data1$WhoTrust06, graph= TRUE, main= "NHS spokesperson", ylab = "Trust06", xlab = "Hype")
tabpct(hy,data1$WhoTrust07, graph= TRUE, main= "Research Scientists/ Universities", ylab = "Trust07", xlab = "Hype")
tabpct(hy,data1$WhoTrust08, graph= TRUE, main= "Family/Friends", ylab = "Trust08", xlab = "Hype")

#GM
pdf("fig5.pdf")
par(mfrow=c(4,2))

tabpct(gm,data1$WhoTrust01, graph= TRUE, main= "Work colleagues", ylab = "Trust01", xlab = "GM")
tabpct(gm,data1$WhoTrust02, graph= TRUE, main= "The Government's Scientific advisers", ylab = "Trust02", xlab = "GM")
tabpct(gm,data1$WhoTrust03, graph= TRUE, main= "The Government", ylab = "Trust03", xlab = "GM")
tabpct(gm,data1$WhoTrust04, graph= TRUE, main= "Celebrities and public figures", ylab = "Trust05", xlab = "GM")
tabpct(gm,data1$WhoTrust05, graph= TRUE, main= "Not-for-profit organisations", ylab = "Trust06", xlab = "GM")
tabpct(gm,data1$WhoTrust06, graph= TRUE, main= "NHS spokesperson", ylab = "Trust07", xlab = "GM")
tabpct(gm,data1$WhoTrust07, graph= TRUE, main= "Research Scientists/ Universities", ylab = "Trust07", xlab = "GM")
tabpct(gm,data1$WhoTrust08, graph= TRUE, main= "Family/Friends", ylab = "Trust08", xlab = "GM")

dev.off()

#VACCINE
pdf("fig6.pdf")
par(mfrow=c(4,2))

tabpct(vac,data1$WhoTrust01, graph= TRUE, main= "Work colleagues", ylab = "Trust01", xlab = "Vaccine")
tabpct(vac,data1$WhoTrust02, graph= TRUE, main= "The Government's Scientific advisers", ylab = "Trust02", xlab = "Vaccine")
tabpct(vac,data1$WhoTrust03, graph= TRUE, main= "The Government", ylab = "Trust03", xlab = "Vaccine")
tabpct(vac,data1$WhoTrust04, graph= TRUE, main= "Celebrities and public figures", ylab = "Trust04", xlab = "Vaccine")
tabpct(vac,data1$WhoTrust05, graph= TRUE, main= "Not-for-profit organisations", ylab = "Trust05", xlab = "Vaccine")
tabpct(vac,data1$WhoTrust06, graph= TRUE, main= "NHS spokesperson", ylab = "Trust06", xlab = "Vaccine")
tabpct(vac,data1$WhoTrust07, graph= TRUE, main= "Research Scientists/ Universities", ylab = "Trust07", xlab = "Vaccine")
tabpct(vac,data1$WhoTrust08, graph= TRUE, main= "Family/Friends", ylab = "Trust08", xlab = "Vaccine")

dev.off()


## calculating odds ratios and P-values 
## note- can't extract calculted P-values calculated using 'cc' function in R so they have been written into a table in excel by hand (3dp). 
## cc and tabpct from package epiDisplay

#running odds ratio calculations
cc(data1$WhoTrust01,tr,decimal=2,graph= FALSE, cctable=NULL)
#identifying and rounding CI 95% values 
tr.who1<-cc(data1$WhoTrust01,tr,decimal=2,graph= FALSE, cctable=NULL)
ci.tr.who1.a<-round(tr.who1$ci.or[1],2)
ci.tr.who1.b <-round(tr.who1$ci.or[2],2)
ci.tr.who1 <- paste(ci.tr.who1.a, ci.tr.who1.b, sep="-")

tr.who2<-cc(data1$WhoTrust02,tr,decimal=2,graph= FALSE, cctable=NULL)
ci.tr.who2.a<-round(tr.who2$ci.or[1],2)
ci.tr.who2.b <-round(tr.who2$ci.or[2],2)
ci.tr.who2 <- paste(ci.tr.who2.a, ci.tr.who2.b, sep="-")

tr.who3<-cc(data1$WhoTrust03,tr,decimal=2,graph= FALSE, cctable=NULL)
ci.tr.who3.a<-round(tr.who3$ci.or[1],2)
ci.tr.who3.b <-round(tr.who3$ci.or[2],2)
ci.tr.who3 <- paste(ci.tr.who3.a, ci.tr.who3.b, sep="-")

tr.who4<-cc(data1$WhoTrust04,tr,decimal=2,graph= FALSE, cctable=NULL)
ci.tr.who4.a<-round(tr.who4$ci.or[1],2)
ci.tr.who4.b <-round(tr.who4$ci.or[2],2)
ci.tr.who4 <- paste(ci.tr.who4.a, ci.tr.who4.b, sep="-")

tr.who5<-cc(data1$WhoTrust05,tr,decimal=2,graph= FALSE, cctable=NULL)
ci.tr.who5.a<-round(tr.who5$ci.or[1],2)
ci.tr.who5.b <-round(tr.who5$ci.or[2],2)
ci.tr.who5 <- paste(ci.tr.who5.a, ci.tr.who5.b, sep="-")

tr.who6<-cc(data1$WhoTrust06,tr,decimal=2,graph= FALSE, cctable=NULL)
ci.tr.who6.a<-round(tr.who6$ci.or[1],2)
ci.tr.who6.b <-round(tr.who6$ci.or[2],2)
ci.tr.who6 <- paste(ci.tr.who6.a, ci.tr.who6.b, sep="-")

tr.who7<-cc(data1$WhoTrust07,tr,decimal=2,graph= FALSE, cctable=NULL)
ci.tr.who7.a<-round(tr.who7$ci.or[1],2)
ci.tr.who7.b <-round(tr.who7$ci.or[2],2)
ci.tr.who7 <- paste(ci.tr.who7.a, ci.tr.who7.b, sep="-")

tr.who8<-cc(data1$WhoTrust08,tr,decimal=2,graph= FALSE, cctable=NULL)
ci.tr.who8.a<-round(tr.who8$ci.or[1],2)
ci.tr.who8.b <-round(tr.who8$ci.or[2],2)
ci.tr.who8 <- paste(ci.tr.who8.a, ci.tr.who8.b, sep="-")

hy.who1<- cc(data1$WhoTrust01,hy,decimal=2,graph= FALSE, cctable=NULL)
ci.hy.who1.a<-round(hy.who1$ci.or[1],2)
ci.hy.who1.b <-round(hy.who1$ci.or[2],2)
ci.hy.who1 <- paste(ci.hy.who1.a, ci.hy.who1.b, sep="-")

hy.who2<-cc(data1$WhoTrust02,hy,decimal=2,graph= FALSE, cctable=NULL)
ci.hy.who2.a<-round(hy.who2$ci.or[1],2)
ci.hy.who2.b <-round(hy.who2$ci.or[2],2)
ci.hy.who2 <- paste(ci.hy.who2.a, ci.hy.who2.b, sep="-")

hy.who3<-cc(data1$WhoTrust03,hy,decimal=2,graph= FALSE, cctable=NULL)
ci.hy.who3.a<-round(hy.who3$ci.or[1],2)
ci.hy.who3.b <-round(hy.who3$ci.or[2],2)
ci.hy.who3 <- paste(ci.hy.who3.a, ci.hy.who3.b, sep="-")

hy.who4<-cc(data1$WhoTrust04,hy,decimal=2,graph= FALSE, cctable=NULL)
ci.hy.who4.a<-round(hy.who4$ci.or[1],2)
ci.hy.who4.b <-round(hy.who4$ci.or[2],2)
ci.hy.who4 <- paste(ci.hy.who4.a, ci.hy.who4.b, sep="-")

hy.who5<-cc(data1$WhoTrust05,hy,decimal=2,graph= FALSE, cctable=NULL)
ci.hy.who5.a<-round(hy.who5$ci.or[1],2)
ci.hy.who5.b <-round(hy.who5$ci.or[2],2)
ci.hy.who5 <- paste(ci.hy.who5.a, ci.hy.who5.b, sep="-")

hy.who6<-cc(data1$WhoTrust06,hy,decimal=2,graph= FALSE, cctable=NULL)
ci.hy.who6.a<-round(hy.who6$ci.or[1],2)
ci.hy.who6.b <-round(hy.who6$ci.or[2],2)
ci.hy.who6 <- paste(ci.hy.who6.a, ci.hy.who6.b, sep="-")

hy.who7<-cc(data1$WhoTrust07,hy,decimal=2,graph= FALSE, cctable=NULL)
ci.hy.who7.a<-round(hy.who7$ci.or[1],2)
ci.hy.who7.b <-round(hy.who7$ci.or[2],2)
ci.hy.who7 <- paste(ci.hy.who7.a, ci.hy.who7.b, sep="-")

hy.who8<-cc(data1$WhoTrust08,hy,decimal=2,graph= FALSE, cctable=NULL)
ci.hy.who8.a<-round(hy.who8$ci.or[1],2)
ci.hy.who8.b <-round(hy.who8$ci.or[2],2)
ci.hy.who8 <- paste(ci.hy.who8.a, ci.hy.who8.b, sep="-")


gm.who1<- cc(data1$WhoTrust01,gm,decimal=2,graph= FALSE, cctable=NULL)
ci.gm.who1.a<-round(gm.who1$ci.or[1],2)
ci.gm.who1.b <-round(gm.who1$ci.or[2],2)
ci.gm.who1 <- paste(ci.gm.who1.a, ci.gm.who1.b, sep="-")

gm.who2<-cc(data1$WhoTrust02,gm,decimal=2,graph= FALSE, cctable=NULL)
ci.gm.who2.a<-round(gm.who2$ci.or[1],2)
ci.gm.who2.b <-round(gm.who2$ci.or[2],2)
ci.gm.who2 <- paste(ci.gm.who2.a, ci.gm.who2.b, sep="-")

gm.who3<-cc(data1$WhoTrust03,gm,decimal=2,graph= FALSE, cctable=NULL)
ci.gm.who3.a<-round(gm.who3$ci.or[1],2)
ci.gm.who3.b <-round(gm.who3$ci.or[2],2)
ci.gm.who3 <- paste(ci.gm.who3.a, ci.gm.who3.b, sep="-")

gm.who4<-cc(data1$WhoTrust04,gm,decimal=2,graph= FALSE, cctable=NULL)
ci.gm.who4.a<-round(gm.who4$ci.or[1],2)
ci.gm.who4.b <-round(gm.who4$ci.or[2],2)
ci.gm.who4 <- paste(ci.gm.who4.a, ci.gm.who4.b, sep="-")

gm.who5<-cc(data1$WhoTrust05,gm,decimal=2,graph= FALSE, cctable=NULL)
ci.gm.who5.a<-round(gm.who5$ci.or[1],2)
ci.gm.who5.b <-round(gm.who5$ci.or[2],2)
ci.gm.who5 <- paste(ci.gm.who5.a, ci.gm.who5.b, sep="-")

gm.who6<-cc(data1$WhoTrust06,gm,decimal=2,graph= FALSE, cctable=NULL)
ci.gm.who6.a<-round(gm.who6$ci.or[1],2)
ci.gm.who6.b <-round(gm.who6$ci.or[2],2)
ci.gm.who6 <- paste(ci.gm.who6.a, ci.gm.who6.b, sep="-")

gm.who7<-cc(data1$WhoTrust07,gm,decimal=2,graph= FALSE, cctable=NULL)
ci.gm.who7.a<-round(gm.who7$ci.or[1],2)
ci.gm.who7.b <-round(gm.who7$ci.or[2],2)
ci.gm.who7 <- paste(ci.gm.who7.a, ci.gm.who7.b, sep="-")

gm.who8<-cc(data1$WhoTrust08,gm,decimal=2,graph= FALSE, cctable=NULL)
ci.gm.who8.a<-round(gm.who8$ci.or[1],2)
ci.gm.who8.b <-round(gm.who8$ci.or[2],2)
ci.gm.who8 <- paste(ci.gm.who8.a, ci.gm.who8.b, sep="-")

vac.who1<- cc(data1$WhoTrust01,vac,decimal=2,graph= FALSE, cctable=NULL)
ci.vac.who1.a<-round(vac.who1$ci.or[1],2)
ci.vac.who1.b <-round(vac.who1$ci.or[2],2)
ci.vac.who1 <- paste(ci.vac.who1.a, ci.vac.who1.b, sep="-")

vac.who2<-cc(data1$WhoTrust02,vac,decimal=2,graph= FALSE, cctable=NULL)
ci.vac.who2.a<-round(vac.who2$ci.or[1],2)
ci.vac.who2.b <-round(vac.who2$ci.or[2],2)
ci.vac.who2 <- paste(ci.vac.who2.a, ci.vac.who2.b, sep="-")

vac.who3<-cc(data1$WhoTrust03,vac,decimal=2,graph= FALSE, cctable=NULL)
ci.vac.who3.a<-round(vac.who3$ci.or[1],2)
ci.vac.who3.b <-round(vac.who3$ci.or[2],2)
ci.vac.who3 <- paste(ci.vac.who3.a, ci.vac.who3.b, sep="-")

vac.who4<-cc(data1$WhoTrust04,vac,decimal=2,graph= FALSE, cctable=NULL)
ci.vac.who4.a<-round(vac.who4$ci.or[1],2)
ci.vac.who4.b <-round(vac.who4$ci.or[2],2)
ci.vac.who4 <- paste(ci.vac.who4.a, ci.vac.who4.b, sep="-")

vac.who5<-cc(data1$WhoTrust05,vac,decimal=2,graph= FALSE, cctable=NULL)
ci.vac.who5.a<-round(vac.who5$ci.or[1],2)
ci.vac.who5.b <-round(vac.who5$ci.or[2],2)
ci.vac.who5 <- paste(ci.vac.who5.a, ci.vac.who5.b, sep="-")

vac.who6<-cc(data1$WhoTrust06,vac,decimal=2,graph= FALSE, cctable=NULL)
ci.vac.who6.a<-round(vac.who6$ci.or[1],2)
ci.vac.who6.b <-round(vac.who6$ci.or[2],2)
ci.vac.who6 <- paste(ci.vac.who6.a, ci.vac.who6.b, sep="-")

vac.who7<-cc(data1$WhoTrust07,vac,decimal=2,graph= FALSE, cctable=NULL)
ci.vac.who7.a<-round(vac.who7$ci.or[1],2)
ci.vac.who7.b <-round(vac.who7$ci.or[2],2)
ci.vac.who7 <- paste(ci.vac.who7.a, ci.vac.who7.b, sep="-")

vac.who8<-cc(data1$WhoTrust08,vac,decimal=2,graph= FALSE, cctable=NULL)
ci.vac.who8.a<-round(vac.who8$ci.or[1],2)
ci.vac.who8.b <-round(vac.who8$ci.or[2],2)
ci.vac.who8 <- paste(ci.vac.who8.a, ci.vac.who8.b, sep="-")

#print table (OR and CI values)
who.tr.data <- data.frame (Factor =c("Work colleagues","The Government's scientific advisors", "The Government", "Celebrities and public figures", "Not-for-profit organisations", "NHS spokesperson", "Research Scientistcs/Universities", "Family/Friends"), 
	Tr_OR = round(c(tr.who1$or, tr.who2$or, tr.who3$or, tr.who4$or, tr.who5$or, tr.who6$or, tr.who7$or, tr.who8$or),2),
	Tr_CI = c(ci.tr.who1, ci.tr.who2, ci.tr.who3, ci.tr.who4, ci.tr.who5, ci.tr.who6, ci.tr.who7, ci.tr.who8),
	hy_OR = round(c(hy.who1$or, hy.who2$or, hy.who3$or, hy.who4$or, hy.who5$or, hy.who6$or, hy.who7$or, hy.who8$or),2),
	hy_CI = c(ci.hy.who1, ci.hy.who2, ci.hy.who3, ci.hy.who4, ci.hy.who5, ci.hy.who6, ci.hy.who7, ci.hy.who8),
	gm_OR = round(c(gm.who1$or, gm.who2$or, gm.who3$or, gm.who4$or, gm.who5$or, gm.who6$or, gm.who7$or, gm.who8$or),2),
	gm_CI = c(ci.gm.who1, ci.gm.who2, ci.gm.who3, ci.gm.who4, ci.gm.who5, ci.gm.who6, ci.gm.who7, ci.gm.who8),
	vac_OR = round(c(vac.who1$or, vac.who2$or, vac.who3$or, vac.who4$or, vac.who5$or, vac.who6$or, vac.who7$or, vac.who8$or),2),
	vac_CI = c(ci.vac.who1, ci.vac.who2, ci.vac.who3, ci.vac.who4, ci.vac.who5, ci.vac.who6, ci.vac.who7, ci.vac.who8))
write.csv (who.tr.data,"Odds_ratio_who_trust.csv")



## running a correlation matrix
sexrecoded<- as.numeric(as.character(revalue(sex, c("Female"=1, "Male"=0 ))))
ethnicrecoded<- as.numeric(as.character(revalue(ethnic, c("White British"=1, "Other"=0 ))))
demo.all <- data.frame(rel2,rel3,pol,ethnicrecoded,age=data1$Age,edu2,edu3,sexrecoded)
demo.all.1 <- na.omit(demo.all)
demo.all.1.M <- as.matrix(demo.all.1)
correlation_matrix<-rcorr(demo.all.1.M, type = "spearman")
R2 <- correlation_matrix$r # matrix of correlation coefficients
p2 <- correlation_matrix$P # matrix of P-value
	## define significance levels as stars
mystars.demo <- ifelse(p2 < .001, "***", ifelse(p2 < .01, "**  ", ifelse(p2 < .05, "*   ", "    "))))
    ## round the correlation matrix to two decimal
R2 <- format(round(cbind(rep(-1.11, ncol(demo.all.1.M)), R2), 2))[,-1]

    ## build a new matrix that includes the correlations with their apropriate stars and print to csv
demo.all.1.M.new <- matrix(paste(R2, mystars.demo, sep=""), ncol=ncol(demo.all.1.M))

#remove lower triangle of correlation matrix
demo.all.1.M.new <- as.matrix(demo.all.1.M.new)
demo.all.1.M.new[upper.tri(demo.all.1.M.new, diag = TRUE)] <- ""
demo.all.1.M.new<- as.data.frame(demo.all.1.M.new)

rownames(demo.all.1.M.new ) = c("Religiosity_2","Religiosity_3","Political_opinion","Ethnicity","Age","Education_level_2","Education_level_3","Sex")
colnames(demo.all.1.M.new ) = c("Religiosity_2","Religiosity_3","Political_opinion","Ethnicity","Age","Education_level_2","Education_level_3","Sex")
write.csv(demo.all.1.M.new, "demographic_correlation_bot.csv",row.names=TRUE)