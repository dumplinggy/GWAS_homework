##Priest Lab Interview Homework
##Part1. Data exploration
## import the anhwdata.bim
setwd("~/Desktop")
anhwdata <- read.delim("~/Desktop/GWAS homework /anhwdata.bim", header=FALSE, na.strings = "-9", sep="")
anhwdata <-  data.frame(anhwdata)

## import the anhwdata.cov
anhwdata1 <- read.delim("~/Desktop/GWAS homework /anhwdata.cov", na.strings = "-9", sep="")
anhwdata1 <-  data.frame(anhwdata1)
## check the dataset
str(anhwdata1)
##get all of the levels in the dataset
levels(as.factor(anhwdata1$age_recruit))

## import the anhwdata.fam
anhwdata2 <- read.delim("~/Desktop/GWAS homework /anhwdata.fam", header=FALSE, na.strings = "-9", sep="")
anhwdata2 <-  data.frame(anhwdata2)
## add the column name
colnames(anhwdata2) <- c("FID","IID","PID","MID","sex_2male.l2","P")
str(anhwdata2)
##checking if the name is the same to each other
anhwdata2$IID %in% anhwdata1$IID
##get all of the levels in the dataset
levels(as.factor(anhwdata2$P))
##count the total number of each phenotype
library(plyr)
count(anhwdata2$P)

## combine two data sets to one data 
FAMCOV <- plyr::join(data.frame(anhwdata2),data.frame(anhwdata1),by="IID")
str(FAMCOV)
FAMCOV <- FAMCOV[,c(2,1,3:17)]

##plot the distribution of ages, stratified by gender and diagnose
head(FAMCOV)
boxplot(FAMCOV$age_recruit~FAMCOV$sex_2male.l2,ylab='age',main='different sex')
boxplot(FAMCOV$age_recruit~FAMCOV$P,ylab='age',main='different diagnose')

##2x2 contigency tables
library(MASS)
View(FAMCOV)
table(FAMCOV$P,FAMCOV$sex_2male.l2)
fisher.test(FAMCOV$P, FAMCOV$sex_2male.l2)
table(FAMCOV$P,FAMCOV$HLDprev.l2)
fisher.test(FAMCOV$P, FAMCOV$HLDprev.l2)
table(FAMCOV$P,FAMCOV$HTNprev.l2)
fisher.test(FAMCOV$P, FAMCOV$HTNprev.l2)
table(FAMCOV$P,FAMCOV$T1Dprev.l2)
fisher.test(FAMCOV$P, FAMCOV$T1Dprev.l2)
table(FAMCOV$P,FAMCOV$T2Dprev.l2)
fisher.test(FAMCOV$P, FAMCOV$T2Dprev.l2)
table(FAMCOV$P,FAMCOV$binSmok.l2)
fisher.test(FAMCOV$P, FAMCOV$binSmok.l2)


