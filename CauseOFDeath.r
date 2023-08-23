# This code is under development and testing -RMF 8/23/2023

library(data.table)
setDTthreads(threads = 0, restore_after_fork = TRUE,throttle = NULL)

## DATA prep and Documentation
# https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm#Mortality_Multiple
# https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/mortality/mort2021us.zip

# Documentation
# https://www.cdc.gov/nchs/nvss/mortality_methods.htm
# https://www.cdc.gov/nchs/nvss/mortality_public_use_data.htm

# Further Documentation
# 2021 Documentation Mortality Multiple Cause-of-Death Public Use Record
# https://www.cdc.gov/nchs/products/nvsr.htm
# Mortality in the United States, 2021
# https://www.cdc.gov/nchs/products/databriefs/db456.htm

# Morbidity and Mortality Weekly Report (MMWR)
# Provisional Mortality Data - United States, 2022
# https://www.cdc.gov/mmwr/volumes/72/wr/mm7218a3.htm

# Provisional Mortality Data - United States, 2021
# https://www.cdc.gov/mmwr/volumes/71/wr/mm7117e1.htm

# Provisional Mortality Data - United States, 2020
# https://www.cdc.gov/mmwr/volumes/70/wr/mm7014e1.htm

# From: https://www.cdc.gov/nchs/data/dvs/Multiple-Cause-Record-Layout-2021.pdf
# "70-73 4 Detail Age Four positions are used to code detail age." 
# "Location 70 identifies age in years, months, days, etc." 
# "Locations 71-73 are the number of years, months, days, etc."

# Preparatory Instructions:
# Download data which comes in tape/position format. The data is not strictly 
# delimited although it generally uses a singular or multiple spaces to delimit most fields:
# e.g gawk '{print $3}' mort2021us.txt outputs 'F1080'
# use awk or gawk to strip needed data column

as.matrix(version)[14,]
print("Coded on version 4.3.1 (2023-06-16 ucrt")

library(data.table)
setDTthreads(threads = 0, restore_after_fork = TRUE,throttle = NULL)

# Location of mort2021us.txt
# assign file location
setwd("F:\\CoVD19\\Mortality.Natality\\mort2021us")

###############NCHS.42
# gawk '{print $3,$8,$9,$10}' mort2021us.txt > AgeSexCOD.txt
SexAgeCOD.2021 <- fread("AgeSexCOD.txt")
SexAgeCOD.2021[Count == 1,.N]
#[!grepl("001",V1),]

SexAgeCOD.2021 <- 
rbind(
SexAgeCOD.2021[(nchar(V3)> 2),][,V3:=NULL][,.(SexAge=V1,ICD.NCHS=V4)],
SexAgeCOD.2021[(nchar(V3)<= 2),][,.(SexAge=V1,ICD.NCHS=V3)])
SexAgeCOD.2021[nchar(ICD.NCHS) > 2,ICD.NCHS:=substr(ICD.NCHS,0,2)]
SexAgeCOD.2021[,c("Sex","Count","Age1","Age2","Age3"):= tstrsplit(SexAge, "",keep=c(1,2,3,4,5))]
SexAgeCOD.2021[,ReportedAge := as.integer(paste0(Age1,Age2,Age3))]

SexAgeCOD.2021[Count == 1,.N,.(ICD.NCHS,Sex)][,dcast(.SD,ICD.NCHS ~ Sex,value.var="N",fun.aggregate=sum)]

setwd("F:\\CoVD19\\Mortality.Natality")

NCHS.42 <- merge(fread("ICD.NCHS.42.csv")[,.(ICD.NCHS=ICD.NCHS,Title,ICD10)],
SexAgeCOD.2021[Count == 1,.(ICD.NCHS=ICD.NCHS,Sex,Age=ReportedAge)])
NCHS.42[,.N]
NCHS.42[between(Age,60,70) & Sex=="M",.N,.(Title,ICD10)][order(-N)][1:10]

NCHS.42.MF <- merge(fread("ICD.NCHS.42.csv")[,.(ICD.NCHS=as.integer(ICD.NCHS),Title)],
SexAgeCOD.2021[Count == 1,.N,.(ICD.NCHS=as.integer(ICD.NCHS),Sex)][,dcast(.SD,ICD.NCHS ~ Sex,value.var="N",fun.aggregate=sum)],by="ICD.NCHS")[order(-F,-M)]
NCHS.42.MF[,.SD[,.(MminF=M-F,Mpct=round(M/(M+F),2),Fpct=round(F/(M+F),2))],.(Title,F,M)]


################NCHS.135
setwd("F:\\CoVD19\\Mortality.Natality\\mort2021us")
# gawk '{print $3,$7,$8,$9}' mort2021us.txt > AgeSexCOD.txt
SexAgeCOD.2021 <- fread("AgeSexCOD.txt")
SexAgeCOD.2021[,.N]
SexAgeCOD.2021[Count == 1,.N]
#[!grepl("001",V1),]

SexAgeCOD.2021 <- 
rbind(SexAgeCOD.2021[as.numeric(V3) < 136,.(SexAge=V1,ICD.NCHS=V3)],
SexAgeCOD.2021[as.numeric(V4) > 42 & as.numeric(V4 < 136),.(SexAge=V1,ICD.NCHS=V4)])
SexAgeCOD.2021[,c("Sex","Count","Age1","Age2","Age3"):= tstrsplit(SexAge, "",keep=c(1,2,3,4,5))]
SexAgeCOD.2021[,ReportedAge := as.integer(paste0(Age1,Age2,Age3))]	

setwd("F:\\CoVD19\\Mortality.Natality")
NCHS.135 <- merge(fread("ICD.NCHS.135.csv")[,.(ICD.NCHS=ICD.NCHS,Title,ICD10)],
SexAgeCOD.2021[Count == 1,.(ICD.NCHS=ICD.NCHS,Sex,Age=ReportedAge)])
NCHS.135[,.N]
NCHS.135[between(Age,60,70) & Sex=="M",.N,.(Title,ICD10)][order(-N)][1:10]

NCHS.135.MF <- merge(fread("ICD.NCHS.135.csv")[,.(ICD.NCHS=ICD.NCHS,Title)],
SexAgeCOD.2021[Count == 1,.N,.(ICD.NCHS=ICD.NCHS,Sex)][,dcast(.SD,ICD.NCHS ~ Sex,value.var="N",fun.aggregate=sum)],by="ICD.NCHS")[order(-F,-M)]
NCHS.135.MF[,.SD[,.(MminF=M-F,Mpct=round(M/(M+F),2),Fpct=round(F/(M+F),2))],.(Title,F,M)][1:10]

################NCHS.358
library(data.table)
setDTthreads(threads = 0, restore_after_fork = TRUE,throttle = NULL)
library(stringi)
setwd("F:\\CoVD19\\Mortality.Natality\\mort2021us")
# gawk '{print $3,$7,$8}' mort2021us.txt > AgeSexCOD.txt
SexAgeCOD.2021 <- fread("AgeSexCOD.txt")
SexAgeCOD.2021[,.N]
#[!grepl("001",V1),]

library(stringi)
# SexAgeCOD.2021 <- SexAgeCOD.2021[,V2:=stri_replace_all_fixed(V2, ' ', '0')]
sa1 <- SexAgeCOD.2021[nchar(V2) < "7",.(SexAge=V1,ICD.NCHS=V3)]
sa1[,.N,.(nchar(ICD.NCHS))]
sa1[,.N]
sa2 <- SexAgeCOD.2021[nchar(V2) == "7",.(SexAge=V1,ICD.NCHS=stri_sub(V2,-3, length=stri_length(V2)))]
sa2[,.N,.(nchar(ICD.NCHS))]
sa1[,.N]
sa3 <- rbind(sa1,sa2)
sa3[,.N,.(nchar(ICD.NCHS))]
sa3[,.N]
# sa4 <- sa3[nchar(ICD.NCHS) >= "7",.(SexAge,ICD.NCHS=stri_sub(ICD.NCHS,-3, length=stri_length(ICD.NCHS)))]
sa4 <- sa3[nchar(ICD.NCHS) == "7",.(SexAge,ICD.NCHS=stri_sub(ICD.NCHS,-3, length=stri_length(ICD.NCHS)))]
sa4[,.N,.(nchar(ICD.NCHS))]
sa4[,.N]

SexAgeCOD.2021 <- rbind(sa1[nchar(ICD.NCHS) != 7,],sa2,sa4)
SexAgeCOD.2021[,c("Sex","Count","Age1","Age2","Age3"):= tstrsplit(SexAge, "",keep=c(1,2,3,4,5))]
SexAgeCOD.2021[,ReportedAge := as.integer(paste0(Age1,Age2,Age3))]
SexAgeCOD.2021[,.N]
SexAgeCOD.2021[,.N,.(Count)]

setwd("F:\\CoVD19\\Mortality.Natality")
fread("ICD.NCHS.386.csv")[,.N]
SexAgeCOD.2021[Count == 1,.N]

NCHS.386 <- merge(fread("ICD.NCHS.386.csv")[,.(ICD.NCHS=as.integer(ICD.NCHS),Title,ICD10)],
SexAgeCOD.2021[Count == "1",.(ICD.NCHS=as.integer(ICD.NCHS),Sex,Age=ReportedAge)],all.y = TRUE,by="ICD.NCHS")
NCHS.386[,.N]
NCHS.386[is.na(Title),.N]
NCHS.386[!is.na(Title),.N]
NCHS.386[is.na(Title),.N,.(ICD.NCHS)]

NCHS.386[!is.na(Title) & between(Age,60,70),.N,.(ICD.NCHS,Title)][order(-N)][1:20]
NCHS.386[!is.na(Title) & between(Age,60,85),.N,.(ICD.NCHS,Title)][order(-N)][1:20]
NCHS.386[!is.na(Title) & between(Age,1,21),.N,.(ICD.NCHS,Title)][order(-N)][1:20]

NCHS.386[between(Age,60,70) & Sex=="M",.N,.(Title,ICD10)][order(-N)][1:10]
NCHS.386.MF <- merge(fread("ICD.NCHS.386.csv")[,.(ICD.NCHS=as.integer(ICD.NCHS),Title)],
SexAgeCOD.2021[Count == 1,.N,.(ICD.NCHS=as.integer(ICD.NCHS),Sex)][,dcast(.SD,ICD.NCHS ~ Sex,value.var="N",fun.aggregate=sum)],by="ICD.NCHS")[order(-F,-M)]
NCHS.386.MF[,.SD[,.(MminF=M-F,Mpct=round(M/(M+F),2),Fpct=round(F/(M+F),2))],.(Title,F,M)][1:10]


###ICD.10
# from https://www.cdc.gov/nchs/nvss/manuals/2022/2e_volume1_2022.htm
# ~2K ICD three letter and four letter codes
library(data.table)
setDTthreads(threads = 0, restore_after_fork = TRUE,throttle = NULL)
library(stringi)
setwd("F:\\CoVD19\\Mortality.Natality\\mort2021us")
# gawk '{print $3,$8}' mort2021us.txt > AgeSexCOD.txt
SexAgeCOD.2021 <- fread("AgeSexCOD.txt",col.names=c("SexAge","ICD.10"))
SexAgeCOD.2021[,.N]
SexAgeCOD.2021 <- print(SexAgeCOD.2021,justify="left")
#[!grepl("001",V1),]

SexAgeCOD.2021 = SexAgeCOD.2021[,.(SexAge,ICD.10=substr(ICD.10,0,3),ICD.10_4=substr(ICD.10,0,4))]
SexAgeCOD.2021[,c("Sex","Count","Age1","Age2","Age3"):= tstrsplit(SexAge, "",keep=c(1,2,3,4,5))]
SexAgeCOD.2021[,Age := as.integer(paste0(Age1,Age2,Age3))]
SexAgeCOD.2021 <- print(SexAgeCOD.2021,justify="left")
SexAgeCOD.2021[,.N]
SexAgeCOD.2021[,.N,.(Count)]
SexAgeCOD.2021[substr(ICD.10,0,1) %in% c(1,2,3,4),]
SexAgeCOD.2021[substr(ICD.10,0,1) %in% LETTERS,]
SexAgeCOD.2021[substr(ICD.10,0,1) %in% c(1,2,3,4),.N,.(Count,ICD.10)][order(-N)][1:20]


ICD.10 <- fread("F:/CoVD19/Mortality.Natality/ICD.10.csv")
merge(SexAgeCOD.2021,ICD.10,by.x="ICD.10",by.y="ICD",all.y=TRUE)[,.N,.(ICD.10,ICD.10_4,Title)][order(-N)][1:10]
merge(SexAgeCOD.2021,ICD.10,by.x="ICD.10",by.y="ICD",all.y=TRUE)[,.N,.(ICD.10,ICD.10_4,Title)][order(-N)][1:40]
merge(SexAgeCOD.2021,ICD.10,by.x="ICD.10",by.y="ICD",all.y=TRUE)[,.N,.(ICD.10,ICD.10_4,Title)][,sum(N)]

merge(SexAgeCOD.2021,ICD.10,by.x="ICD.10",by.y="ICD",all.y=TRUE)[
	grepl("diabetes",Title,ignore.case=TRUE),.N,.(Decade=Age%/%10,Title)][order(Decade)]
	
merge(SexAgeCOD.2021,ICD.10,by.x="ICD.10",by.y="ICD",all.y=TRUE)[
	grepl("diabetes",Title,ignore.case=TRUE),.N,.(Decade=Age%/%10,Title)][order(Decade)][
	,dcast(.SD,Decade ~ Title,value.var="N",fun.aggregate=sum)]















	

