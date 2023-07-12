as.matrix(version)[14,]
print("Coded on version 4.3.1 (2023-06-16 ucrt")

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

# Preparatory Instructions:
# Download data which comes in tape/position format. 
# The data is not strictly delimited although it generally uses a space to delimit most fields:
# e.g gawk '{print $3}' mort2021us.txt outputs 'F1080'
# use awk or gawk to strip needed data column

# From: https://www.cdc.gov/nchs/data/dvs/Multiple-Cause-Record-Layout-2021.pdf
# "70-73 4 Detail Age Four positions are used to code detail age." 
# "Location 70 identifies age in years, months, days, etc." 
# "Locations 71-73 are the number of years, months, days, etc."

# Location of mort2021us.txt
# assign file location (select your own)
setwd("F:\\CoVD19\\Mortality.Natality\\mort2021us")

# from bash prompt strip
# cd /mnt/f/CoVD19/Mortality.Natality/mort2021us
# gawk '{print $3}' mort2021us.txt > SexAge.txt

# transform and split into data.table
# SexAge <- fread("SexAge.txt",col.names="SexAge")
# The column 'Count' below refers to Location 70 as described above
SexAge.2021 <- fread("SexAge.txt",col.names="SexAge")
SexAge.2021[,c("Sex","Count","Age1","Age2","Age3"):= tstrsplit(SexAge, "",keep=c(1,2,3,4,5))]
SexAge.2021[,ReportedAge := as.integer(paste0(Age1,Age2,Age3))]

## Part I Exploratory Data Analysis see readme.first.txt
# [Some sample calculations and graphs]
# 6 categories for the 4 'detail age positions'
# 2 categories are straightforward (1,9)
dev.new();plot.new()
SexAge.2021[,.N,.(Count)][order(Count)]
cat('
   Count       N
1:     1 3451936 #  Age in years
2:     2    7170
3:     4    5785
4:     5    4553
5:     6    2484
6:     9     191 # Age unknown
')

# Discrepancies in the data
# Unknown Age spread throughout 'Count'
# '999' (Reported Age unknown) is spread over four location 70 categories:
SexAge.2021[ReportedAge == 999,.N,.(ReportedAge,Count)][order(-N)]
cat('
   ReportedAge Count   N
1:         999     9 191
2:         999     1  48
3:         999     6  27
4:         999     5  24
') 

# Some data groking and probing
SexAge.2021[Count == 9,.N,.(ReportedAge)][order(ReportedAge)]
SexAge.2021[Count != 9 & Count != 1,.N,.(ReportedAge)][order(ReportedAge)]

dev.new();plot.new()
SexAge.2021[,.N,.(ReportedAge)][order(ReportedAge)][,barplot(N~ReportedAge)]
dev.new();plot.new()
SexAge.2021[Count == 1,.N,.(ReportedAge)][order(ReportedAge)][,barplot(N~ReportedAge)]

SexAge.2021[between(ReportedAge,0,19),.N,.(ReportedAge)][order(ReportedAge)]
SexAge.2021[Count == 1 & between(ReportedAge,0,19),.N,.(ReportedAge)][order(ReportedAge)]

SexAge.2021[between(ReportedAge,101,121),.N,.(ReportedAge)][order(ReportedAge)]
SexAge.2021[Count == 1 & between(ReportedAge,101,121),.N,.(ReportedAge)][order(ReportedAge)]

SexAge.2021[between(ReportedAge,50,70),.N,.(Count,ReportedAge)][,
	dcast(.SD,ReportedAge ~ Count,value.var="N",fun.aggregate=sum)]
SexAge.2021[Count == 1 & between(ReportedAge,50,70),.N,.(ReportedAge)][order(ReportedAge)][1:21]
dev.new();plot.new()
SexAge.2021[Count == 1 & between(ReportedAge,50,70),.N,.(ReportedAge)][order(ReportedAge)][,barplot(N~ReportedAge)]

# SexAge.2021[Count == 1,.N,.(ReportedAge)][,sum(N)]
Count1.total <- SexAge.2021[Count == 1,sum(.N)]

# Some subsetting and summarizations with bar graphs:
# Notes:
# RA == "Reported Age"

dt1 <- setnames(
as.data.table(rbind(
RA.0_49.years = SexAge.2021[Count == 1 & between(ReportedAge,0,49),.N,.(ReportedAge)][,sum(N)],
RA.50_79.years = SexAge.2021[Count == 1 & between(ReportedAge,50,70),.N,.(ReportedAge)][,sum(N)],
RA.80_121.years = SexAge.2021[Count == 1 & between(ReportedAge,71,121),.N,.(ReportedAge)][,sum(N)]
),keep.rownames=TRUE),c("Age.Group","Count"))
dev.new();plot.new()
dt1[,.SD[,.(Pct=(Count/Count1.total) * 100)],.(Age.Group,Count)]
dt1[,barplot(Count~Age.Group,ylab="Count US deaths 2021",col=rainbow(3))]
grid(lwd = 3,col="black")


# For Count = 1 # e.g "Years" but some specific ReportedAge by 20 year periods
dt1 <- setnames(
as.data.table(rbind(
RA1.0_19.years = SexAge.2021[Count == 1 & between(ReportedAge,0,19),.N,.(ReportedAge)][,sum(N)],
RA2.20_39.years = SexAge.2021[Count == 1 & between(ReportedAge,20,39),.N,.(ReportedAge)][,sum(N)],
RA3.40.59.years = SexAge.2021[Count == 1 & between(ReportedAge,40,59),.N,.(ReportedAge)][,sum(N)],
RA4.60_79.years = SexAge.2021[Count == 1 & between(ReportedAge,60,79),.N,.(ReportedAge)][,sum(N)],
RA5.80_99.years = SexAge.2021[Count == 1 & between(ReportedAge,80,99),.N,.(ReportedAge)][,sum(N)],
RA6.100_121.years = SexAge.2021[Count == 1 & between(ReportedAge,100,121),.N,.(ReportedAge)][,sum(N)]
),keep.rownames=TRUE),c("Age.Group","Count"))
dev.new();plot.new()
dt1[,.SD[,.(Pct=(Count/Count1.total) * 100)],.(Age.Group,Count)]
dt1[,barplot(Count~Age.Group,ylab="Count US deaths 2021",col=rainbow(6))]
grid(lwd = 3,col="black")


# For all Count not equal 9 # e.g "Age Not Reported" but some specific ReportedAge by 20 year periods
dt1 <- setnames(
as.data.table(rbind(
RA1.0_19.years = SexAge.2021[Count !=9 & between(ReportedAge,0,19),.N,.(ReportedAge)][,sum(N)],
RA2.20_39.years = SexAge.2021[Count !=9 & between(ReportedAge,20,39),.N,.(ReportedAge)][,sum(N)],
RA3.40.59.years = SexAge.2021[Count !=9 & between(ReportedAge,40,59),.N,.(ReportedAge)][,sum(N)],
RA4.60_79.years = SexAge.2021[Count !=9 & between(ReportedAge,60,79),.N,.(ReportedAge)][,sum(N)],
RA5.80_99.years = SexAge.2021[Count !=9 & between(ReportedAge,80,99),.N,.(ReportedAge)][,sum(N)],
RA6.100_121.years = SexAge.2021[Count !=9 & between(ReportedAge,100,121),.N,.(ReportedAge)][,sum(N)]
),keep.rownames=TRUE),c("Age.Group","Count"))
dev.new();plot.new()
dt1[,.SD[,.(Pct=(Count/Count1.total) * 100)],.(Age.Group,Count)]
dt1[,barplot(Count~Age.Group,ylab="Count US deaths 2021",col=rainbow(6))]
grid(lwd = 3,col="black")


# For all Count not = 9 # e.g "Age Not Reported" but some specific ReportedAge by 10 year periods
dt1 <- setnames(
as.data.table(rbind(
RA1.0_9.years = SexAge.2021[Count !=9 & between(ReportedAge,0,10),.N,.(ReportedAge)][,sum(N)],
RA2.10_19.years = SexAge.2021[Count !=9 & between(ReportedAge,11,19),.N,.(ReportedAge)][,sum(N)],
RA3.20_29.years = SexAge.2021[Count !=9 & between(ReportedAge,20,29),.N,.(ReportedAge)][,sum(N)],
RA4.30_39years = SexAge.2021[Count !=9 & between(ReportedAge,30,39),.N,.(ReportedAge)][,sum(N)],
RA5.40.49.years = SexAge.2021[Count !=9 & between(ReportedAge,40,49),.N,.(ReportedAge)][,sum(N)],
RA6.50.59.years = SexAge.2021[Count !=9 & between(ReportedAge,50,59),.N,.(ReportedAge)][,sum(N)],
RA7.60_69.years = SexAge.2021[Count !=9 & between(ReportedAge,60,69),.N,.(ReportedAge)][,sum(N)],
RA8.70_79.years = SexAge.2021[Count !=9 & between(ReportedAge,70,79),.N,.(ReportedAge)][,sum(N)],
RA9.80_89.years = SexAge.2021[Count !=9 & between(ReportedAge,80,89),.N,.(ReportedAge)][,sum(N)],
RA9a.90_99.years = SexAge.2021[Count !=9 & between(ReportedAge,90,99),.N,.(ReportedAge)][,sum(N)],
RA9b.100_109.years = SexAge.2021[Count !=9 & between(ReportedAge,100,109),.N,.(ReportedAge)][,sum(N)]
),keep.rownames=TRUE),c("Age.Group","Count"))
dev.new();plot.new()
dt1[,.SD[,.(Pct=(Count/Count1.total) * 100)],.(Age.Group,Count)]
dt1[,barplot(Count~Age.Group,ylab="Count US deaths 2021",col=rainbow(11))]
grid(lwd = 3,col="black")

# Getting Fancy with charts:
# "A death tale of four scales: Youth, Adulthood, Middle Age and the Elder Years"
# Thinking of US 2021 death stages in four parts...

dev.new();plot.new()
par(mfrow=c(2,2))
SexAge.2021[Count == 1 & between(ReportedAge,50,85),.N,.(ReportedAge)][order(-N)][,barplot(N~ReportedAge)]
grid(lwd = 3,col="black")
SexAge.2021[Count == 1 & between(ReportedAge,71,121),.N,.(ReportedAge)][order(-N)][,barplot(N~ReportedAge)]
grid(lwd = 3,col="black")

SexAge.2021[Count == 1 & between(ReportedAge,62,68),.N,.(ReportedAge)][order(-N)][,barplot(N~ReportedAge,ylim=c(0,70000))]
grid(lwd = 3,col="black")
SexAge.2021[Count == 1 & between(ReportedAge,60,70),.N,.(ReportedAge)][order(-N)][,barplot(N~ReportedAge,ylim=c(0,70000))]
grid(lwd = 3,col="black")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
SexAge.2021[Count == 1 & between(ReportedAge,0,25),.N,.(ReportedAge)][order(-N)][,barplot(N~ReportedAge,main="Youth",cex.main=2,col="red")]
grid(lwd = 3,col="black")
SexAge.2021[Count == 1 & between(ReportedAge,26,50),.N,.(ReportedAge)][order(-N)][,barplot(N~ReportedAge,main="Adulthood",cex.main=2,col="blue")]
grid(lwd = 3,col="black")

SexAge.2021[Count == 1 & between(ReportedAge,51,75),.N,.(ReportedAge)][order(-N)][,barplot(N~ReportedAge,main="Middle Age",cex.main=2,col="purple")]
grid(lwd = 3,col="black")
SexAge.2021[Count == 1 & between(ReportedAge,76,125),.N,.(ReportedAge)][order(-N)][,barplot(N~ReportedAge,main="Elder Years",cex.main=2,col="salmon")]
grid(lwd = 3,col="black")
par(mfrow=c(1,1))
mtext("A death tale of four scales: Youth, Adulthood, Middle Age and the Elder Years",side=3,line=-1,cex=2,col="mediumorchid")


# Part II Writing function and creating time series charts

# EST Population (Census) Data:
# https://www.census.gov/data/tables/time-series/demo/popest/2020s-national-detail.html
# https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/national/asrh/nc-est2022-agesex-res.csv
# removes AGE == 999


Age.by.Sex <-   fread("F:/CoVD19/Mortality.Natality/nc-est2022-agesex-res.csv")[,
		dcast(.SD,AGE ~ SEX,value.var="POPESTIMATE2021",fun.aggregate=sum)][,
		setnames(.SD,c("AGE","Total.AGE.Pop","Male","Female"))][AGE != 999,]


# Some sample functions for creating Age Groups:
# Age 0 --  Age 120 in 20 year groups
N.Count <- SexAge.2021[Count == 1 & between(ReportedAge,0,120),length(Count)]
l <- as.data.table({});
 for(i in seq(0,120,20)) {range=data.table(start = i, end = (i + 19));
 SexAge.2021[Count == 1 & ReportedAge %inrange% range,
 .(l <<- rbind(l,cbind(range,total_deaths=sum(.N),pct=round(sum(.N)/N.Count * 100,2))))]};
l.20.0.120.20years <- l
l.20.0.120.20years

# Age 0 --  Age 120 in 15 year groups
N.Count <- SexAge.2021[Count == 1 & between(ReportedAge,0,120),length(Count)]
l <- as.data.table({});
 for(i in seq(0,120,15)) {range=data.table(start = i, end = (i + 14));
 SexAge.2021[Count == 1 & ReportedAge %inrange% range,
 .(l <<- rbind(l,cbind(range,total_deaths=sum(.N),pct=round(sum(.N)/N.Count * 100,2))))]};
l.15.0.120.15years <- l
l.15.0.120.15years

# Age 0 --  Age 120 in 10 year groups
N.Count <- SexAge.2021[Count == 1 & between(ReportedAge,0,120),length(Count)]
l <- as.data.table({});
 for(i in seq(0,120,10)) {range=data.table(start = i, end = (i + 9));
 SexAge.2021[Count == 1 & ReportedAge %inrange% range,
 .(l <<- rbind(l,cbind(range,total_deaths=sum(.N),pct=round(sum(.N)/N.Count * 100,2))))]};
l.10.0.120.10years <- l
l.10.0.120.10years

# Age 0 --  Age 120 in 5 year groups
N.Count <- SexAge.2021[Count == 1 & between(ReportedAge,0,120),length(Count)]
l <- as.data.table({});
 for(i in seq(0,120,5)) {range=data.table(start = i, end = (i + 4));
 SexAge.2021[Count == 1 & ReportedAge %inrange% range,
 .(l <<- rbind(l,cbind(range,total_deaths=sum(.N),pct=round(sum(.N)/N.Count * 100,2))))]};
l.5.0.120.5years <- l
l.5.0.120.5years

# Age 0 --  Age 120 in 1 year groups
N.Count <- SexAge.2021[Count == 1 & between(ReportedAge,0,120),length(Count)]
l <- as.data.table({});
 for(i in seq(0,120,1)) {range=data.table(start = i, end = (i));
 SexAge.2021[Count == 1 & ReportedAge %inrange% range,
.(l <<- rbind(l,cbind(range,total_deaths=sum(.N),pct=round(sum(.N)/N.Count * 100,2))))]};
l.0.120.1years <- l
l.0.120.1years

# Conceptual (experimental) 'forward look' at projected deaths by separate 'year' (unit) intervals -RMF 7:38 AM 7/12/2023
library(lattice)
all_groups <-
	rbind(
	cbind(l.20.0.120.20years,units=20),
	cbind(l.15.0.120.15years,units=15),
	cbind(l.10.0.120.10years,units=10),
	cbind(l.5.0.120.5years,units=5),
	cbind(l.0.120.1years,units=1),fill=TRUE)
# by unit as year
all_groups[order(start,end)][,.(deaths=sum(total_deaths)),.(by.year=units)]	
all_groups[,dcast(.SD,start+end ~ units,value.var="total_deaths",fun.aggregate=sum)][order(start,end)]
all_groups[,dcast(.SD,start+end ~ units,value.var="total_deaths",fun.aggregate=sum)][order(start,end)][start >= 20 & start <= 100,]

chart_all_groups <- all_groups[,dcast(.SD,start+end ~ units,value.var="total_deaths",fun.aggregate=sum)][,
setnames(.SD,c("start","end","one_year","five_year","ten_year","fifteen_year","twenty_year"))][order(start,end)]

dev.new();plot.new()
chart_all_groups[,barchart(~one_year + five_year + ten_year + fifteen_year + twenty_year | as.factor(start),origin=0)]
# (start >= 20 & start <= 100) shows useful information
dev.new();plot.new()
chart_all_groups[(start >= 20 & start <= 100) & endsWith(as.character(start),'0'),barchart(~five_year + ten_year | as.factor(start),origin=0)]
dev.new();plot.new()
chart_all_groups[(start >= 20 & start <= 100) & endsWith(as.character(start),'0') ,barchart(~five_year + ten_year + twenty_year | as.factor(start),origin=0)]


# Main data for rest of charts is Ages 20 - 100. 
# I eliminate data for under 20 and over 100 since those age groups are a defacto
# distraction for determining when I should take Social Security monies 
# l.20.100.1years
# Age 0 --  Age 120 in 1 year groups
N.Count <- SexAge.2021[Count == 1 & between(ReportedAge,0,120),length(Count)]
l <- as.data.table({});
for(i in seq(20,100,2))
	 {range=data.table(start = i, end = (i + 1));
	 l <- SexAge.2021[Count == 1 & ReportedAge %inrange% range,
	.(rbind(l,cbind(range,total_deaths=sum(.N),pct=round(sum(.N)/N.Count * 100,2))))]};
l
l[,sum(total_deaths)]


# l.20.100.1years
# par(mfrow=c(1,1));
l <- as.data.table({});
for(i in seq(20,100,1))
	 {range=data.table(start = i, end = (i));
	 l <- SexAge.2021[Count == 1 & ReportedAge %inrange% range,
	.(rbind(l,cbind(AGE=range$start,total_deaths=sum(.N))))]};
l

# prototype function 
# l.0.120.20years
print("prototype age group function")
cat('
l <- as.data.table({});
for(i in seq(0,120,20))
	 {range=data.table(start = i, end = (i+19));
	 l <- SexAge.2021[Count == 1 & ReportedAge %inrange% range,
	.(rbind(l,cbind(AGE=range$start,AGE_=range$end,total_deaths=sum(.N))))]};
l
')


# Main function to create AGE groups for graphs by ages 20:100 by 1 year
# By Year between 20 and 100
# l.20.100.1years
# par(mfrow=c(1,1));
l <- as.data.table({});
for(i in seq(20,100,1))
	 {range=data.table(start = i, end = (i));
	 l <- SexAge.2021[Count == 1 & ReportedAge %inrange% range,
	.(rbind(l,cbind(AGE=range$start,total_deaths=sum(.N))))]};
l

# Add Census data
sum.deaths <- l[,sum(total_deaths)]
k <- merge(l,Age.by.Sex,by="AGE")
j <- k[,deaths.pctAGEgrp:=round(total_deaths/Total.AGE.Pop * 100,2)]
i <- j[,deaths.pctTOTpop:=round(total_deaths/sum.deaths * 100,2)]
h <- i[,.(AGE,Total.AGE.Pop,total_deaths,deaths.pctTOTpop,deaths.pctAGEgrp)]

# for deaths.pctAGEgrp and deaths.pctTOTpop
dev.new();plot.new()
par(mfrow=c(2,1));
h[,plot(deaths.pctAGEgrp ~ AGE,type="b",xlab="Age at death",ylab="Deaths pct per Age Group POP",col="red",lwd=3,axes=TRUE)]
grid(lwd = 2,col="black")
abline(v=c(55,85),col="salmon",lwd=2)
abline(v=c(62,68),col="purple",lwd=2)

h[,plot(deaths.pctTOTpop ~ AGE,type="b",xlab="Age at death",ylab="Deaths per Age Group as a pct of all deaths",col="red",lwd=3,axes=TRUE)]
grid(lwd = 2,col="black")
abline(v=c(55,85),col="salmon",lwd=2)
abline(v=c(62,68),col="purple",lwd=2)
par(mfrow=c(1,1))

mtext("Comparing US 2021 yearly death percentages per age group and per age group for all total US deaths",line=3,side=3,cex=1.5)
mtext("Vertical red lines = c(55,85); Vertical purple lines = c(62,68)",line=1,side=3,cex=1.5)
mtext("Mortality Data from https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm#Mortality_Multiple ;
Age Data from https://www.census.gov/data/tables/time-series/demo/popest/2020s-national-detail.html",side=1,cex=1)

# for Total.AGE.Pop total_deaths
dev.new();plot.new()
par(mfrow=c(2,1));
h[,plot(total_deaths ~ AGE,type="b",xlab="Age at death",ylab="Deaths per Age Group",col="red",lwd=3,axes=TRUE)]
grid(lwd = 2,col="black")
abline(v=c(55,85),col="salmon",lwd=2)
abline(v=c(62,68),col="purple",lwd=2)

h[,plot(Total.AGE.Pop ~ AGE,type="b",xlab="2021 Total pop per Age Group",col="red",
ylab="POP per Age Group",lwd=3,axes=TRUE)]
grid(lwd = 2,col="black")
abline(v=c(55,85),col="salmon",lwd=2)
abline(v=c(62,68),col="purple",lwd=2)
par(mfrow=c(1,1))

mtext("Comparing US 2021 yearly deaths per age group and total population per age group",line=3,side=3,cex=1.5)
mtext("Vertical red lines = c(55,85); Vertical purple lines = c(62,68)",line=1,side=3,cex=1.5)
mtext("Mortality Data from https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm#Mortality_Multiple ;
Age Data from https://www.census.gov/data/tables/time-series/demo/popest/2020s-national-detail.html",side=1,cex=1)


# Part III Lattice charts
library(lattice)
dev.new();plot.new()
h[between(AGE,20,49),barchart(~ deaths.pctTOTpop + deaths.pctAGEgrp | as.factor(AGE),origin= 0,
main ="Ages 20:49 - Deaths as pct of total deaths (blue) and deaths as percentage of AGE group (red)",col=c("red","blue"),lwd=3,axes=TRUE)]

dev.new();plot.new()
h[between(AGE,50,75),barchart(~ deaths.pctTOTpop + deaths.pctAGEgrp | as.factor(AGE),origin= 0,
main ="Ages 50:75 - Deaths as pct of total deaths (blue) and deaths as percentage of AGE group (red)",col=c("red","blue"),lwd=3,axes=TRUE)]

dev.new();plot.new()
h[between(AGE,76,100),barchart(~ deaths.pctTOTpop + deaths.pctAGEgrp | as.factor(AGE),origin= 0,
main ="Ages 76:100 - Deaths as pct of total deaths (blue) and deaths as percentage of AGE group (red)",col=c("red","blue"),lwd=3,axes=TRUE)]
