# This code is under development and testing -RMF 8/23/2023
#Charting Templates COD
library(data.table)
library(lattice)
library(grid)

lattice.options(default.args = list(as.table = TRUE,
                                    grid = TRUE,
                                    auto.key = TRUE))

jpeg_create <- function() {
          systime <- as.numeric(Sys.time())
          # dev.new()
          jpeg(filename = systime,
          width = 1024, height = 768, units = "px", pointsize = 12,
          quality = 100, bg = "white", res = NA, family = "", restoreConsole = TRUE,
          type = c("windows"))
          Sys.sleep(2)
                  }
									
									

#NCHS.135
# specific age groups
btwn60.70_top40 <-  NCHS.135[between(Age,60,70),
	.N,.(Title)][order(-N)][1:40]
NCHS.135[between(Age,60,70) &
 (Title %in% btwn60.70_top40$Title),.N,.(Title,Age)][
	,xyplot( N ~ Age | as.factor(Title))]

btwn60.100_top50 <-  NCHS.135[between(Age,60,100),
	.N,.(Title)][order(-N)][1:50]
NCHS.135[between(Age,60,100) &
 (Title %in% btwn60.100_top50$Title),.N,.(Title,Age)][
	,xyplot( N ~ Age | as.factor(Title))]

# top 40 Ages 1 : 100
btwn1.100_top40 <-  NCHS.135[between(Age,1,100),
	.N,.(Title)][order(-N)][1:40]
NCHS.135[between(Age,1,100) &
 (Title %in% btwn1.100_top40$Title),.N,.(Title,Age)][
	,xyplot( N ~ Age | as.factor(Title),par.strip.text = list(cex = 0.65))]


# Ages 1:120 top 20 and top 10
btwn1.120_top20 <-  NCHS.135[between(Age,1,120),
	.N,.(Title)][order(-N)][1:20]
NCHS.135[between(Age,1,120) &
 (Title %in% btwn1.120_top20$Title),.N,.(Title,Age)][
	,xyplot( N ~ Age | as.factor(Title),origin=0,par.strip.text = list(cex = 0.65))]


lattice.options(default.args = list(as.table = TRUE,
                                    grid = TRUE,
                                    auto.key = TRUE))


dev.new();plot.new();
btwn1.39_top10 <-  NCHS.135[between(Age,1,39),
	.N,.(Title)][order(-N)][1:10]
NCHS.135[between(Age,1,39) &
 (Title %in% btwn1.39_top10$Title),.N,.(Title,Age)][
	,xyplot( N ~ Age | as.factor(Title),origin=0,par.strip.text = list(cex = 0.65))]

dev.new();plot.new();
btwn40.59_top10 <-  NCHS.135[between(Age,40,59),
	.N,.(Title)][order(-N)][1:10]
NCHS.135[between(Age,40,59) &
 (Title %in% btwn40.59_top10$Title),.N,.(Title,Age)][
	,xyplot( N ~ Age | as.factor(Title),origin=0,par.strip.text = list(cex = 0.65))]

dev.new();plot.new();
btwn60.100_top10 <-  NCHS.135[between(Age,60,100),
	.N,.(Title)][order(-N)][1:10]
NCHS.135[between(Age,60,100) &
 (Title %in% btwn60.100_top10$Title),.N,.(Title,Age)][
	,xyplot( N ~ Age | as.factor(Title),origin=0,par.strip.text = list(cex = 0.65))]

dev.new();plot.new();
btwn1.120_top10 <-  NCHS.135[between(Age,1,120),
	.N,.(Title)][order(-N)][1:10]
NCHS.135[between(Age,1,120) &
 (Title %in% btwn1.120_top10$Title),.N,.(Title,Age)][
	,xyplot( N ~ Age | as.factor(Title),origin=0,par.strip.text = list(cex = 0.65))]

#not run


#NCHS.386
NCHS.386 <- NCHS.386[!is.na(Title),]

lattice.options(default.args = list(as.table = TRUE,
                                    grid = TRUE,
                                    auto.key = TRUE))

dev.new();plot.new();
btwn1.39_top10 <-  NCHS.386[between(Age,1,39),
	.N,.(Title)][order(-N)][1:10]
NCHS.386[between(Age,1,39) &
 (Title %in% btwn1.39_top10$Title),.N,.(Title,Age)][
	,xyplot( N ~ Age | as.factor(Title),origin=0,par.strip.text = list(cex = 0.65),
	main="Top 10 US 2021 COD Ages 1 - 39")]

dev.new();plot.new();
btwn40.59_top10 <-  NCHS.386[between(Age,40,59),
	.N,.(Title)][order(-N)][1:10]
NCHS.386[between(Age,40,59) &
 (Title %in% btwn40.59_top10$Title),.N,.(Title,Age)][
	,xyplot( N ~ Age | as.factor(Title),origin=0,par.strip.text = list(cex = 0.65),
	main="Top 10 US 2021 COD Ages 40 - 59")]

dev.new();plot.new();
btwn60.100_top10 <-  NCHS.386[between(Age,60,100),
	.N,.(Title)][order(-N)][1:10]
NCHS.386[between(Age,60,100) &
 (Title %in% btwn60.100_top10$Title),.N,.(Title,Age)][
	,xyplot( N ~ Age | as.factor(Title),origin=0,par.strip.text = list(cex = 0.65),
	main="Top 10 US 2021 COD Ages 60 - 100")]

dev.new();plot.new();
btwn1.120_top10 <-  NCHS.386[between(Age,1,120),
	.N,.(Title)][order(-N)][1:10]
NCHS.386[between(Age,1,120) &
 (Title %in% btwn1.120_top10$Title),.N,.(Title,Age)][
	,xyplot( N ~ Age | as.factor(Title),origin=0,par.strip.text = list(cex = 0.65),
	main="Top 10 US 2021 COD Ages 1 - 120")]

#not run


dev.new();plot.new();
btwn60.70_top40 <-  NCHS.386[between(Age,60,70),
	.N,.(Title)][order(-N)][1:40]
NCHS.386[between(Age,60,70) &
 (Title %in% btwn60.70_top40$Title),.N,.(Title,Age)][
	,xyplot( N ~ Age | as.factor(Title),origin=0,par.strip.text = list(cex = 0.75))]

t1 <- NCHS.386[between(Age,60,70) &
 (Title %in% btwn60.70_top40$Title),.N,.(Title,Age)][,
	dcast(.SD,Title ~ Age,value.var="N",fun.aggregate=sum)]

t1 <- setnames(cbind(t1,as.matrix(rowSums(t1[1:40,2:11]))),
	"V1","Totals")[order(-Totals)]


t1 <- NCHS.386[between(Age,1,121) &
 (Title %in% btwn1.121_top12$Title),.N,.(Title,Age)][,
	dcast(.SD,Age ~ Title,value.var="N",fun.aggregate=sum)]
t1

btwn1.121_top12 <-  NCHS.386[between(Age,1,121) & !is.na(Title),
	.N,.(Title)][order(-N)][1:12]
btwn1.121_top12

NCHS.386[between(Age,1,121) &
 (Title %in% btwn1.121_top12$Title),.N,.(Title,Age)][,
	xyplot( N ~ Age | as.factor(Title),
	origin=0,par.strip.text = list(cex = 1),main="How the Top Twelve Causes of Death killed Americans in 2021")]


t2 <- setnames(cbind(t1,as.matrix(rowSums(t1[1:114,2:6]))),
	"V1","Totals")[order(-Totals)]
	
	
#subset top 12 COD between 55 - 85 inclusive
t12.55.85 <- NCHS.386[!is.na(Title) & between(Age,55,85),.N,.(Title)][order(-N)][1:12]
t12.list <- as.list(t12.55.85$Title)


dev.new();plot.new()
NCHS.386[!is.na(Title) & between(Age,55,85),.N,.(Title,Age)][
	Age != 999 & Title %in% t12.list,
	xyplot(N ~ Age |as.factor(Title),type=c("p"),
	cex=1.25,pch=19,col=c("red","blue","green"))]
	panel.grid(40,40,"gray")
#

dev.new();plot.new()
NCHS.386[!is.na(Title) & between(Age,55,85),.N,.(Title,Age)][
	Age != 999 & Title %in% t12.list,
	xyplot(N ~ Age,groups=as.factor(Title),type=c("p"),
	cex=1.25,pch=19,auto.key=list(cex=c(text=1.25,points=1.25)))]
	panel.grid(40,40,"gray")
#


#subset top 12 COD between 60 - 70 inclusive
t12.60.70 <- NCHS.386[!is.na(Title) & between(Age,60,70),.N,.(Title)][order(-N)][1:12]
t12.list <- as.list(t12.60.70$Title)


dev.new();plot.new()
NCHS.386[!is.na(Title) & between(Age,60,70),.N,.(Title,Age)][
	Age != 999 & Title %in% t12.list,
	xyplot(N ~ Age |as.factor(Title),type=c("p"),
	cex=1.25,pch=19,col=c("red","blue","green"))]
	panel.grid(40,40,"gray")
#

dev.new();plot.new()
NCHS.386[!is.na(Title) & between(Age,60,70),.N,.(Title,Age)][
	Age != 999 & Title %in% t12.list,
	xyplot(N ~ Age,groups=as.factor(Title),type=c("p"),
	cex=1.25,pch=19,auto.key=list(cex=c(text=1.25,points=1.25)))]
	panel.grid(40,40,"gray")
#


## ICD.10
library(lattice)
jpeg_create <- function() {
          systime <- as.numeric(Sys.time())
          # dev.new()
          jpeg(filename = systime,
          width = 1024, height = 768, units = "px", pointsize = 12,
          quality = 100, bg = "white", res = NA, family = "", restoreConsole = TRUE,
          type = c("windows"))
          Sys.sleep(2)
                  }



lattice.options(default.args = list(as.table = TRUE,
                                    grid = TRUE,
                                    auto.key = TRUE))
ICD.10 <- fread("F:/CoVD19/Mortality.Natality/ICD.10.csv")
ICD.10.merge <- merge(SexAgeCOD.2021,ICD.10,by.x="ICD.10",by.y="ICD",all.y=TRUE)
ICD.10.merge
ICD.10.merge[is.na(Title),]

btwn1.20_top10 <-  ICD.10.merge[between(Age,1,20),
	.N,.(Title)][order(-N)][1:10]
btwn1.39_top10 <-  ICD.10.merge[between(Age,1,39),
	.N,.(Title)][order(-N)][1:10]
btwn40.59_top10 <-  ICD.10.merge[between(Age,40,59),
	.N,.(Title)][order(-N)][1:10]
btwn60.100_top10 <-  ICD.10.merge[between(Age,60,100),
	.N,.(Title)][order(-N)][1:10]
btwn1.120_top10 <-  ICD.10.merge[between(Age,1,120),
	.N,.(Title)][order(-N)][1:10]
	
btwn1.20_top10
btwn1.39_top10
btwn40.59_top10
btwn60.100_top10
btwn1.120_top10


jpeg_create <- function() {
          systime <- as.numeric(Sys.time())
          # dev.new()
          jpeg(filename = systime,
          width = 2560, height = 962, units = "px", pointsize = 16,
          quality = 100, bg = "white", res = NA, family = "", restoreConsole = TRUE,
          type = c("windows"))
          Sys.sleep(2)
                  }
				  
					
dev.new();plot.new();
# jpeg_create()
btwn1.20_top10 <-  ICD.10.merge[between(Age,1,20),
	.N,.(Title)][order(-N)][1:10]
ICD.10.merge[between(Age,1,20) &
 (Title %in% btwn1.20_top10$Title),.N,.(Title,Age)][
	,xyplot( N ~ Age | as.factor(substr(Title,0,45)),origin=0,
	cex=1.25,pch=19,col="blueviolet",par.strip.text = list(cex = 1.25),
	main="Top 10 US ICD.10 2021 COD Ages 1 - 20")]


dev.new();plot.new();
btwn1.39_top10 <-  ICD.10.merge[between(Age,1,39),
	.N,.(Title)][order(-N)][1:10]
ICD.10.merge[between(Age,1,39) &
 (Title %in% btwn1.39_top10$Title),.N,.(Title,Age)][
	,xyplot( N ~ Age | as.factor(substr(Title,0,45)),origin=0,
	cex=1.25,pch=19,col="blue",par.strip.text = list(cex = 1.25),
	main="Top 10 US ICD.10 2021 COD Ages 1 - 39")]

dev.new();plot.new();
btwn40.59_top10 <-  ICD.10.merge[between(Age,40,59),
	.N,.(Title)][order(-N)][1:10]
ICD.10.merge[between(Age,40,59) &
 (Title %in% btwn40.59_top10$Title),.N,.(Title,Age)][,
	xyplot( N ~ Age | as.factor(substr(Title,0,45)),origin=0,
	cex=1.25,pch=19,col="blue1",par.strip.text = list(cex =1.25),
	main="Top 10 US ICD.10 2021 COD Ages 40 - 59")]

dev.new();plot.new();
btwn60.100_top10 <-  ICD.10.merge[between(Age,60,100),
	.N,.(Title)][order(-N)][1:10]
ICD.10.merge[between(Age,60,100) &
 (Title %in% btwn60.100_top10$Title),.N,.(Title,Age)][,
	xyplot( N ~ Age | as.factor(substr(Title,0,45)),origin=0,
	cex=1.25,pch=19,col="blue2",par.strip.text = list(cex = 1.25),
	main="Top 10 US ICD.10 2021 COD Ages 60 - 100")]

dev.new();plot.new();
btwn1.120_top10 <-  ICD.10.merge[between(Age,1,120),
	.N,.(Title)][order(-N)][1:10]
ICD.10.merge[between(Age,1,120) &
 (Title %in% btwn1.120_top10$Title),.N,.(Title,Age)][,
	xyplot( N ~ Age | as.factor(substr(Title,0,45)),origin=0,
	cex=1.25,pch=19,col="blue3",par.strip.text = list(cex = 1.25),
	main="Top 10 US ICD.10 2021 COD Ages 1 - 120")]

#not run


dev.new();plot.new();
btwn60.70_top40 <-  ICD.10.merge[between(Age,60,70),
	.N,.(Title)][order(-N)][1:40]
ICD.10.merge[between(Age,60,70) &
 (Title %in% btwn60.70_top40$Title),.N,.(Title,Age)][
	,xyplot( N ~ Age | as.factor(Title),origin=0,par.strip.text = list(cex = 0.75))]

t1 <- ICD.10.merge[between(Age,60,70) &
 (Title %in% btwn60.70_top40$Title),.N,.(Title,Age)][,
	dcast(.SD,Title ~ Age,value.var="N",fun.aggregate=sum)]

t1 <- setnames(cbind(t1,as.matrix(rowSums(t1[1:40,2:11]))),
	"V1","Totals")[order(-Totals)]


t1 <- ICD.10.merge[between(Age,1,121) &
 (Title %in% btwn1.121_top12$Title),.N,.(Title,Age)][,
	dcast(.SD,Age ~ Title,value.var="N",fun.aggregate=sum)]
t1

btwn1.121_top12 <-  ICD.10.merge[between(Age,1,121) & !is.na(Title),
	.N,.(Title)][order(-N)][1:12]
btwn1.121_top12

ICD.10.merge[between(Age,1,121) &
 (Title %in% btwn1.121_top12$Title),.N,.(Title,Age)][,
	xyplot( N ~ Age | as.factor(Title),
	origin=0,par.strip.text = list(cex = 1),main="How the Top Twelve Causes of Death killed Americans in 2021")]


t2 <- setnames(cbind(t1,as.matrix(rowSums(t1[1:114,2:6]))),
	"V1","Totals")[order(-Totals)]
	
	
#subset top 12 COD between 55 - 85 inclusive
t12.55.85 <- ICD.10.merge[!is.na(Title) & between(Age,55,85),.N,.(Title)][order(-N)][1:12]
t12.list <- as.list(t12.55.85$Title)


dev.new();plot.new()
ICD.10.merge[!is.na(Title) & between(Age,55,85),.N,.(Title,Age)][
	Age != 999 & Title %in% t12.list,
	xyplot(N ~ Age |as.factor(Title),type=c("p"),
	cex=1.25,pch=19,col=c("red","blue","green"))]
	panel.grid(40,40,"gray")
#

dev.new();plot.new()
ICD.10.merge[!is.na(Title) & between(Age,55,85),.N,.(Title,Age)][
	Age != 999 & Title %in% t12.list,
	xyplot(N ~ Age,groups=as.factor(Title),type=c("p"),
	cex=1.25,pch=19,auto.key=list(cex=c(text=1.25,points=1.25)))]
	panel.grid(40,40,"gray")
#



#subset top 12 COD between 60 - 70 inclusive
t12.60.70 <- ICD.10.merge[!is.na(Title) & between(Age,60,70),.N,.(Title)][order(-N)][1:12]
t12.list <- as.list(t12.60.70$Title)


dev.new();plot.new()
ICD.10.merge[!is.na(Title) & between(Age,60,70),.N,.(Title,Age)][
	Age != 999 & Title %in% t12.list,
	xyplot(N ~ Age |as.factor(Title),type=c("p"),
	cex=1.25,pch=19,col=c("red","blue","green"))]
	panel.grid(40,40,"gray")
#

dev.new();plot.new()
ICD.10.merge[!is.na(Title) & between(Age,60,70),.N,.(Title,Age)][
	Age != 999 & Title %in% t12.list,
	xyplot(N ~ Age,groups=as.factor(Title),type=c("p"),
	cex=1.25,pch=19,auto.key=list(cex=c(text=1.25,points=1.25)))]
	panel.grid(40,40,"gray")
#





















