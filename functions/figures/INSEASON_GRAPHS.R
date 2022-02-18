

username=""
password=""

AKFINusername=""
AKFINpassword=""


library(RODBC)
library(data.table)
library(ggplot2)
require(reshape2)
require(rgdal)
require(dplyr)
require(lubridate)
library(reshape)
 



 

  if(R.Version()$arch=="i386"){
    AFSC=odbcConnect("AFSC",username,password)
    }
  else {
    AFSC=odbcConnect("AFSC",username,password,believeNRows=FALSE)
    AKFIN=odbcConnect("AKFIN",AKFINusername,AKFINpassword,believeNRows=FALSE)
    }


test<-paste0("SELECT OBSINT.CURRENT_SPCOMP.SPECIES,
  CAST(OBSINT.CURRENT_HAUL.HAUL_JOIN AS VARCHAR(20)) AS HAUL_JOIN,
  OBSINT.CURRENT_HAUL.CRUISE,
  OBSINT.CURRENT_HAUL.VESSEL,
  OBSINT.CURRENT_HAUL.GEAR_TYPE,
  OBSINT.CURRENT_HAUL.VESSEL_TYPE,
  OBSINT.CURRENT_SPCOMP.YEAR,
  OBSINT.CURRENT_HAUL.NMFS_AREA,
  OBSINT.CURRENT_SPCOMP.SAMPLE_TYPE,
  OBSINT.CURRENT_SPCOMP.SAMPLE_NUMBER,
  OBSINT.CURRENT_SPCOMP.SAMPLE_SIZE,
  OBSINT.CURRENT_SPCOMP.SAMPLE_WEIGHT,
  OBSINT.CURRENT_SPCOMP.WEIGHT,
  OBSINT.CURRENT_SPCOMP.COUNT,
  OBSINT.CURRENT_HAUL.OFFICIAL_TOTAL_CATCH,
  OBSINT.CURRENT_HAUL.DEPLOYMENT_DATE,
  OBSINT.CURRENT_HAUL.RETRIEVAL_DATE,
  to_char(OBSINT.CURRENT_HAUL.RETRIEVAL_DATE,'mm') as MONTH,
  OBSINT.CURRENT_HAUL.TOTAL_HOOKS_POTS,
  OBSINT.CURRENT_HAUL.LATDD_END,
  OBSINT.CURRENT_HAUL.LONDD_END,
  OBSINT.CURRENT_HAUL.BOTTOM_DEPTH_FATHOMS
FROM OBSINT.CURRENT_HAUL
INNER JOIN OBSINT.CURRENT_SPCOMP
ON OBSINT.CURRENT_HAUL.CRUISE       = OBSINT.CURRENT_SPCOMP.CRUISE
AND OBSINT.CURRENT_HAUL.PERMIT      = OBSINT.CURRENT_SPCOMP.PERMIT
AND OBSINT.CURRENT_HAUL.HAUL_SEQ    = OBSINT.CURRENT_SPCOMP.HAUL_SEQ
WHERE OBSINT.CURRENT_SPCOMP.SPECIES = 202")


test2<-paste0("SELECT COUNCIL.COMPREHENSIVE_OBS_HAUL.CRUISE,
  CAST(COUNCIL.COMPREHENSIVE_OBS_HAUL.HAUL_JOIN AS VARCHAR(20)) AS HAUL_JOIN,
  COUNCIL.COMPREHENSIVE_OBS_HAUL.TARGET_FISHERY_NAME,
  COUNCIL.COMPREHENSIVE_OBS_HAUL.TRIP_TARGET_NAME
FROM COUNCIL.COMPREHENSIVE_OBS_HAUL
WHERE COUNCIL.COMPREHENSIVE_OBS_HAUL.YEAR > 2007")






data_CATCH<- data.table(sqlQuery(AKFIN,test2,as.is=T))
data_ALL<- data.table(sqlQuery(AFSC,test,as.is=T))

data_ALL2=merge(data_ALL,data_CATCH,by=c("CRUISE","HAUL_JOIN"))

data_ALL<-data_ALL2[TRIP_TARGET_NAME%in% c("Pacific Cod")]




data_ALL<-data_ALL[GEAR_TYPE %in% c(1,2,6,8)]


data<-data_ALL[!is.na(DEPLOYMENT_DATE)&!is.na(RETRIEVAL_DATE)]

data$WEIGHT<-as.numeric(data$WEIGHT)
data$SAMPLE_WEIGHT<-as.numeric(data$SAMPLE_WEIGHT)
data$OFFICIAL_TOTAL_CATCH<-as.numeric(data$OFFICIAL_TOTAL_CATCH)
data$COUNT<-as.numeric(data$COUNT)
data$TOTAL_HOOKS_POTS<-as.numeric(data$TOTAL_HOOKS_POTS)
data$SAMPLE_SIZE<-as.numeric(data$SAMPLE_SIZE)
data$SAMPLE_NUMBER<-as.numeric(data$SAMPLE_NUMBER)
data$BOTTOM_DEPTH_FATHOMS<-as.numeric(data$BOTTOM_DEPTH_FATHOMS)
data$LONDD_END<-as.numeric(data$LONDD_END)

#data<-data[(data$WEIGHT/data$OFFICIAL_TOTAL_CATCH)>0.3]
#dataL<-data[GEAR_TYPE%in%c(6,8)]

#data<-rbind(dataT,dataL)
data$COD_WEIGHT<-0
data$COD_NUMBER<-0
data$duration<-as.numeric(as.POSIXct(data$RETRIEVAL_DATE, format = "%Y-%m-%d %H:%M:%S")-as.POSIXct(data$DEPLOYMENT_DATE, format = "%Y-%m-%d %H:%M:%S"))/60
data<-data[!is.na(duration)]
data$CPUE_N<-0
data$CPUE_W<-0


data[GEAR_TYPE<5]$COD_WEIGHT<-(data[GEAR_TYPE<5]$SAMPLE_WEIGHT/data[GEAR_TYPE<5]$SAMPLE_SIZE)*data[GEAR_TYPE<5]$OFFICIAL_TOTAL_CATCH
data[GEAR_TYPE<5]$COD_NUMBER<- data[GEAR_TYPE<5]$COD_WEIGHT/(data[GEAR_TYPE<5]$SAMPLE_WEIGHT/data[GEAR_TYPE<5]$SAMPLE_NUMBER) 

data[GEAR_TYPE<5]$CPUE_W<-data[GEAR_TYPE<5]$COD_WEIGHT/data[GEAR_TYPE<5]$duration
data[GEAR_TYPE<5]$CPUE_N<-data[GEAR_TYPE<5]$COD_NUMBER/data[GEAR_TYPE<5]$duration


#data[GEAR_TYPE>5]$COD_WEIGHT<-((data[GEAR_TYPE>5]$COUNT/data[GEAR_TYPE>5]$SAMPLE_SIZE)*data[GEAR_TYPE>5]$TOTAL_HOOKS_POTS)*(data[GEAR_TYPE>5]$WEIGHT/data[GEAR_TYPE>5]$COUNT)

data[GEAR_TYPE>5]$COD_WEIGHT<-(data[GEAR_TYPE>5]$WEIGHT/data[GEAR_TYPE>5]$COUNT)*(data[GEAR_TYPE>5]$TOTAL_HOOKS_POTS*data[GEAR_TYPE>5]$SAMPLE_NUMBER/data[GEAR_TYPE>5]$SAMPLE_SIZE)
data[GEAR_TYPE>5]$COD_NUMBER<-data[GEAR_TYPE>5]$COUNT/data[GEAR_TYPE>5]$SAMPLE_SIZE*data[GEAR_TYPE>5]$TOTAL_HOOKS_POTS


#data[GEAR_TYPE>5]$CPUE_W<-data[GEAR_TYPE>5]$COD_WEIGHT/data[GEAR_TYPE>5]$TOTAL_HOOKS_POTS
#data[GEAR_TYPE>5]$CPUE_N<-data[GEAR_TYPE>5]$SAMPLE_NUMBER/data[GEAR_TYPE>5]$SAMPLE_SIZE

data[GEAR_TYPE>5&CPUE_N==0]$CPUE_N<-data[GEAR_TYPE>5&CPUE_N==0]$COUNT/data[GEAR_TYPE>5&CPUE_N==0]$TOTAL_HOOKS_POTS
data[GEAR_TYPE>5&CPUE_W==0]$CPUE_W<-data[GEAR_TYPE>5&CPUE_W==0]$WEIGHT/data[GEAR_TYPE>5&CPUE_W==0]$TOTAL_HOOKS_POTS



data$GEAR<-"LONGLINE"
data[GEAR_TYPE==6]$GEAR<-"POT"
data[GEAR_TYPE==1]$GEAR<-"BOTTOM TRAWL"
data[GEAR_TYPE==2]$GEAR<-"PELAGIC TRAWL"


data_10<-data[YEAR>2007]



now.year<-year(Sys.time())
old.years<-paste0(now.year-10,"-",now.year-1)
data_10$YEAR2<-old.years
data_10[YEAR==now.year]$YEAR2<-paste0(now.year)

data_10$YEAR2<-factor(data_10$YEAR2,levels=c(old.years,paste0(now.year)))

data_10$MONTH<-month(data_10$RETRIEVAL_DATE)
data_10$REGION<-"BS"
data_10[NMFS_AREA%in%c(610)]$REGION<-"Western GOA"
data_10[NMFS_AREA%in%c(620,630)]$REGION<-"Central GOA"
data_10[NMFS_AREA%in%c(540:544)]$REGION<-"AI"

data_GOA<-data_10[REGION%in%c("Central GOA","Western GOA")]


d<-ggplot(data_GOA[GEAR!="PELAGIC TRAWL"&MONTH%in%c(1:4)&TARGET_FISHERY_NAME=='Pacific Cod'],aes(as.factor(YEAR),log(CPUE_N),group=as.factor(YEAR)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(REGION~GEAR,nrow=2,scales="free_y")
   d<-d+xlab("Year")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("Number CPUE by year,gear, and region"))+theme_bw(base_size=18)+theme(axis.text.x = element_text(hjust=1, angle = 90)) 
  d



windows()
  d<-ggplot(data_10[GEAR_TYPE%in% c(6)&NMFS_AREA%in%c(620,630)],aes(as.factor(MONTH),log(CPUE_W),group=as.factor(MONTH)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(~YEAR)
   d<-d+xlab("Year")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("CPUE by month for Area 630 pot"))
  d
  windows()
  d<-ggplot(data_10[GEAR_TYPE%in% c(8)&NMFS_AREA%in%c(620,630)],aes(as.factor(MONTH),log(CPUE),group=as.factor(MONTH)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(~YEAR)
   d<-d+xlab("Year")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("CPUE by month for Area 620 and 630 longline"))
  d
  windows()
  d<-ggplot(data_10[GEAR_TYPE%in% c(6)&NMFS_AREA%in%c(610)],aes(as.factor(MONTH),log(CPUE_W),group=as.factor(MONTH)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(~YEAR)
   d<-d+xlab("Year")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("CPUE by month for Area 610 pot"))
  d
  windows()
  d<-ggplot(data_10[GEAR_TYPE%in% c(8)&NMFS_AREA%in%c(610)],aes(as.factor(MONTH),log(CPUE),group=as.factor(MONTH)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(~YEAR)
   d<-d+xlab("Year")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("CPUE by month for Area 610 longline"))
  d
  windows()
  d<-ggplot(data_10[GEAR_TYPE%in% c(1)&NMFS_AREA%in%c(610)],aes(as.factor(MONTH),log(CPUE),group=as.factor(MONTH)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(~YEAR)
   d<-d+xlab("Year")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("CPUE by month for Area 610 bottom trawl"))
  d
windows()
  d<-ggplot(data_10[GEAR_TYPE%in% c(1,2,6,8)&NMFS_AREA%in%c(620,630)],aes(as.factor(MONTH),log(CPUE),group=as.factor(MONTH)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(~YEAR)
   d<-d+xlab("Year")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("CPUE by month for Area 620 and 630 GOA bottom trawl"))
  d


## Bering Sea
windows()
  d<-ggplot(data_10[GEAR_TYPE%in% c(1,6,8)&NMFS_AREA%in%c(610)&MONTH>5&MONTH<10],aes(as.factor(YEAR),log(CPUE_W),group=as.factor(YEAR)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(~GEAR,scales="free_y")
   d<-d+xlab("Year")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("Weight CPUE by Year for WGOA Jun-Sep"))+theme_bw(base_size=18)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  d

windows()
d<-ggplot(data_10[GEAR_TYPE%in% c(1,6,8)&NMFS_AREA%in%c(610,620,630)&YEAR<2020&MONTH>0&MONTH<5],aes(as.factor(YEAR),log(CPUE_W),group=as.factor(YEAR)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(~GEAR,scales="free_y")
   d<-d+xlab("Year")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("Weight CPUE by Year for GOA Jan-Apr"))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  d


  windows()
  d<-ggplot(data_10[GEAR_TYPE%in% c(1,6,8)&NMFS_AREA%in%c(508:513,515:519)&YEAR<2020&MONTH>0&MONTH<3],aes(as.factor(YEAR),log(CPUE_N),group=as.factor(YEAR)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(~GEAR,scales="free_y")
   d<-d+xlab("Year")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("Number CPUE by Year for Southeastern EBS Jan-Feb"))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  d
windows()
d<-ggplot(data_10[GEAR_TYPE%in% c(1,6,8)&NMFS_AREA%in%c(508:513,515:519)&YEAR<2020&MONTH>0&MONTH<3],aes(as.factor(YEAR),log(CPUE_W),group=as.factor(YEAR)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(~GEAR,scales="free_y")
   d<-d+xlab("Year")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("Weight CPUE by Year for Southeastern EBS Jan-Feb"))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  d


  windows()
  d<-ggplot(data_10[GEAR_TYPE%in% c(1,6,8)&NMFS_AREA%in%c(630)&YEAR<2020&MONTH>0&MONTH<3],aes(as.factor(YEAR),log(CPUE),group=as.factor(YEAR)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(~GEAR,scales="free_y")
   d<-d+xlab("Year")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("CPUE by Year for Area 630 Jan-Feb"))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  d

##(508:513,515:519)
windows()
  d<-ggplot(data_10[GEAR_TYPE%in% c(8)&NMFS_AREA%in%c(514,521,524)],aes(as.factor(MONTH),log(CPUE),group=as.factor(MONTH)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(~YEAR)
   d<-d+xlab("Month")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("CPUE by month for Northern EBS Longline"))
  d


windows()
  d<-ggplot(data_10[GEAR_TYPE%in% c(2,6,8)&NMFS_AREA%in%c(514,521,524)],aes(as.factor(MONTH),log(CPUE),group=as.factor(MONTH)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(~YEAR)
   d<-d+xlab("Year")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("CPUE by month for Northern EBS trawl"))
  d


windows()
  d<-ggplot(data_10[GEAR_TYPE%in% c(1)&NMFS_AREA%in%c(630)],aes(as.factor(MONTH),LONDD_END,group=as.factor(MONTH)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(~YEAR)
   d<-d+xlab("Year")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("CPUE by month for Area 630 GOA bottom trawl"))
  d


windows()
  d<-ggplot(data_10[GEAR_TYPE%in% c(1,6,8)&NMFS_AREA%in%c(610,620,630)&!is.na(BOTTOM_DEPTH_FATHOMS)],aes(as.factor(YEAR),-BOTTOM_DEPTH_FATHOMS,group=as.factor(YEAR)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(GEAR~NMFS_AREA,scales="free_y",nrow=3)
   d<-d+xlab("Year")+ylab("Bottom depth (f)")+ylim(-150,0)
   d<-d+ggtitle(paste0("Bottom depth fished over the previous 10 years" ))
d
windows()
    d<-ggplot(data_10[GEAR_TYPE%in% c(1,6,8)&NMFS_AREA%in%c(610,620,630)],aes(as.factor(YEAR),LONDD_END,group=as.factor(YEAR)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(GEAR~NMFS_AREA,scales="free_y",nrow=3)
   d<-d+xlab("Year")+ylab("Longitude")
   d<-d+ggtitle(paste0("Longitude fished over previous 10 years" ))
     d

now<-yday(Sys.time())
now.30<-now-90

data_10CUR<-data_10[yday(RETRIEVAL_DATE)>=now.30&yday(RETRIEVAL_DATE)<=now]

windows()
  d<-ggplot(data_10CUR[GEAR_TYPE%in% c(1,6,8)&NMFS_AREA%in%c(508:513,515:519)],aes(as.factor(YEAR2),log(CPUE),group=as.factor(YEAR2)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(~GEAR,scales="free_y",nrow=3)
   d<-d+xlab("Year")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("Southern Bering Sea CPUE for past 90 days compared to same period in previous 10 years for ",format(Sys.time(),"%d %B %Y")))
  d

windows()
  d<-ggplot(data_10CUR[GEAR_TYPE%in% c(1,6,8)&NMFS_AREA%in%c(610,620,630)],aes(as.factor(YEAR2),BOTTOM_DEPTH_FATHOMS,group=as.factor(YEAR2)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(GEAR~NMFS_AREA,scales="free_y",nrow=3)
   d<-d+xlab("Year")+ylab("Bottom depth (f)")
   d<-d+ggtitle(paste0("Bottom depth fished for past 90 days compared to same period in previous 10 years for ",format(Sys.time(),"%d %B %Y")))
  d

windows()
  d<-ggplot(data_10CUR[GEAR_TYPE%in% c(1,6,8)&NMFS_AREA%in%c(610,620,630)],aes(as.factor(YEAR2),LONDD_END,group=as.factor(YEAR2)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(GEAR~NMFS_AREA,scales="free_y",nrow=3)
   d<-d+xlab("Year")+ylab("Longitude")
   d<-d+ggtitle(paste0("Longitude fished for past 90 days compared to same period in previous 10 years for ",format(Sys.time(),"%d %B %Y")))
     d


windows()
  d<-ggplot(data_10CUR[GEAR_TYPE%in% c(1,6,8)&NMFS_AREA%in%c(508:524)],aes(as.factor(YEAR2),log(CPUE),group=as.factor(YEAR2)))
  d<-d+geom_boxplot()
  d<-d+facet_wrap(GEAR~NMFS_AREA,scales="free_y",nrow=3)
   d<-d+xlab("Year")+ylab("log(CPUE)")
   d<-d+ggtitle(paste0("CPUE for past 90 days compared to same period in previous 10 years for ",format(Sys.time(),"%d %B %Y")))
  d






  test2<-paste0("SELECT OBSINT.CURRENT_SPCOMP.SPECIES,
  CAST(OBSINT.CURRENT_HAUL.HAUL_JOIN AS VARCHAR(20)) AS HAUL_JOIN,
  OBSINT.CURRENT_HAUL.CRUISE,
  OBSINT.CURRENT_HAUL.GEAR_TYPE,
  OBSINT.CURRENT_SPCOMP.YEAR,
  OBSINT.CURRENT_HAUL.NMFS_AREA,
  OBSINT.CURRENT_SPCOMP.SAMPLE_NUMBER,
  OBSINT.CURRENT_SPCOMP.SAMPLE_SIZE,
  OBSINT.CURRENT_SPCOMP.SAMPLE_WEIGHT,
  OBSINT.CURRENT_SPCOMP.WEIGHT,
  OBSINT.CURRENT_SPCOMP.COUNT,
  OBSINT.CURRENT_HAUL.DEPLOYMENT_DATE,
  OBSINT.CURRENT_HAUL.RETRIEVAL_DATE,
  to_char(OBSINT.CURRENT_HAUL.RETRIEVAL_DATE,'mm') as MONTH,
  OBSINT.CURRENT_HAUL.TOTAL_HOOKS_POTS,
  OBSINT.CURRENT_HAUL.LATDD_END,
  OBSINT.CURRENT_HAUL.LONDD_END,
  OBSINT.CURRENT_HAUL.BOTTOM_DEPTH_FATHOMS,
  OBSINT.CURRENT_HAUL.VESSEL_TYPE,
  OBSINT.CURRENT_HAUL.OFFICIAL_TOTAL_CATCH,
  OBSINT.CURRENT_SPCOMP.SAMPLE_TYPE,
  OBSINT.CURRENT_SPCOMP.VESSEL,
  OBSINT.CURRENT_LENGTH.LENGTH,
  OBSINT.CURRENT_LENGTH.FREQUENCY
FROM OBSINT.CURRENT_HAUL
INNER JOIN OBSINT.CURRENT_SPCOMP
ON OBSINT.CURRENT_HAUL.CRUISE    = OBSINT.CURRENT_SPCOMP.CRUISE
AND OBSINT.CURRENT_HAUL.PERMIT   = OBSINT.CURRENT_SPCOMP.PERMIT
AND OBSINT.CURRENT_HAUL.HAUL_SEQ = OBSINT.CURRENT_SPCOMP.HAUL_SEQ
INNER JOIN OBSINT.CURRENT_LENGTH
ON OBSINT.CURRENT_HAUL.HAUL_JOIN    = OBSINT.CURRENT_LENGTH.HAUL_JOIN
AND OBSINT.CURRENT_LENGTH.SPECIES   = OBSINT.CURRENT_SPCOMP.SPECIES
WHERE OBSINT.CURRENT_SPCOMP.SPECIES = 202")

data_L<- data.table(sqlQuery(AFSC,test2,as.is=T))
data_L2=merge(data_L,data_CATCH,by=c("CRUISE","HAUL_JOIN"))

data_L<-data_L2[TRIP_TARGET_NAME=="Pacific Cod"]

data_L<-data_L[GEAR_TYPE %in% c(1,2,6,8)]





data<-data_L[!is.na(DEPLOYMENT_DATE)&!is.na(RETRIEVAL_DATE)]

data$WEIGHT<-as.numeric(data$WEIGHT)
data$SAMPLE_WEIGHT<-as.numeric(data$SAMPLE_WEIGHT)
data$OFFICIAL_TOTAL_CATCH<-as.numeric(data$OFFICIAL_TOTAL_CATCH)
data$COUNT<-as.numeric(data$COUNT)
data$TOTAL_HOOKS_POTS<-as.numeric(data$TOTAL_HOOKS_POTS)
data$SAMPLE_SIZE<-as.numeric(data$SAMPLE_SIZE)
data$LENGTH<-as.numeric(data$LENGTH)
data$FREQUENCY<-as.numeric(data$FREQUENCY)


#data<-data[(data$WEIGHT/data$OFFICIAL_TOTAL_CATCH)>0.3]
#dataL<-data[GEAR_TYPE%in%c(6,8)]

x6<-untable(data[,c(1:23)],num=data$FREQUENCY)

#data<-rbind(dataT,dataL)
#x6<-untable(data[,c(1:21)],num=data$FREQUENCY)
x6<-x6[LENGTH<200]

now.year<-year(Sys.time())
old.years<-paste0(now.year-10,"-",now.year-1)
x6$YEAR2<-old.years
x6[YEAR==now.year]$YEAR2<-paste0(now.year)

x6$YEAR2<-factor(x6$YEAR2,levels=c(old.years,paste0(now.year)))


x6$GEAR<-"LONGLINE"
x6[GEAR_TYPE==6]$GEAR<-"POT"
x6[GEAR_TYPE==1]$GEAR<-"BOTTOM TRAWL"
x6[GEAR_TYPE==2]$GEAR<-"PELAGIC TRAWL"

d<-ggplot(x6[YEAR==2018], aes(x=LENGTH)) + geom_histogram(binwidth=1)
d<-d+facet_wrap(~GEAR)


d<-ggplot(x6[YEAR%in%c(2018)&NMFS_AREA %in% c(610,620,630)], aes(x=LENGTH,fill=NMFS_AREA,color=NMFS_AREA,y=..density..)) + geom_histogram(binwidth=1)
 d<-d+facet_wrap(~GEAR)
 d<-d+xlim(0,120)
 d<-d+xlab("Fork Length (cm)")
 d




now<-yday(Sys.time())
now.30<-now-90

x6CUR<-x6[yday(RETRIEVAL_DATE)>=now.30&yday(RETRIEVAL_DATE)<=now]


windows()
d<-ggplot(x6[GEAR_TYPE %in% c(1,6,8) & NMFS_AREA %in% c(610) &MONTH %in% c("01","02","03")],aes(YEAR,LENGTH))
     d<-d+geom_boxplot()
     d<-d+facet_wrap(~GEAR_TYPE,nrow=3)
        d<-d+xlab("Year")+ylab("Length (cm)")
        d<-d+ggtitle(paste0("Pcod Length for past 90 days compared to same period in previous 10 years for ",format(Sys.time(),"%d %B %Y")))
     d



     windows()
d<-ggplot(x6[GEAR_TYPE %in% c(1,6,8) & NMFS_AREA %in% c(630) &MONTH %in% c("01","02","03","04","05","06","07","08")],aes(YEAR,LENGTH))
     d<-d+geom_boxplot()
     d<-d+facet_wrap(~GEAR_TYPE,nrow=3)
        d<-d+xlab("Year")+ylab("Length (cm)")
        d<-d+ggtitle(paste0("Pcod Length"))
     d


test3<-paste0("SELECT OBSINT.CURRENT_SPCOMP.SPECIES,
  OBSINT.CURRENT_HAUL.GEAR_TYPE,
  OBSINT.CURRENT_SPCOMP.YEAR,
  OBSINT.CURRENT_HAUL.NMFS_AREA,
  OBSINT.CURRENT_SPCOMP.SAMPLE_NUMBER,
  OBSINT.CURRENT_SPCOMP.SAMPLE_SIZE,
  OBSINT.CURRENT_SPCOMP.SAMPLE_WEIGHT,
  OBSINT.CURRENT_SPCOMP.WEIGHT,
  OBSINT.CURRENT_SPCOMP.COUNT,
  OBSINT.CURRENT_HAUL.DEPLOYMENT_DATE,
  OBSINT.CURRENT_HAUL.RETRIEVAL_DATE,
  to_char(OBSINT.CURRENT_HAUL.RETRIEVAL_DATE,'mm') as MONTH,
  OBSINT.CURRENT_HAUL.TOTAL_HOOKS_POTS,
  OBSINT.CURRENT_HAUL.LATDD_END,
  OBSINT.CURRENT_HAUL.LONDD_END,
  OBSINT.CURRENT_HAUL.BOTTOM_DEPTH_FATHOMS,
  OBSINT.CURRENT_HAUL.VESSEL_TYPE,
  OBSINT.CURRENT_HAUL.OFFICIAL_TOTAL_CATCH,
  OBSINT.CURRENT_SPCOMP.SAMPLE_TYPE,
  OBSINT.CURRENT_SPCOMP.VESSEL,
  OBSINT.CURRENT_HAUL.HAUL_JOIN,
  OBSINT.CURRENT_SPCOMP.CRUISE,
  OBSINT.CURRENT_SPCOMP.HAUL
FROM OBSINT.CURRENT_HAUL
INNER JOIN OBSINT.CURRENT_SPCOMP
ON OBSINT.CURRENT_HAUL.CRUISE       = OBSINT.CURRENT_SPCOMP.CRUISE
AND OBSINT.CURRENT_HAUL.PERMIT      = OBSINT.CURRENT_SPCOMP.PERMIT
AND OBSINT.CURRENT_HAUL.HAUL_SEQ    = OBSINT.CURRENT_SPCOMP.HAUL_SEQ
AND OBSINT.CURRENT_SPCOMP.HAUL_JOIN = OBSINT.CURRENT_HAUL.HAUL_JOIN
WHERE OBSINT.CURRENT_HAUL.GEAR_TYPE = 2")

data_P<- data.table(sqlQuery(AFSC,test3,as.is=T))

data_P$ID<-paste(data_P$CRUISE,data_P$VESSEL,data_P$HAUL,sep="_")

data_P<-data_P[MONTH%in%c("01","02","03")&NMFS_AREA%in%c(610,620,630)]

#data_P<-data_P[MONTH<3&NMFS_AREA<600]

HAULS=data_P[,list(NHAULS=length(unique(ID))),by=c("NMFS_AREA","YEAR")]
grid<-expand.grid(NMFS_AREA=c("610","620","630"),YEAR=unique(HAULS$YEAR))
HAULS<-merge(HAULS,grid,all=T)
HAULS[is.na(NHAULS)]$NHAULS=0


#dataP1<-data_P[,list(NHAULS=length(unique(ID))),by=c("YEAR","NMFS_AREA")]

data_C<-data_P[SPECIES==202]

dataP2<-data_C[,list(NCOD=length(unique(ID))),by=c("YEAR","NMFS_AREA")]

#x<-data.table(YEAR=c(2019,2020,2020,rep(2021,3)),NMFS_AREA=c(620,610,620,630,620,610),NCOD=rep(0,))
#dataP2<-rbind(dataP2,x)
dataP3<-merge(dataP2,HAULS,all=T,by=c("NMFS_AREA","YEAR"))
dataP3[is.na(NCOD)]$NCOD<-0
#dataP3<-merge(dataP1,dataP2)
dataP3$PCOD<-dataP3$NCOD/dataP3$NHAULS

dataP3<-dataP3[order(YEAR,NMFS_AREA),]

dataP3$YEAR<-factor(dataP3$YEAR,levels=c(2008:2021))
dataP3$NMFS_AREA<-factor(dataP3$NMFS_AREA,levels=c(610,620,630))




windows()
d<-ggplot(dataP3,aes(YEAR,PCOD,group=""))+ylim(0,1)
d<-d+geom_line(size=1,color="gray20")
d<-d+geom_point(size=4,color="red")
d<-d+ylab("Proportion of hauls with Pcod")+xlab("Year")
d<-d+facet_wrap(~NMFS_AREA)
d<-d+ggtitle("Pcod bycatch in GOA pelagic fisheries 2008-2021 Jan-March - Observed hauls only")
d<-d+theme(strip.text = element_text(size=25))+theme(axis.text.x = element_text(hjust=1, angle = 90))
d

windows()
d<-ggplot(dataP3,aes(x=YEAR,y=NHAULS))+geom_col()
d<-d+ylab("Number of pelagic hauls")+xlab("Year")
d<-d+facet_wrap(~NMFS_AREA)
d<-d+ggtitle("Number of observed hauls in GOA pelagic fisheries 2008-2021 Jan-March")
d<-d+theme(strip.text = element_text(size=25))+theme(axis.text.x = element_text(hjust=1, angle = 90))
d



test4=paste0("SELECT DISTINCT
    a.cruise,
    a.permit,
    c.name,
    a.offload_number,
    a.nmfs_area,
    a.pgm_code,
    d.description,
    a.landing_report_id,
    a.delivered_weight      AS delivered_weight_lb,
    e.total_sample_weight   AS total_sample_weight_kg,
    e.sample_number,
    f.species_code,
    f.species_weight,
    f.species_number,
    TO_CHAR(b.embark_date, 'yyyy') AS YEAR
FROM
    norpac.atl_offload                a,
    norpac.ols_vessel_plant           b,
    norpac.atl_lov_plant              c,
    norpac.atl_lov_elanding_mgt_pgm   d,
    norpac.atl_sample                 e,
    norpac.atl_species_composition    f
WHERE
    a.cruise = b.cruise
    AND a.permit = b.permit
    AND b.permit = c.permit
    AND a.pgm_code = d.pgm_code
    AND a.cruise = e.cruise
    AND a.permit = e.permit
    AND a.offload_seq = e.offload_seq
    AND e.cruise = f.cruise
    AND e.permit = f.permit
    AND e.sample_seq = f.sample_seq
    AND a.nmfs_area < 600
    AND a.pgm_code = 8
    AND b.special_deployment_seq IS NOT NULL
ORDER BY
    a.cruise,
    a.permit,
    a.offload_number,
    e.sample_number,
    f.species_code")

data_EM<- data.table(sqlQuery(AFSC,test4,as.is=T))


    AND a.pgm_code = 8
    AND b.special_deployment_seq IS NOT NULL




test<-paste0("SELECT OBSINT.CURRENT_SPCOMP.SPECIES,
  CAST(OBSINT.CURRENT_HAUL.HAUL_JOIN AS VARCHAR(20)) AS HAUL_JOIN,
  OBSINT.CURRENT_HAUL.CRUISE,
  OBSINT.CURRENT_HAUL.VESSEL,
  OBSINT.CURRENT_HAUL.GEAR_TYPE,
  OBSINT.CURRENT_HAUL.VESSEL_TYPE,
  OBSINT.CURRENT_SPCOMP.YEAR,
  OBSINT.CURRENT_HAUL.NMFS_AREA,
  OBSINT.CURRENT_SPCOMP.SAMPLE_TYPE,
  OBSINT.CURRENT_SPCOMP.SAMPLE_NUMBER,
  OBSINT.CURRENT_SPCOMP.SAMPLE_SIZE,
  OBSINT.CURRENT_SPCOMP.SAMPLE_WEIGHT,
  OBSINT.CURRENT_SPCOMP.WEIGHT,
  OBSINT.CURRENT_SPCOMP.COUNT,
  OBSINT.CURRENT_HAUL.OFFICIAL_TOTAL_CATCH,
  OBSINT.CURRENT_HAUL.DEPLOYMENT_DATE,
  OBSINT.CURRENT_HAUL.RETRIEVAL_DATE,
  to_char(OBSINT.CURRENT_HAUL.RETRIEVAL_DATE,'mm') as MONTH,
  OBSINT.CURRENT_HAUL.TOTAL_HOOKS_POTS,
  OBSINT.CURRENT_HAUL.LATDD_END,
  OBSINT.CURRENT_HAUL.LONDD_END,
  OBSINT.CURRENT_HAUL.BOTTOM_DEPTH_FATHOMS
FROM OBSINT.CURRENT_HAUL
INNER JOIN OBSINT.CURRENT_SPCOMP
ON OBSINT.CURRENT_HAUL.CRUISE       = OBSINT.CURRENT_SPCOMP.CRUISE
AND OBSINT.CURRENT_HAUL.PERMIT      = OBSINT.CURRENT_SPCOMP.PERMIT
AND OBSINT.CURRENT_HAUL.HAUL_SEQ    = OBSINT.CURRENT_SPCOMP.HAUL_SEQ")



test<-paste0("SELECT OBSINT.DEBRIEFED_SPCOMP.SPECIES,
  CAST(OBSINT.DEBRIEFED_HAUL.HAUL_JOIN AS VARCHAR(20)) AS HAUL_JOIN,
  OBSINT.DEBRIEFED_HAUL.CRUISE,
  OBSINT.DEBRIEFED_SPCOMP.YEAR,
  OBSINT.DEBRIEFED_SPCOMP.EXTRAPOLATED_WEIGHT,
  OBSINT.DEBRIEFED_SPCOMP.EXTRAPOLATED_NUMBER,
  OBSINT.DEBRIEFED_HAUL.OFFICIAL_TOTAL_CATCH,
  OBSINT.DEBRIEFED_HAUL.DEPLOYMENT_DATE,
  OBSINT.DEBRIEFED_HAUL.RETRIEVAL_DATE,
   OBSINT.DEBRIEFED_HAUL.NMFS_AREA,
  to_char(OBSINT.DEBRIEFED_HAUL.RETRIEVAL_DATE,'mm') as MONTH
FROM OBSINT.DEBRIEFED_HAUL
INNER JOIN OBSINT.DEBRIEFED_SPCOMP
ON OBSINT.DEBRIEFED_HAUL.CRUISE       = OBSINT.DEBRIEFED_SPCOMP.CRUISE
AND OBSINT.DEBRIEFED_HAUL.PERMIT      = OBSINT.DEBRIEFED_SPCOMP.PERMIT
WHERE OBSINT.DEBRIEFED_SPCOMP.YEAR > 2007
AND OBSINT.DEBRIEFED_HAUL.GEAR_TYPE = 1
AND OBSINT.DEBRIEFED_HAUL.NMFS_AREA > 609 
AND OBSINT.DEBRIEFED_HAUL.NMFS_AREA < 650")




test2<-paste0("SELECT COUNCIL.COMPREHENSIVE_OBS_HAUL.CRUISE,
  CAST(COUNCIL.COMPREHENSIVE_OBS_HAUL.HAUL_JOIN AS VARCHAR(20)) AS HAUL_JOIN,
  COUNCIL.COMPREHENSIVE_OBS_HAUL.TARGET_FISHERY_NAME,
  COUNCIL.COMPREHENSIVE_OBS_HAUL.TRIP_TARGET_NAME
FROM COUNCIL.COMPREHENSIVE_OBS_HAUL
WHERE COUNCIL.COMPREHENSIVE_OBS_HAUL.YEAR > 2007")


data_CATCH<- data.table(sqlQuery(AKFIN,test2,as.is=T))
data_ALL<- data.table(sqlQuery(AFSC,test,as.is=T))

data_ALL2=merge(data_ALL,data_CATCH,by=c("CRUISE","HAUL_JOIN"))

data_SWFF<-data_ALL2[TRIP_TARGET_NAME=="Shallow Water Flatfish - GOA" & MONTH < 4]
data_SWFF_C<-data.table(data_SWFF[SPECIES==202])

d1<-data_SWFF_C[,list(COD=sum(as.numeric(SAMPLE_WEIGHT))/1000),by=c("YEAR")]
d2<-data_SWFF[,list(TOTAL_W=sum(as.numeric(SAMPLE_WEIGHT))/1000),by=c("YEAR")]
x=merge(d1,d2)
x$prop<-x$COD/x$TOTAL_W

ggplot(x[YEAR<=2021],aes(x=YEAR,y=prop,group=""))+geom_line(size=1)+geom_point(size=4,color="red",fill="yellow")+
theme_bw(base_size=16)+xlab("Year")+ylab("Pacific cod (t)/Total Catch(t)")+theme(axis.text.x = element_text(hjust=1, angle = 90))+
ggtitle("Pcod bycatch in GOA Shallow water flatfish fisheries 2008-2021")


data_SWFF<-data_ALL2[TRIP_TARGET_NAME=="Sablefish"]
data_SWFF_C<-data.table(data_SWFF[SPECIES==202])
d1<-data_SWFF_C[,list(COD=sum(as.numeric(SAMPLE_WEIGHT))/1000),by=c("YEAR")]
d2<-data_SWFF[,list(TOTAL_W=sum(as.numeric(SAMPLE_WEIGHT))/1000),by=c("YEAR")]
x=merge(d1,d2)
x$prop<-x$COD/x$TOTAL_W

ggplot(x[YEAR<=2020],aes(x=YEAR,y=COD,group=""))+geom_line(size=1)+geom_point(size=4,color="red",fill="yellow")+
theme_bw(base_size=16)+xlab("Year")+ylab("Pacific cod (t)/Total Catch(t)")+theme(axis.text.x = element_text(hjust=1, angle = 90))+
ggtitle("Pcod bycatch in GOA Sablefish fisheries 2008-2021")

+facet_wrap(~NMFS_AREA)+ylim(0,0.14)


data_SWFF<-data_ALL2[TRIP_TARGET_NAME=="Sablefish"]
data_SWFF_C<-data.table(data_SWFF[SPECIES==202])
d1<-data_SWFF_C[,list(COD=sum(as.numeric(EXTRAPOLATED_WEIGHT))/1000),by=c("YEAR")]
d2<-data_SWFF[,list(TOTAL_W=sum(as.numeric(EXTRAPOLATED_WEIGHT))/1000),by=c("YEAR")]
x=merge(d1,d2)
x$prop<-x$COD/x$TOTAL_W

ggplot(x[YEAR<=2020],aes(x=YEAR,y=COD,group=""))+geom_line(size=1)+geom_point(size=4,color="red",fill="yellow")+
theme_bw(base_size=16)+xlab("Year")+ylab("Pacific cod (t)/Total Catch(t)")+theme(axis.text.x = element_text(hjust=1, angle = 90))+
ggtitle("Pcod bycatch in GOA Sablefish fisheries 2008-2021")

+facet_wrap(~NMFS_AREA)+ylim(0,0.14)






data_SWFF<-data_ALL2[TRIP_TARGET_NAME=="Arrowtooth Flounder"&NMFS_AREA%in%c(600:700)]
data_SWFF_C<-data.table(data_SWFF[SPECIES==202])
d1<-data_SWFF_C[,list(COD=sum(as.numeric(EXTRAPOLATED_WEIGHT))/1000),by=c("YEAR")]
d2<-data_SWFF[,list(TOTAL_W=sum(as.numeric(EXTRAPOLATED_WEIGHT))/1000),by=c("YEAR")]
x=merge(d1,d2)
x$prop<-x$COD/x$TOTAL_W

ggplot(x[YEAR<=2020],aes(x=YEAR,y=COD,group=""))+geom_line(size=1)+geom_point(size=4,color="red",fill="yellow")+
theme_bw(base_size=16)+xlab("Year")+ylab("Pacific cod (t)/Total Catch(t)")+theme(axis.text.x = element_text(hjust=1, angle = 90))+
ggtitle("GOA Arrowtooth Flounder fisheries 2008-2021")





data_SWFF<-data_ALL2[TRIP_TARGET_NAME=="Arrowtooth Flounder"&NMFS_AREA%in%c(600:700)]
data_SWFF_C<-data.table(data_SWFF[SPECIES==202])
d1<-data_SWFF_C[,list(COD=sum(as.numeric(SAMPLE_WEIGHT))/1000),by=c("YEAR")]
d2<-data_SWFF[,list(TOTAL_W=sum(as.numeric(SAMPLE_WEIGHT))/1000),by=c("YEAR")]
x=merge(d1,d2)
x$prop<-x$COD/x$TOTAL_W

ggplot(x[YEAR<=2021],aes(x=YEAR,y=COD,group=""))+geom_line(size=1)+geom_point(size=4,color="red",fill="yellow")+
theme_bw(base_size=16)+xlab("Year")+ylab("Pacific cod (t)/Total Catch(t)")+theme(axis.text.x = element_text(hjust=1, angle = 90))+
ggtitle("GOA Arrowtooth Flounder fisheries 2005-2020")




data_SWFF<-data_ALL2[TRIP_TARGET_NAME== "Rockfish"&NMFS_AREA%in%c(600:700)]
data_SWFF_C<-data.table(data_SWFF[SPECIES==202])
d1<-data_SWFF_C[,list(COD=sum(as.numeric(SAMPLE_WEIGHT))/1000),by=c("YEAR")]
d2<-data_SWFF[,list(TOTAL_W=sum(as.numeric(SAMPLE_WEIGHT))/1000),by=c("YEAR")]
x=merge(d1,d2)
x$prop<-x$COD/x$TOTAL_W

ggplot(x[YEAR<=2021],aes(x=YEAR,y=prop,group=""))+geom_line(size=1)+geom_point(size=4,color="red",fill="yellow")+
theme_bw(base_size=16)+xlab("Year")+ylab("Pacific cod (t)/Total Catch(t)")+theme(axis.text.x = element_text(hjust=1, angle = 90))+
ggtitle("Pcod bycatch in GOA rockfish fisheries 2008-2021")+ylim(0,0.05)




data_SWFF<-data_ALL2[TRIP_TARGET_NAME== "Rockfish"&NMFS_AREA%in%c(600:700)]
data_SWFF_C<-data.table(data_SWFF[SPECIES==202])
d1<-data_SWFF_C[,list(COD=sum(as.numeric(EXTRAPOLATED_WEIGHT))/1000),by=c("YEAR")]
d2<-data_SWFF[,list(TOTAL_W=sum(as.numeric(EXTRAPOLATED_WEIGHT))/1000),by=c("YEAR")]
x=merge(d1,d2)
x$prop<-x$COD/x$TOTAL_W

ggplot(x[YEAR<=2021],aes(x=YEAR,y=prop,group=""))+geom_line(size=1)+geom_point(size=4,color="red",fill="yellow")+
theme_bw(base_size=16)+xlab("Year")+ylab("Pacific cod (t)/Total Catch(t)")+theme(axis.text.x = element_text(hjust=1, angle = 90))+
ggtitle("Pcod bycatch in GOA rockfish fisheries 2008-2020")+ylim(0,0.08)







dataP1<-data_SWFF[,list(NHAULS=length(unique(HAUL_JOIN))),by=c("YEAR","NMFS_AREA")]
dataP2<-data_SWFF_C[,list(NCOD=length(unique(HAUL_JOIN))),by=c("YEAR","NMFS_AREA")]


dataP3<-merge(dataP1,dataP2)
dataP3$PCOD<-dataP3$NCOD/dataP3$NHAULS

windows()
d<-ggplot(dataP3[YEAR<2020],aes(YEAR,PCOD,group=""))
d<-d+geom_line(size=1,color="gray20")
d<-d+geom_point(size=4,color="red")
d<-d+ylab("Proportion of hauls with Pcod")+xlab("Year")
d<-d+ggtitle("Pcod bycatch in GOA Arrowtooth flounder fisheries 2008-2021")
d<-d+theme_bw(base_size=18)+theme(axis.text.x = element_text(hjust=1, angle = 90))+facet_wrap(~NMFS_AREA)
