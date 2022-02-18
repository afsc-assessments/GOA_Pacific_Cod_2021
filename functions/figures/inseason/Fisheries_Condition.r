username=""
password=""


 AFSC=odbcConnect("AFSC",username,password,believeNRows=FALSE)

test<-paste0("SELECT OBSINT.CURRENT_SPCOMP.YEAR,  \n",
  "OBSINT.CURRENT_SPCOMP.SPECIES, \n",
  "TO_CHAR(OBSINT.CURRENT_HAUL.HAUL_DATE, 'mm') AS MONTH, \n",
  "OBSINT.CURRENT_SPCOMP.SAMPLE_TYPE, \n",
  "OBSINT.CURRENT_SPCOMP.SAMPLE_NUMBER, \n",
  "OBSINT.CURRENT_SPCOMP.SAMPLE_SIZE, \n",
  "OBSINT.CURRENT_SPCOMP.SAMPLE_WEIGHT, \n",
  "OBSINT.CURRENT_SPCOMP.WEIGHT, \n",
  "OBSINT.CURRENT_SPCOMP.COUNT,\n",
  "OBSINT.CURRENT_HAUL.RETRIEVAL_DATE - OBSINT.CURRENT_HAUL.DEPLOYMENT_DATE AS DURATION, \n",
  "OBSINT.CURRENT_HAUL.BOTTOM_DEPTH_FATHOMS, \n",
  "OBSINT.CURRENT_HAUL.GEAR_TYPE, \n",
  "OBSINT.CURRENT_HAUL.NMFS_AREA, \n",
  "OBSINT.CURRENT_HAUL.LATDD_END, \n",
  "OBSINT.CURRENT_HAUL.LONDD_END, \n",
  "OBSINT.CURRENT_HAUL.TOTAL_HOOKS_POTS, \n",
  "OBSINT.CURRENT_HAUL.OFFICIAL_TOTAL_CATCH,\n",
  "OBSINT.CURRENT_HAUL.VESSEL_TYPE \n",
"FROM OBSINT.CURRENT_HAUL \n",
"INNER JOIN OBSINT.CURRENT_SPCOMP \n",
"ON OBSINT.CURRENT_HAUL.CRUISE       = OBSINT.CURRENT_SPCOMP.CRUISE \n",
"AND OBSINT.CURRENT_HAUL.PERMIT      = OBSINT.CURRENT_SPCOMP.PERMIT \n",
"AND OBSINT.CURRENT_HAUL.HAUL_SEQ    = OBSINT.CURRENT_SPCOMP.HAUL_SEQ \n",
"WHERE OBSINT.CURRENT_SPCOMP.SPECIES = 202 \n",
"AND OBSINT.CURRENT_HAUL.NMFS_AREA   > 600 \n",
"AND OBSINT.CURRENT_HAUL.NMFS_AREA   < 640 \n",
"ORDER BY OBSINT.CURRENT_SPCOMP.YEAR, \n",
  "MONTH")

data <- data.table(sqlQuery(AFSC,test))
data<-data[DURATION>0]

data$WCPUE <-data$WEIGHT/data$TOTAL_HOOKS_POTS
data[is.na(data$TOTAL_HOOKS_POTS)]$WCPUE=data[is.na(data$TOTAL_HOOKS_POTS)]$WEIGHT/data[is.na(data$TOTAL_HOOKS_POTS)]$DURATION


data$TRIMESTER<-1
data$TRIMESTER[data$MONTH>4]<-2
data$TRIMESTER[data$MONTH>8]<-3

data$AREA<-"CG"
data[NMFS_AREA==610]$AREA <- "WG"

 d<-ggplot(data[TRIMESTER%in%c(1) & AREA=="WG"&GEAR_TYPE==1],aes(WCPUE,color=factor(GEAR_TYPE),fill=factor(GEAR_TYPE)))
 d<-d+geom_histogram(bins=50)
 d<-d+scale_color_manual(name="GEAR",values=c("red","blue","green","purple"))+scale_fill_manual(name="GEAR",values=c("red","blue","green","purple"))
 d<-d+facet_wrap(~YEAR,ncol=3, scale="free_y")
 #d<-d+theme1(base_size=15)
 
 d

data8<-data[GEAR_TYPE==8]
test<-data8[,list(MEAN_LOGCPUE=mean(log(WCPUE)),MEDCPUE=median(WCPUE)),by=c("YEAR,TRIMESTER,AREA")]
d<-ggplot(test,aes(x=YEAR,y=MEAN_LOGCPUE,color=factor(TRIMESTER)))
d<-d+facet_wrap(~AREA)
d<-d+geom_line(size=1.25)
d



test<-paste0("SELECT
    obsint.debriefed_age.nmfs_area,
    obsint.debriefed_age.gear,
    obsint.debriefed_age.species,
    obsint.debriefed_age.length,
    obsint.debriefed_age.weight,
    obsint.debriefed_age.year,
    TO_CHAR(obsint.debriefed_age.haul_offload_date, 'mm') AS month,
    obsint.debriefed_spcomp.extrapolated_weight AS catch
FROM
    obsint.debriefed_age
    INNER JOIN obsint.debriefed_spcomp ON obsint.debriefed_age.haul_join = obsint.debriefed_spcomp.haul_join
WHERE
    obsint.debriefed_age.nmfs_area > 600
    AND obsint.debriefed_age.nmfs_area < 640
    AND obsint.debriefed_age.species = 202
    AND obsint.debriefed_spcomp.species = 202")

data <- data.table(sqlQuery(AFSC,test))
data<-data[LENGTH>0&WEIGHT>0]
data$TRIMESTER<-1
data$TRIMESTER[data$MONTH>4]<-2
data$TRIMESTER[data$MONTH>8]<-3

data$AREA<-"CG"
data[NMFS_AREA==610]$AREA <- "WG"

data$LLENGTH=log(data$LENGTH)
data$LWEIGHT=log(data$WEIGHT)


data1<-data[TRIMESTER%in%c(1,2,3)&GEAR==8&&AREA=="CG"&YEAR>2000&YEAR<2018]

test1<-lm(LWEIGHT~LLENGTH+factor(YEAR)-1,data=data1)
n1<-length(unique(data1$YEAR))

x1=as.numeric(summary(test1)[[4]][2:(n1+1),1])-mean(as.numeric(summary(test1)[[4]][2:(n1+1),1]))


cond<-data.table(YEAR=sort(unique(data1$YEAR)),COND=x1,sd=as.numeric(summary(test1)[[4]][2:(n1+1),2]))

 d<-ggplot(cond,aes(x=YEAR,y=COND))
 d<-d+geom_errorbar(aes(ymin=COND-sd,ymax=COND+sd))
 d<-d+geom_point(size=2, color="red")
d<-d+geom_hline(yintercept=0)
 d<-d+ylab("Weight at length condition factor")
 d<-d+ggtitle("Pacific cod condition in first trimester Longline fishery Central Gulf")
  d<-d+theme_bw(base_size=20)

 d

data1<-data[TRIMESTER==1&GEAR==8 & AREA=="CG"&YEAR>1991]


test<-lm(LWEIGHT~LLENGTH,data=data1)

data1$PREDW<-predict(test,newdata=data1)
data1$resid<-(data1$LWEIGHT-data1$PREDW)/data1$PREDW

data1$K<-100000*(data1$WEIGHT/data1$LENGTH^3)


data1$BIN<-trunc(data1$LENGTH/10)*10
data1$BINS<-paste0(data1$BIN," to ",data1$BIN+10," CM")
data2<-data1[,list(MRESID=mean(resid),SDRESID=sd(resid), K=mean(K),SDK=sd(K)),by="YEAR,BIN,BINS"]
Kd<-data1[,list(Kmean=mean(K)),by="BIN"]
data2<-merge(data2,Kd)
data2$KS<-(data2$K-data2$Kmean)/data2$Kmean

data2<-data2[order(as.numeric(BIN),YEAR),]

d<-ggplot(data2[BIN>40&BIN<90],aes(x=factor(YEAR),y=MRESID,group=BINS))
d<-d+geom_hline(yintercept=0,color="gray80")
d<-d+geom_point()+geom_path()+facet_wrap(~BINS,scale="free_y",ncol=1)
d<-d+theme_bw(base_size=15)

d<-d+ylab("Condition by length category")+xlab("Year")
d<-d+ggtitle("Pacific cod condition at length for Central GOA longline fishery January-April")
d




require(RODBC)
require(data.table)
require(ggplot2)

##data<-read.csv("PCOD_AGES_BS.csv")
test<-paste0("SELECT
    racebase.cruise.region,
    racebase.specimen.species_code,
    racebase.haul.abundance_haul,
    racebase.specimen.age      AS age,
    racebase.specimen.length   AS length,
    racebase.specimen.weight   AS weight,
    TO_CHAR(racebase.haul.start_time, 'yyyy') AS year,
    racebase.catch.weight      AS catch
FROM
    racebase.cruise
    INNER JOIN racebase.haul ON racebase.haul.cruisejoin = racebase.cruise.cruisejoin
    INNER JOIN racebase.specimen ON racebase.specimen.cruisejoin = racebase.haul.cruisejoin
                                    AND racebase.specimen.hauljoin = racebase.haul.hauljoin
    INNER JOIN racebase.catch ON racebase.haul.cruisejoin = racebase.catch.cruisejoin
                                 AND racebase.haul.hauljoin = racebase.catch.hauljoin
WHERE
    racebase.cruise.region = 'GOA'
    AND racebase.specimen.species_code = 21720
    AND racebase.haul.abundance_haul = 'Y'
    AND racebase.specimen.length IS NOT NULL
    AND racebase.specimen.weight IS NOT NULL
    AND racebase.catch.species_code = 21720
ORDER BY
    year,
    length")

AFSC=odbcConnect("AFSC",username,password,believeNRows=FALSE)
data <- sqlQuery(AFSC,test)

  #  AND racebase.haul.end_longitude < - 145
  #  AND racebase.haul.end_longitude > - 159


lw.resids<-function(length,weight,outlier.rm=FALSE){
# For testing
# length<-tempdata$LENGTH
# weight<-tempdata$WEIGHT
# outlier.rm<-TRUE
  
  require(car)
  loglength<-log(length)
  logwt<-log(weight)
    lw.res<-lm(logwt~loglength)
  #Assessing Outliers using Bonferoni Outlier Test
  #Identify if there are any outliers in your data that exceed cutoff = 0.05 (default)
  if(outlier.rm==TRUE){
  #outlierTest(lw.res,n.max=Inf)
    #QQ residual plot with SD's 
 # qqPlot(lw.res, main="QQ Plot") #qq plot for studentized resid
  #Produce a bonferoni value for each point in your data
  test1<-outlierTest(lw.res,n.max=Inf,cutoff=Inf,order=FALSE)$bonf.p 
  remove<-which(test1<.7,)
  print("Outlier rows removed")
  print(unname(remove))
  logwt[remove]<-NA
  lw.res<-lm(logwt~loglength,na.action=na.exclude)
  lw.res<-residuals(lw.res) 
  }
    
 if(outlier.rm==FALSE){ lw.res<-residuals(lw.res)}
  return(lw.res)}

#' A function to weight length-weight residuals by catch
#'
#' This function weights length-weight residuals by a catch column. This
#' catch can be CPUE from the tow where the fish was caught (most common) or
#' stratum CPUE or biomass. 
#' @param year Year of sample must be the same length as the residuals
#' @param residual Residual that will be weighted by catch
#' @param catch Catch for weighting residual (default = 1) must be the same length as residuals
#' @keywords length, weight, groundfish condition
#' @export
#' @examples
#' weighted_resids()

weighted_resids<-function(year, residuals, catch=1){
wtlw.res<-residuals
if(length(catch)==1){catch<-rep(1,length(residuals))}
years1<-unique(year)
for(i in 1:length(years1)){
d0<-which(year==years1[i])
d1<-residuals[d0]
d2<-catch[d0]
 var1<-d1*d2
 var2<-sum(d2)
 var3<-var1/var2*length(d2)
wtlw.res[d0]<-var3}
return(wtlw.res)}

data <- sqlQuery(AFSC,test)
data<-data.table(data)
data=data[LENGTH %in% 1:2000]


data=data.frame(data)
lwdata_by_year<-array(dim=c(0,5))
colnames(lwdata_by_year)<-c("yrs","ymeans","yn","ysd","yse")
    data["residuals"]<-lw.resids(data$LENGTH,data$WEIGHT,outlier.rm=T) # Use the lw.resids function to calculate the residuals
    data["residuals.wt"]<-weighted_resids(data$YEAR,data$residuals,data$CATCH)
    data<-subset(data,is.na(data$residuals.wt)==FALSE) 
    yrs=sort(unique(data$YEAR)) #Sort by year
    ymeans=tapply(data$residuals.wt,data$YEAR,mean) #Calculate mean by year
    yn=tapply(data$residuals.wt,data$YEAR,length) #Count the number of observations by year
    ysd=tapply(data$residuals.wt,data$YEAR,sd) #Calculate the sd of the mean by year
    yse=ysd/sqrt(yn) #Calculate the standard error
    data.summary<-data.frame(yrs,ymeans,yn,ysd,yse) #Put the mean, SE and year into a data frame for plotting
lwdata_by_year<-rbind(lwdata_by_year,data.summary)  
data.summary=data.table(data.summary)
p1<-ggplot(data.summary[yrs%in%c(1980:2020)], aes(x = yrs, y = ymeans),cex=2) +  geom_point(shape=1,size=2.5)+geom_line(linetype=2)+
  #geom_bar(position = position_dodge(), width=1.25,stat="identity", fill="gray70",col="black") + 
  geom_errorbar(aes(ymin=ymeans-yse, ymax=ymeans+yse),width=0.30)+ scale_x_continuous(breaks=seq(1980,2020,by=5))+ #xlim(1988.5, 2018.5)+
 ##ggtitle(paste("Pacific cod")) + 
  geom_hline(yintercept=0, color="black")+
  theme_bw(base_size=20) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=18)) +
  theme(plot.title = element_text(size = rel(0.9), hjust = 0.5, vjust = 1))+
  #theme(axis.title.x = element_text(size=18))+
  #theme(axis.title.y = element_text(size=18))+
  labs(y = "Length-weight residual", x = "Years",title="AFSC Bottom trawl survey  GOA Pacific cod -  Catch weighted")#+ylim(-0.11,0.1)
p1


## data pull for fishery data from AFSC database
test<-paste0("SELECT
    obsint.debriefed_age.nmfs_area,
    obsint.debriefed_age.gear,
    obsint.debriefed_age.species,
    obsint.debriefed_age.length,
    obsint.debriefed_age.weight,
    obsint.debriefed_age.year,
    TO_CHAR(obsint.debriefed_age.haul_offload_date, 'mm') AS month,
    obsint.debriefed_spcomp.extrapolated_weight AS catch
FROM
    obsint.debriefed_age
    INNER JOIN obsint.debriefed_spcomp ON obsint.debriefed_age.haul_join = obsint.debriefed_spcomp.haul_join
WHERE
    obsint.debriefed_age.nmfs_area > 600
    AND obsint.debriefed_age.nmfs_area < 640
    AND obsint.debriefed_age.species = 202
    AND obsint.debriefed_spcomp.species = 202")

dataF <- data.table(sqlQuery(AFSC,test))
dataF<-dataF[LENGTH>0&WEIGHT>0]
dataF$TRIMESTER<-1
dataF$TRIMESTER[dataF$MONTH>4]<-2
dataF$TRIMESTER[dataF$MONTH>8]<-3

dataF$AREA<-"CG"
dataF[NMFS_AREA==610]$AREA <- "WG"
dataF1<-dataF[LENGTH<150]

data=data.frame(dataF1[GEAR%in%c(8)&AREA=="WG"&TRIMESTER%in%c(1)&YEAR>1991&YEAR<=2021])  ## Specify the area and time and gear type for plot
lwdata_by_year<-array(dim=c(0,5))
colnames(lwdata_by_year)<-c("yrs","ymeans","yn","ysd","yse")
    data["residuals"]<-lw.resids(data$LENGTH,data$WEIGHT,outlier.rm=TRUE) # Use the lw.resids function to calculate the residuals
    data["residuals.wt"]<-weighted_resids(data$YEAR,data$residuals,data$CATCH)
    data<-subset(data,is.na(data$residuals.wt)==FALSE) 
    yrs=sort(unique(data$YEAR)) #Sort by year
    ymeans=tapply(data$residuals.wt,data$YEAR,mean) #Calculate mean by year
    yn=tapply(data$residuals.wt,data$YEAR,length) #Count the number of observations by year
    ysd=tapply(data$residuals.wt,data$YEAR,sd) #Calculate the sd of the mean by year
    yse=ysd/sqrt(yn) #Calculate the standard error
    data.summary<-data.frame(yrs,ymeans,yn,ysd,yse) #Put the mean, SE and year into a data frame for plotting
lwdata_by_year<-rbind(lwdata_by_year,data.summary)  
data.summary=data.table(data.summary)
p1<-ggplot(data.summary[yrs%in%c(1997:2021)], aes(x = yrs, y = ymeans),cex=2) +  
  geom_bar(position = position_dodge(), width=1,stat="identity", fill="gray70",col="black") + 
  geom_errorbar(aes(ymin=ymeans-yse, ymax=ymeans+yse),width=0.30)+  scale_x_continuous(breaks=c(1998:2022),limits=c(1998,2022))+ ylim(-0.2,0.2)+#xlim(1988.5, 2018.5)+
 ##ggtitle(paste("Pacific cod")) + 
  geom_hline(yintercept=0, color="black")+
  theme_bw(base_size=20) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=18)) +
  theme(axis.text.x = element_text(hjust=1, angle = 90))+
  #theme(axis.title.x = element_text(size=18))+
  #theme(axis.title.y = element_text(size=18))+
  labs(y = "Length-weight residual 20cm-50cm", x = "Year",title="Longline fishery data Western GOA Jan-Mar")
p1

## number of vessels
username1="sbarbeaux"
password1="$tockmen12"


 AKFIN=odbcConnect("AKFIN",username1,password1,believeNRows=FALSE)


test1<-paste0("SELECT
    council.comprehensive_blend_ca.week_end_date,
    TO_CHAR(council.comprehensive_blend_ca.week_end_date, 'mm') AS month,
    council.comprehensive_blend_ca.year,
    council.comprehensive_blend_ca.vessel_id,
    council.comprehensive_blend_ca.trip_target_code,
    council.comprehensive_blend_ca.fmp_subarea,
    council.comprehensive_blend_ca.fmp_gear,
    council.comprehensive_blend_ca.weight_posted,
    council.comprehensive_blend_ca.species_name,
    council.comprehensive_blend_ca.trip_target_name,
    council.comprehensive_blend_ca.reporting_area_code,
    council.comprehensive_blend_ca.fmp_area
FROM
    council.comprehensive_blend_ca
WHERE
    council.comprehensive_blend_ca.species_name = 'cod, Pacific (gray)'
    AND council.comprehensive_blend_ca.fmp_area = 'GOA'")

dataC <- data.table(sqlQuery(AKFIN,test1))

dataCJF<-dataC[MONTH%in%c(1:6)&TRIP_TARGET_CODE=="C"]

Effort<-dataCJF[,list(NUM_VES=length(unique(VESSEL_ID))),by=c("YEAR","FMP_GEAR","FMP_SUBAREA")]


p<-ggplot(Effort[FMP_SUBAREA%in%c("CG","WG")], aes(x=YEAR,y=NUM_VES,fill=FMP_GEAR)) +
  geom_area(stat ="identity", alpha=0.6)+scale_x_continuous(breaks=c(2003:2020),limits=c(2003,2019))+theme_bw(base_size=18)+
  theme(axis.text.x = element_text(angle=90))+labs(y="Number of unique vessels",x="Year",fill="Gear Type")+facet_wrap(~FMP_SUBAREA,scale="free_y",nrow=2)
