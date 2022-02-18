
CPUE_ALL<-function(username="username",password="password",OBS_SP_CODE=202, TRIP_TAR_CODE="'C'",SPECIES_GRP_CODE="'PCOD'",area='GOA'){
	require(RODBC)
	require(data.table)
	require(ggplot2)

	AKFIN=odbcConnect("AKFIN",username,password,believeNRows=FALSE)

	
     data1a<-vector("list",length=length(1996:year(Sys.Date())))

	for(i in 1996:year(Sys.Date())){
		
		test1<- paste0("SELECT
    	council.comprehensive_obs.haul_join,
    	council.comprehensive_obs.obs_vessel_id,
    	council.comprehensive_obs.obs_gear_code,
    	council.comprehensive_obs.total_hooks_pots,
    	council.comprehensive_obs.year,
    	council.comprehensive_obs.duration_in_min,
    	council.comprehensive_obs.extrapolated_weight,
    	council.comprehensive_obs.extrapolated_number,    
    	council.comprehensive_obs.fmp_area,
    	council.comprehensive_obs.fmp_subarea,
    	council.comprehensive_obs.FMP_GEAR,
    	council.comprehensive_obs.VES_AKR_LENGTH,
    	council.comprehensive_obs.week_end_date,
    	to_char(council.comprehensive_obs.week_end_date,'mm') as MONTH
	FROM
    	council.comprehensive_obs
	WHERE
    	council.comprehensive_obs.year = ",i,
    	"AND council.comprehensive_obs.obs_specie_code in (", OBS_SP_CODE,") 
    	AND council.comprehensive_obs.trip_target_code in (", TRIP_TAR_CODE,")") 
  


    	#AND council.comprehensive_obs.trip_target_code in (", TRIP_TAR_CODE,")")

	 data1a[[i-1995]] <- data.table(sqlQuery(AKFIN,test1,as.is=T))
	}

	data1<-data.table(do.call(rbind,data1a))

    data1 <- data1[OBS_GEAR_CODE%in%c(1,2,6,8)]


    if(area=='GOA'){
        data1<-data1[FMP_SUBAREA%in%c('WG','CG')]
    }

    if(area=='AI'){
       data1<-data1[FMP_SUBAREA%in%c('AI')]
    }

    if(area=='BS'){
        data1<-data1[FMP_SUBAREA%in%c('BS')]
    }

   if(area=='ALL'){
        data1<-data1[FMP_SUBAREA%in%c('BS','AI','WG','CG')]
    }

	
	data1<-unique(data1)

	data1$EXTRAPOLATED_WEIGHT <-as.numeric(data1$EXTRAPOLATED_WEIGHT)
	data1$EXTRAPOLATED_NUMBER <-as.numeric(data1$EXTRAPOLATED_NUMBER)

    data1<-data1[,list(EXTRAPOLATED_WEIGHT=sum(EXTRAPOLATED_WEIGHT),EXTRAPOLATED_NUMBER=sum(EXTRAPOLATED_NUMBER)), 
    	by=c("HAUL_JOIN","OBS_VESSEL_ID","OBS_GEAR_CODE","TOTAL_HOOKS_POTS","YEAR","DURATION_IN_MIN","FMP_AREA","FMP_SUBAREA",
    	"FMP_GEAR","VES_AKR_LENGTH","WEEK_END_DATE","MONTH")]


	data1$count=1
	
	data1$DURATION_IN_MIN     <-as.numeric(data1$DURATION_IN_MIN)
	data1$TOTAL_HOOKS_POTS    <-as.numeric(data1$TOTAL_HOOKS_POTS)
	data1$TOTAL_HOOKS_POTS    <-as.numeric(data1$TOTAL_HOOKS_POTS)
	data1$YEAR                <-as.numeric(data1$YEAR)
	data1$OBS_GEAR_CODE       <-as.numeric(data1$OBS_GEAR_CODE)
	data1$MONTH               <-as.numeric(data1$MONTH)
	data1$VES_AKR_LENGTH      <-as.numeric(data1$VES_AKR_LENGTH)


	data1$VCLASS<-65	
	data1[VES_AKR_LENGTH>=65&VES_AKR_LENGTH<=125]$VCLASS<- 125
	data1[VES_AKR_LENGTH>125]$VCLASS<-150


    data1a2<-data1[OBS_GEAR_CODE%in% c(1,2) & DURATION_IN_MIN > 0]
    data1b2<-data1[OBS_GEAR_CODE%in% c(6,8) & TOTAL_HOOKS_POTS > 0]
    data1<-rbind(data1a2,data1b2)

	data1$CPUEW <- 0
	data1$CPUEN <- 0

	data1[OBS_GEAR_CODE%in%c(1,2)]$CPUEW <- data1[OBS_GEAR_CODE%in%c(1,2)]$EXTRAPOLATED_WEIGHT/data1[OBS_GEAR_CODE%in%c(1,2)]$DURATION_IN_MIN
	data1[OBS_GEAR_CODE%in%c(6,8)]$CPUEW <- data1[OBS_GEAR_CODE%in%c(6,8)]$EXTRAPOLATED_WEIGHT/data1[OBS_GEAR_CODE%in%c(6,8)]$TOTAL_HOOKS_POTS

	data1[OBS_GEAR_CODE%in%c(1,2)]$CPUEN <- data1[OBS_GEAR_CODE%in%c(1,2)]$EXTRAPOLATED_NUMBER/data1[OBS_GEAR_CODE%in%c(1,2)]$DURATION_IN_MIN
	data1[OBS_GEAR_CODE%in%c(6,8)]$CPUEN <- data1[OBS_GEAR_CODE%in%c(6,8)]$EXTRAPOLATED_NUMBER/data1[OBS_GEAR_CODE%in%c(6,8)]$TOTAL_HOOKS_POTS



	Vessels<- data1[,list(x=sum(count)),by=c("YEAR","FMP_GEAR","VCLASS","OBS_GEAR_CODE","FMP_SUBAREA","MONTH","OBS_VESSEL_ID")]
	Vessels$count=1
	Vessels<- Vessels[,list(NVES=sum(count)),by=c("YEAR","FMP_GEAR","VCLASS","OBS_GEAR_CODE","FMP_SUBAREA","MONTH")]
    

	#data2<-merge(data1,Vessels,by=c("YEAR","FMP_GEAR","VCLASS","OBS_GEAR_CODE","FMP_SUBAREA","MONTH"))
	#data2<-data2[NVES>=2]
	

	HAULS<-data1[,list(NHAUL=sum(count),MCPUEW=mean(CPUEW),MCPUEN=mean(CPUEN),sdCPUEW=sd(CPUEW),sdCPUEN=sd(CPUEN)),by=c("YEAR","OBS_GEAR_CODE","VCLASS","FMP_SUBAREA","MONTH")]
    HAULS<- merge(HAULS,Vessels, all.x=T, all.y=T)

	HAULS<-HAULS[NVES>=2]
	HAULS<-HAULS[NHAUL>3]
	
	HAULS$WStERR <- (HAULS$sdCPUEW/sqrt(HAULS$NHAUL))^2
	HAULS$NStERR <- (HAULS$sdCPUEN/sqrt(HAULS$NHAUL))^2


	

	HAULS2<-data1[,list(MYCPUEW=mean(CPUEW),MYCPUEN=mean(CPUEN)),by=c("OBS_GEAR_CODE","VCLASS","FMP_SUBAREA")]
	HAULS3<-HAULS[,list(MWStERR=mean(WStERR),MNStERR=mean(NStERR)),by=c("OBS_GEAR_CODE","VCLASS","FMP_SUBAREA")]
	HAULS2<-merge(HAULS2,HAULS3,by=c("OBS_GEAR_CODE","VCLASS","FMP_SUBAREA"))

	HAULS=merge(HAULS,HAULS2,by=c("OBS_GEAR_CODE","VCLASS","FMP_SUBAREA"))

	HAULS$MCPUEN2 <-HAULS$MCPUEN/HAULS$MYCPUEN
	HAULS$MCPUEW2 <-HAULS$MCPUEW/HAULS$MYCPUEW
	HAULS$WStERR2 <-HAULS$WStERR/(HAULS$MYCPUEW^2)
	HAULS$NStERR2 <-HAULS$NStERR/(HAULS$MYCPUEN^2)


	HAULS3.1<-HAULS[,c(1:8,12:14)]

	HAULS3.2<-HAULS[,c(1:6,12,19:22)]
	HAULS3.2$DAY=ISOdate(HAULS3.2$YEAR,HAULS3.2$MONTH,1)

	#d<-ggplot(HAULS3.2[FMP_SUBAREA=='BS'],aes(x=DAY,y=MCPUEN2))+geom_point()+geom_errorbar(aes(ymin=MCPUEN2-(2*NStERR2),ymax=MCPUEN2+(2*NStERR2)))+facet_wrap(OBS_GEAR_CODE~FMP_SUBAREA,scales='free')
    # ((1+year_effect(yr)+month_effect(mo) - meanCPUE(yr,mo))/sigmaCPUE(yr,mo))^2,

	test <- paste0("SELECT SUM(COUNCIL.COMPREHENSIVE_BLEND_CA.WEIGHT_POSTED) AS TONS,
        COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_GEAR,
        COUNCIL.COMPREHENSIVE_BLEND_CA.AGENCY_GEAR_CODE,
        COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_AREA,
        COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA,
        COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR AS YEAR,
        COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE,
        COUNCIL.COMPREHENSIVE_BLEND_CA.VES_AKR_LENGTH,
        to_char(COUNCIL.COMPREHENSIVE_BLEND_CA.week_end_date,'mm') as MONTH
      FROM 
        COUNCIL.COMPREHENSIVE_BLEND_CA
      WHERE 
        COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR > 1990 
        AND COUNCIL.COMPREHENSIVE_BLEND_CA.TRIP_TARGET_CODE in (",TRIP_TAR_CODE,")
        AND COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE in (",SPECIES_GRP_CODE,")
      GROUP BY 
        COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_GEAR,
        COUNCIL.COMPREHENSIVE_BLEND_CA.AGENCY_GEAR_CODE,
        COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR,
        COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE,
        COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA,
        COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_AREA,
        COUNCIL.COMPREHENSIVE_BLEND_CA.VES_AKR_LENGTH,
        to_char(COUNCIL.COMPREHENSIVE_BLEND_CA.week_end_date,'mm')")


    CATCH<-data.table(sqlQuery(AKFIN,test))
    #odbcClose(AKFIN)
    CATCH<-CATCH[YEAR>=min(HAULS3.1$YEAR)]

    CATCH$OBS_GEAR_CODE<- 0
    CATCH[AGENCY_GEAR_CODE=='PTR']$OBS_GEAR_CODE <- 2
    CATCH[AGENCY_GEAR_CODE=='NPT']$OBS_GEAR_CODE <- 1
    CATCH[AGENCY_GEAR_CODE=='BTR']$OBS_GEAR_CODE <- 1
    CATCH[AGENCY_GEAR_CODE=='TRW']$OBS_GEAR_CODE <- 1
    CATCH[AGENCY_GEAR_CODE=='POT']$OBS_GEAR_CODE <- 6
    CATCH[AGENCY_GEAR_CODE=='HAL']$OBS_GEAR_CODE <- 8
    CATCH <- CATCH[OBS_GEAR_CODE>0]


    CATCH$VCLASS<-65	
	CATCH[VES_AKR_LENGTH>=65&VES_AKR_LENGTH<=125]$VCLASS<- 125
	CATCH[VES_AKR_LENGTH>125]$VCLASS<-150



    CATCH<-CATCH[FMP_SUBAREA%in%c('BS','AI','WG','CG')]
    CATCH<-merge(CATCH,HAULS3.1,by=c('YEAR','MONTH','OBS_GEAR_CODE',"VCLASS",'FMP_SUBAREA'))

	Catch_Gear_Month <- CATCH[,list(MonthGearCatch=sum(TONS)), by=c('YEAR','MONTH','OBS_GEAR_CODE','VCLASS','FMP_SUBAREA')]
	Catch_Gear_Year  <- CATCH[,list(YearGearCatch=sum(TONS)),by=c('YEAR','OBS_GEAR_CODE','VCLASS','FMP_SUBAREA')]
	Catch_Year       <- CATCH[,list(YearCatch=sum(TONS)),by=c('YEAR','FMP_SUBAREA')]

	CatchPROPYEAR <- merge(Catch_Gear_Year,Catch_Year)
	CatchPROPYEAR$YEARGEARPROP <- CatchPROPYEAR$YearGearCatch/CatchPROPYEAR$YearCatch

	Catch_Gearprop <-merge(Catch_Gear_Month, Catch_Gear_Year,by=c('YEAR','OBS_GEAR_CODE','VCLASS','FMP_SUBAREA'))
	Catch_Gearprop$CATCHGearPROP<-Catch_Gearprop$MonthGearCatch/Catch_Gearprop$YearGearCatch

	CATCH3<-merge(HAULS3.1,Catch_Gearprop,all.y=T,by=c("YEAR","MONTH","OBS_GEAR_CODE","VCLASS","FMP_SUBAREA"))

	CATCH3$MCPUEN2<-CATCH3$MCPUEN*CATCH3$CATCHGearPROP
	CATCH3$MCPUEW2<-CATCH3$MCPUEW*CATCH3$CATCHGearPROP
	CATCH3$NStERR2<-CATCH3$NStERR*CATCH3$CATCHGearPROP^2
	CATCH3$WStERR2<-CATCH3$WStERR*CATCH3$CATCHGearPROP^2


	CATCH4<-CATCH3[,list(SMCPUEN2=sum(MCPUEN2),SMCPUEW2=sum(MCPUEW2),SNStERR2=sum(NStERR2),SWStERR2=sum(WStERR2)),by=c("YEAR","OBS_GEAR_CODE","VCLASS","FMP_SUBAREA")]
	CATCH5<-CATCH4[,list(ASMCPUEN2=mean(SMCPUEN2),ASMCPUEW2=mean(SMCPUEW2)),by=c("OBS_GEAR_CODE","VCLASS","FMP_SUBAREA")]

	CATCH4<-merge(CATCH4,CATCH5,by=c("OBS_GEAR_CODE","VCLASS","FMP_SUBAREA"))

	CATCH4$NCPUE_INDEX <- CATCH4$SMCPUEN2/CATCH4$ASMCPUEN2
	CATCH4$WCPUE_INDEX <- CATCH4$SMCPUEW2/CATCH4$ASMCPUEW2

	CATCH4$NCPUE_INDEX_StERR       <- CATCH4$SNStERR2/(CATCH4$ASMCPUEN2^2)
	CATCH4$WCPUE_INDEX_StERR       <- CATCH4$SWStERR2/(CATCH4$ASMCPUEW2^2)

	CATCH5<-CATCH4[,c(1:4,11:14)]


	CATCH5<-merge(CATCH5,CatchPROPYEAR,all.x=T,by=c("YEAR","OBS_GEAR_CODE","VCLASS","FMP_SUBAREA"))
	CATCH5$NCPUE_INDEX2<-CATCH5$NCPUE_INDEX*CATCH5$YEARGEARPROP
	CATCH5$WCPUE_INDEX2<-CATCH5$WCPUE_INDEX*CATCH5$YEARGEARPROP

	CATCH5$NCPUE_INDEX_StERR2       <- CATCH5$NCPUE_INDEX_StERR *CATCH5$YEARGEARPROP^2
	CATCH5$WCPUE_INDEX_StERR2       <- CATCH5$WCPUE_INDEX_StERR *CATCH5$YEARGEARPROP^2


	CATCH5.1<-CATCH5[,list(NCPUE_INDEX=sum(NCPUE_INDEX2),WCPUE_INDEX=sum(WCPUE_INDEX2),NStERR=sqrt(sum(NCPUE_INDEX_StERR2)),WStERR=sqrt(sum(WCPUE_INDEX_StERR2))),by=c("YEAR","OBS_GEAR_CODE","FMP_SUBAREA")]

    dw<-ggplot(CATCH5.1,aes(x=YEAR,y=WCPUE_INDEX))+geom_line()+geom_errorbar(aes(ymin=WCPUE_INDEX-WStERR,ymax=WCPUE_INDEX+WStERR))+facet_wrap(OBS_GEAR_CODE~FMP_SUBAREA,scales='free_y',ncol=2)
	dw<-dw+theme_bw(base_size=20)+labs(title="CPUE by weight of fish",y="CPUE by weight",x="Year")
	windows()
	print(dw)

    dn<-ggplot(CATCH5.1,aes(x=YEAR,y=NCPUE_INDEX))+geom_line()+geom_errorbar(aes(ymin=NCPUE_INDEX-NStERR,ymax=NCPUE_INDEX+NStERR))+facet_wrap(OBS_GEAR_CODE~FMP_SUBAREA,scales='free_y',ncol=2)
	dn<-dn+theme_bw(base_size=20)+labs(title="CPUE by number of fish",y="CPUE by number",x="Year")
	windows()
	print(dn)



	CATCH6<-CATCH5[,list(NCPUE_INDEX=sum(NCPUE_INDEX2),WCPUE_INDEX=sum(WCPUE_INDEX2),NStERR=sqrt(sum(NCPUE_INDEX_StERR2)),WStERR=sqrt(sum(WCPUE_INDEX_StERR2))),by=c("YEAR","FMP_SUBAREA")]


	d1<-ggplot(CATCH6,aes(x=YEAR,y=WCPUE_INDEX))+geom_line()+geom_errorbar(aes(ymin=WCPUE_INDEX-WStERR,ymax=WCPUE_INDEX+WStERR))+facet_wrap(~FMP_SUBAREA,scales='free')
	d1<-d1+theme_bw(base_size=20)+labs(title="CPUE by weight of fish",y="CPUE by weight",x="Year")
	windows()
	print(d1)

	d2<-ggplot(CATCH6,aes(x=YEAR,y=NCPUE_INDEX))+geom_line()+geom_errorbar(aes(ymin=NCPUE_INDEX-NStERR,ymax=NCPUE_INDEX+NStERR))+facet_wrap(~FMP_SUBAREA,scales='free')
	d2<-d2+theme_bw(base_size=20)+labs(title="CPUE by number of fish",y="CPUE by number",x="Year")
	windows()
	print(d2)


	output=list(CPUE_INDEX=CATCH6, GEAR_SPEC_INDEX=HAULS3.2 ,WEIGHT_Fig1=dw,NUMBER_FIG2=dn, WEIGHT_FIG2=d1,NUMBER_FIG2=d2)# , MONTH_GEAR_CPUE=HAUL3.2)
	return(output)
}



CPUE_COD<-CPUE_ALL(username="",password="",OBS_SP_CODE=202,TRIP_TAR_CODE="'C'",SPECIES_GRP_CODE="'PCOD'",area='GOA')



##
## For each gear and year:
##   1. For each month, weight the CPUE mean by that month's proportion of the gear- and year-specific catch.
##   2. Sum the month-specific, catch-weighted CPUE mean obtained in step #1 across months, except for the terminal year, where the sum for the year to date is adjusted by the sum of the "month effects" estimated in my previous method so as to produce a year-end equivalent.
##   3. Divide the summed monthly catch-weighted CPUE means obtained in step #2 by the average of the sum computed in step #2.
##   4. Repeat the above 3 steps for the squared CPUE standard error, except that the weighting in step #1 will be by the proportion squared, and the divisor in step #3 will be the square of the average of the sum for the catch-weighted CPUE means
## Then, for each year:
##   5. For each gear, weight the catch-weighted-and-scaled CPUE means obtained above by that gear's proportion of the year-specific catch.
##   6. Sum the gear-specific, doubly-catch-weighted-and-scaled CPUE means obtained in step #1 across gear.
##   7. Repeat the above 2 steps for the catch-weighted-and-scaled squared CPUE standard error, except that the weighting in step #5 will be by the proportion squared.

test <- paste0("SELECT SUM(COUNCIL.COMPREHENSIVE_BLEND_CA.WEIGHT_POSTED) AS TONS,
        COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_GEAR,
        COUNCIL.COMPREHENSIVE_BLEND_CA.AGENCY_GEAR_CODE,
        COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_AREA,
        COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA,
        COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR AS YEAR,
        COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE,
        COUNCIL.COMPREHENSIVE_BLEND_CA.VES_AKR_LENGTH,
        to_char(COUNCIL.COMPREHENSIVE_BLEND_CA.week_end_date,'mm') as MONTH
      FROM 
        COUNCIL.COMPREHENSIVE_BLEND_CA
      WHERE 
        COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR > 1990 
        AND COUNCIL.COMPREHENSIVE_BLEND_CA.TRIP_TARGET_CODE in (",TRIP_TAR_CODE,")
        AND COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE in (",SPECIES_GRP_CODE,")
      GROUP BY 
        COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_GEAR,
        COUNCIL.COMPREHENSIVE_BLEND_CA.AGENCY_GEAR_CODE,
        COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR,
        COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE,
        COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA,
        COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_AREA,
        COUNCIL.COMPREHENSIVE_BLEND_CA.VES_AKR_LENGTH,
        to_char(COUNCIL.COMPREHENSIVE_BLEND_CA.week_end_date,'mm')")


    CATCH<-data.table(sqlQuery(AKFIN,test))
    #odbcClose(AKFIN)
    CATCH<-CATCH[YEAR>=min(HAULS3.1$YEAR)]

    CATCH$OBS_GEAR_CODE<- 0
    CATCH[AGENCY_GEAR_CODE=='PTR']$OBS_GEAR_CODE <- 2
    CATCH[AGENCY_GEAR_CODE=='NPT']$OBS_GEAR_CODE <- 1
    CATCH[AGENCY_GEAR_CODE=='BTR']$OBS_GEAR_CODE <- 1
    CATCH[AGENCY_GEAR_CODE=='TRW']$OBS_GEAR_CODE <- 1
    CATCH[AGENCY_GEAR_CODE=='POT']$OBS_GEAR_CODE <- 6
    CATCH[AGENCY_GEAR_CODE=='HAL']$OBS_GEAR_CODE <- 8
    CATCH <- CATCH[OBS_GEAR_CODE>0]


    CATCH$VCLASS<-65	
	CATCH[VES_AKR_LENGTH>=65&VES_AKR_LENGTH<=125]$VCLASS<- 125
	CATCH[VES_AKR_LENGTH>125]$VCLASS<-150

 ABC<-read.csv("C:/WORKING_FOLDER/2021_PCOD_SEPTEMBER/catch.csv")

 x2<-CATCH[,list(TONS=sum(TONS)),by=c("YEAR","MONTH","FMP_AREA")]
 x2<-x2[FMP_AREA=="GOA"]
 x2<-x2[order(YEAR,MONTH,)]
 x2[,cumulative_sum := cumsum(TONS),by=.(YEAR)]
 x2=merge(x2,ABC)
 x2$CUMPROP<-x2$cumulative_sum/x2$ABC


ggplot(x2[YEAR>2014],aes(x=MONTH,y=CUMPROP,color=factor(YEAR)))+geom_line(size=1.25)+theme_bw(base_size=16)+labs(title="GOA Pacific cod catch",color="Year",y="Proportion of TAC",x="Month")+scale_x_continuous(breaks=seq(1,12,by=1),limits=c(1,12))
 

ggplot(x2[YEAR>2014],aes(x=MONTH,y=TONS,color=factor(YEAR)))+geom_line(size=1.25)+theme_bw(base_size=16)+labs(title="GOA Pacifi cod catch",color="Year",y="Catch (T)",x="Month")+scale_x_continuous(breaks=seq(1,12,by=1),limits=c(1,12))

ggplot(x2[YEAR>2014],aes(x=MONTH,y=cumulative_sum,color=factor(YEAR)))+geom_line(size=1.25)+theme_bw(base_size=16)+labs(title="GOA Pacific cod catch",color="Year",y="Cumulative Catch (T)",x="Month")+scale_x_continuous(breaks=seq(1,12,by=1),limits=c(1,12))


