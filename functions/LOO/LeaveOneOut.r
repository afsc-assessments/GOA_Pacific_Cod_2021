 SS_doLOO<-function (masterdir, oldsubdir, newsubdir = "LeaveOneOut", 
    subdirstart = "LOO", years = 0:-10, overwrite = TRUE, 
    exefile = "ss", extras = "-nox", intern = FALSE, 
    CallType = "system", RemoveBlocks = FALSE) 
{
    require(data.table)
    require(r4ss)

    OS <- .Platform[["OS.type"]]
    prefix <- ifelse(OS == "windows", "", "./")
    oldwd <- getwd()
    on.exit(setwd(oldwd))
    olddir <- file.path(masterdir, oldsubdir)
    newdir <- file.path(masterdir, newsubdir)
    if (!is.null(exefile) & OS == "windows") {
        exefiles <- dir(olddir)[grep(".exe", dir(olddir))]
        if (length(exefiles) == 1) {
            exefile <- exefiles
        }
        if (exefile == "ss") {
            exefile <- "ss.exe"
        }
        if (!exefile %in% exefiles) {
            stop("Missing executable file ", exefile, " in ", 
                olddir)
        }
    }
    
    startfile1   <- dir(olddir)[tolower(dir(olddir)) == "starter.ss"]
    forefile    <- dir(olddir)[tolower(dir(olddir)) == "forecast.ss"]
    wtatagefile <- dir(olddir)[tolower(dir(olddir)) == "wtatage.ss"]
    testfile    <- dir(olddir)[tolower(dir(olddir)) == "test.ss"]
    parfile    <- dir(olddir)[tolower(dir(olddir)) == "ss.par"]

    if (length(startfile1) == 0) 
        stop("No starter.ss file found in ", olddir)
    startfile <- file.path(olddir, startfile1)
    message("Getting input file names from starter file:\n", 
        startfile)
    starter <- SS_readstarter(startfile, verbose = FALSE)
    ctlfile <- starter[["ctlfile"]]
    datfile <- starter[["datfile"]]
    datfileget<-paste0(olddir,"/",datfile)

    datafile<-SS_readdat(file=datfileget)

    filenames <- c(exefile, forefile, ctlfile, startfile1, wtatagefile, parfile,
        testfile)
    message("copying model files from\n", olddir, "\n  to\n", 
        newdir)
    message("model files to copy:\n ", paste(filenames, 
        collapse = "\n "))
    if (!file.exists(newdir)) 
        dir.create(newdir)
    subdirnames <- paste0(subdirstart, years)

    for (iyr in 1:length(years)) {
        if (!file.exists(file.path(newdir, subdirnames[iyr]))) {
            dir.create(file.path(newdir, subdirnames[iyr]))
        }
        copy.test <- file.copy(file.path(olddir, filenames), 
            file.path(newdir, subdirnames[iyr], filenames), overwrite = TRUE)
        if (!all(copy.test)) {
            stop("error copying file(s): ", filenames[!copy.test])
        }
        
       # starter[["retro_yr"]] <- years[iyr]
       # starter[["init_values_src"]] <- 0

            CPUE<-data.table(datafile$CPUE)
            agecomp<-data.table(datafile$agecomp)
            lencomp<-data.table(datafile$lencomp)

        datafile2<-datafile

        yr <- year(Sys.Date())+years[iyr]

        
        CPUE[year==yr & index>0]$index    <- CPUE[year==yr & index>0]$index*-1
        agecomp[Yr==yr & FltSvy>0]$FltSvy <- agecomp[Yr==yr & FltSvy>0]$FltSvy*-1
        lencomp[Yr==yr & FltSvy>0]$FltSvy <- lencomp[Yr==yr & FltSvy>0]$FltSvy*-1

        datafile2$CPUE    <- data.frame(CPUE)
        datafile2$agecomp <- data.frame(agecomp)
        datafile2$lencomp  <- data.frame(lencomp)

        setwd(file.path(newdir, subdirnames[iyr]))

        SS_writedat(datafile2, outfile= datfile, verbose = FALSE,overwrite=TRUE)

        ctl <- readLines(ctlfile)
        if (RemoveBlocks == TRUE) {
            ctl[grep("block designs", ctl)] <- "0 # Number of block designs for time varying parameters"
            ctl[grep("blocks per design", ctl) + 0:2] <- "# blocks deleted"
        }
        file.remove(ctlfile)
        writeLines(ctl, ctlfile)
        if (length(grep(" ", exefile)) > 0) {
            exefile_to_run <- paste0("\"", exefile, "\"")
        }
        else {
            exefile_to_run <- exefile
        }
        command <- paste0(prefix, exefile_to_run, " ", 
            extras)
        message("Running model in ", getwd(), "\n", 
            "using the command:\n   ", command, sep = "")
        if (file.exists("covar.sso")) 
            file.remove("covar.sso")
        if (intern) {
            message("ADMB output generated during model run will be written to:\n   ", 
                getwd(), "/ADMBoutput.txt. \n   To change this, set intern=FALSE\n", 
                "Note: ignore message about 'Error trying to open data input file ss3.dat'\n", 
                sep = "")
        }
        if (CallType == "system") {
            ADMBoutput <- system(command, intern = intern)
        }
        if (CallType == "shell") {
            ADMBoutput <- shell(command, intern = intern)
        }
        if (!file.exists("Report.sso")) {
            warning("The LOO model run failed in ", 
                getwd())
        }
        if (intern) {
            writeLines(c("###", "ADMB output", as.character(Sys.time()), 
                "###", " ", ADMBoutput), con = "ADMBoutput.txt")
        }
        setwd("..")
    }
    setwd(oldwd)
}



SS_doLOO(masterdir=getwd(), oldsubdir="Model21.1e", newsubdir = "Model21.1e_LOO", subdirstart = "LOO", years = 0:-10)

SS_doLOO(masterdir=getwd(), oldsubdir="Model21.5a", newsubdir = "Model21.5a_LOO", subdirstart = "LOO", years = 0:-10)

SS_doLOO(masterdir=getwd(), oldsubdir="Model19.1", newsubdir = "Model19.1_LOO", subdirstart = "LOO", years = 0:-10)

SS_doLOO(masterdir=getwd(), oldsubdir="Model21.1c", newsubdir = "Model21.1c_LOO", subdirstart = "LOO", years = 0:-10)






mods=c("LOO0","LOO-1","LOO-2","LOO-3","LOO-4","LOO-5","LOO-6","LOO-7","LOO-8","LOO-9","LOO-10")
mods1<-SSgetoutput(dirvec=mods)
#modsS<-SSsummarize(mods1)
#SStableComparisons(modsS)
#SSplotComparisons(modsS,legendlabels=mods,densitynames = c("SSB_unfished","SSB_2022","ForeCatch_2022"))

#dome=array()
#Nat_M_env<-array()
Nat_M<-array()
Q<-array()
SSB_UN<-array()
annF_Btgt<-array()
Nat_M_SD<-array()
Q_SD<-array()
SSB_UN_SD<-array()
annF_Btgt_SD<-array()
SSB2022<-array()
SSB2022_SD<-array()
ABC2022<-array()
ABC2022_SD<-array()

for(i in 1:11){
    x               <- data.table(mods1[[i]]$parameters)
    y               <- data.table(mods1[[i]]$derived_quants)
    annF_Btgt[i]    <-y[Label=='annF_Btgt']$Value
    annF_Btgt_SD[i] <-y[Label=='annF_Btgt']$StdDev
    Nat_M[i]        <-x[Label=="NatM_uniform_Fem_GP_1"]$Value
    Nat_M_SD[i]     <-x[Label=="NatM_uniform_Fem_GP_1"]$Parm_StDev
    Q[i]            <-x[Label%in%"LnQ_base_Srv(4)"]$Value
    Q_SD[i]         <-x[Label%in%"LnQ_base_Srv(4)"]$Parm_StDev
    SSB_UN[i]       <-y[Label=='SSB_unfished']$Value
    SSB_UN_SD[i]    <-y[Label=='SSB_unfished']$StdDev
    SSB2022[i]      <-y[Label=='SSB_2022']$Value
    SSB2022_SD[i]   <-y[Label=='SSB_2022']$StdDev
    ABC2022[i]      <-y[Label=='ForeCatch_2022']$Value
    ABC2022_SD[i]   <-y[Label=='ForeCatch_2022']$StdDev

    }

#mods0<-SSgetoutput(dirvec="C:/WORKING_FOLDER/2021 Stock Assessments/2021 Pacific cod/Models/Model19.1")

mods0<-SSgetoutput(dirvec="C:/WORKING_FOLDER/2021 Stock Assessments/2021 Pacific cod/Models/Model19.1_2021M")

#mods0<-SSgetoutput(dirvec="C:/WORKING_FOLDER/2021 Stock Assessments/2021 Pacific cod/Models/Model21.1e")



    x0              <- data.table(mods0[[1]]$parameters)
    y0              <- data.table(mods0[[1]]$derived_quants)
    annF_Btgt0   <-y0[Label=='annF_Btgt']$Value
    annF_Btgt_SD0 <-y0[Label=='annF_Btgt']$StdDev
    Nat_M0       <-x0[Label=="NatM_uniform_Fem_GP_1"]$Value
    Nat_M_SD0     <-x0[Label=="NatM_uniform_Fem_GP_1"]$Parm_StDev
    Q0           <-x0[Label%in%"LnQ_base_Srv(4)"]$Value
    Q_SD0         <-x0[Label%in%"LnQ_base_Srv(4)"]$Parm_StDev
    SSB_UN0      <-y0[Label=='SSB_unfished']$Value
    SSB_UN_SD0    <-y0[Label=='SSB_unfished']$StdDev
    SSB20220      <-y0[Label=='SSB_2022']$Value
    SSB2022_SD0    <-y0[Label=='SSB_2022']$StdDev
    ABC20220      <-y0[Label=='ForeCatch_2022']$Value
    ABC2022_SD0   <-y0[Label=='ForeCatch_2022']$StdDev

x0<-data.table(LOO=0,Nat_M=Nat_M0, Nat_M_SD=Nat_M_SD0, Nat_M_CV=Nat_M_SD0/Nat_M0, annF_Btgt=annF_Btgt0, annF_Btgt_SD=annF_Btgt_SD0,annF_Btgt_CV=annF_Btgt_SD0/annF_Btgt0,
 Q=exp(Q0), Q_SD=Q_SD0, SSB_UN=SSB_UN0,SSB_UN_SD=SSB_UN_SD0,SSB_UN_CV=SSB_UN_SD0/SSB_UN0,SSB2022=SSB20220,SSB2022_SD=SSB2022_SD0,SSB2022_CV=SSB2022_SD0/SSB20220,ABC2022=ABC20220,ABC2022_SD=ABC2022_SD0,ABC2022_CV=ABC2022_SD0/ABC20220)


x1<-data.table(LOO=c(1:11),Nat_M=Nat_M, Nat_M_SD=Nat_M_SD, Nat_M_CV=Nat_M_SD/Nat_M, annF_Btgt=annF_Btgt, annF_Btgt_SD=annF_Btgt_SD,annF_Btgt_CV=annF_Btgt_SD/annF_Btgt,
 Q=exp(Q), Q_SD=Q_SD, SSB_UN=SSB_UN,SSB_UN_SD=SSB_UN_SD,SSB_UN_CV=SSB_UN_SD/SSB_UN,SSB2022=SSB2022,SSB2022_SD=SSB2022_SD,SSB2022_CV=SSB2022_SD/SSB2022, ABC2022=ABC2022,ABC2022_SD=ABC2022_SD,ABC2022_CV=ABC2022_SD/ABC2022)

x<-rbind(x0,x1)

x2<-melt(x,"LOO")

x3<-x2[!variable%like%"_SD"]
x3<-x3[!variable%like%"_CV"]
x4<-x2[variable%like%"_SD"]
x3$SD <- x4$value

x3$Year<-2022-x3$LOO

LOO19.1<-x3
LOO19.1$Model<-"Model 19.1"
LOO21.1<-x3
LOO21.1$Model<-"Model 21.1"
LOO21.2<-x3
LOO21.2$Model<-"Model 21.2"

x3<-rbind(LOO19.1,LOO21.1,LOO21.2)

d<-ggplot(x3[LOO!=0],aes(x=Year,y=value))+geom_errorbar(aes(ymin=value-1.96*SD, ymax=value+1.96*SD), width=.25)+geom_point(size=3)+geom_hline(data=x3[LOO==0],aes(yintercept=value),size=1.25,linetype=2,color="red")+theme_bw(base_size=16)+labs(x='Leave one out year',y='Parameter value')+facet_wrap(variable~Model,scales="free_y",ncol=3)

d19.1<-ggplot(LOO19.1[LOO!=0],aes(x=Year,y=value))+geom_errorbar(aes(ymin=value-1.96*SD, ymax=value+1.96*SD), width=.25)+geom_point(size=3)+geom_hline(data=LOO19.1[LOO==0],aes(yintercept=value),size=1.25,linetype=2,color="red")+theme_bw(base_size=16)+labs(x='Leave one out year',y='Parameter value',title="Model 19.1")+facet_wrap(~variable,scales="free_y",ncol=1)
d21.1<-ggplot(LOO21.1[LOO!=0],aes(x=Year,y=value))+geom_errorbar(aes(ymin=value-1.96*SD, ymax=value+1.96*SD), width=.25)+geom_point(size=3)+geom_hline(data=LOO21.1[LOO==0],aes(yintercept=value),size=1.25,linetype=2,color="red")+theme_bw(base_size=16)+labs(x='Leave one out year',y='Parameter value',title="Model 21.1")+facet_wrap(~variable,scales="free_y",ncol=1)
d21.2<-ggplot(LOO21.2[LOO!=0],aes(x=Year,y=value))+geom_errorbar(aes(ymin=value-1.96*SD, ymax=value+1.96*SD), width=.25)+geom_point(size=3)+geom_hline(data=LOO21.2[LOO==0],aes(yintercept=value),size=1.25,linetype=2,color="red")+theme_bw(base_size=16)+labs(x='Leave one out year',y='Parameter value',title="Model 21.2")+facet_wrap(~variable,scales="free_y",ncol=1)



xt<-x3[,list(MV=mean(value),SDV=sd(value)),by=c("variable","Model")]
x5<-merge(x3,xt)
x5$V1<-(x5$value-x5$MV)/x5$SDV
x5$V1UCI<-(x5$UCI-x5$MV)/x5$SDV
x5$V1LCI<-(x5$LCI-x5$MV)/x5$SDV

d<-ggplot(x5[LOO!=0],aes(x=Year,y=V1))+geom_errorbar(aes(ymin=V1LCI, ymax=V1UCI), width=.25)+geom_point(size=3)+geom_hline(aes(yintercept=0),size=1.25,linetype=2,color="red")+theme_bw(base_size=16)+labs(x='Leave one out year',y='Standardized values')+facet_wrap(variable~Model,ncol=3)










x3     <-data.table(Nat_M=x$Nat_M,annF_Btgt=x$annF_Btgt,Q=x$Q,SSB_UN=x$SSB_UN,SSB2022=x$SSB2022,ABC2022=x$ABC2022 )
x4     <-x3[1,]
x4     <-data.table(Nat_M=rep(x4$Nat_M,12),annF_Btgt=rep(x4$annF_Btgt,12),Q=rep(x4$Q,12),SSB_UN=rep(x4$SSB_UN,12),SSB2022=rep(x4$SSB2022,12),ABC2022=rep(x4$ABC2022,12))
x4     <-x3[2:12,]-x4[2:12,]
x4$LOO <-c(1:11)
x4     <-melt(x4,'LOO')

BIAS   <-x4[,list(mean(value)),by='variable']
BIAS$P <-BIAS$V1/t(x3[1,])
BIAS$RMSE <- x4[,list(mean(value^2)),by='variable']$V1

DEV  <- x2[LOO>0][,list(MN=mean(value),SD=sd(value)),by='variable']

DEV$CV = DEV$SD/DEV$MN
