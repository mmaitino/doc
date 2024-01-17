#Setting up R to run analysis
library(Amelia)
library(foreign)
library(lme4)

climate<-read.csv("ClimateChangeAttendanceFPA.csv", header=TRUE)

#Using Amelia to impute the values: note if you do this step and do not use our attached imputed values, you will get a different set of results because the imputed values will be slighlty different every time.
nocol<-c(1,3,5:7,9:11,15:17,21,25,32:34,36:37,40,46, 50:52, 54:55,58)
climateImpute<-climate[,-nocol]
a.outRR1218 <- amelia(climateImpute, m = 5, ts = "Year", cs= "CountryCode", ords=c("Polity2","envcpia.2"),noms=c("AOSISMembers","OPECDummy","UNSC","WBEB","NoExec","RightExec","LeftExec","CDMDonor","CDMHost", "G20","GEFFundsDummy","EnvMinistry","GEFCouncil","EU","Annex1Dummy"))
write.amelia(a.outRR1218, separate = TRUE, file.stem="ClimateImputationsRR1218")
save(a.out2RR1218, file = "imputationsclimateSplitEnv.RData")

climatedat1<-a.outRR1218$imputations[[1]]
climatedat2<-a.outRR1218$imputations[[2]]
climatedat3<-a.outRR1218$imputations[[3]]
climatedat4<-a.outRR1218$imputations[[4]]
climatedat5<-a.outRR1218$imputations[[5]]