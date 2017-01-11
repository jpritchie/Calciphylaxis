##CALCIPHYLAXSIS / SKS-HD MERGING

#PACKAGES 
require(readr)
require(readxl)
require(dplyr)
require(MatchIt)

#WORKING DIRECTORY
setwd("~/Dropbox/Calciphylaxis/Calc_Matching")

#DATA IMPORT
#caliphlaxsis baseline data
Calciphylaxis230616 <- read_excel("~/Dropbox/Calciphylaxis/Calc_Matching/Calciphylaxis230616.xlsx", sheet = "Baseline")
#exclude patients needing clinical review for accuracy of recorded data
Calciphylaxis230616<-subset(Calciphylaxis230616, Calciphylaxis230616$StudyID!=84)
Calciphylaxis230616<-subset(Calciphylaxis230616, Calciphylaxis230616$StudyID!=30)
Calciphylaxis230616<-subset(Calciphylaxis230616, Calciphylaxis230616$StudyID!=45)
Calciphylaxis230616<-subset(Calciphylaxis230616, Calciphylaxis230616$StudyID!=49)
Calciphylaxis230616<-subset(Calciphylaxis230616, Calciphylaxis230616$StudyID!=54)


#sks-hd baseline data (note needed to resave as .xlsx)
CRISIS_20HD_20March_202015_20BASELINE <- read_excel("~/Dropbox/Calciphylaxis/Calc_Matching/CRISIS%20HD%20March%202015%20BASELINE.xlsx")

#SIMPLIFY DATAFRAMES FOR MATCHING - CALCIPHLAXSIS
Calc_Vars<-c("StudyID", "1b_Gender", "1c_Age", "1a_ConsentDate", "3_DateFirstTreatment")
Calciphylaxis<-Calciphylaxis230616[Calc_Vars]

#replace -1 coding for gender with NA
Calciphylaxis[Calciphylaxis == -1] <- NA

#remove incomplete cases
Calciphylaxis_complete<-Calciphylaxis[complete.cases(Calciphylaxis),]

#calculate dialysis vinatge in years
 #change to date format
Calciphylaxis_complete$Consent<-as.Date(Calciphylaxis_complete$`1a_ConsentDate`, "%d/%m/%Y")
Calciphylaxis_complete$Start<-as.Date(Calciphylaxis_complete$`3_DateFirstTreatment`, "%d/%m/%Y")
Calciphylaxis_complete$`1a_ConsentDate`<-NULL
Calciphylaxis_complete$`3_DateFirstTreatment`<-NULL
Calciphylaxis_complete$Vintage<-Calciphylaxis_complete$Consent-Calciphylaxis_complete$Start
Calciphylaxis_complete$Vintage<-as.numeric(Calciphylaxis_complete$Vintage)
Calciphylaxis_complete$Vintage<-round((Calciphylaxis_complete$Vintage/365.25),digits=2)
Calciphylaxis_complete$Consent<-NULL
Calciphylaxis_complete$Start<-NULL

#standardise names for dataframe rbind step 
names(Calciphylaxis_complete)[names(Calciphylaxis_complete)=="1b_Gender"]<-"Gender"
names(Calciphylaxis_complete)[names(Calciphylaxis_complete)=="1c_Age"]<-"Age"

#add study identifier column
Calciphylaxis_complete$STUDY<-"CALCIPHLAXIS"

#SIMPLIFY DATAFRAMES FOR MATCHING - CRISIS-HD
CRISIS_Vars<-c("StudyID", "SEX", "Age", "RRTvintage")
CRISIS<-CRISIS_20HD_20March_202015_20BASELINE[CRISIS_Vars]

#remove incomplete cases
CRISIS<-CRISIS[complete.cases(CRISIS),]

#standardise names for dataframe rbind step 
names(CRISIS)[names(CRISIS)=="SEX"]<-"Gender"
names(CRISIS)[names(CRISIS)=="Age"]<-"Age"
names(CRISIS)[names(CRISIS)=="RRTvintage"]<-"Vintage"

CRISIS$Vintage<-round(CRISIS$Vintage, digits=2)

#add study identifier column
CRISIS$STUDY<-"CRISIS-HD"


##RBIND THE DATAFRAMES TO CREATE ONE
Master<-rbind(Calciphylaxis_complete, CRISIS)

set.seed(1234)
Master$Group <- as.logical(Master$STUDY == "CALCIPHLAXIS")
#note 1:1 matching
jim.match<- matchit(Group ~ Age + Gender+Vintage, data = Master, method="nearest", ratio=1)

#save matched patients as a dataframe
Matched_IDs <- match.data(jim.match)[1:ncol(Master)]


##GENERATE AN ANALYSABLE DATAFRAME
#vector of ID's to subset from (need to think how to automate the row allocation here)
Cal_matched_ID<-Matched_IDs[1:53,1]
#then subset
Cal_matched<- Calciphylaxis230616[Calciphylaxis230616$StudyID %in% Cal_matched_ID, ]

#repeat for SKS
SKS_matched_ID<-Matched_IDs[54:106,1]
SKS_matched<- CRISIS_20HD_20March_202015_20BASELINE[CRISIS_20HD_20March_202015_20BASELINE$StudyID %in% SKS_matched_ID, ]

#calciphylaxsis DF renaming columns to allow rbind
Cal_matched$Male<-ifelse(Cal_matched$`1b_Gender`=="1", 1,0)
Cal_matched$Prev_PD<-ifelse(Cal_matched$`5_PreviousRR_PD`=="1",1,0)
Cal_matched$Prev_Tx<-ifelse(Cal_matched$`5_PreviousRR_Tx`=="1",1,0)
Cal_matched$MI<-ifelse(Cal_matched$`6_MI`=="1", 1, 0)
Cal_matched$Age<-as.numeric(as.character(Cal_matched$`1c_Age`))
Cal_matched$Diabetes<-ifelse(Cal_matched$`6_Diabetes`=="1", 1,0)
Cal_matched$PVD<-ifelse(Cal_matched$`6_PeripheralvascularDisease`=="1",1,0)
Cal_matched$CVD<-ifelse(Cal_matched$`6_Cerebrovascular`=="1",1,0)
Cal_matched$Warfarin<-ifelse(Cal_matched$`9_Calcimimetics_VitaminK`=="1",1,0)
Cal_matched$VitD<-ifelse(Cal_matched$`9_VitaminD`=="1",1,0)
Cal_matched$PO4_binder<-ifelse(Cal_matched$`9_PhosphateBinders`=="1",1,0)
Cal_matched$PTH_surgery<-ifelse(Cal_matched$`6_Parathyroid`=="1",1,0)
Cal_matched$Cinacalcet<-ifelse(Cal_matched$`9_Calcimimetics`=="1",1,0)
  #calculation of vintage
  Cal_matched$Consent<-as.Date(Cal_matched$`1a_ConsentDate`, "%d/%m/%Y")
  Cal_matched$Start<-as.Date(Cal_matched$`3_DateFirstTreatment`, "%d/%m/%Y")
  Cal_matched$Vintage<-Cal_matched$Consent-Cal_matched$Start
  Cal_matched$Vintage<-as.numeric(Cal_matched$Vintage)
  Cal_matched$Vintage<-round((Cal_matched$Vintage/365.25),digits=2)
Cal_matched$Group<-"Cal"
  
#SKS DF renaming columns to allow rbind
SKS_matched[is.na(SKS_matched)] <- 0 
SKS_matched$Male<-ifelse(SKS_matched$SEX=="1", 1,0)
SKS_matched$Prev_PD<-ifelse(SKS_matched$PD=="1",1,0)
SKS_matched$Prev_Tx<-ifelse(SKS_matched$RenalTransplant=="1", 1, 0)
SKS_matched$MI<-ifelse(SKS_matched$MIYN=="1",1,0) ##may need clinical review of these data
SKS_matched$Age<-round(SKS_matched$Age, digits=0)
SKS_matched$Diabetes<-ifelse(SKS_matched$DMYN=="1",1,0)
SKS_matched$PVD<-SKS_matched$PVD ##field already exists, documented here for balance / record
SKS_matched$CVD<-ifelse(SKS_matched$cvastroke=="1" | SKS_matched$tia=="1", 1,0)
SKS_matched$Warfarin<-ifelse(SKS_matched$warfarin=="1", 1,0)
SKS_matched$VitD<-ifelse(SKS_matched$Alphacalcidol=="1",1,0)
SKS_matched$PO4_binder<-ifelse(SKS_matched$Fosrenol=="1" | SKS_matched$Renagel=="1" | SKS_matched$Phosex=="1" | SKS_matched$Lanthanum=="1" | SKS_matched$sevelamer=="1" | SKS_matched$phsoslo=="1" | SKS_matched$Calciumcarbonate=="1" | SKS_matched$Calciumacetate=="1",1,0)
SKS_matched$PTH_surgery<-ifelse(SKS_matched$Parathyroidecotm=="1",1,0)
  #cinacalcet - stored as free text in meds columns (first is 139, there are 20. Range 139:159)
  #function to search by row for spelling varients
  SKS_matched$CIN<-apply(SKS_matched,1,function(x)sum(grepl("Cinacalcet",x)))
  SKS_matched$CIN2<-apply(SKS_matched,1,function(x)sum(grepl("cinacalcet",x)))
  SKS_matched$CIN3<-apply(SKS_matched,1,function(x)sum(grepl("Mimpara",x)))
  SKS_matched$CIN4<-apply(SKS_matched,1,function(x)sum(grepl("mimpara",x)))
SKS_matched$Cinacalcet<-ifelse(SKS_matched$CIN=="1" |SKS_matched$CIN2=="1" |SKS_matched$CIN3=="1" |SKS_matched$CIN4=="1",1,0)
SKS_matched$Vintage<-as.numeric(as.character(SKS_matched$RRTvintage))
SKS_matched$Group<-"SKS"

#list of columns to keep; then subset step
Keep_vars<-c("Group", "StudyID", "Male","Prev_PD", "Prev_Tx", "MI", "Age", "Diabetes", "PVD", "CVD", "Warfarin", "VitD", "PO4_binder", "PTH_surgery", "Vintage", "Cinacalcet" )

SKS_matched<-SKS_matched[Keep_vars]
Cal_matched<-Cal_matched[Keep_vars]

#rbind to single DF
Analysis_df<-rbind(Cal_matched, SKS_matched)

## will need a step to replace na with 0 values
Analysis_df[is.na(Analysis_df)] <- 0 