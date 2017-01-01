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
