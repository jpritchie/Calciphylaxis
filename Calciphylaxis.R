#SET WORKING DIRECTORY
setwd("~/Dropbox/Calciphylaxis")

#DEFINE PACKAGES
require(readxl)
require(dplyr)
require(tidyr)
require(ggplot2)

#IMPORT DATA
  CAL_baseline<-read_excel("Calciphylaxis.xlsx",sheet=1)
  CAL_followup<-read_excel("Calciphylaxis.xlsx",sheet=2)

#ASSIGN DATA CHARACTERISITCS / LABEL CORRECTLY
  #baseline
  CAL_baseline$LocalContactEmail<-NULL #Not useful to analysis
  CAL_baseline$Date<-NULL #missing data and appears largely to duplicate consent date column
  CAL_baseline$`1_Consent`<-NULL #Not useful to analysis
  CAL_baseline$`1a_ConsentDate`<-as.Date(CAL_baseline$`1a_ConsentDate`, "%d/%m/%Y")
  CAL_baseline$`1b_Gender`<-ifelse(CAL_baseline$`1b_Gender`=="1","Male","Female")
  CAL_baseline$`1d_Weight`<-NULL #38/71 missing
  CAL_baseline$`1e_BMI`<-NULL #39/71 missing
  CAL_baseline$`2_Race`<-NULL #Awaiting key from Abby; only 4 are not coded as '1' so likely uninformative for analysis
  CAL_baseline$`3_DateFirstTreatment`<-as.Date(CAL_baseline$`3_DateFirstTreatment`, "%D/%m/%Y") #For analysis, presume this is first HD - need to confirm with Abby
  #Columns 4_ to 5_ are not yet addressed as need to understand the coding
  
  
  #Draft attempt at function / lapply code to run ifelse statement over multiple columns to recode e.g. comorbidity
  
  Comorbid_code <- function(df, x) {
    df$x<-ifelse(df$x=="1", "Yes", "No")
    return(result)
  }
  