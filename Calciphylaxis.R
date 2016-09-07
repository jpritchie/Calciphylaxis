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

##### FIRST CLEAN BASELINE DATA SHEET  
  
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
  CAL_baseline$`3_DateFirstTreatment`<-as.Date(CAL_baseline$`3_DateFirstTreatment`, "%d/%m/%Y") #For analysis, presume this is first HD - need to confirm with Abby
  #Column 5_ not yet addressed as need to understand the coding
  CAL_baseline$`4_RenalStatus`<-ifelse(CAL_baseline$`4_RenalStatus`=="1", "CKD1", 
                                ifelse(CAL_baseline$`4_RenalStatus`=="2", "CKD2", 
                                ifelse(CAL_baseline$`4_RenalStatus`=="3", "CKD3a", 
                                ifelse(CAL_baseline$`4_RenalStatus`=="4", "CKD3b", 
                                ifelse(CAL_baseline$`4_RenalStatus`=="5", "CKD4", 
                                ifelse(CAL_baseline$`4_RenalStatus`=="6", "CKD5", 
                                ifelse(CAL_baseline$`4_RenalStatus`=="7", "Transplant", 
                                ifelse(CAL_baseline$`4_RenalStatus`=="8", "HD/HDF", 
                                ifelse(CAL_baseline$`4_RenalStatus`=="9", "PD", 
                                ifelse(CAL_baseline$`4_RenalStatus`=="10", "Non-CKD", NA))))))))))
  
  #Update binary co-morbidities to Yes / No for clarity of reading (columns 5 / 6)
  #function
  Comorbid_code<- function(x) {
                     x<-as.character(x)
                     x<-ifelse(x == "1", "Yes",ifelse(x =="2","No",  NA)) #<<- to return to global environment
                     x<<-as.factor(x)
  }
  
  #lapply over appropraite columns
  CAL_baseline[7:17]<-lapply(CAL_baseline[7:17], Comorbid_code)
  
  CAL_baseline$`6_Parathyroid_Date`<-as.Date(CAL_baseline$`6_Parathyroid_Date`, "%d/%m/%Y")
  CAL_baseline$`6_Reimplantation`<-NULL #Dropped presently for analysis as no key availalbe to understand coding
  
  #standardise iPTH and PTH units to pmol/L
  CAL_baseline$`7_iPTH`<-ifelse(CAL_baseline$`7_iPTH`>0 & CAL_baseline$`7_iPTH_Unit`=="ng/L", CAL_baseline$`7_iPTH`*0.106, CAL_baseline$`7_iPTH`)
  CAL_baseline$`7_iPTH`<-round(CAL_baseline$`7_iPTH`, digits=0)
  CAL_baseline$`7_iPTH_Unit`<-NULL
  CAL_baseline$`7_PTH`<-ifelse(CAL_baseline$`7_PTH`>0 & CAL_baseline$`7_PTH_Unit`=="ng/L", CAL_baseline$`7_PTH`*0.106, CAL_baseline$`7_PTH`)
  CAL_baseline$`7_PTH`<-round(CAL_baseline$`7_PTH`, digits=0)
  CAL_baseline$`7_PTH_Unit`<-NULL
  
  CAL_baseline$`7_CRP`<-round(CAL_baseline$`7_CRP`, digits=0)
  
  #standardise haemoglobin values to g/dL
  CAL_baseline$`7_Haemoglobin`<-ifelse(CAL_baseline$`7_Haemoglobin`>0 & CAL_baseline$`7_Haemoglobin_Unit`=="g/L" | CAL_baseline$`7_Haemoglobin`>0 & CAL_baseline$`7_Haemoglobin_Unit`=="g/l", CAL_baseline$`7_Haemoglobin`/10, CAL_baseline$`7_Haemoglobin`)
  CAL_baseline$`7_Haemoglobin_Unit`<-NULL
  
  #clean RRT data
  CAL_baseline$`8_Haemodialysis_Sessions`[CAL_baseline$`8_Haemodialysis_Sessions`=="0"]<-NA #remove 0 as integer fornumber of HD sessions - this has been entered for some transplant patients
  CAL_baseline$`8_Haemodialysis_KtV`<-NULL #significant missing data (58/71)
  CAL_baseline$`8_Haemodialysis_BloodUreaReductionRatio`[CAL_baseline$`8_Haemodialysis_BloodUreaReductionRatio`=="N/A"]<-NA #correctly label na values 
  CAL_baseline$`8_Haemodialysis_BloodUreaReductionRatio`<-gsub("%", "", CAL_baseline$`8_Haemodialysis_BloodUreaReductionRatio`)
  CAL_baseline$`8_Haemodialysis_BloodUreaReductionRatio`<-as.numeric(as.character(CAL_baseline$`8_Haemodialysis_BloodUreaReductionRatio`))
  CAL_baseline$`8_Haemodialysis_BloodUreaReductionRatio`<-round(CAL_baseline$`8_Haemodialysis_BloodUreaReductionRatio`, digits=0)
    #remove PD data as only 4 patients hence uninformative for detailed analysis
    CAL_baseline$`8_PeritonealDialysis_KtV`<-NULL
    CAL_baseline$`8_PeritonealDialysis_KtV`<-NULL
    CAL_baseline$`8_PeritonealDialysis_Therapy`<-NULL
    CAL_baseline$`8_PeritonealDialysis_Creatinine`<-NULL
    CAL_baseline$`8_PeritonealDialysis_DialysisVolume`<-NULL
    
  #Vitamin d
  CAL_baseline[34:35]<-lapply(CAL_baseline[35:35], Comorbid_code)
  CAL_baseline$`9_DrugName`<-ifelse(CAL_baseline$`9_DrugName`=="alfacalcidol" 
                                    |CAL_baseline$`9_DrugName`=="alpha calcidol" 
                                    | CAL_baseline$`9_DrugName`=="alphacacidol" 
                                    | CAL_baseline$`9_DrugName`=="alphacalcidol" 
                                    | CAL_baseline$`9_DrugName`=="one alpha calcidol" 
                                    | CAL_baseline$`9_DrugName`=="onealphacalcidol" 
                                    |CAL_baseline$`9_DrugName`=="Alphacalcidol", "Alphacalcidol", 
                                    ifelse(CAL_baseline$`9_DrugName`=="calcitriol"
                                    | CAL_baseline$`9_DrugName`=="Calcitrol", "Calcitriol", 
                                    ifelse(CAL_baseline$`9_DrugName`=="Colecalciferol"
                                    |CAL_baseline$`9_DrugName`=="Colecalciferol 800", "Colecalciferol", 
                                    ifelse(CAL_baseline$`9_DrugName`=="paricalcitol", "Paricalcitol", NA))))
  CAL_baseline$`9_Dose`<-NULL #inconsistent; needs review before use in analysis
  CAL_baseline$`9_Unit`<-NULL #dependent on above
  CAL_baseline$`9_Frequency`<-NULL #dependent on above
  CAL_baseline$`9_Duration`<-NULL #needs discussion with study leads
  
  #make PO4 binder binary
  CAL_baseline[37]<-lapply(CAL_baseline[37], Comorbid_code)
  #huge spread of binder names - simplify to Number of binders / Ca containing binder
  CAL_baseline$`9_PhosphateBinders_DrugName1`<-as.numeric(as.character(CAL_baseline$`9_PhosphateBinders_DrugName1`))
  CAL_baseline$`9_PhosphateBinders_DrugName2`<-as.numeric(as.character(CAL_baseline$`9_PhosphateBinders_DrugName2`))
  CAL_baseline$`9_PhosphateBinders_DrugName3`<-as.numeric(as.character(CAL_baseline$`9_PhosphateBinders_DrugName3`))
    #drop redundent data for analysis
    CAL_baseline$`9_PhosphateBinder_Dose1`<-NULL
    CAL_baseline$`9_PhosphateBinder_Dose2`<-NULL
    CAL_baseline$`9_PhosphateBinder_Dose3`<-NULL
    CAL_baseline$`9_PhosphateBinder_Unit1`<-NULL
    CAL_baseline$`9_PhosphateBinder_Unit2`<-NULL
    CAL_baseline$`9_PhosphateBinder_Unit3`<-NULL
    CAL_baseline$`9_PhosphateBinder_Duration1`<-NULL
    CAL_baseline$`9_PhosphateBinder_Duration2`<-NULL
    CAL_baseline$`9_PhosphateBinder_Duration3`<-NULL
    CAL_baseline$`9_PhosphateBinder_Frequency1`<-NULL
    CAL_baseline$`9_PhosphateBinder_Frequency2`<-NULL
    CAL_baseline$`9_PhosphateBinder_Frequency3`<-NULL
    CAL_baseline$`9_PhosphateBinders_DrugName1_Other`<-NULL
    CAL_baseline$`9_PhosphateBinders_DrugName2_Other`<-NULL
    CAL_baseline$`9_PhosphateBinders_DrugName3_Other`<-NULL
  
    CAL_baseline$`9_Binder_Number`<-ifelse(CAL_baseline$`9_PhosphateBinders_DrugName1`>0 
                                           & CAL_baseline$`9_PhosphateBinders_DrugName2`>0 
                                           & CAL_baseline$`9_PhosphateBinders_DrugName3`>0, 3, 
                                           ifelse(CAL_baseline$`9_PhosphateBinders_DrugName1`>0 
                                           & CAL_baseline$`9_PhosphateBinders_DrugName2`>0 
                                           & CAL_baseline$`9_PhosphateBinders_DrugName3`<0, 2, 
                                           ifelse(CAL_baseline$`9_PhosphateBinders_DrugName1`>0 
                                           & CAL_baseline$`9_PhosphateBinders_DrugName2`<0 
                                           & CAL_baseline$`9_PhosphateBinders_DrugName3`<0, 1, 0)))
    
    CAL_baseline$`9_Phosphate_Binder_Calcium`<-ifelse(CAL_baseline$`9_PhosphateBinders_DrugName1`=="4"
                                                      |CAL_baseline$`9_PhosphateBinders_DrugName1`=="5"
                                                      |CAL_baseline$`9_PhosphateBinders_DrugName1`=="7"
                                                      |CAL_baseline$`9_PhosphateBinders_DrugName1`=="8"
                                                      |CAL_baseline$`9_PhosphateBinders_DrugName2`=="4"
                                                      |CAL_baseline$`9_PhosphateBinders_DrugName2`=="5"
                                                      |CAL_baseline$`9_PhosphateBinders_DrugName2`=="7"
                                                      |CAL_baseline$`9_PhosphateBinders_DrugName2`=="8"
                                                      |CAL_baseline$`9_PhosphateBinders_DrugName3`=="4"
                                                      |CAL_baseline$`9_PhosphateBinders_DrugName3`=="5"
                                                      |CAL_baseline$`9_PhosphateBinders_DrugName3`=="7"
                                                      |CAL_baseline$`9_PhosphateBinders_DrugName3`=="8", "Yes", "No")
    
    CAL_baseline$`9_PhosphateBinders_DrugName1`<-NULL
    CAL_baseline$`9_PhosphateBinders_DrugName2`<-NULL
    CAL_baseline$`9_PhosphateBinders_DrugName3`<-NULL
    #move these new columns into the appropriate position
    CAL_baseline<-CAL_baseline[,c(1:37,74,75,38:73)]
    #calcimimetics - relabel and drop additional info
    CAL_baseline[40]<-lapply(CAL_baseline[40], Comorbid_code)
      CAL_baseline$`9_Calcimimetics_DrugName`<-NULL
      CAL_baseline$`9_Calcimimetics_Unit`<-NULL
      CAL_baseline$`9_Calcimimetics_Dose`<-NULL
      CAL_baseline$`9_Calcimimetics_Frequency`<-NULL
      CAL_baseline$`9_Calcimimetics_Duration`<-NULL
   CAL_baseline[41]<-lapply(CAL_baseline[41], Comorbid_code)
   CAL_baseline[43:44]<-lapply(CAL_baseline[43:44], Comorbid_code)
   CAL_baseline$`9_Calcimimetics_VitaminK_Indication`<-NULL #almost all for AF
   
   #baseline diagnosis of calciphlaxis details
   CAL_baseline$`10_Date`<-as.Date(CAL_baseline$`10_Date`, "%d/%m/%Y")
   CAL_baseline[46]<-lapply(CAL_baseline[46], Comorbid_code)
   CAL_baseline$`10_ContributingEvent`<-NULL
   CAL_baseline$`10_ContributingEventIfYes`<-NULL #12 contributing events recorded, 4 trauma, 8 other. Limited use in analysis at present
   CAL_baseline[46:51]<-lapply(CAL_baseline[46:51], Comorbid_code)
   CAL_baseline$`10_DiagnosisMadeBy_Other`<-NULL #not used at this time
   CAL_baseline[52:61]<-lapply(CAL_baseline[52:61], Comorbid_code)
   CAL_baseline$`10_LocationOfLesions_Other`<-NULL
   CAL_baseline[62]<-lapply(CAL_baseline[62], Comorbid_code)
   
   #drop final three biochemical columns due to missing data
   CAL_baseline$`10_Phosphate`<-NULL
   CAL_baseline$`10_MaximumSerum`<-NULL
   CAL_baseline$`10_PTH`<-NULL 
 
###########################################################################################################   
     
###NOW CLEAN THE FOLLOW UP DATA SHEET
   CAL_followup$Date<-as.Date(CAL_followup$Date, "%d/%m/%Y")
   CAL_followup$LocalContactEmail<-NULL
   
   #format dates (these are now dropped as minimal difference from actual follow up date)
   CAL_followup$`1_Creatinine_Date`<-as.Date(CAL_followup$`1_Creatinine_Date`, "%d/%m/%Y")
   CAL_followup$`1_Calcium_Date`<-as.Date(CAL_followup$`1_Calcium_Date`, "%d/%m/%Y")
   CAL_followup$`1_CorrectedCalcium_Date`<-as.Date(CAL_followup$`1_CorrectedCalcium_Date`, "%d/%m/%Y")
   CAL_followup$`1_PTH_Date`<-as.Date(CAL_followup$`1_PTH_Date`, "%d/%m/%Y")
   CAL_followup$`1_iPTH_Date`<-as.Date(CAL_followup$`1_iPTH_Date`, "%d/%m/%Y")
   CAL_followup$`1_TotalProtein_Date`<-as.Date(CAL_followup$`1_TotalProtein_Date`, "%d/%m/%Y")
   CAL_followup$`1_Albumin_Date`<-as.Date(CAL_followup$`1_Albumin_Date`, "%d/%m/%Y")
   CAL_followup$`1_AlkalinePhosphatise_Date`<-as.Date(CAL_followup$`1_AlkalinePhosphatise_Date`, "%d/%m/%Y")
   CAL_followup$`1_CRP_Date`<-as.Date(CAL_followup$`1_CRP_Date`, "%d/%m/%Y")
   CAL_followup$`1_Haemoglobin_Date`<-as.Date(CAL_followup$`1_Haemoglobin_Date`, "%d/%m/%Y")
   CAL_followup$`1_Phosphate_Date`<-as.Date(CAL_followup$`1_Phosphate_Date`, "%d/%m/%Y")
   
     CAL_followup$`1_Creatinine_Date`<-NULL
     CAL_followup$`1_Calcium_Date`<-NULL
     CAL_followup$`1_CorrectedCalcium_Date`<-NULL
     CAL_followup$`1_PTH_Date`<-NULL
     CAL_followup$`1_iPTH_Date`<-NULL
     CAL_followup$`1_TotalProtein_Date`<-NULL
     CAL_followup$`1_Albumin_Date`<-NULL
     CAL_followup$`1_AlkalinePhosphatise_Date`<-NULL
     CAL_followup$`1_CRP_Date`<-NULL
     CAL_followup$`1_Haemoglobin_Date`<-NULL
     CAL_followup$`1_Phosphate_Date`<-NULL
     
    CAL_followup$`2_LocationOfLesions_Other`<-NULL
    CAL_followup$`3_MedicalInterventions_DialysisModalityDescribe`<-NULL
    CAL_followup$`3_MedicalInterventionsBisphosphonates_Name`<-NULL
    CAL_followup$`3_MedicalInterventionsBisphosphonates_Dose`<-NULL
    CAL_followup$`3_MedicalInterventionsBisphosphonates_Route`<-NULL
    CAL_followup$`3_MedicalInterventiosnBisphosphonates_Length`<-NULL
    CAL_followup$`3_MedicalInterventionsHyperbaricOxygen_Prescription`<-NULL
    CAL_followup$`3_MedicalInterventiosnSodiumThiosulphate_Length`<-NULL
    CAL_followup$`3_MedicalInterventionsSodiumThiosulphate_Dose`<-NULL
    
    CAL_followup[15:41]<-lapply(CAL_followup[15:41], Comorbid_code)
    
    CAL_followup$`4_ParathyroidectomyDate`<-as.Date(CAL_followup$`4_ParathyroidectomyDate`, "%d/%m/%Y")
    
    CAL_followup$`4_Other`<-NULL
    
    CAL_followup[43:59]<-lapply(CAL_followup[43:59], Comorbid_code)
    
    CAL_followup$`7_SkinLesionsResolveDate`<-as.Date(CAL_followup$`7_SkinLesionsResolveDate`, "%d/%m/%Y")
    
    #end points - clean up death information
    CAL_followup$`8_Death`[CAL_followup$`8_Death`=="N/A"]<-NA #remove 0 as integer fornumber of HD sessions - this has been entered for some transplant patients
    CAL_followup$`8_Death`[CAL_followup$`8_Death`=="N/A pt still alive"]<-NA #remove 0 as integer fornumber of HD sessions - this has been entered for some transplant patients
    CAL_followup$`8_Death`[CAL_followup$`8_Death`=="unknown"]<-NA #remove 0 as integer fornumber of HD sessions - this has been entered for some transplant patients
    CAL_followup$`8_Death`<-ifelse(is.na(CAL_followup$`8_Death`), 0,1)
    
      #placeholder update as ID 84 is RIP and not properly recorded in provided data
      CAL_followup$`8_Death`<-ifelse(CAL_followup$StudyID=="84",1,CAL_followup$`8_Death`)
    
    CAL_followup[62:63]<-lapply(CAL_followup[62:63], Comorbid_code)
    
    CAL_followup$`11_Comments`<-NULL 