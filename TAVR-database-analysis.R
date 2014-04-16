#### R script for TAVR database analysis ####
#### Complied by Hanghang Wang, April 8th 2014 ####

## Install analysis packages
install.packages("gmodels")
library(gmodels)
install.packages("MASS")
library(MASS)

## Save your Excel file into a csv file, delete the MRN column (often causes problems in reading)

## Read data in csv format into data frame ##
fulldata <- read.table(file="/Users/username/foldername/filename.csv", header=T, sep=",")

## Save data as an R object ##
save(fulldata,file="/Users/username/foldername/filename.Robj")

## Load R object ##
load("/Users/username/foldername/filename.Robj")

## Check how your variables are classified ##
str(fulldata)

## Variable re-classifying ##
fulldata$Date.of.Surgery <- as.Date(fulldata$Date.of.Surgery, format="%m/%d/%Y") # Convert Date of Surgery to dates

fulldata$dob <- as.Date(fulldata$dob, format="%m/%d/%Y") # Convert Date of birth to dates

fulldata$Date.of.Discharge <- as.Date(fulldata$Date.of.Discharge, format="%m/%d/%Y") # Convert Date of Discharge to dates

fulldata$po_echo_date <- as.Date(fulldata$po_echo_date, format="%m/%d/%Y") # Convert postop ECHO date to dates

fulldata$dod <- as.Date(fulldata$dod, format="%m/%d/%Y") # Convert date of death to dates

## Variable re-naming##
colnames(fulldata)[colnames(fulldata)=="prev_vsr"] <- "Previous.valve-sparing.root"

colnames(fulldata)[colnames(fulldata)=="prev_arr"] <- "Previous.root.replacement"

colnames(fulldata)[colnames(fulldata)=="prev_streno"] <- "Previous.sternotomy"

colnames(fulldata)[colnames(fulldata)=="bep_ava"] <- "Baseline.ECHO.aortic.valve.area"

colnames(fulldata)[colnames(fulldata)=="a_ic"] <- "Iliac conduit"
# You can rename variables as you like, using the format: colnames(dataframe name)[colnames(dataframe name)== "original variable name"] <- "new name" 

## Re-classify variables ##
# Reclassify paravalvular leak as none, 1+ or 2+ (add > 2+ if needed)
fulldata$Paravalvular.leak<- factor(fulldata$Paravalvular.leak..post.TAVI., labels=c("None", "1+", "2+"))

# Reclassify postop AR as none, trace, mild (add severe if needed)
fulldata$aor_regurg_1mo <- factor(fulldata$aor_regurg_1mo, labels=c("None", "trace", "mild", "moderate"))

fulldata$aor_regurg_6mo <- factor(fulldata$aor_regurg_6mo, labels=c("None", "trace", "mild", "moderate"))

fulldata$aor_regurg_12mo <- factor(fulldata$aor_regurg_12mo, labels=c("None", "trace", "mild", "moderate"))

# Create device variable, assuming no missing data #
n = nrow(fulldata) # number of rows - cases
for (i in 1:n){
  if (fulldata$Edward.Sapien[i]==777){
    fulldata$device[i] = "CoreValve"
  }
}
for (i in 1:n){
  if (fulldata$Medtronic.CoreValue[i]==777){
    fulldata$device[i] = "Sapien"
  }
}


# Recode preop permanent pacemaker as missing data so that they won't be included in analyses #
for (i in 1:n){
  if (fulldata$Permanent.pacemaker[i]==888){
    fulldata$Permanent.pacemaker[i] = NA
  }
}

## Summary of continuous variables: min, max, median, mean, 1st qu, 3rd qu for each variable ##
summary(fulldata)

## Chi-squared test of categorical variables by device type ##
# Post-dilation
CrossTable(fulldata$Post.dilatation, fulldata$device,digits=3, prop.r=FALSE,prop.t=FALSE,chisq=TRUE,fisher=TRUE,format=c("SAS")) 

# Pacemaker rate
CrossTable(fulldata$Permanent.pacemaker, fulldata$device, prop.r=FALSE,prop.t=FALSE,chisq=TRUE,fisher=TRUE,format=c("SAS"))

# Paravalvular leak 
# Introp
CrossTable(fulldata$Paravalvular.leak, fulldata$device,digits=3, prop.r=FALSE,prop.t=FALSE,chisq=TRUE,fisher=TRUE,format=c("SAS"))

# Pre-discharge
CrossTable(fulldata$po_ar, fulldata$device,digits=3, prop.r=FALSE,prop.t=FALSE,chisq=TRUE,fisher=TRUE,format=c("SAS"))

# 1 mo
CrossTable(fulldata$aor_regurg_1mo, fulldata$device,digits=3, prop.r=FALSE,prop.t=FALSE,chisq=TRUE,fisher=TRUE,format=c("SAS"))

# 6 mo
CrossTable(fulldata$aor_regurg_6mo, fulldata$device,digits=3, prop.r=FALSE,prop.t=FALSE,chisq=TRUE,fisher=TRUE,format=c("SAS"))

# 12 mo
CrossTable(fulldata$aor_regurg_12mo, fulldata$device,digits=3, prop.r=FALSE,prop.t=FALSE,chisq=TRUE,fisher=TRUE,format=c("SAS"))





