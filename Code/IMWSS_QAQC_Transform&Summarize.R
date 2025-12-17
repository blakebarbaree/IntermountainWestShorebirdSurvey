###Intermountain West Shorebird Survey##
#Originally developed by Blake Barbaree, Point Blue Conservation Science, bbarbaree@pointblue.org

###QA/QC and Data Collation###
##After each survey season several steps to quality control the data entries

#First download and prep data
df<-IMWSS_AreaSearch_12162025

#The Data downloader tool on the Avian Knowledge Network has changed the csv format over time 
#Always crosscheck column numbers and names each time
#rename and define as characters to facilitate data transformations
df <- setNames(df,c("Project","Protocol","SamplingUnit","Visit","Date","StartTime","EndTime","BirdCount","Spp","CommonName",
                    "ScientificName","DetectionCueId","BreedingEvidence","Note","ObservationNote","Researcher","OtherObserversCount",
                    "OtherObserversNames","DataStatus"))
df$Project<-as.character(df$Project)
df$Protocol<-as.character(df$Protocol)
df$SamplingUnit<-as.character(df$SamplingUnit)
df$Date<-as.character(df$Date)
df$StartTime<-as.character(df$StartTime)
df$EndTime<-as.character(df$EndTime)
df$Spp<-as.character(df$Spp)
df$Note<-as.character(df$Note)
df$CommonName<-as.character(df$CommonName)
df$BirdCount<-as.numeric(df$BirdCount)
df$DetectionCueId<- NULL
df$BreedingEvidence<- NULL
df$Visit <- NULL
df$OtherObserversCount <- NULL
df$OtherObserversNames <- NULL
df$DataStatus <- NULL

##create Year and Season columns
df$Date<-as.Date(df$Date, format = "%Y/%m/%d") # this line may not be necessary - omit if already in this format
df$Year <- substring(df$Date,1,4)
df$Month <- substring(df$Date,6,7)
df$Season <- ifelse(df$Month == "03", "Spring",
                    ifelse(df$Month == "04", "Spring",
                            ifelse(df$Month == "05", "Spring",
                                   ifelse(df$Month == "08", "Fall",
                                          ifelse(df$Month == "09", "Fall", NA)))))
df$SeasonYear <- paste(df$Season,df$Year)
df$SeasonYear<-as.factor(df$SeasonYear)
df$Season<-as.factor(df$Season)
df$Year<-as.factor(df$Year)
df$Month<-as.factor(df$Month)
summary(df$Month)
summary(df$SeasonYear)##check to ensure no NAs


###Begin QA/QC process
#Run survey filter code to remove previously known dupes or unwanted surveys efore starting the process below.

##identify duplicate surveys or data entries - determine if dupes are errors or intentional 
#investigate all duplicate surveys for the most recent season using this process.
##Dupes are usually:
# 1) errors that require correction
# 2) counts of separate areas within the site (requiring aggregation for the IMWSS site total)
# 3) sites with >1 legitimate survey that season requires identifying the survey retained for IMWSS analyses and adding others to the Survye Filter list.

#create unique identifier for each survey at a sampling unit
df$ID <- paste(df$SamplingUnit,df$Date,df$Spp,sep="-") #dupe entries for a species on same day (often errors #1)
df$IDs <- paste(df$SamplingUnit,df$SeasonYear,df$Spp,sep="-") #identify sites with multiple surveys in a season

length(unique(df$ID)) #get number of unique observations
length(unique(df$ID)) == nrow(df) #if yes, no need for next step to identify dupes
length(unique(df$IDs)) #get number of unique observations
length(unique(df$IDs)) == nrow(df) #if yes, no need for next step to identify dupes

#identify duplicate observations
n_occur <- data.frame(table(df$ID)) # create a table with the observations
n_occur[n_occur$Freq > 1,] #find and list values with >1 occurrence

n_occurs <- data.frame(table(df$IDs)) # create a table with the observations
n_occurs[n_occurs$Freq > 1,] #find and list values with >1 occurrence

#identify outlier counts using Interquatile range method
Q1 <- quantile(df$BirdCount, 0.25) #calculate 1st and 3rd quartiles
Q3 <- quantile(df$BirdCount, 0.75)
IQR_value <- Q3 - Q1 #IQR range
lower_bound <- Q1 - 1.5 * IQR_value #Define the Outlier Fences - outliers defined as values that fall below Q1 - 1.5 * IQR or above Q3 + 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR_value
outliers <- df[df$BirdCount < lower_bound | df$BirdCount > upper_bound, ] #new df with outlier
outliers #list outliers values and sort by bird count to review numbers to ensure no erros and high counts only at large sites

### Restructuring data for analysis
wdf<-unique(df[,c("SamplingUnit","Date","Season","SeasonYear","State","Site","StartTime","EndTime","BirdCount","Spp","CommonName","Researcher","Note")]) 
nrow(wdf) == nrow(df)

#aggregate the counts for that species, survey unit, and date-time
#this combines multiple counts on the same day or season
tdf<-aggregate(as.formula("BirdCount~SamplingUnit+Spp+CommonName+Season+SeasonYear+State+Site"), data=wdf,FUN=sum) ##removed Date to aggregate by season, ensure survey filter code is up to date
nrow(tdf)

#transform from long to wide format for site/survey summaries
library(tidyr)
rdf<-pivot_wider(tdf, 
                 id_cols = c("SamplingUnit","Season","SeasonYear","State","Site"),
                 names_from = "Spp",
                 values_from = "BirdCount")
nrow(rdf)


#replace all NA's with 0's - only for species data NOTE that you can edit the concepts above (i.e, add more fields)
#and the results of this code will be unaffected.
tdf[is.na(tdf)] <- 0  ##zeros long format
rdf[is.na(rdf)] <- 0 ##zeros in wide format data

## create all shorebird column from IMW data spring 2023
# two ways to add the species into single total
shorebirds<-c('AMAV','AMGP','BASA','BBPL','BBSA','BNST','DOWI','DUNL','GRYE','HUGO','KILL','LARG','LBCU','LBDO','LESA','LEYE','MAGO','MEDI',
              'PAGP','PEEP','PESA','PHAL','PIPL','REKN','REPH','RNPH','RUFF','RUTU','SAND','SBDO','SEPL','SESA','SMAL','SNPL',
              'SOSA','SPSA','STSA','UNKN','UNPL','UNPP','UNSH','UPSA','USAN','WATA','WESA','WHIM','WILL','WIPH','WISN',
              'WLSA','WRSA','XCAL','XDOW','XMSS','XPHL','XPLU','XWLD','XWLS','XWSL','XWSS','XXSH','XYEL','YELL')
rdf$allshbd<-rowSums(rdf[,which(names(rdf) %in% shorebirds)]) 

df$Spp<-as.factor(df$Spp)
rdf$TOTAL <- rdf$AMAV+rdf$AMGP+rdf$BASA+rdf$BBPL+rdf$BBSA+rdf$BNST+rdf$DOWI+rdf$DUNL+rdf$GRYE+rdf$HUGO+
              rdf$KILL+rdf$LARG+rdf$LBCU+rdf$LBDO+rdf$LESA+rdf$LEYE+rdf$MAGO+rdf$MEDI+rdf$PAGP+rdf$PEEP+
              rdf$PESA+rdf$PHAL+rdf$PIPL+rdf$REKN+rdf$REPH+rdf$RNPH+rdf$RUFF+rdf$RUTU+rdf$SAND+rdf$SBDO+
              rdf$SEPL+rdf$SESA+rdf$SMAL+rdf$SNPL+rdf$SOSA+rdf$SPSA+rdf$STSA+rdf$UNDO+rdf$UNPL+rdf$UNPP+
              rdf$UNSH+rdf$UPSA+rdf$USAN+rdf$WATA+rdf$WESA+rdf$WHIM+rdf$WILL+rdf$WIPH+rdf$WISN+rdf$WLSA+rdf$CUSA+
              rdf$WRSA+rdf$XCAL+rdf$XDOW+rdf$XMSS+rdf$XNUM+rdf$XPHL+rdf$XPLU+rdf$XWLD+rdf$XWLS+rdf$XWRP+rdf$XWNG+
              rdf$XWSL+rdf$XWSS+rdf$XXSH+rdf$XYEL+rdf$YELL #this method more reliable but need to verify all species included


##convert to factors for summary as needed
rdf$Month <- as.factor(rdf$Month)
rdf$Year <- as.factor(rdf$Year)
rdf$Date<-as.factor(rdf$Date)

rdf$Season <- as.factor(rdf$Season)
rdf$SeasonYear <- as.factor(rdf$SeasonYear)
rdf$SamplingUnit <- as.factor(rdf$SamplingUnit)

df$Site <- as.factor(df$Site)
df$State <- as.factor(df$State)
df$SeasonYear <- as.factor(df$SeasonYear)

##optional - combine some species groupings and remove combined species
rdf$CALIDRIS <- rdf$PEEP + rdf$XCAL + rdf$UNPP + rdf$USAN + rdf$XWLD + rdf$XWLS + rdf$XWSL + rdf$XWSS + rdf$WCAL ## Calidris sp
rdf <- rdf[!(row.names(rdf) %in% c("XCAL","UNPP","USAN","XWLD","XWLS","XWSS","PEEP","WCAL")),] # remove rows for summary if not interested in dividing into species

rdf$DOWITCHER <- rdf$XDOW + rdf$DOWI + rdf$UNDO + rdf$LBDO + rdf$SBDO ## dowitcher sp
rdf <- rdf[!(row.names(rdf) %in% c("XDOW","DOWI","UNDO","LBDO","SBDO")),] # remove rows for summary if not interested in dividing into species

rdf$PHALAROPE <- rdf$XPHL + rdf$PHAL + rdf$XWRP ##phalarope spp
rdf <- rdf[!(row.names(rdf) %in% c("XPHL","PHAL","XWRP")),]

rdf$YELLOWLEGS <- rdf$XYEL + rdf$YELL  ##yellowlegs spp
rdf <- rdf[!(row.names(rdf) %in% c("XYEL","YELL")),]

rdf$PLOVER <- rdf$XPLU + rdf$UNPL  ##plover spp
rdf <- rdf[!(row.names(rdf) %in% c("XPLU","UNPL")),]

rdf$UNKSHBD <- rdf$UNSH + rdf$XXSH ## unknown shorebird
rdf <- rdf[!(row.names(rdf) %in% c("UNSH","XXSH")),]

rdf$UNKLARGE <- rdf$XWNG + rdf$LARG + rdf$XNUM ## unknown large including WHIM/LBCU/MAGO/WILL
rdf <- rdf[!(row.names(rdf) %in% c("XWNG","XNUM","LARG")),]

rdf$UNKMEDSMALL <- rdf$XMSS + rdf$SMAL + rdf$MEDI ## unknown medium small
rdf <- rdf[!(row.names(rdf) %in% c("XMSS","SMAL","MEDI")),]

rdf[is.na(rdf)] <- 0 ##zeros in wide format data

#make summary by state/site/season
dfsum <- aggregate(TOTAL ~ Site+State+Year+Season+SeasonYear, data = df, FUN = sum, na.rm = TRUE)
dfsum <- aggregate(TOTAL ~ Site+State+SeasonYear, data = rdf, FUN = sum, na.rm = TRUE)
dfsumall <- aggregate(cbind(TOTAL,CALIDRIS,DOWITCHER,PHALAROPE,YELLOWLEGS,PLOVER,UNKLARGE,UNKMEDSMALL,AMAV,AMGP,BASA,BBPL,
                         BBSA,BNST,DUNL,GRYE,HUGO,KILL,LBCU,LESA,LEYE,MAGO,PAGP,PESA,PIPL,REKN,REPH,RNPH,RUFF,RUTU,SAND,SEPL,SESA,
                         SOSA,SPSA,STSA,UPSA,WATA,WESA,WHIM,WILL,WIPH,WISN,CUSA) ~ Site+State+SeasonYear, data = rdf, FUN = sum, na.rm = FALSE)

# regional total shorebirds by season
aggregate(TOTAL ~ SeasonYear, data = rdf, FUN = sum, na.rm = TRUE)
aggregate(SNPL ~ SeasonYear, data = df, FUN = sum, na.rm = TRUE)

table(dfsum$SeasonYear) #number of sites per season

##number of unique survey sites
length(unique(df[["Site"]]))

##full summary with all shorebirds
dfsum <- aggregate(cbind(TOTAL, SNPL) ~ Site+State+SeasonYear, data = rdf, FUN = sum, na.rm = TRUE)

##write out in csv
write.csv(wdf,'IMWSS_allsurveys_17Dec2025.csv')
write.csv(tdf,'IMWSS_UnitSeasonTotals_17Dec2025.csv')
write.csv(dfsumall,'IMWSSdata_Dec2025_summary.csv')

