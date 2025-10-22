###Intermountain West Shorebird Survey##
#Originally developed by Blake Barbaree, Point Blue Conservation Science, bbarbaree@pointblue.org

###QA/QC and Data Collation###
##After each survey season several steps to quality control the data entries

#First download and prep data
df<-IMWSS_27June2025_AreaSearch

#There are multiple versions of the Data downloader tool - this code relates to the version access via Prject Leader on the AKN
#crosscheck column numbers each time
# again in June 2025 column names are different but no spaces when downloaded, so ignored this column
colnames(df)[2] <- "SamplingUnit" ## change names with spaces for processing
colnames(df)[5] <- "Date" ## change names with spaces for processing
colnames(df)[6] <- "StartTime" ## change names with spaces for processing
colnames(df)[7] <- "EndTime" ## change names with spaces for processing
colnames(df)[10] <- "CommonName" ## change names with spaces for processing
colnames(df)[14] <- "Notes" ## change names with spaces for processing

#define as characters to facilitate data transformations
df$Project<-as.character(df$ProjectId)
df$Protocol<-as.character(df$ProtocolId)
df$SamplingUnit<-as.character(df$SamplingUnitName)
df$Date<-as.character(df$EventDate)
df$StartTime<-as.character(df$EventStartTime)
df$EndTime<-as.character(df$EventEndTime)
df$Spp<-as.character(df$BirdCd)
df$Notes<-as.character(df$EventNote)
df$CommonName<-as.character(df$CommonName)

###Begin QA/QC process
#list and identify unusual or potentially erroneous species entries
unique(df$Spp)

##identify duplicate entries as data entry errors or intentional if two separate surveyors covered the same unit 
#create unique identifier for each survey at a sampling unit
df$ID <- paste(df$SamplingUnit,df$Date,df$Spp,sep="-") #dupe entries for a species on same day
#identify and correct/delete entries as needed
length(unique(df$ID)) #get number of unique observations
length(unique(df$ID)) == nrow(df) #if yes, no need for next step to identify dupes

#identify duplicate observations
n_occur <- data.frame(table(df$ID)) # create a table with the observations
n_occur[n_occur$Freq > 1,] #find and list values with >1 occurrence

#identify outlier counts using Interquatile range method
Q1 <- quantile(df$BirdCount, 0.25) #calculate 1st and 3rd quartiles
Q3 <- quantile(df$BirdCount, 0.75)
IQR_value <- Q3 - Q1 #IQR range
lower_bound <- Q1 - 1.5 * IQR_value #Define the Outlier Fences - outliers defined as values that fall below Q1 - 1.5 * IQR or above Q3 + 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR_value
outliers <- df[df$BirdCount < lower_bound | df$BirdCount > upper_bound, ] #new df with outlier
outliers #list outliers values

### Restructuring data for analysis and identify erroneous entries
wdf<-unique(df[,c("SamplingUnit","Date","StartTime","EndTime","BirdCount","Spp","CommonName","Researcher","Notes")]) 

nrow(wdf)

#aggregate the counts for that species, survey unit, and date-time
tdf<-aggregate(as.formula("BirdCount~SamplingUnit+Date+StartTime+EndTime+Spp+CommonName"), data=df,FUN=sum) 
nrow(tdf)

library(tidyr)
rdf<-pivot_wider(tdf, 
                 id_cols = c("SamplingUnit", "Date", "StartTime", "EndTime"),
                 names_from = "Spp",
                 values_from = "BirdCount")

nrow(rdf)


#replace all NA's with 0's - only for species data NOTE that you can edit the concepts above (i.e, add more fields)
#and the results of this code will be unaffected.
res.df[is.na(res.df)] <- 0  ##zeros long format
rdf[is.na(rdf)] <- 0 ##zeros in wide format data

## create all shorebird column from IMW data spring 2023
# two ways to add the species into single total
shorebirds<-c('AMAV','AMGP','BASA','BBPL','BBSA','BNST','DOWI','DUNL','GRYE','HUGO','KILL','LARG','LBCU','LBDO','LESA','LEYE','MAGO','MEDI',
              'PAGP','PEEP','PESA','PHAL','PIPL','REKN','REPH','RNPH','RUFF','RUTU','SAND','SBDO','SEPL','SESA','SMAL','SNPL',
              'SOSA','SPSA','STSA','UNKN','UNPL','UNPP','UNSH','UPSA','USAN','WATA','WESA','WHIM','WILL','WIPH','WISN',
              'WLSA','WRSA','XCAL','XDOW','XMSS','XPHL','XPLU','XWLD','XWLS','XWSL','XWSS','XXSH','XYEL','YELL')
rdf$allshbd<-rowSums(rdf[,which(names(rdf) %in% shorebirds)]) 

rdf$TOTAL <- rdf$AMAV+rdf$AMGP+rdf$BASA+rdf$BBPL+rdf$BBSA+rdf$BNST+rdf$DOWI+rdf$DUNL+rdf$GRYE+rdf$HUGO+
  rdf$KILL+rdf$LARG+rdf$LBCU+rdf$LBDO+rdf$LESA+rdf$LEYE+rdf$MAGO+rdf$MEDI+rdf$PAGP+rdf$PEEP+
  rdf$PESA+rdf$PHAL+rdf$PIPL+rdf$REKN+rdf$REPH+rdf$RNPH+rdf$RUFF+rdf$RUTU+rdf$SAND+rdf$SBDO+
  rdf$SEPL+rdf$SESA+rdf$SMAL+rdf$SNPL+rdf$SOSA+rdf$SPSA+rdf$STSA+rdf$UNDO+rdf$UNPL+rdf$UNPP+
  rdf$UNSH+rdf$UPSA+rdf$USAN+rdf$WATA+rdf$WESA+rdf$WHIM+rdf$WILL+rdf$WIPH+rdf$WISN+rdf$WLSA+
  rdf$WRSA+rdf$XCAL+rdf$XDOW+rdf$XMSS+rdf$XNUM+rdf$XPHL+rdf$XPLU+rdf$XWLD+rdf$XWLS+rdf$XWRP+rdf$XWNG+
  rdf$XWSL+rdf$XWSS+rdf$XXSH+rdf$XYEL+rdf$YELL #this method more reliable but need to verify all species included

##combine some species groupings and remove combined species
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


##create Year and Season columns
rdf$Date<-as.Date(rdf$Date, format = "%m/%d/%Y")
rdf$Year <- substring(rdf$Date,1,4) 
rdf$Month <- substring(rdf$Date,6,7) 
rdf$Season <- ifelse(rdf$Month == "03", "Spring",
                     ifelse(rdf$Month == "04", "Spring",
                            ifelse(rdf$Month == "05", "Spring",       
                                   ifelse(rdf$Month == "08", "Fall",
                                          ifelse(rdf$Month == "09", "Fall", NA)))))
rdf$SeasonYear <- paste(rdf$Season,rdf$Year)

##convert to factors for summary
rdf$Month <- as.factor(rdf$Month)
rdf$Year <- as.factor(rdf$Year)
rdf$Season <- as.factor(rdf$Season)
rdf$SeasonYear <- as.factor(rdf$SeasonYear)
rdf$SamplingUnit <- as.factor(rdf$SamplingUnit)
rdf$Date<-as.factor(rdf$Date)
