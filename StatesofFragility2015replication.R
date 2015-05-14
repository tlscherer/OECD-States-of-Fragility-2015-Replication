############################
# Replication of the States of Fragility 2015 report by the OECD
# Available at http://www.oecd.org/publications/states-of-fragility-2015-9789264227699-en.htm
#
# By Thomas Leo Scherer
#
# Last Updated 5/6/2015
#
# Note: this script was run with R version 3.2.0.
# Runing an earlier version may change the results, due to issues with the countrycode package.
#
############################


#### Load Packages ####
#install.packages("WDI")
#install.packages("ggplot2")
#install.packages("xlsx")
#install.packages("countrycode")
library(WDI)
library(xlsx)
library(ggplot2)
library(countrycode)


#### BattleDeaths #####
load(url("http://www.pcr.uu.se/digitalAssets/124/124934_1ucdpbattle-relateddeathsdatasetv.5-2014dyadic.rdata"))
# set year 2012
BattleDeaths <- ucdpBRDDyadic[ucdpBRDDyadic$Year == 2012,]
# if Best estiamte is missing use low estimate
BattleDeaths$BdBest[BattleDeaths$BdBest<0] <- BattleDeaths$BdLow[BattleDeaths$BdBest<0] 
# Take care of countries with multiple locations 
BattleDeaths$CountryCount<-stringr::str_count(as.character(BattleDeaths$LocationInc),",")
ToSplit<-subset(BattleDeaths,CountryCount>0) 
Conflictsub<-subset(BattleDeaths,CountryCount==0) # no need to split
if(nrow(ToSplit)+nrow(Conflictsub)!=nrow(BattleDeaths)) warning("DANGER DANGER")
if(sum(BattleDeaths$CountryCount)>0){
  for(i in 1:nrow(ToSplit)){
    Splits<-ToSplit[rep(rownames(ToSplit[i,]),ToSplit$CountryCount[i]+1),]
    Splits$LocationInc<-stringr::str_split(ToSplit$LocationInc[i], ", ")[[1]]
    Conflictsub<-rbind(Conflictsub, Splits)
  }
}
BattleDeaths<-Conflictsub[,c("LocationInc", "BdBest")]
# aggregate by country
BattleDeaths <-aggregate(BattleDeaths[,c("BdBest")], by=list(BattleDeaths$LocationInc), FUN=sum, na.rm=TRUE)
# column names
names(BattleDeaths)<-c("LocationInc", "BattleDeaths")
# fix countries
BattleDeaths$Country <- countrycode(BattleDeaths$LocationInc, "country.name", "country.name")
if(sum(BattleDeaths$Country=="Sudan")>1) warning("Error, South Sudan turned to Sudan. You likely need to update R to latest version.")
BattleDeaths <- BattleDeaths[c("Country", "BattleDeaths")]
# had to change Yemen Arab Republic to Yemen
BattleDeaths$Country[BattleDeaths$Country=="Yemen Arab Republic"]<-"Yemen"



#### BattleDeaths2013 #####
load(url("http://www.pcr.uu.se/digitalAssets/124/124934_1ucdpbattle-relateddeathsdatasetv.5-2014dyadic.rdata"))
# set year 2013
BattleDeaths2013 <- ucdpBRDDyadic[ucdpBRDDyadic$Year == 2013,]
# if Best estiamte is missing use low estimate
BattleDeaths2013$BdBest[BattleDeaths2013$BdBest<0] <- BattleDeaths2013$BdLow[BattleDeaths2013$BdBest<0] 
# Take care of countries with multiple locations 
BattleDeaths2013$CountryCount<-stringr::str_count(as.character(BattleDeaths2013$LocationInc),",")
ToSplit<-subset(BattleDeaths2013,CountryCount>0) 
Conflictsub<-subset(BattleDeaths2013,CountryCount==0) # no need to split
if(nrow(ToSplit)+nrow(Conflictsub)!=nrow(BattleDeaths2013)) warning("DANGER DANGER")
if(sum(BattleDeaths2013$CountryCount)>0){
  for(i in 1:nrow(ToSplit)){
    Splits<-ToSplit[rep(rownames(ToSplit[i,]),ToSplit$CountryCount[i]+1),]
    Splits$LocationInc<-stringr::str_split(ToSplit$LocationInc[i], ", ")[[1]]
    Conflictsub<-rbind(Conflictsub, Splits)
  }
}
BattleDeaths2013<-Conflictsub[,c("LocationInc", "BdBest")]
# aggregate by country
BattleDeaths2013 <-aggregate(BattleDeaths2013[,c("BdBest")], by=list(BattleDeaths2013$LocationInc), FUN=sum, na.rm=TRUE)
# column names
names(BattleDeaths2013)<-c("LocationInc", "BattleDeaths")
# fix countries
BattleDeaths2013$Country <- countrycode(BattleDeaths2013$LocationInc, "country.name", "country.name")
if(sum(BattleDeaths2013$Country=="Sudan")>1) warning("Error, South Sudan turned to Sudan. You likely need to update R to latest version.")
BattleDeaths2013 <- BattleDeaths2013[c("Country", "BattleDeaths")]
# had to change Yemen Arab Republic to Yemen
BattleDeaths2013$Country[BattleDeaths2013$Country=="Yemen Arab Republic"]<-"Yemen"


#### InterpersonalInjuries ####
# Download and save as csv
GHE_DALY_2012_country <- read.csv("GHE_DALY_2012_country.csv")
InterpersonalInjuries<-t(as.data.frame(rbind(GHE_DALY_2012_country[6,8:179], GHE_DALY_2012_country[9,8:179], GHE_DALY_2012_country[156,8:179])))
# set row/ and column names
rownames(InterpersonalInjuries) <- NULL
colnames(InterpersonalInjuries)<-c("Country", "Population", "rawInterpersonalInjuries")
# formatting
InterpersonalInjuries<-as.data.frame(InterpersonalInjuries)
InterpersonalInjuries$InterpersonalInjuries <- as.numeric(as.character(InterpersonalInjuries$rawInterpersonalInjuries))
# fix countries
InterpersonalInjuries$Country<-as.character(InterpersonalInjuries$Country)
InterpersonalInjuries$Country <- countrycode(InterpersonalInjuries$Country, "country.name", "country.name")
InterpersonalInjuries<-InterpersonalInjuries[,c("Country", "InterpersonalInjuries")]


#### PoliticalInstability (wgi estimates) ####
# formatting
wgiestimates <- as.data.frame(as.matrix(read.csv("wgiestimates2012.csv")))
wgiestimates[wgiestimates=="#N/A"]<-NA
wgiestimates$PolStability<-as.numeric(as.character(wgiestimates$PolStability))
wgiestimates$Voice<-as.numeric(as.character(wgiestimates$Voice))
wgiestimates$GovEffect<-as.numeric(as.character(wgiestimates$GovEffect))
wgiestimates$RegQuality<-as.numeric(as.character(wgiestimates$RegQuality))
wgiestimates$RuleofLaw<-as.numeric(as.character(wgiestimates$RuleofLaw))
wgiestimates$Corruption<-as.numeric(as.character(wgiestimates$Corruption))
# fix country
wgiestimates$Country <- countrycode(wgiestimates$Country.Territory, "country.name", "country.name")
wgiestimates <- wgiestimates[!is.na(wgiestimates$Country),]
# select all relevant datasets
wgiestimates <- wgiestimates[,c("Country", "PolStability", "Voice", "GovEffect", "RegQuality", "RuleofLaw", "Corruption")]

#### PoliticalInstability 2013 ####
# formatting
wgiestimates2013 <- as.data.frame(as.matrix(read.csv("wgiestimates.csv")))
wgiestimates2013[wgiestimates2013=="#N/A"]<-NA
wgiestimates2013$PolStability<-as.numeric(as.character(wgiestimates2013$PolStability))
wgiestimates2013$Voice<-as.numeric(as.character(wgiestimates2013$Voice))
wgiestimates2013$GovEffect<-as.numeric(as.character(wgiestimates2013$GovEffect))
wgiestimates2013$RegQuality<-as.numeric(as.character(wgiestimates2013$RegQuality))
wgiestimates2013$RuleofLaw<-as.numeric(as.character(wgiestimates2013$RuleofLaw))
wgiestimates2013$Corruption<-as.numeric(as.character(wgiestimates2013$Corruption))
# fix country
wgiestimates2013$Country <- countrycode(wgiestimates2013$Country.Territory, "country.name", "country.name")
wgiestimates2013 <- wgiestimates2013[!is.na(wgiestimates2013$Country),]
# select all relevant datasets
wgiestimates2013 <- wgiestimates2013[,c("Country", "PolStability", "Voice", "GovEffect", "RegQuality", "RuleofLaw", "Corruption")]


#### BirthRegistration ####
#load data
Birth_registration_Oct2014 <- as.data.frame(read.csv("Birth_registration_Oct2014_135.csv")[5:201,c(1:2,22)])
# change row and column names
rownames(Birth_registration_Oct2014)<-NULL
names(Birth_registration_Oct2014) <- c("RawCountry", "BirthRegistration", "ReferenceYear")
# change birth registration into usable form and deal with missing variables
Birth_registration_Oct2014$BirthRegistration <- as.character(Birth_registration_Oct2014$BirthRegistration)
Birth_registration_Oct2014$BirthRegistration[Birth_registration_Oct2014$ReferenceYear==""]<-NA
Birth_registration_Oct2014$BirthRegistration <- round(as.numeric(as.character(Birth_registration_Oct2014$BirthRegistration)), digits = 1)
# set country names
Birth_registration_Oct2014$RawCountry <- as.character(Birth_registration_Oct2014$RawCountry)
Birth_registration_Oct2014$RawCountry[Birth_registration_Oct2014$RawCountry=="Democratic People's Republic of Korea"] <- "North Korea"
Birth_registration_Oct2014$Country <- countrycode(Birth_registration_Oct2014$RawCountry, "country.name", "country.name")
Birth_registration_Oct2014 <- Birth_registration_Oct2014[,c("Country", "BirthRegistration")]


#### ControlofCorruption ####
# in wgi estimates, see PoliticalInstability

#### RuleofLaw ####
# in wgi estimates, see PoliticalInstability

#### GovermentEffectiveness ####
# in wgi estimates, see PoliticalInstability

#### RegulatoryQuality ####
# in wgi estimates, see PoliticalInstability

#### VoiceandAccountability ####
# in wgi estimates, see PoliticalInstability


#### DoingBusinessIndex ####
DoingBusiness <- WDI(country = "all", indicator = "IC.BUS.EASE.XQ", start = 2013, end = 2013, extra = FALSE, cache = NULL)
# fix countries
DoingBusiness$Country <- countrycode(DoingBusiness$country, "country.name", "country.name")
DoingBusiness <- DoingBusiness[!is.na(DoingBusiness$Country),]
DoingBusiness <- DoingBusiness[!(DoingBusiness$iso2c %in% c("ZG", "ZF", "EU")),]
DoingBusiness <- DoingBusiness[,c("Country", "IC.BUS.EASE.XQ")]


#### DoingBusinessIndex 2014 ####
DoingBusiness2014 <- WDI(country = "all", indicator = "IC.BUS.EASE.XQ", start = 2014, end = 2014, extra = FALSE, cache = NULL)
# fix countries
DoingBusiness2014$Country <- countrycode(DoingBusiness2014$country, "country.name", "country.name")
DoingBusiness2014 <- DoingBusiness2014[!is.na(DoingBusiness2014$Country),]
DoingBusiness2014 <- DoingBusiness2014[!(DoingBusiness2014$iso2c %in% c("ZG", "ZF", "EU")),]
DoingBusiness2014 <- DoingBusiness2014[,c("Country", "IC.BUS.EASE.XQ")]


#### EducationYears ####
BarroLee2013 <- WDI(country = "all", indicator = "BAR.PRM.SCHL.15UP", start = 2010, end = 2010, extra = FALSE, cache = NULL)[28:241,]
# fix countries
BarroLee2013$Country <- countrycode(BarroLee2013$country, "country.name", "country.name")
EducationYears <- BarroLee2013[!(is.na(BarroLee2013$Country)),c("BAR.PRM.SCHL.15UP", "Country")]
names(EducationYears) <- c("BLeducationyears", "Country")


#### GDPGrowth, 10-year-average ####
GDPGrowthRaw <- read.csv("WEOOct2014all.csv", stringsAsFactors=FALSE)
# isolate variable
GDPGrowthRaw <- GDPGrowthRaw[GDPGrowthRaw$WEO.Subject.Code=="NGDP_RPCH",]
GDPGrowthRaw[GDPGrowthRaw=="n/a"]<-NA
# fix country
GDPGrowthRaw$Country <- countrycode(GDPGrowthRaw$Country, "country.name", "country.name")
# identify column of most recent real data for each country (up to 2013)
GDPGrowthRaw$aggyear<-GDPGrowthRaw$Estimates.Start.After-1970
GDPGrowthRaw$aggyear[GDPGrowthRaw$Country=="Bhutan"]<-43
# get average
GDPGrowthRaw$tenyearavg<-NA
for(i in 1:nrow(GDPGrowthRaw)){
  GDPGrowthRaw$tenyearavg[i] <- mean(as.numeric(as.character(GDPGrowthRaw[i,(GDPGrowthRaw$aggyear[i]-10):GDPGrowthRaw$aggyear[i]])), na.rm=T)
}
GDPGrowth2013<-GDPGrowthRaw[,c("Country", "tenyearavg")]
# identify column of most recent real data for each country (up to 2012)
GDPGrowthRaw$aggyear[GDPGrowthRaw$aggyear==43]<-42
# get average
GDPGrowthRaw$tenyearavg<-NA
for(i in 1:nrow(GDPGrowthRaw)){
  GDPGrowthRaw$tenyearavg[i] <- mean(as.numeric(as.character(GDPGrowthRaw[i,(GDPGrowthRaw$aggyear[i]-10):GDPGrowthRaw$aggyear[i]])), na.rm=T)
}
GDPGrowth<-GDPGrowthRaw[,c("Country", "tenyearavg")]


#### IncomeInequality ####
# WDIsearch("gini")
WDIginiraw <- WDI(country = "all", indicator = "SI.POV.GINI"  , start = 1980, end = 2020, extra = FALSE, cache = NULL)
# identify most recent year with a Gini value for each country
WDIgini <- merge(aggregate(year ~ country, WDIginiraw[is.na(WDIginiraw$SI.POV.GINI)==0,], max), WDIginiraw[is.na(WDIginiraw$SI.POV.GINI)==0,])
# set countries
WDIgini$Country <- countrycode(WDIgini$country, "country.name", "country.name")
WDIgini <- WDIgini[, c("SI.POV.GINI", "Country")]


#### HealthcareCapabilities ####
healthcarecapabilities <- read.csv("healthcarecapabilities.csv")
# fix countries
healthcarecapabilities$Country <- countrycode(healthcarecapabilities$Country, "country.name", "country.name")
healthcarecapabilities <- healthcarecapabilities[,c("Country", "Health.Care.Capabilities")]


#### Vulnerability ####
# retrieved from http://www.ehs.unu.edu/file/get/11895.pdf
worldriskindex <- read.csv("worldriskindex.csv")
worldriskindex$Country <- as.character(worldriskindex$Country)
# fix countries
worldriskindex$Country[worldriskindex$Country=="Dom. Republic "] <- "Dominican Republic"
worldriskindex$Country <- countrycode(worldriskindex$Country, "country.name", "country.name")
worldriskindex <- worldriskindex[!is.na(worldriskindex$Country),]
# formatting
worldriskindex$WorldRiskIndex <- as.numeric(as.character(worldriskindex$WorldRiskIndex))
worldriskindex <- worldriskindex[,c("Country", "WorldRiskIndex")]


#### Merge ####
# Select which year of datasets you will use. 
## These are the datasets with multiple options:
## BattleDeaths / BattleDeaths2013      # WGIestimates / WGIestimates2013
## DoingBusiness / DoingBusiness2014    # GDPGrowth / GDPGrowth2013
list.of.data.frames = list(BattleDeaths2013, InterpersonalInjuries, wgiestimates2013, Birth_registration_Oct2014, DoingBusiness2014, EducationYears, GDPGrowth2013, WDIgini, healthcarecapabilities, worldriskindex)
rawmerge = Reduce(function(...) merge(..., by="Country", all=T), list.of.data.frames)
if(sum(rawmerge$Country=="Sudan", na.rm=TRUE)>1) warning("Error, South Sudan turned to Sudan. You likely need to update R to latest version.")

dataraw<-rawmerge
# Assume battle death NA means 0 battle deaths
write.csv(dataraw, "unscaled_inputs.csv")

dataraw$BattleDeaths[is.na(dataraw$BattleDeaths)] <- 0

#### Drop countries missing over 10 inputs ####
dataraw$NACount <- apply(dataraw, 1, function(x) length(x[is.na(x)]))
table(dataraw$NACount)
dataraw <- dataraw[dataraw$NACount < 11,]
# try with only using complete data
#data <- data[data$NACount < 1,]

#### Drop small countries and territories ####
# drop countries that would be identified as fragile if included and are either [A] are smaller than KIRIBATI or [B] part of a larger country (ie Martinique) 
smallcountries <- c("American Samoa","Andorra","Anguilla","Antigua And Barbuda","Cayman Islands","Dominica","Greenland","Grenada","Jersey","Liechtenstein","Marshall Islands","Micronesia, Federated States of","Monaco","Nauru","Netherlands Antilles","Palau","Saint Kitts And Nevis","Saint Lucia","San Marino","Seychelles","Tonga","Tuvalu","Vanuatu","Virgin Islands, U.S.")
territories<-c("American Samoa","Anguilla","Aruba","Cayman Islands","Greenland","Guam","Hong Kong","Jersey","Macao","Martinique","Netherlands Antilles","Puerto Rico","Reunion","Taiwan, Province Of China","Virgin Islands, U.S.")
dataraw <- dataraw[!(dataraw$Country %in% smallcountries),]
dataraw <- dataraw[!(dataraw$Country %in% territories),]

#### Scale the data from 0 to 100 ####
datascale <- function(data){
  for(i in 1:ncol(data)){
    if(is.numeric(data[,i])){
      data[,i] <- (data[,i] - min(na.omit(data[,i]))) * 100 / max(na.omit(data[,i] - min(na.omit(data[,i]))))
    }
  }
  return(data)
}


data<-datascale(dataraw)

#### Flip the 5 scores where lower values are better ####
data$BattleDeaths = 100 - data$BattleDeaths
data$InterpersonalInjuries = 100 - data$InterpersonalInjuries
data$IC.BUS.EASE.XQ = 100 - data$IC.BUS.EASE.XQ
data$SI.POV.GINI = 100 - data$SI.POV.GINI
data$WorldRiskIndex = 100 - data$WorldRiskIndex
write.csv(data, "scaled_data.csv")

#### average across every 3 values to average, then assign rank and identify bottom 50 ####
indices<-as.data.frame(data$Country)
names(indices)<-"Country"
# set how many of the 3 values can be missing (currently set to calculate a value if up to 2 are missing)
allowedNA <- c(0,1,2)
# peace indice
indices$peaceavg<-rowMeans(data[,c("BattleDeaths", "InterpersonalInjuries", "PolStability")], na.rm=TRUE) * ifelse(
  (rowSums(is.na(data[,c("BattleDeaths", "InterpersonalInjuries", "PolStability")])) %in% allowedNA),1,NA)
indices$peacerank <- rank(indices$peaceavg, ties.method= "min") 
# justice indice
indices$justavg<-rowMeans(data[,c("BirthRegistration", "Corruption", "RuleofLaw")], na.rm=TRUE) * ifelse((rowSums(is.na(data[,c("BirthRegistration", "Corruption", "RuleofLaw")])) %in% allowedNA),1,NA)
indices$justrank <- rank(indices$justavg, ties.method= "min") 
# institution indice
indices$instavg<-rowMeans(data[,c("GovEffect", "RegQuality", "Voice")], na.rm=TRUE) * ifelse((rowSums(is.na(data[,c("GovEffect", "RegQuality", "Voice")])) %in% allowedNA),1,NA)
indices$instrank <- rank(indices$instavg, ties.method= "min") 
# economic indice
indices$econavg<-rowMeans(data[,c("IC.BUS.EASE.XQ", "BLeducationyears", "tenyearavg")], na.rm=TRUE) * ifelse(
  (rowSums(is.na(data[,c("IC.BUS.EASE.XQ", "BLeducationyears", "tenyearavg")])) %in% allowedNA),1,NA)
indices$econrank <- rank(indices$econavg, ties.method= "min") 
# resilience indice
indices$resilavg<-rowMeans(data[,c("SI.POV.GINI", "Health.Care.Capabilities", "WorldRiskIndex")], na.rm=TRUE) * ifelse((rowSums(is.na(data[,c("SI.POV.GINI", "Health.Care.Capabilities", "WorldRiskIndex")])) %in% allowedNA),1,NA)
indices$resilrank <- rank(indices$resilavg, ties.method= "min") 
write.csv(indices, "indice_values.csv")

# find bottom 50 for each indice
fragility <- as.data.frame(data$Country)
names(fragility) <- "Country"
fragility$Country <- as.character(fragility$Country)
fragility$peace <- as.numeric(indices$peacerank < 51) 
fragility$just <- as.numeric(indices$justrank < 51) 
fragility$inst <- as.numeric(indices$instrank < 51) 
fragility$econ <- as.numeric(indices$econrank < 51)   
fragility$resil <- as.numeric(indices$resilrank < 51) 
fragility$sum <- rowSums(fragility[,2:6])
# identify those states vulnerable in more than one area
fragility$fragile <- fragility$sum > 1

# verify that only 50 states were identified as fragile
colSums(fragility[,2:6])
sum(fragility$fragile)
colSums(fragility[fragility$fragile==T,2:6])

#### Compare replication results with actual results ####
report<-read.csv("reportedfragility.csv")
report$Country <- countrycode(report$Country, "country.name", "country.name")
report$repfrag <- as.numeric(report$repsum>1)
compare <- merge(fragility, report, by="Country", all=TRUE)

# check for countries where replication completely matches report results
compare$allmatch<-NULL
for(i in 1:nrow(compare)){
  compare$allmatch[i] <- as.numeric(compare$peace[i]==compare$reppeace[i]) + as.numeric(compare$just[i]==compare$repjust[i]) + as.numeric(compare$inst[i]==compare$repinst[i]) + as.numeric(compare$econ[i]==compare$repecon[i]) + as.numeric(compare$resil[i]==compare$represil[i])
}

# If country is not on report, set fragility values to -1
compare[is.na(compare)] <- (-1)

# Identify countries on the Venn diagram that need to move
compare$toarrow<-(compare$sum>1) * (compare$allmatch<5)

# export comparison
write.csv(compare, "results_comparison.csv")

#### Total matches for each country ####
table(compare$allmatch[compare$repfrag==1])

#### Identify countries that were moved into or off of Venn diagram #### 
compare$Country[compare$fragile == 1 & compare$repfrag != 1]
compare$Country[compare$fragile == 0 & compare$repfrag == 1]

#### Pull out ranks for the clusters where a state crossed the bottom-50 threshold ####

compare$peacediff <- (compare$reppeace==1 & compare$peace ==0) | (compare$reppeace<1 & compare$peace ==1)
compare$justdiff <- (compare$repjust==1 & compare$just ==0) | (compare$repjust<1 & compare$just ==1)
compare$instdiff <- (compare$repinst==1 & compare$inst ==0) | (compare$repinst<1 & compare$inst ==1)
compare$econdiff <- (compare$repecon==1 & compare$econ ==0) | (compare$repecon<1 & compare$econ ==1)
compare$resildiff <- (compare$represil==1 & compare$resil ==0) | (compare$represil<1 & compare$resil ==1)

changeranks <- indices[,c("peacerank", "justrank", "instrank", "econrank", "resilrank")]*compare[,c("peacediff", "justdiff", "instdiff", "econdiff", "resildiff")]
changeranks[changeranks==0] <- NA
names(changeranks) <- c("peace", "justice", "institutions", "economic", "resilience")
changeranks$Country <- compare$Country


