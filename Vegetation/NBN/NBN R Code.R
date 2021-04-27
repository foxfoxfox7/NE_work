#############################
##########Setup##############
#############################

laptop_dir <- 'C:/Users/kiera/Work/NE_work/Vegetation/NBN/'
setwd(laptop_dir)

library('openxlsx')
library('rgdal')
library('uuid')
library('zoo')

##################################
###Load Plot + Species from A2E###
##################################

#Load plot and species data ensuring it matches template

file_name <- '../Data/Data_set/Ingleborough_LTMN_Vegetation_2010_MAVIS.XLSX'

plots <- read.xlsx(file_name, sheet = 2, skipEmptyRows = TRUE)
species <- read.xlsx(file_name, sheet = 3, skipEmptyRows = TRUE)

#manually check for any empty rows at end and delete
plots <- head(plots, -1) ##########MANUAL CHANGE

############################################
##########Add fields to plot table##########
############################################

#1 Eastings + Northings. Note: This creates a sptial data frame object

coordinates(plots) <- c("EASTINGS", "NORTHINGS")
proj4string(plots) <- CRS("+init=epsg:27700")
plotsWGS <- spTransform(plots, CRS("+init=epsg:4326"))

#add converted coordinates to original plot table
plots$Latitude <- plotsWGS@coords[,2]
plots$Longitude <- plotsWGS@coords[,1]

#2 Create event ID. This needs to be a GUID.

for (i in 1:(nrow(plots))){
  plots$GUID[i] = UUIDgenerate()
}

#3 Abbreviation + Location ID
plots$abbreviation <- 'WYR' ########## MANUAL CHANGE

for (i in 1:(nrow(plots))){
  plots$LocationID[i] <- paste(plots$SITECODE[i], plots$abbreviation[i], plots$YEAR[i], plots$PLOT_ID[i], sep="_")
}

#4 Change date format to ISO 8601

plots$SDATE = strftime(convertToDate(plots$SDATE, origin = "1900-01-01"), format = "%Y-%m-%dT%H:%M:%S", tz="GMT")

##########################################
##########Create occurrence table##########
##########################################

occurrence <- data.frame(id=character(),
                        occurrenceID=character(),
                        eventID=character(),
                        collectionCode=character(),
                        datasetName=character(),
                        institutionCode=character(),
                        license=character(),
                        rightsHolder=character(),
                        scientificName=character(),
                        taxonID=character(),
                        identificationVerificationStatus=character(),
                        eventDate=character(),
                        recordedBy=character(),
                        identifiedBy=character(),
                        coordinateUncertaintyInMeters=integer(),
                        decimalLatitude=double(),
                        decimalLongitude=double(),
                        sampleSizeValue=integer(),
                        sampleSizeUnit=character(),
                        footprintWKT=character(),
                        locationID=character(),
                        locality=character(),
                        basisOfRecord=character(),
                        occurrenceStatus=character(),
                        individualCount=integer(),
                        organismQuantity=integer(),
                        organismQuantityType=character(),
                        dataCode1=character(),
                        dataCode2=character(),
                        dataCode3=character(),
                        species=character(), stringsAsFactors = F)

                        
                        
#############################################
#####Transfer data accross to occurrence######
#############################################
#Stage 2: Start transferring over data from both sheets to occurrence

for (i in 1:(nrow(species))){

  #Add eventID based locationID in original species data lookedup against the newly created location ID in plots.
  occurrence[i, 'eventID'] <- plots$GUID[match(species[i, 'PLOT_ID'], plots$PLOT_ID)]
  
  #Copy this to the id value
  occurrence[i, 'id'] <- occurrence[i, 'eventID']
  
  #Add unique occurrence ID
  occurrence[i, 'occurrenceID'] <- UUIDgenerate()
  
  #Add collection code
  occurrence[i, 'collectionCode'] <- 'LTMN'
  
  #Add datasetName
  occurrence[i, 'datasetName'] <- 'Long-Term Monitoring Network Vegetation Quadrats'
  
  #Add institution code
  occurrence[i, 'institutionCode'] <- 'Natural England'
  
  #Add license
  occurrence[i, 'license'] <- 'OGL'
  
  #Add rights holder
  occurrence[i, 'rightsHolder'] <- 'Natural England'
  
  #Leave scientific name blank for time being as populated once taxon match has been undertaken
  
  #Same for taxon ID
  
  #Add event date (SDATE from plots table)
  occurrence[i, 'eventDate'] <- plots$SDATE[match(species[i, 'PLOT_ID'], plots$PLOT_ID)]
  
  #Add recorded by
  occurrence[i, 'recordedBy'] <- 'LTMN Surveyers'
  
  #Add identifed by
  #occurrence[i, 'identifiedBy'] <- NULL
  
  #Add coordinate uncertainty in metres
  occurrence[i, 'coordinateUncertaintyInMeters'] <- 1
  
  #Add decimal latitude
  occurrence[i, 'decimalLatitude'] <- plots$Latitude[match(species[i, 'PLOT_ID'], plots$PLOT_ID)]
  
  #Add decimal longitude
  occurrence[i, 'decimalLongitude'] <- plots$Longitude[match(species[i, 'PLOT_ID'], plots$PLOT_ID)]
  
  #Add sample size value
  occurrence[i, 'sampleSizeValue'] <- 4
  
  #Add sample size unit
  occurrence[i, 'sampleSizeUnit'] <- 'square_metre'
  
  #Add footprint (unique for each site - basic boundaing box from QGIS)
  occurrence[i, 'footprintWKT'] <- 'POLYGON ((372466 274666,377134 274666,377134 278978,372466 278978,372466 274666))'
  ########MANUAL CHANGE. See boundin boxes excel. Use bboxes tool in QGIS processing
  # then export to CSV with geometry as WKT.
  
  #Add location ID
  occurrence[i, 'locationID'] <- plots$LocationID[match(species[i, 'PLOT_ID'], plots$PLOT_ID)]
  
  #Add locality
  occurrence[i, 'locality'] <- 'Wyre Forest' #########MANUAL CHANGE
  
  #Add basis of record
  occurrence[i, 'basisOfRecord'] <- "HumanObservation"
  
  #Add occurrence status
  occurrence[i, 'occurrenceStatus'] <- 'present'
  
  #Add individual count. Cant use match because different value for each plot id.
  # IE not a plot metic but a occurrence metric
  occurrence[i, 'individualCount'] <- species$CELL_1[i] + species$CELL_2[i] + species$CELL_3[i] + species$CELL_4[i] + species$CELL_5[i] +
    species$CELL_6[i] + species$CELL_7[i] + species$CELL_8[i] + species$CELL_9[i] + species$CELL_10[i] + species$CELL_11[i] + species$CELL_12[i] +
    species$CELL_13[i] + species$CELL_14[i] + species$CELL_15[i] + species$CELL_16[i] + species$CELL_17[i] + species$CELL_18[i] + species$CELL_19[i] +
    species$CELL_20[i] + species$CELL_21[i] + species$CELL_22[i] + species$CELL_23[i] + species$CELL_24[i] + species$CELL_25[i]
  
  #Add organism quantity
  occurrence[i, 'organismQuantity'] <- (occurrence[i, 'individualCount'] / 25) * 100
  
  #Add organism quantity type
  occurrence[i, 'organismQuantityType'] <- 'PercentFrequency' #######POTENTUALLY WONT THIS TO BE PERCENTCOVER???
  
  #Add datacode1 - to be deleted
  occurrence[i, 'dataCode1'] <- species[i, 'QA_CODE1']
  
  #Add datacode2 - to be deleted
  occurrence[i, 'dataCode2'] <- species[i, 'QA_CODE2']
  
  #Add datacode3 - to be deleted
  occurrence[i, 'dataCode2'] <- species[i, 'QA_CODE3']
  
  #Add species - to be deleted
  occurrence[i, 'species'] <- species[i, 'DESC_LATIN']
}

########################################
#########Add verification Rules#########
########################################

surveyType <- 'IN_HOUSE' #########MANUAL CHANGE
qualityAssurance <- 'NO' #########MANUAL CHANGE NO if before 2015

for (i in 1:(nrow(occurrence))){
 if((surveyType == 'IN_HOUSE') & (qualityAssurance == 'YES') & !is.na(occurrence[i, 'dataCode1'])){
   if((occurrence[i, 'dataCode1'] == '999') || (occurrence[i, 'dataCode1'] == 'QA600') || (occurrence[i, 'dataCode1'] == 'QA601') ||
      (occurrence[i, 'dataCode1'] == 'QA602') || (occurrence[i, 'dataCode1'] == 'QA603') || (occurrence[i, 'dataCode1'] == 'QA604') ||
      (occurrence[i, 'dataCode1'] == 'QA605') || (occurrence[i, 'dataCode1'] == 'QA606') || (occurrence[i, 'dataCode1'] == 'QA607') ||
      (occurrence[i, 'dataCode1'] == 'QA608') || (occurrence[i, 'dataCode1'] == 'QA610') || (occurrence[i, 'dataCode1'] == 'QA777') ||
      (occurrence[i, 'dataCode1'] == 'QA900') || (occurrence[i, 'dataCode1'] == 'QA901') || (occurrence[i, 'dataCode1'] == 'QA902') ||
      (occurrence[i, 'dataCode1'] == 'QA903')) {
     occurrence[i, "identificationVerificationStatus"] <- 'Accepted'
   } else if((occurrence[i, 'dataCode1'] == 'QA766') || (occurrence[i, 'dataCode1'] == 'QA788') || (occurrence[i, 'dataCode1'] == 'QA999')){
     occurrence[i, "identificationVerificationStatus"] <- 'Unconfirmed'
   }
 } else if ((surveyType == 'IN_HOUSE') & (qualityAssurance == 'YES') & is.na(occurrence[i, 'dataCode1'])){
   occurrence[i, "identificationVerificationStatus"] <- 'Accepted'
 } else if ((surveyType == 'IN_HOUSE') & (qualityAssurance == 'NO')){
   occurrence[i, "identificationVerificationStatus"] <- 'Accepted - considered correct'
 } else if (surveyType == 'CONTRACT'){
   occurrence[i, "identificationVerificationStatus"] <- 'Accepted'
 }
}

##########################################
######Export to CSV for taxon match#######
##########################################

#Stage 3 tidy up

#Drop data code fields - do this manually at the end of the matching process.
#Add P and check for plots


#Stage 4: Export to individual file (Omit NAs)

#######MANUAL CHANGE
write.csv(occurrence, file="test_occurence.csv", na="")

#Stage 5: Append to master

