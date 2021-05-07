# Tidy up the CSV ready to be put on TRIM

setwd("V:/Exception_ LTMN Working Data/01_Raw_Data/_All_sites/Climate/_All_sites_collation_up_to_2020/BMA")
inputFile <- read.csv("Bure_Marshes_suspect_error_test.csv", stringsAsFactors = F)

#set working directory("V:/Exception_ LTMN Working Data/01_Raw_Data/_All_sites/Climate/_All_sites_collation_up_to_2020/<site>")
#inputFile <- read.csv("<site>_suspect_error_complete.csv", stringsAsFactors = F)

#remove TimeDiff and DateTime columns

inputFile["TimeDiff"] <- NULL
inputFile["DateTime"] <- NULL

#Remove first column (there was one called X that came from the suspect error extract to CSV)
inputFile[1] <- NULL

print("columns removed")

#Replace NA with blank in QA code column. This is caused by no QA codes being written into a field
#because no SUSPECT data exists in the value field. Could be fixed by adding another condition
#in the for loop but decided to do here.
#MUST BE A QUICKER WAY THAN LOOPING. A SINGLE FUNCTION SHOULD EXIST??
#QA_CODE_LIST = list("DRY_BULB_TEMP_QA_CODE", "WET_BULB_TEMP_QA_CODE", "RELATIVE_HUMIDITY_QA_CODE", "SOLAR_RAD_QA_CODE",
                #"WIND_SPEED_QA_CODE", "WIND_DIR_QA_CODE", "RAIN_MM_QA_CODE", "SOIL_TEMP_30CM_QA_CODE", "SOIL_TEMP_10CM_QA_CODE")

#for (codes in QA_CODE_LIST) {
#  for (i in 1:nrow(inputFile)) {
#    if (is.na(inputFile[i, codes])) {
#      inputFile[i, codes] = NA
#    }
#  }
#}

# based on https://stackoverflow.com/a/15673180
paste3 <- function(...,sep=", ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  ret <-gsub(paste0("(^",sep,"+|",sep,"+$)"),"",
             gsub(paste0(sep,"+"),sep,
                  do.call(paste,c(L,list(sep=sep)))))
  is.na(ret) <- ret==""
  ret
}

# Merge QA code fields into one

mergedInput <- transform(inputFile, QA_CODE_1=paste3(DRY_BULB_TEMP_QA_CODE, WET_BULB_TEMP_QA_CODE, RELATIVE_HUMIDITY_QA_CODE, SOLAR_RAD_QA_CODE,
                                                   WIND_SPEED_QA_CODE, WIND_DIR_QA_CODE, RAIN_MM_QA_CODE, SOIL_TEMP_30CM_QA_CODE, SOIL_TEMP_10CM_QA_CODE, sep = ","))

# Remove existing QA code fields

mergedInput["DRY_BULB_TEMP_QA_CODE"] <- NULL
mergedInput["WET_BULB_TEMP_QA_CODE"] <- NULL
mergedInput["RELATIVE_HUMIDITY_QA_CODE"] <- NULL
mergedInput["SOLAR_RAD_QA_CODE"] <- NULL
mergedInput["WIND_SPEED_QA_CODE"] <- NULL
mergedInput["WIND_DIR_QA_CODE"] <- NULL
mergedInput["RAIN_MM_QA_CODE"] <- NULL
mergedInput["SOIL_TEMP_30CM_QA_CODE"] <- NULL
mergedInput["SOIL_TEMP_10CM_QA_CODE"] <- NULL

print ("QA codes merged")

# Reorder columns

columnOrder = c("SITE", "DATA_SOURCE", "DATE", "JULIAN_DATE", "HOUR_MIN", "SOLAR_RAD", "SOLAR_RAD_QA_FLAG", "NET_RAD", "WET_BULB_TEMP", "WET_BULB_TEMP_QA_FLAG",
                "DRY_BULB_TEMP", "DRY_BULB_TEMP_QA_FLAG", "DRY_BULB_MAX", "DRY_BULB_MIN", "WIND_SPEED", "WIND_SPEED_QA_FLAG", "WIND_DIR", "WIND_DIR_QA_FLAG", "WIND_DIR_SD", "RAIN_MM",
                "RAIN_MM_QA_FLAG", "SOIL_TEMP_10CM", "SOIL_TEMP_10CM_QA_FLAG", "SOIL_TEMP_30CM", "SOIL_TEMP_30CM_QA_FLAG", "SOILWATER_CRNS", "SOILWATER_TDT_1", "SOILWATER_TDT_2", "RELATIVE_HUMIDITY",
                "RELATIVE_HUMIDITY_QA_FLAG", "ABSOLUTE_HUMIDITY", "AIR_TEMP_RH", "GRASS_TEMP", "SOLAR_RAD_IN_SW", "SOLAR_RAD_OUT_SW", "SOLAR_RAD_IN_LW", "SOLAR_RAD_OUT_LW", "PRESSURE", "POT_EVAP", "QA_CODE_1", "QA_CODE_2", "QA_CODE_3", "FINAL_QA", "DATA_ENTRY_COMMENTS")
                
                
mergedOutput <- mergedInput[, columnOrder]   

print ("columns reordered")
                
#Extract to CSV
write.csv(mergedOutput, file = "Bure_Marshes_final_error_test.csv", na = "")

print ("file written")