#Completes gross error checks and outputs a dataset to be imported to the
#suspect error checks script. All rules for these checks have been provided by the Met
#Office

setwd("V:/Exception_ LTMN Working Data/01_Raw_Data/_All_sites/Climate/_All_sites_collation_up_to_2020/BMA")
inputFile <- read.csv("LTMN_AWS_Bure_Marshes_raw_data.csv", stringsAsFactors = F)

print ("file loaded")

#set working directory ("V:/Exception_ LTMN Working Data/01_Raw_Data/_All_sites/Climate/_All_sites_collation_up_to_2020/DOW")
#inputFile <- read.csv("LTMN_AWS_Downton_Gorge_raw_data.csv", stringsAsFactors = F)
#Output file will also be saved here

##########################################
############Create extra columns##########
##########################################

# List of weather variables (soil 100 has been omitted as no data from campbells stations)
weatherVariables = list("DRY_BULB_TEMP", "WET_BULB_TEMP", "RELATIVE_HUMIDITY", "SOLAR_RAD", "WIND_SPEED", "WIND_DIR", "RAIN_MM", "SOIL_TEMP_10CM", "SOIL_TEMP_30CM")


# Check if QA columns exist, if so remove them and recreate so the process can be rerun.
# If not, then create column for each variable

for (variables in weatherVariables) {
  if (paste(variables,"_QA_FLAG", sep="") %in% colnames(inputFile)) {
    inputFile[paste(variables,"_QA_FLAG", sep="")] <- NULL
    inputFile[paste(variables,"_QA_FLAG", sep="")] <- NA
  } else {
    inputFile[paste(variables,"_QA_FLAG", sep="")] <- NA
  }
}

for (variables in weatherVariables) {
  if (paste(variables,"_QA_CODE", sep="") %in% colnames(inputFile)) {
    inputFile[paste(variables,"_QA_CODE", sep="")] <- NULL
    inputFile[paste(variables,"_QA_CODE", sep="")] <- NA
  } else {
    inputFile[paste(variables,"_QA_CODE", sep="")] <- NA
  }
}
print ("QA columns made")

#########################################
######Manage times and dates#############
#########################################

# Read data into date-time object and add as new column to the data frame
inputFile$DateTime <- with(inputFile, strptime(paste(DATE, HOUR_MIN), "%d/%m/%Y %H:%M"))

# Find difference in hours between subsequent rows, and add as new column.

for (i in 1:(nrow(inputFile) - 1)){
  j = i + 1
  
  times = inputFile[i:j, "DateTime"]
  inputFile[j, 'TimeDiff'] = abs(as.numeric(difftime(times[1], times[2], units="hours", tz="GMT")))
}

# Check for known issue in the data
# Occasionally 23, 23;00: 23, 00:00 exists, which doesn't make sense.
# This provides a value of 23 for the time difference which should be 1.
# We change the time difference rather than the underlying time data!

for (i in 2:nrow(inputFile)){
  j = i - 1
  k = i + 1
  
  #Check that TimeDiff is 23 and the Hour_min is 00:00 and the day for both cells is the same.
  #If both day cells not the same then could be a genuine difference of 23.
  if (!is.na(inputFile[i, "TimeDiff"]) & inputFile[i, "TimeDiff"] == 23 & inputFile[i, "HOUR_MIN"] == "00:00" & length(unique(inputFile[j:i, "DATE"])) == 1){
    inputFile[i, "TimeDiff"] = 1  
    
    #The next row has a difference of 25 because we haven't changed the uderlying data, so need to change to 1.
    inputFile[k, "TimeDiff"] = 1   
  }
}
print ("time difference calculated")

##########################################
############Gross error checks############
##########################################

# Air temperature

for (i in 1:nrow(inputFile)){
  values = inputFile[i, "DRY_BULB_TEMP"]
  if (values == -6999 || is.na(values)){
    inputFile[i, "DRY_BULB_TEMP_QA_FLAG"] = "NO_DATA"
  } else if (values > 40 || values < -30) {
    inputFile[i, "DRY_BULB_TEMP_QA_FLAG"] = "BAD"
  } else {
    inputFile[i,"DRY_BULB_TEMP_QA_FLAG"] = "GOOD"
  }
}
print ("air temp checked")

# Wet bulb temperature

for (i in 1:nrow(inputFile)){
  values = inputFile[i, "WET_BULB_TEMP"]
  if (values == -6999 || is.na(values)){
    inputFile[i, "WET_BULB_TEMP_QA_FLAG"] = "NO_DATA"
  } else if (values > 40 || values < -30) {
    inputFile[i, "WET_BULB_TEMP_QA_FLAG"] = "BAD"
  } else {
    inputFile[i, "WET_BULB_TEMP_QA_FLAG"] = "GOOD"
  }
}
print ("wet bulb checked")

# Humidity

for (i in 1:nrow(inputFile)){
  values = inputFile[i, "RELATIVE_HUMIDITY"]
  if (values == -6999 || is.na(values)){
    inputFile[i, "RELATIVE_HUMIDITY_QA_FLAG"] = "NO_DATA"
  } else if (values > 100 || values < 20) {
    inputFile[i, "RELATIVE_HUMIDITY_QA_FLAG"] = "BAD"
  } else {
    inputFile[i, "RELATIVE_HUMIDITY_QA_FLAG"] = "GOOD"
  }
}
print ("humidity checked")

# Solar radiation

for (i in 1:nrow(inputFile)){
  values = inputFile[i, "SOLAR_RAD"]
  if (values == -6999 || is.na(values)){
    inputFile[i, "SOLAR_RAD_QA_FLAG"] = "NO_DATA"
  } else if (values > 1500 || values < -30) {
    inputFile[i, "SOLAR_RAD_QA_FLAG"] = "BAD"
  } else {
    inputFile[i, "SOLAR_RAD_QA_FLAG"] = "GOOD"
  }
}
print ("solar rad checked")

# Wind speed

for (i in 1:nrow(inputFile)){
  values = inputFile[i, "WIND_SPEED"]
  if (values == -6999 || is.na(values)){
    inputFile[i, "WIND_SPEED_QA_FLAG"] = "NO_DATA"
  } else if (values > 100 || values < 0) {
    inputFile[i, "WIND_SPEED_QA_FLAG"] = "BAD"
  } else {
    inputFile[i, "WIND_SPEED_QA_FLAG"] = "GOOD"
  }
}
print ("wind speed checked")

# Wind direction

for (i in 1:nrow(inputFile)){
  values = inputFile[i, "WIND_DIR"]
  if (values == -6999 || is.na(values)){
    inputFile[i, "WIND_DIR_QA_FLAG"] = "NO_DATA"
  } else if (values > 360 || values < 0) {
    inputFile[i, "WIND_DIR_QA_FLAG"] = "BAD"
  } else {
    inputFile[i, "WIND_DIR_QA_FLAG"] = "GOOD"
  }
}
print ("wind dir checked")

# Soil 30

for (i in 1:nrow(inputFile)){
  values = inputFile[i, "SOIL_TEMP_30CM"]
  if (values == -6999 || is.na(values)){
    inputFile[i, "SOIL_TEMP_30CM_QA_FLAG"] = "NO_DATA"
  } else if (values > 25 || values < -5) {
    inputFile[i, "SOIL_TEMP_30CM_QA_FLAG"] = "BAD"
  } else {
    inputFile[i, "SOIL_TEMP_30CM_QA_FLAG"] = "GOOD"
  }
}
print ("soil 30cm checked")

# Soil 10

for (i in 1:nrow(inputFile)){
  values = inputFile[i, "SOIL_TEMP_10CM"]
  if (values == -6999 || is.na(values)){
    inputFile[i, "SOIL_TEMP_10CM_QA_FLAG"] = "NO_DATA"
  } else if (values > 32 || values < -15) {
    inputFile[i, "SOIL_TEMP_10CM_QA_FLAG"] = "BAD"
  } else {
    inputFile[i, "SOIL_TEMP_10CM_QA_FLAG"] = "GOOD"
  }
}
print ("soil 10cm checked")

# Rainfall

for (i in 1:nrow(inputFile)){
  values = inputFile[i, "RAIN_MM"]
  if (values == -6999 || is.na(values)){
    inputFile[i, "RAIN_MM_QA_FLAG"] = "NO_DATA"
  } else if (values > 100 || values < 0) {
    inputFile[i, "RAIN_MM_QA_FLAG"] = "BAD"
  } else {
    inputFile[i, "RAIN_MM_QA_FLAG"] = "GOOD"
  }
}
print ("rainfall checked")

#extract data frame back to csv
write.csv(inputFile, file = "Bure_Marshes_gross_error_complete.csv")

print ("file written")



