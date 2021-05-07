# Completes suspect data checks for all fields using rules as specified by Met Offic Guidance.
# Final CSV will contain QA FLAGS (GOOD, BAD, SUSPECT) and QA CODES where data has been flagged as SUSPECT.

#This script is to be run after the gross error checks have been completed.

library('lubridate')

setwd("V:/Exception_ LTMN Working Data/01_Raw_Data/_All_sites/Climate/_All_sites_collation_up_to_2020/BMA")
inputFile <- read.csv("Bure_Marshes_gross_error_test.csv", stringsAsFactors = F)

#set working directory ("V:/Exception_ LTMN Working Data/01_Raw_Data/_All_sites/Climate/_All_sites_collation_up_to_2020/<site>")
#inputFile <- read.csv("<site>_gross_error_complete.csv", stringsAsFactors = F)

print ("file loaded")

#Remove first column (there was one called X that came from the gross error extract to CSV)
inputFile[1] <- NULL

#Define codes
airTempStuckCode = "QA101"
airTempStepCode = "QA102"
airTempConsistencyCode = "QA103"

################################
#########Stuck check############
################################
# Same value reported for 6 hours.

# remove NA's in code column
inputFile["DRY_BULB_TEMP_QA_CODE"] = NA

for (i in 1:(nrow(inputFile)-5)){
  
  # Collect next 6 records for each step in the loop
  # Ignore following process if any of the 6 Values are flagged as BAD or NO_DATA as cant be sure data is actually stuck
  # Include anything that is marked as SUSPECT
  j = i + 5 
  
  values = inputFile[i:j, "DRY_BULB_TEMP"]
  timeDifference = inputFile[i:j, "TimeDiff"]
  
  # Check that the 6 records are all separated by an hour.
  # Create flag
  dateFlag = ""
  
  for (records in timeDifference){
    if (is.na(records)){
      next()
    } else if (records != 1){
      dateFlag = FALSE
    }
  }
  
  #If gap between records is more than an hour, then skip to next set
  if (dateFlag == FALSE){
    next()
    
  } else {
    if ('BAD' %in% inputFile[i:j, "DRY_BULB_TEMP_QA_FLAG"] || 'NO_DATA' %in% inputFile[i:j, "DRY_BULB_TEMP_QA_FLAG"]){
      next()
    } else {
      
      # Check that all selected values are the same (should only be 1 unique value)
      if (length(unique(values)) == 1) {
        inputFile[i:j, "DRY_BULB_TEMP_QA_FLAG"] = "SUSPECT"
        
        # Update codes. If previous code exists, new one is concatenated
        for (codes in inputFile[i:j, "DRY_BULB_TEMP_QA_CODE"]){
          if (is.na(codes)) {
            inputFile[i:j, "DRY_BULB_TEMP_QA_CODE"] = airTempStuckCode
          } else {
            inputFile[i:j, "DRY_BULB_TEMP_QA_CODE"] = paste(inputFile[i:j, "DRY_BULB_TEMP_QA_CODE"], airTempStuckCode, sep = ",")
          }
        }
      }
    }
  }
}


################################
##########Step check############
################################
# Change more than 5 degrees per hour.

#inputFile <- read.csv("air_temp_step_test.csv", stringsAsFactors = F)

for (i in 1:(nrow(inputFile) - 1)){
  
  j = i+1
  values = inputFile[i:j, "DRY_BULB_TEMP"]
  timeDifference = inputFile[i:j, "TimeDiff"]
  
  # Check that the 2 records are all separated by an hour.
  # Create flag
  dateFlag = ""
  
  for (records in timeDifference){
    if (is.na(records)){
      next()
    } else if (records != 1){
      dateFlag = FALSE
    }
  }
  
  if (dateFlag == FALSE){
    next()
    
  } else {
    
    if ('BAD' %in% inputFile[i:j, "DRY_BULB_TEMP_QA_FLAG"] || 'NO_DATA' %in% inputFile[i:j, "DRY_BULB_TEMP_QA_FLAG"]){
      next()
    } else {
      
      if (abs(values[1] - values[2]) > 5) {
        inputFile[i:j, "DRY_BULB_TEMP_QA_FLAG"] = "SUSPECT"
        
        for (codes in inputFile[i:j, "DRY_BULB_TEMP_QA_CODE"]){
          if (is.na(codes) || codes == ""){
            inputFile[i:j, "DRY_BULB_TEMP_QA_CODE"] = airTempStepCode
          } else {
            inputFile[i:j, "DRY_BULB_TEMP_QA_CODE"] = paste(inputFile[i:j, "DRY_BULB_TEMP_QA_CODE"], airTempStepCode, sep = ",")
          }
        }
      }
    }
  }
}

################################
######Internal consistency######
################################
# If wet bulb > air temp, delete air temp if it is found to be suspect.

#inputFile <- read.csv("air_temp_consistency_test.csv", stringsAsFactors = F)

for (i in 1:nrow(inputFile)){
  
  airTempValues = inputFile[i, "DRY_BULB_TEMP"]
  wetBulbValues = inputFile[i, "WET_BULB_TEMP"]
  
  if (inputFile[i, "DRY_BULB_TEMP_QA_FLAG"] == "SUSPECT" & inputFile[i, "WET_BULB_TEMP_QA_FLAG"] != "NO_DATA"
      & airTempValues < wetBulbValues){
    inputFile[i, "DRY_BULB_TEMP"] = NA
    inputFile[i, "DRY_BULB_TEMP_QA_FLAG"] = "NO_DATA"
    
    if (is.na(inputFile[i, "DRY_BULB_TEMP_QA_CODE"]) || inputFile[i, "DRY_BULB_TEMP_QA_CODE"] == ""){
      inputFile[i, "DRY_BULB_TEMP_QA_CODE"] = airTempConsistencyCode
    } else {
      inputFile[i, "DRY_BULB_TEMP_QA_CODE"] = paste(inputFile[i, "DRY_BULB_TEMP_QA_CODE"], "QA103", sep = ",")
    }
  }
}

#################################
##########Clean codes############
#################################
# Remove duplicate codes.

for (i in 1:nrow(inputFile)){
  if (!is.na(inputFile[i, "DRY_BULB_TEMP_QA_CODE"])){
    uniqueCodeList = unique(unlist(strsplit(inputFile[i, "DRY_BULB_TEMP_QA_CODE"], ",")))
    inputFile[i, "DRY_BULB_TEMP_QA_CODE"] = paste(uniqueCodeList, collapse = ",")
  }
}

print ("air temp checked")

#Define codes
wetBulbStuckCode = "QA104"
wetBulbStepCode = "QA105"
wetBulbConsistencyCode = "QA106"

################################
#########Stuck check############
################################
# Same value reported for 12 hours.

# remove NA's in code column
inputFile["WET_BULB_TEMP_QA_CODE"] = NA

for (i in 1:(nrow(inputFile)-11)){
  
  # Collect next 12 records for each step in the loop
  # Ignore following process if any of the 12 Values are flagged as BAD or NO_DATA as cant be sure data is actually stuck
  # Include anything that is marked as SUSPECT
  j = i + 11 
  
  values = inputFile[i:j, "WET_BULB_TEMP"]
  timeDifference = inputFile[i:j, "TimeDiff"]
  
  # Check that the 12 records are all seperated by an hour.
  # Create flag
  dateFlag = ""
  
  for (records in timeDifference){
    if (is.na(records)){
      next()
    } else if (records != 1){
      dateFlag = FALSE
    }
  }
  
  #If gap between records is more than an hour, then skip to next set
  if (dateFlag == FALSE){
    next()
    
  } else {
    
    if ('BAD' %in% inputFile[i:j, "WET_BULB_TEMP_QA_FLAG"] || 'NO_DATA' %in% inputFile[i:j, "WET_BULB_TEMP_QA_FLAG"]){
      next()
    } else {
      
      # Check that all selected values are the same (should only be 1 unique value)
      if (length(unique(values)) == 1) {
        inputFile[i:j, "WET_BULB_TEMP_QA_FLAG"] = "SUSPECT"
        
        # Update codes. If previous code exists, new one is concatenated
        for (codes in inputFile[i:j, "WET_BULB_TEMP_QA_CODE"]){
          if (is.na(codes)) {
            inputFile[i:j, "WET_BULB_TEMP_QA_CODE"] = wetBulbStuckCode
          } else {
            inputFile[i:j, "WET_BULB_TEMP_QA_CODE"] = paste(inputFile[i:j, "WET_BULB_TEMP_QA_CODE"], wetBulbStuckCode, sep = ",")
          }
        }
      }
    }
  }
}

################################
##########Step check############
################################
# Change more than 5 degrees per hour.

#inputFile <- read.csv("wet_bulb_step_test.csv", stringsAsFactors = F)

for (i in 1:(nrow(inputFile) - 1)){
  
  j = i+1
  values = inputFile[i:j, "WET_BULB_TEMP"]
  timeDifference = inputFile[i:j, "TimeDiff"]
  
  # Check that the 2 records are all seperated by an hour.
  # Create flag
  dateFlag = ""
  
  for (records in timeDifference){
    if (is.na(records)){
      next()
    } else if (records != 1){
      dateFlag = FALSE
    }
  }
  
  if (dateFlag == FALSE){
    next()
    
  } else {
    
    if ('BAD' %in% inputFile[i:j, "WET_BULB_TEMP_QA_FLAG"] || 'NO_DATA' %in% inputFile[i:j, "WET_BULB_TEMP_QA_FLAG"]){
      next()
    } else {
      
      if (abs(values[1] - values[2]) > 5) {
        inputFile[i:j, "WET_BULB_TEMP_QA_FLAG"] = "SUSPECT"
        
        for (codes in inputFile[i:j, "WET_BULB_TEMP_QA_CODE"]){
          if (is.na(codes) || codes == ""){
            inputFile[i:j, "WET_BULB_TEMP_QA_CODE"] = wetBulbStepCode
          } else {
            inputFile[i:j, "WET_BULB_TEMP_QA_CODE"] = paste(inputFile[i:j, "WET_BULB_TEMP_QA_CODE"], wetBulbStepCode, sep = ",")
          }
        }
      }
    }
  }
}

################################
######Internal consistency######
################################
# If wet bulb > air temp, delete wet bulb + humidity if wet bulb is found to be SUSPECT

#inputFile <- read.csv("wet_bulb_consistency_test.csv", stringsAsFactors = F)

for (i in 1:nrow(inputFile)){
  airTempValues = inputFile[i, "DRY_BULB_TEMP"]
  wetBulbValues = inputFile[i, "WET_BULB_TEMP"]
  humidityValues = inputFile[i, "RELATIVE_HUMIDITY"]
  
  # Check fors non empty cells and then the appropiate condition.
  if (inputFile[i, "WET_BULB_TEMP_QA_FLAG"] == "SUSPECT" & inputFile[i, "DRY_BULB_TEMP_QA_FLAG"] != "NO_DATA"
      & airTempValues < wetBulbValues){
    inputFile[i, "WET_BULB_TEMP"] = NA
    inputFile[i, "RELATIVE_HUMIDITY"] = NA
    inputFile[i, "WET_BULB_TEMP_QA_FLAG"] = "NO_DATA"
    inputFile[i, "RELATIVE_HUMIDITY_QA_FLAG"] = "NO_DATA"
    
    if (is.na(inputFile[i, "WET_BULB_TEMP_QA_CODE"]) || inputFile[i, "WET_BULB_TEMP_QA_CODE"] == ""){
      inputFile[i, "WET_BULB_TEMP_QA_CODE"] = wetBulbConsistencyCode
    } else {
      inputFile[i, "WET_BULB_TEMP_QA_CODE"] = paste(inputFile[i, "WET_BULB_TEMP_QA_CODE"], wetBulbConsistencyCode, sep = ",")
    }
  }
}

#################################
##########Clean codes############
#################################
# Remove duplicate codes.

for (i in 1:nrow(inputFile)){
  if (!is.na(inputFile[i, "WET_BULB_TEMP_QA_CODE"])){
    uniqueCodeList = unique(unlist(strsplit(inputFile[i, "WET_BULB_TEMP_QA_CODE"], ",")))
    inputFile[i, "WET_BULB_TEMP_QA_CODE"] = paste(uniqueCodeList, collapse = ",")
  }
}
print ("wet bulb checked")

#Define codes
humidityStuckCode = "QA107"
humidityStepCode = "QA108"

################################
#########Stuck check############
################################
# Same value reported for 24 hours.

# remove NA's in code column
inputFile["RELATIVE_HUMIDITY_QA_CODE"] = NA

for (i in 1:(nrow(inputFile)-23)){
  
  # Collect next 24 records for each step in the loop
  # Ignore following process if any of the 12 Values are flagged as BAD or NO_DATA as cant be sure data is actually stuck
  # Include anything that is marked as SUSPECT
  j = i + 23 
  
  values = inputFile[i:j, "RELATIVE_HUMIDITY"]
  timeDifference = inputFile[i:j, "TimeDiff"]
  
  # Check that the 24 records are all seperated by an hour.
  # Create flag
  dateFlag = ""
  
  for (records in timeDifference){
    if (is.na(records)){
      next()
    } else if (records != 1){
      dateFlag = FALSE
    }
  }
  
  #If gap between records is more than an hour, then skip to next set
  if (dateFlag == FALSE){
    next()
    
  } else {
    
    if ('BAD' %in% inputFile[i:j, "RELATIVE_HUMIDITY_QA_FLAG"] || 'NO_DATA' %in% inputFile[i:j, "RELATIVE_HUMIDITY_QA_FLAG"]){
      next()
    } else {
      
      # Check that all selected values are the same 
      if (length(unique(values)) == 1) {
        inputFile[i:j, "RELATIVE_HUMIDITY_QA_FLAG"] = "SUSPECT"
        
        # Update codes. If previous code exists, new one is concatenated
        for (codes in inputFile[i:j, "RELATIVE_HUMIDITY_QA_CODE"]){
          if (is.na(codes)) {
            inputFile[i:j, "RELATIVE_HUMIDITY_QA_CODE"] = humidityStuckCode
          } else {
            inputFile[i:j, "RELATIVE_HUMIDITY_QA_CODE"] = paste(inputFile[i:j, "RELATIVE_HUMIDITY_QA_CODE"], humidityStuckCode, sep = ",")
          }
        }
      }
    }
  }
}

################################
##########Step check############
################################
# Change more than 40% per hour.

#inputFile <- read.csv("humidity_step_test.csv", stringsAsFactors = F)

for (i in 1:(nrow(inputFile) - 1)){
  
  j = i+1
  values = inputFile[i:j, "RELATIVE_HUMIDITY"]
  timeDifference = inputFile[i:j, "TimeDiff"]
  
  # Check that the 2 records are all seperated by an hour.
  # Create flag
  dateFlag = ""
  
  for (records in timeDifference){
    if (is.na(records)){
      next()
    } else if (records != 1){
      dateFlag = FALSE
    }
  }
  
  if (dateFlag == FALSE){
    next()
    
  } else {
    
    if ('BAD' %in% inputFile[i:j, "RELATIVE_HUMIDITY_QA_FLAG"] || 'NO_DATA' %in% inputFile[i:j, "RELATIVE_HUMIDITY_QA_FLAG"]){
      next()
    } else {
      
      if (abs(values[1] - values[2]) > 40) {
        inputFile[i:j, "RELATIVE_HUMIDITY_QA_FLAG"] = "SUSPECT"
        
        for (codes in inputFile[i:j, "RELATIVE_HUMIDITY_QA_CODE"]){
          if (is.na(codes) || codes == ""){
            inputFile[i:j, "RELATIVE_HUMIDITY_QA_CODE"] = humidityStepCode
          } else {
            inputFile[i:j, "RELATIVE_HUMIDITY_QA_CODE"] = paste(inputFile[i:j, "RELATIVE_HUMIDITY_QA_CODE"], humidityStepCode, sep = ",")
          }
        }
      }
    }
  }
}

#################################
##########Clean codes############
#################################
# Remove duplicate codes.

for (i in 1:nrow(inputFile)){
  if (!is.na(inputFile[i, "RELATIVE_HUMIDITY_QA_CODE"])){
    uniqueCodeList = unique(unlist(strsplit(inputFile[i, "RELATIVE_HUMIDITY_QA_CODE"], ",")))
    inputFile[i, "RELATIVE_HUMIDITY_QA_CODE"] = paste(uniqueCodeList, collapse = ",")
  }
}
print ("humidity checked")

#Define codes
radiationStuckCode = "QA109"
radiationStepCode = "QA110"

################################
#########Stuck check############
################################
# Same value reported for 24 hours.

# remove NA's in code column
inputFile["SOLAR_RAD_QA_CODE"] = NA

for (i in 1:(nrow(inputFile)-23)){
  
  # Collect next 24 records for each step in the loop
  # Ignore following process if any of the 12 Values are flagged as BAD or NO_DATA as cant be sure data is actually stuck
  # Include anything that is marked as SUSPECT
  j = i + 23 
  
  values = inputFile[i:j, "SOLAR_RAD"]
  timeDifference = inputFile[i:j, "TimeDiff"]
  
  # Check that the 24 records are all separated by an hour.
  # Create flag
  dateFlag = ""
  
  for (records in timeDifference){
    if (is.na(records)){
      next()
    } else if (records != 1){
      dateFlag = FALSE
    }
  }
  
  #If gap between records is more than an hour, then skip to next set
  if (dateFlag == FALSE){
    next()
    
  } else {
    
    if ('BAD' %in% inputFile[i:j, "SOLAR_RAD_QA_FLAG"] || 'NO_DATA' %in% inputFile[i:j, "SOLAR_RAD_QA_FLAG"]){
      next()
    } else {
      
      # Check that all selected values are the same 
      if (length(unique(values)) == 1) {
        inputFile[i:j, "SOLAR_RAD_QA_FLAG"] = "SUSPECT"
        
        # Update codes. If previous code exists, new one is concatenated
        for (codes in inputFile[i:j, "SOLAR_RAD_QA_CODE"]){
          if (is.na(codes)) {
            inputFile[i:j, "SOLAR_RAD_QA_CODE"] = radiationStuckCode
          } else {
            inputFile[i:j, "SOLAR_RAD_QA_CODE"] = paste(inputFile[i:j, "SOLAR_RAD_QA_CODE"], radiationStuckCode, sep = ",")
          }
        }
      }
    }
  }
}

################################
##########Step check############
################################
# Change more 5 during night time periods (defined as 10pm to 6am)

#inputFile <- read.csv("radiation_step_test.csv", stringsAsFactors = F)

for (i in 1:(nrow(inputFile) - 1)){
  
  j = i+1
  values = inputFile[i:j, "SOLAR_RAD"]
  timeDifference = inputFile[i:j, "TimeDiff"]
  
  # Check that the 2 records are all separated by an hour.
  # Create flag
  dateFlag = ""
  
  for (records in timeDifference){
    if (is.na(records)){
      next()
    } else if (records != 1){
      dateFlag = FALSE
    }
  }
  
  if (dateFlag == FALSE){
    next()
    
  } else {
    
    if ('BAD' %in% inputFile[i, "SOLAR_RAD_QA_FLAG"] || 'NO_DATA' %in% inputFile[i, "SOLAR_RAD_QA_FLAG"]){
      next()
    } else {
      
      # Extract hours and check they fall in the night time
      if (hour(inputFile[i, "DateTime"]) > 21 || hour(inputFile[i, "DateTime"]) < 6){
        
        if (inputFile[i, "SOLAR_RAD"] > 5) {
          inputFile[i, "SOLAR_RAD_QA_FLAG"] = "SUSPECT"
          
          #Following loop due to historical reasons
          
          for (codes in inputFile[i, "SOLAR_RAD_QA_CODE"]) {
            if (is.na(codes) || codes == ""){
              inputFile[i, "SOLAR_RAD_QA_CODE"] = radiationStepCode
            } else {
              inputFile[i, "SOLAR_RAD_QA_CODE"] = paste(inputFile[i, "SOLAR_RAD_QA_CODE"], radiationStepCode, sep = ",")
            }
          }
        }
      }
    }
  }
}

#################################
##########Clean codes############
#################################
# Remove duplicate codes.

for (i in 1:nrow(inputFile)){
  if (!is.na(inputFile[i, "SOLAR_RAD_QA_CODE"])){
    uniqueCodeList = unique(unlist(strsplit(inputFile[i, "SOLAR_RAD_QA_CODE"], ",")))
    inputFile[i, "SOLAR_RAD_QA_CODE"] = paste(uniqueCodeList, collapse = ",")
  }
}
print ("solar rad checked")

#Define codes
soil30StuckCode = "QA117"
soil30StepCode = "QA118"

################################
#########Stuck check############
################################
# Same value reported for 72 hours.

# remove NA's in code column
inputFile["SOIL_TEMP_30CM_QA_CODE"] = NA

for (i in 1:(nrow(inputFile)-71)){
  
  # Collect next 72 records for each step in the loop
  # Ignore following process if any of the 72 Values are flagged as BAD or NO_DATA as cant be sure data is actually stuck
  # Include anything that is marked as SUSPECT
  j = i + 71 
  
  values = inputFile[i:j, "SOIL_TEMP_30CM"]
  timeDifference = inputFile[i:j, "TimeDiff"]
  
  # Check that the 12 records are all seperated by an hour.
  # Create flag
  dateFlag = ""
  
  for (records in timeDifference){
    if (is.na(records)){
      next()
    } else if (records != 1){
      dateFlag = FALSE
    }
  }
  
  #If gap between records is more than an hour, then skip to next set
  if (dateFlag == FALSE){
    next()
    
  } else {
    
    if ('BAD' %in% inputFile[i:j, "SOIL_TEMP_30CM_QA_FLAG"] || 'NO_DATA' %in% inputFile[i:j, "SOIL_TEMP_30CM_QA_FLAG"]){
      next()
    } else {
      
      # Check that all selected values are the same 
      if (length(unique(values)) == 1) {
        inputFile[i:j, "SOIL_TEMP_30CM_QA_FLAG"] = "SUSPECT"
        
        # Update codes. If previous code exists, new one is concatenated
        for (codes in inputFile[i:j, "SOIL_TEMP_30CM_QA_CODE"]){
          if (is.na(codes)) {
            inputFile[i:j, "SOIL_TEMP_30CM_QA_CODE"] = soil30StuckCode
          } else {
            inputFile[i:j, "SOIL_TEMP_30CM_QA_CODE"] = paste(inputFile[i:j, "SOIL_TEMP_30CM_QA_CODE"], soil30StuckCode, sep = ",")
          }
        }
      }
    }
  }
}

################################
##########Step check############
################################
# Change more than 2 degree per hour.

#inputFile <- read.csv("wind_speed_step_test.csv", stringsAsFactors = F)

for (i in 1:(nrow(inputFile) - 1)){
  
  j = i+1
  values = inputFile[i:j, "SOIL_TEMP_30CM"]
  timeDifference = inputFile[i:j, "TimeDiff"]
  
  # Check that the 2 records are all seperated by an hour.
  # Create flag
  dateFlag = ""
  
  for (records in timeDifference){
    if (is.na(records)){
      next()
    } else if (records != 1){
      dateFlag = FALSE
    }
  }
  
  if (dateFlag == FALSE){
    next()
    
  } else {
    
    if ('BAD' %in% inputFile[i:j, "SOIL_TEMP_30CM_QA_FLAG"] || 'NO_DATA' %in% inputFile[i:j, "SOIL_TEMP_30CM_QA_FLAG"]){
      next()
    } else {
      
      if (abs(values[1] - values[2]) > 2) {
        inputFile[i:j, "SOIL_TEMP_30CM_QA_FLAG"] = "SUSPECT"
        
        for (codes in inputFile[i:j, "SOIL_TEMP_30CM_QA_CODE"]){
          if (is.na(codes) || codes == ""){
            inputFile[i:j, "SOIL_TEMP_30CM_QA_CODE"] = soil30StepCode
          } else {
            inputFile[i:j, "SOIL_TEMP_30CM_QA_CODE"] = paste(inputFile[i:j, "SOIL_TEMP_30CM_QA_CODE"], soil30StepCode, sep = ",")
          }
        }
      }
    }
  }
}

#################################
##########Clean codes############
#################################
# Remove duplicate codes.
# Need to take into account when there are no QA codes in the field.

for (i in 1:nrow(inputFile)){
  if (!is.na(inputFile[i, "SOIL_TEMP_30CM_QA_CODE"])){
    uniqueCodeList = unique(unlist(strsplit(inputFile[i, "SOIL_TEMP_30CM_QA_CODE"], ",")))
    inputFile[i, "SOIL_TEMP_30CM_QA_CODE"] = paste(uniqueCodeList, collapse = ",")
  }
}
print ("soil 30cm checked")


#Define codes
soil10StepCode = "QA119"

################################
##########Step check############
################################
# Change more than 6 degree per hour.

#inputFile <- read.csv("wind_speed_step_test.csv", stringsAsFactors = F)

for (i in 1:(nrow(inputFile) - 1)){
  
  j = i+1
  values = inputFile[i:j, "SOIL_TEMP_10CM"]
  timeDifference = inputFile[i:j, "TimeDiff"]
  
  # Check that the 2 records are all separated by an hour.
  # Create flag
  dateFlag = ""
  
  for (records in timeDifference){
    if (is.na(records)){
      next()
    } else if (records != 1){
      dateFlag = FALSE
    }
  }
  
  if (dateFlag == FALSE){
    next()
    
  } else {
    
    if ('BAD' %in% inputFile[i:j, "SOIL_TEMP_10CM_QA_FLAG"] || 'NO_DATA' %in% inputFile[i:j, "SOIL_TEMP_10CM_QA_FLAG"]){
      next()
    } else {
      
      if (abs(values[1] - values[2]) > 6) {
        inputFile[i:j, "SOIL_TEMP_10CM_QA_FLAG"] = "SUSPECT"
        
        for (codes in inputFile[i:j, "SOIL_TEMP_10CM_QA_CODE"]){
          if (is.na(codes) || codes == ""){
            inputFile[i:j, "SOIL_TEMP_10CM_QA_CODE"] = soil10StepCode
          } else {
            inputFile[i:j, "SOIL_TEMP_10CM_QA_CODE"] = paste(inputFile[i:j, "SOIL_TEMP_10CM_QA_CODE"], soil10StepCode, sep = ",")
          }
        }
      }
    }
  }
}

#################################
##########Clean codes############
#################################
# Remove duplicate codes.
# Need to take into account when there are no QA codes in the field.

for (i in 1:nrow(inputFile)){
  if (!is.na(inputFile[i, "SOIL_TEMP_10CM_QA_CODE"])){
    uniqueCodeList = unique(unlist(strsplit(inputFile[i, "SOIL_TEMP_10CM_QA_CODE"], ",")))
    inputFile[i, "SOIL_TEMP_10CM_QA_CODE"] = paste(uniqueCodeList, collapse = ",")
  }
}
print ("soil 10cm checked")


#Define codes
windDirectionStuckCode = "QA113"
windDirectionStepCode = "QA114"

################################
#########Stuck check############
################################
# Same value reported for 18 hours.

# remove NA's in code column
inputFile["WIND_DIR_QA_CODE"] = NA

for (i in 1:(nrow(inputFile)-17)){
  
  # Collect next 18 records for each step in the loop
  # Ignore following process if any of the 18 Values are flagged as BAD or NO_DATA as cant be sure data is actually stuck
  # Include anything that is marked as SUSPECT
  j = i + 17 
  
  values = inputFile[i:j, "WIND_DIR"]
  timeDifference = inputFile[i:j, "TimeDiff"]
  
  # Check that the 18 records are all seperated by an hour.
  # Create flag
  dateFlag = ""
  
  for (records in timeDifference){
    if (is.na(records)){
      next()
    } else if (records != 1){
      dateFlag = FALSE
    }
  }
  
  #If gap between records is more than an hour, then skip to next set
  if (dateFlag == FALSE){
    next()
    
  } else {
    
    if ('BAD' %in% inputFile[i:j, "WIND_DIR_QA_FLAG"] || 'NO_DATA' %in% inputFile[i:j, "WIND_DIR_QA_FLAG"]){
      next()
    } else {
      
      # Check that all selected values are the same 
      if (length(unique(values)) == 1) {
        inputFile[i:j, "WIND_DIR_QA_FLAG"] = "SUSPECT"
        
        # Update codes. If previous code exists, new one is concatenated
        for (codes in inputFile[i:j, "WIND_DIR_QA_CODE"]){
          if (is.na(codes)) {
            inputFile[i:j, "WIND_DIR_QA_CODE"] = windDirectionStuckCode
          } else {
            inputFile[i:j, "WIND_DIR_QA_CODE"] = paste(inputFile[i:j, "WIND_DIR_QA_CODE"], windDirectionStuckCode, sep = ",")
          }
        }
      }
    }
  }
}

################################
##########Step check############
################################
# Wind speed > 10 and direction changing by more than 100 degree per hour

#inputFile <- read.csv("wind_speed_step_test.csv", stringsAsFactors = F)

for (i in 1:(nrow(inputFile) - 1)){
  
  j = i+1
  values = inputFile[i:j, "WIND_DIR"]
  timeDifference = inputFile[i:j, "TimeDiff"]
  
  # Check that the 2 records are all separated by an hour.
  # Create flag
  dateFlag = ""
  
  for (records in timeDifference){
    if (is.na(records)){
      next()
    } else if (records != 1){
      dateFlag = FALSE
    }
  }
  
  if (dateFlag == FALSE){
    next()
    
  } else {
    
    if ('BAD' %in% inputFile[i:j, "WIND_DIR_QA_FLAG"] || 'NO_DATA' %in% inputFile[i:j, "WIND_DIR_QA_FLAG"]){
      next()
    } else {
      angle = 180 - abs(abs(a1 - a2) - 180)
      #if (inputFile[i, "WIND_SPEED"] > 10 & abs(values[1] - values[2]) > 100) {
      if ( (inputFile[i, "WIND_SPEED"] > 10) & (180 - abs(abs(values[1] - values[2]) - 180) > 100) ) {
        inputFile[i:j, "WIND_DIR_QA_FLAG"] = "SUSPECT"
        
        for (codes in inputFile[i:j, "WIND_DIR_QA_CODE"]){
          if (is.na(codes) || codes == ""){
            inputFile[i:j, "WIND_DIR_QA_CODE"] = windDirectionStepCode
          } else {
            inputFile[i:j, "WIND_DIR_QA_CODE"] = paste(inputFile[i:j, "WIND_DIR_QA_CODE"], windDirectionStepCode, sep = ",")
          }
        }
      }
    }
  }
}

#################################
##########Clean codes############
#################################
# Remove duplicate codes.

for (i in 1:nrow(inputFile)){
  if (!is.na(inputFile[i, "WIND_DIR_QA_CODE"])){
    uniqueCodeList = unique(unlist(strsplit(inputFile[i, "WIND_DIR_QA_CODE"], ",")))
    inputFile[i, "WIND_DIR_QA_CODE"] = paste(uniqueCodeList, collapse = ",")
  }
}
print ("wind dir checked")

#Define codes
windSpeedStuckCode = "QA111"
windSpeedStepCode = "QA112"

################################
#########Stuck check############
################################
# Same value reported for 12 hours.

# remove NA's in code column
inputFile["WIND_SPEED_QA_CODE"] = NA

for (i in 1:(nrow(inputFile)-11)){
  
  # Collect next 12 records for each step in the loop
  # Ignore following process if any of the 12 Values are flagged as BAD or NO_DATA as cant be sure data is actually stuck
  # Include anything that is marked as SUSPECT
  j = i + 11 
  
  values = inputFile[i:j, "WIND_SPEED"]
  timeDifference = inputFile[i:j, "TimeDiff"]
  
  # Check that the 12 records are all seperated by an hour.
  # Create flag
  dateFlag = ""
  
  for (records in timeDifference){
    if (is.na(records)){
      next()
    } else if (records != 1){
      dateFlag = FALSE
    }
  }
  
  #If gap between records is more than an hour, then skip to next set
  if (dateFlag == FALSE){
    next()
    
  } else {
    
    if ('BAD' %in% inputFile[i:j, "WIND_SPEED_QA_FLAG"] || 'NO_DATA' %in% inputFile[i:j, "WIND_SPEED_QA_FLAG"]){
      next()
    } else {
      
      # Check that all selected values are the same 
      if (length(unique(values)) == 1) {
        inputFile[i:j, "WIND_SPEED_QA_FLAG"] = "SUSPECT"
        
        # Update codes. If previous code exists, new one is concatenated
        for (codes in inputFile[i:j, "WIND_SPEED_QA_CODE"]){
          if (is.na(codes)) {
            inputFile[i:j, "WIND_SPEED_QA_CODE"] = windSpeedStuckCode
          } else {
            inputFile[i:j, "WIND_SPEED_QA_CODE"] = paste(inputFile[i:j, "WIND_SPEED_QA_CODE"], windSpeedStuckCode, sep = ",")
          }
        }
      }
    }
  }
}

################################
##########Step check############
################################
# Change more than 25% per hour.

#inputFile <- read.csv("wind_speed_step_test.csv", stringsAsFactors = F)

for (i in 1:(nrow(inputFile) - 1)){
  
  j = i+1
  values = inputFile[i:j, "WIND_SPEED"]
  timeDifference = inputFile[i:j, "TimeDiff"]
  
  # Check that the 2 records are all seperated by an hour.
  # Create flag
  dateFlag = ""
  
  for (records in timeDifference){
    if (is.na(records)){
      next()
    } else if (records != 1){
      dateFlag = FALSE
    }
  }
  
  if (dateFlag == FALSE){
    next()
    
  } else {
    
    if ('BAD' %in% inputFile[i:j, "WIND_SPEED_QA_FLAG"] || 'NO_DATA' %in% inputFile[i:j, "WIND_SPEED_QA_FLAG"]){
      next()
    } else {
      
      if (abs(values[1] - values[2]) > 25) {
        inputFile[i:j, "WIND_SPEED_QA_FLAG"] = "SUSPECT"
        
        for (codes in inputFile[i:j, "WIND_SPEED_QA_CODE"]){
          if (is.na(codes) || codes == ""){
            inputFile[i:j, "WIND_SPEED_QA_CODE"] = windSpeedStepCode
          } else {
            inputFile[i:j, "WIND_SPEED_QA_CODE"] = paste(inputFile[i:j, "WIND_SPEED_QA_CODE"], windSpeedStepCode, sep = ",")
          }
        }
      }
    }
  }
}

#################################
##########Clean codes############
#################################
# Remove duplicate codes.

for (i in 1:nrow(inputFile)){
  if (!is.na(inputFile[i, "WIND_SPEED_QA_CODE"])){
    uniqueCodeList = unique(unlist(strsplit(inputFile[i, "WIND_SPEED_QA_CODE"], ",")))
    inputFile[i, "WIND_SPEED_QA_CODE"] = paste(uniqueCodeList, collapse = ",")
  }
}
print ("wind speed checked")

#Extract to CSV
write.csv(inputFile, file = "Bure_Marshes_suspect_error_test.csv", na = "")

print ("file written")

