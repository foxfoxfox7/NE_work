library(tidyverse)
library(readxl) # reading in the excel files
library(stringr) # capitalising the first letter of a string
library(openxlsx) # writing the data to an excel file
library(stringdist) # matching strings in the rename function
library(plyr) # mapvalues in the name swap
library(googlesheets4) # for writing to google sheets
library(googledrive) # for moving the subsequent google sheet to the right dir



# This is for if you want to make the outputs into googlesheets
google_dir <- 'https://drive.google.com/drive/folders/1dCINS3zQFK0M_YTfwIeVFPEHvVTjQH29/'
drive_auth()
gs4_auth(token = drive_token())

# CHANGE THIS TO THE FOLDER YOU ARE WORKING IN ON YOUR COMPUTER !!!!!!!!!!!!!!
laptop_dir <- 'C:/Users/kiera/Work/NE_work/Species_list'
setwd(laptop_dir)

dir <- './dataframes/'

################################################################################
# Functions
################################################################################

rename <- function(input, list_of_species, naming_cutoff = 0.1) {
  # Makes automatic name changes where the name is close to the names in a list
  # of accepted names in MAVIS
  if ( (!input %in% list_of_species) & (!is.na(input)) ) {
    list_of_matches <- stringdist(input, list_of_species, method='jw')
    if (min(list_of_matches) < naming_cutoff) {
      new_name <- list_of_species[which.min(list_of_matches)]
      return(new_name)
    } else {
      return(NA)
    }
  } else {
    return(input)
  }
}

append_str <- function(input, suffix) {
  # Appends ' sp.' after species names when there is only one word
  # because it is a genus
  if (!grepl(' ', input)) {
    return(paste(input, suffix, sep = ''))
  } else {
    return(input)
  }
}

append_str2 <- function(input, find, suffix) {
  # usesd for adding a '.' after a 'sp'
  reg <- paste(find, '$', sep='')
  
  if (grepl(reg, input)) {
    return(paste(input, suffix, sep = ''))
  } else {
    return(input)
  }
}

################################################################################
# extracting the names from the BSBI and VESPAN dictionaries
# This has already been done but this code is kept in case the list is lost
################################################################################

# BSBI_species <- read_excel(paste(dir, 'BSBI_plant_names.xlsx', sep=''), sheet = "LIST2007")
# B_sList <- BSBI_species[['Species']]
# 
# VESPAN_species <- read.csv(paste(dir, 'ECN_VESPAN_dictionary.csv', sep=''))
# V_sList <- VESPAN_species[['DESC_LATIN']]
# 
# full_species_list <- c(B_sList, V_sList) %>%
#   map_chr(append_str2, ' sp', '.') %>%
#   c(., unique(gsub("([A-Za-z]+).*", "\\1", .))) %>%
#   map_chr(append_str, ' sp.') %>%
#   c(., unique(gsub("([A-Za-z]+).*", "\\1", .))) %>%
#   map_chr(append_str, ' seedling') %>%
#   unique() %>%
#   sort()
# 
# full_species_df <- data.frame(All_species = full_species_list)
# spec_list_gs <- gs4_create("full_species_list", sheets = list(All_species = full_species_df))
# drive_mv("full_species_list", google_dir2)
# 
# write.csv(full_species_df, paste0(dir, 'full_species_list.csv'), row.names=FALSE)

################################################################################
# Setup
################################################################################

# Getting out list of all the species that are accepted
dat <- read.csv(paste(dir, 'full_species_list.csv', sep=''), header = TRUE)
# the first column is from BSBI and VESPAN
# the second column is species we have added
full_species_list <- c(dat[['All_species']],
                       dat[['Added_species']][(dat[['Added_species']] != '')])

# reading in our list of typos to change
name_swap <- read.csv(paste0(dir, 'rename.csv'))

file_dir <- '../Data/'
list_of_files <- list.files(file_dir)
N_files <- length(list_of_files)
print(list_of_files)

survey_list_full = list()
survey_list = list()
sitecode_list = list()

################################################################################
# reading in the data
################################################################################

for (ii in 1:N_files) {
  
  survey_name <- list_of_files[ii]
  print(survey_name)
  file_path <- paste(file_dir, survey_name, sep='')
  species <- read_excel(file_path, sheet = "Species Template")
  
  site <- species[['SITECODE']][1]
  year <- species[['YEAR']][1]
  survey <- paste(site, year, sep = '_')
  
  species[['DESC_LATIN']] <- trimws(species[['DESC_LATIN']], which = "both")
  species[['DESC_LATIN']] <- str_to_sentence(species[['DESC_LATIN']])
  
  survey_list_full[[survey]] <- species[['DESC_LATIN']]
  
  species <- species[!is.na(species[['DESC_LATIN']]), ]
  survey_list[[survey]] <- sort(unique(species[['DESC_LATIN']]))
  
  sitecode_list[[ii]] <- site
}

################################################################################
# site specific lists
################################################################################

surveys <- names(survey_list)
sitecodes <- unique(unlist(sitecode_list))

site_list = list()
for (ii in 1:length(sitecodes)) {
  
  site <- sitecodes[[ii]]
  survey_nums <- grep(sitecodes[ii], surveys)
  
  site_list[[site]] <- sort(unique(as.vector(unlist(survey_list[survey_nums]))))
  #site_list[[site]] <- sort(unique(as.vector(unlist(site_list[[site]]))))
}

all_spec <- list("All species" = sort(unique(as.vector(unlist(site_list)))))

################################################################################
# Fixing the names
################################################################################

all_spec[[1]] <- all_spec[[1]] %>%
  map_chr(append_str, ' sp.') %>%
  map_chr(append_str2, ' sp', '.') %>%
  unique() %>%
  mapvalues(name_swap[['wrong_name']], name_swap[['right_name']], warn_missing = FALSE)

site_list2 <- site_list %>%
  lapply(map_chr, append_str, ' sp.') %>%
  lapply(map_chr, append_str2, ' sp', '.') %>%
  lapply(unique) %>%
  lapply(mapvalues, name_swap[['wrong_name']], name_swap[['right_name']], warn_missing = FALSE)

survey_list <- survey_list %>%
  lapply(map_chr, append_str, ' sp.') %>%
  lapply(map_chr, append_str2, ' sp', '.') %>%
  lapply(unique) %>%
  lapply(mapvalues, name_swap[['wrong_name']], name_swap[['right_name']], warn_missing = FALSE)

survey_list_full <- survey_list_full %>%
  lapply(map_chr, append_str, ' sp.') %>%
  lapply(map_chr, append_str2, ' sp', '.') %>%
  # Na get converted to 'NA sp.' so they need to be converted back to blank
  lapply(function(x) {
    gsub("NA sp.", NA, x)
    }) %>%
  lapply(mapvalues, name_swap[['wrong_name']], name_swap[['right_name']], warn_missing = FALSE)

#############################################################################
# printing the lists
################################################################################

# Converts our lists of lists to dataframes and fills in the gaps left by
# the different sized lists with blanks
site_list_df <- data.frame(lapply(site_list, "length<-", max(lengths(site_list))))
survey_list_df <- data.frame(lapply(survey_list, "length<-", max(lengths(survey_list))))
survey_list_df <- data.frame(lapply(survey_list_full, "length<-", max(lengths(survey_list_full))))

# creates a list of all our information ready to go into the excel file
species_sheets <- list(All_species = all_spec,
                       Sites = site_list_df,
                       Surveys_unique = survey_list_df,
                       Surveys = survey_list_df)

write.xlsx(species_sheets, file = "Species_lists.xlsx")


# This finds all the names in our list of all surveyed species that aren't
# in our list of accepted names
missing_names <- data.frame(
  missing_names = sort(setdiff(all_spec[[1]], full_species_list))
)
print(missing_names[['missing_names']])
write.csv(missing_names, 'Missing_species.csv', row.names = FALSE)

