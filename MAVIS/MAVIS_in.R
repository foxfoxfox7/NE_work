library(tidyverse)
library(readxl)
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)
library(stringdist)
library(openxlsx)
library(plyr)

################################################################################
# My functions
################################################################################

rename <- function(input) {
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

missing_names <- function(input) {
  # Finds all the names that can't be automatically converted
  if ( (!input %in% list_of_species) & (!is.na(input)) ) {
    list_of_matches <- stringdist(input, list_of_species, method='jw')
    if (min(list_of_matches) < naming_cutoff) {
      return(NA)
    } else {
      return(input)
    }
  } else {
    return(NA)
  }
}

convert_freq <- function(input) {
  # Function which converts the frequency column into the MAVIS scale of I-V
  if (input<2) {
    return(1)
  } else if (input<3) {
    return(2)
  } else if (input<6) {
    return(3)
  } else if (input<12) {
    return(4)
  } else {
    return(5)
  }
}

################################################################################
# Setting up the environment
################################################################################

# Working folder.
# CHANGE THIS TO THE FOLDER YOU ARE WORKING ON IN YOUR COMPUTER !!!!!!!!!!!!!!
setwd('//CAM381FS/x955120$/Projects/LTMN_veg')
getwd()

# How close the species name needs to be for it to be automatically changed
naming_cutoff <- 0.15

file_dir <- './Data/'
nvc_dir <- './NVC_input/'
ge_dir <- './GE_input/'
dat_dir <- './R_dat/'

dir.create(nvc_dir)
dir.create('NVC_output')
dir.create(ge_dir)
dir.create('GE_output')
dir.create(dat_dir)

# An empty list to store survey by survey diagnostics
dlis_pc = list()
dlis_names = list()
dlis_miss_names = list()

################################################################################
# Getting basic data
################################################################################

# Getting the list of surveys
list_of_files <- list.files(file_dir)
N_files <- length(list_of_files)

# Getting data from the lookup tab in a spreadsheet
lookups <- read_csv('species_list.csv')
list_of_species <- lookups$Name

name_swap <- read.csv('rename.csv')
name_swap_vector <- with(name_swap, setNames(right_name, wrong_name))

################################################################################
# Looping through the sureys making edits
################################################################################
#N_files
for (ii in 1:N_files) {

  survey_name <- list_of_files[ii]

  file_path <- paste(file_dir, survey_name, sep='')
  sheets <- excel_sheets(file_path)
  species <- read_excel(file_path, sheet = "Species Template")

  # Creating the file name for mavis input
  #file_root <- gsub('.{23}$', '', survey_name)
  file_root <- gsub('.{5}$', '', survey_name)
  print(file_root)

  # Getting the names of the columns
  col_names <- names(species)

  # The list of columns I want to keep. only part of the string
  keep_list <- c('PLOT_ID', 'DESC', 'CELL', 'PERCENT', 'FREQ')
  # Reducing the size of the tibble to only the kept columns
  small_tib <- species %>%
    select(contains(keep_list))
  # There are often many trailing empyty rows
  small_tib <- small_tib[!is.na(small_tib$DESC_LATIN), ]

  ##############################################################################
  # changing the plot ID and making a dicionary of changes
  #

  # Getting the plot IDs and making a dictionary of name swaps
  small_tib$PLOT_ID <- as.character(small_tib$PLOT_ID)
  # adding a temp string to the plot ids so they have unique swaps
  small_tib$PLOT_ID <- paste(small_tib$PLOT_ID, 'temp', sep='-')

  plotids <- unique(small_tib[['PLOT_ID']])
  num_plots <- length(plotids)
  name_dict <- setNames(plotids, 1:num_plots)
  # Making the swaps
  for (jj in 1:num_plots) {
    small_tib$PLOT_ID <- replace(
      small_tib$PLOT_ID,
      small_tib$PLOT_ID == name_dict[jj], names(name_dict[jj])
    )
  }

  # SAving the dictionary
  saveRDS(name_dict, file=paste(dat_dir, file_root, '.RData', sep=''))

  ##############################################################################
  # Fixing errors with names
  #

  # Making the manual swaps
  for (jj in 1:length(name_swap_vector)) {
    small_tib$DESC_LATIN <- replace(
      small_tib$DESC_LATIN,
      small_tib$DESC_LATIN == names(name_swap_vector[jj]), name_swap_vector[jj]
      )
  }
  # Getting the names that can't be fixed
  small_tib$broke_names <- map_chr(small_tib$DESC_LATIN, missing_names)
  # Adding it to a separate df earl as they get deleted form small tib soon
  diag_dat_miss_names <- data.frame(broke_names = small_tib$broke_names)
  dlis_miss_names[[ii]] <- diag_dat_miss_names

  # Fixing the names
  mis_name_pre <- small_tib$DESC_LATIN
  # converts them to nearest name in a list of accepted names
  small_tib$DESC_LATIN <- small_tib$DESC_LATIN %>%
    map_chr(., rename)
  mis_name_post <- small_tib$DESC_LATIN

  ##############################################################################
  # Fixing errors with percent cover and requency
  #

  # FIXING THE FREQUENCY COLUMNS
  # Getting the names of the CELL columns
  cell_names <- col_names[grep('CELL', col_names)]
  # Changing all missing data in CELL columns to 0
  small_tib[cell_names][is.na(small_tib[cell_names])] <- 0
  # Calculating FREQ from the contents of CELL
  small_tib <- small_tib %>%
    mutate(FREQ2 = rowSums(.[cell_names]))

  # turning FREQ into mavis values 1-V
  small_tib$new_freq <- small_tib$FREQ2 %>%
    map_dbl(., convert_freq)


  # FIXING THE PERCENT COVER COLUMNS

  ##############################################################################
  # Creating the output for the nvc MAVIS input
  #
  nvs_cols <- c('PLOT_ID', 'DESC', 'new_freq')
  df_nvc <- small_tib %>%
    select(contains(nvs_cols))
  # Ading the G to the plot ID col
  df_nvc$PLOT_ID <- paste("G", df_nvc$PLOT_ID, sep="")

  # Making the file name
  nvc_file_name <- paste(file_root, 'MAVIS_groups.txt', sep='_')
  # Writing the nvc groups input file
  write_delim(
    df_nvc,
    file = paste(nvc_dir, nvc_file_name, sep=''),
    delim = " ",
    na = "NA",
    col_names = FALSE
  )
  # Removing all the quotation marks
  tx  <- readLines(paste(nvc_dir, nvc_file_name, sep=''))
  tx2  <- gsub(pattern = "\"", replace = "", x = tx)
  writeLines(tx2, con=paste(nvc_dir, nvc_file_name, sep=''))

  ##############################################################################
  # Creating the output for the grimes and ellenberg MAVIS input
  #
  nvs_cols <- c('PLOT_ID', 'DESC', 'PERCENT')
  df_nvc <- small_tib %>%
    select(contains(nvs_cols))
  # Making the file name
  ge_file_name <- paste(file_root, 'MAVIS_plots.txt', sep='_')
  # Writing the ge input file
  write_delim(
    df_nvc,
    file = paste(ge_dir, ge_file_name, sep=''),
    delim = " ",
    na = "NA",
    col_names = FALSE
  )
  # Removing all the quotation marks
  tx  <- readLines(paste(ge_dir, ge_file_name, sep=''))
  tx2  <- gsub(pattern = "\"", replace = "", x = tx)
  writeLines(tx2, con=paste(ge_dir, ge_file_name, sep=''))

  ##############################################################################
  # Diagnositcs input
  #
  diag_dat_pc <- data.frame(site = file_root,
                            pc_cover_miss = sum(is.na(small_tib$PERCENT_COVER)),
                            total_inst = dim(small_tib)[1])
  dlis_pc[[ii]] <- diag_dat_pc

  # Getting the lists of changed species
  diag_dat_names <- data.frame(pre_name_lst = mis_name_pre,
                               post_name_lst = mis_name_post)
  dlis_names[[ii]] <- diag_dat_names
}

################################################################################
# Diagnositcs output
################################################################################

diagnostics_pc <- bind_rows(dlis_pc)
diagnostics_names <- bind_rows(dlis_names)
diagnostics_miss_names <- bind_rows(dlis_miss_names)

# Creating the list of unique name changes
names_dif <- diagnostics_names %>%
  as_tibble(.) %>%
  subset(., .$pre_name_lst != .$post_name_lst)
names_unique <- distinct(names_dif)

# Counting the number of times each unique swap happens
names_count <- dplyr::count(names_dif, pre_name_lst, post_name_lst)
# combining and ordering
name_changes <- full_join(names_unique, names_count) %>%
  arrange(desc(n))
print(name_changes)
write.csv(name_changes, file = "name_changes.csv", row.names=FALSE)

# Making a list of all the names that couldn't be converted
miss_names <- diagnostics_miss_names %>%
  as_tibble(.) %>%
  subset(., !is.na(.$broke_names)) %>%
  dplyr::count(., broke_names) %>%
  arrange(., desc(n))
print(miss_names)
write.csv(miss_names, file = "miss_names.csv", row.names=FALSE)
