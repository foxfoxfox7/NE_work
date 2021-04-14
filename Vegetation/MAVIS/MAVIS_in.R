library(tidyverse)
library(readxl)
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)
library(stringdist)
library(openxlsx)
library(plyr)
library(sjmisc) # str_contains

################################################################################
# My functions
################################################################################

rename <- function(input, list_of_species) {
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

missing_names <- function(input, list_of_species) {
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

append_str <- function(input, suffix) {
  # Appends ' sp.' after species names when there is only one word
  # because it is a genus
  if (!grepl(' ', input)) {
    return(paste(input, suffix, sep = ''))
  } else {
    return(input)
  }
}

rem_dom <- function(input) {
  # removes the wrong domain names from the tree desc_latin
  if (sjmisc::str_contains(input, 'seedling/sp')) {
    return(gsub("\\s*\\([^\\)]+\\)", "", input))
  } else {
    return(input)
  }
}

################################################################################
# Setting up the environment
################################################################################

# Working folder.
# CHANGE THIS TO THE FOLDER YOU ARE WORKING ON IN YOUR COMPUTER !!!!!!!!!!!!!!
setwd('C:/Users/kiera/Work/NE_work/MAVIS/')
getwd()

df_dir <- './dataframes/'

# How close the species name needs to be for it to be automatically changed
naming_cutoff <- 0.15

#file_dir <- '../Data/'
file_dir <- './Data_set_change/'
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
tree_list = list()

################################################################################
# Getting basic data
################################################################################

# Getting the list of surveys
list_of_files <- list.files(file_dir)
N_files <- length(list_of_files)

# Getting data from the lookup tab in a spreadsheet
lookups <- read_csv(paste0(df_dir, 'species_list.csv'))
MAVIS_species <- lookups$Name

name_swap <- read.csv(paste0(df_dir, 'rename.csv'))
tree_name_swap <- read.csv(paste0(df_dir, 'tree_rename.csv'))

# strings used for the tree section
wrong_sp <- c('sp', 'spp', 'sp.', 'spp.', 'seedling', 'sp/seedling', 'seedling/sp')
DOM_codes <- c('C', 'S', 'I', 'U', 'H', 'A', 'E')

################################################################################
# Looping through the sureys making edits
################################################################################

for (ii in 1:N_files) {

  survey_name <- list_of_files[ii]
  file_path <- paste(file_dir, survey_name, sep='')
  species <- read_excel(file_path, sheet = "Species Template")

  # Creating the file name for mavis input. 5 characters is '.xlsx'
  file_root <- gsub('.{5}$', '', survey_name)
  print(file_root)

  # Getting the names of the columns
  col_names <- names(species)

  # The list of columns I want to keep. only part of the string
  # Reducing the size of the tibble to only the kept columns
  small_tib <- select(species, contains(c('PLOT_ID', 'DESC', 'CELL', 'PERCENT', 'FREQ'))) %>%
    # There are often many trailing empty rows
    .[!is.na(.$DESC_LATIN), ]
  
  small_tib$PLOT_ID <- as.character(small_tib$PLOT_ID)
  small_tib$PERCENT_COVER <- as.numeric(small_tib$PERCENT_COVER)
  
  ##############################################################################
  # Getting the tree species
  #
  
  # getting only the important columns from the tree sheet
  # dominannce sheet has the information in the same format as spec template
  tree <- read_excel(file_path, sheet = "Dominance Template") %>%
    select(., contains(c('PLOT_ID', 'DESC_LATIN', 'DOM_CODE'))) %>%
    .[!is.na(.$DESC_LATIN), ]
  
  tree[['PLOT_ID']] <- as.character(tree[['PLOT_ID']])
  tree[['DOM_CODE']] <- as.character(tree[['DOM_CODE']])
  
  # there are lots of typos around sp. for trees in needs to be 'seedling/sp'
  tree[['DESC_LATIN']] <- tree[['DESC_LATIN']] %>%
    gsub("[.]", '', .) %>%
    gsub("[/]", '', .)
  
  for (jj in 1:length(wrong_sp)) {
    tree[['DESC_LATIN']] <- sub(wrong_sp[jj], '', tree[['DESC_LATIN']])
  }
  # Getting rid of the wrong mavis dominance codes
  # needs to be done early to add the appropriate sp/seedling
  tree[['DESC_LATIN']] <- tree[['DESC_LATIN']] %>%
    str_trim() %>%
    gsub("\\s*\\([^\\)]+\\)", "", .) %>%
    map_chr(., append_str, ' seedling/sp')
  
  # sometimes the dominance code isnt one of the accepted ones but still
  # can be understood
  tree[['DOM_CODE']] <- mapvalues(tree[['DOM_CODE']], 
                                  c('Tree'), 
                                  c('C'), 
                                  warn_missing = FALSE)
  
  # sometimes the dominance codes are combined into one col
  for (jj in 1:length(DOM_codes)) {
    tree <- tree %>%
      bind_rows(., filter(., str_detect(DOM_CODE, DOM_codes[jj])) %>%
                  mutate(DOM_CODE = DOM_codes[jj])) %>%
      arrange(PLOT_ID)
  }
  # removes all the combined codes
  tree <-  filter(tree, DOM_CODE %in% c(DOM_codes, NA))
  
  tree[['PERCENT_COVER']] <- tree[['DOM_CODE']] %>%
    mapvalues(.,
      c('C', 'S', 'I', 'U', 'H', 'A', 'E'),
      c(100, 80, 70, 60, 30, 5, 2),
      warn_missing = FALSE) %>%
    as.numeric(.)
  
  tree[['FREQ2']] <- tree[['DOM_CODE']] %>%
    mapvalues(.,
      c('C', 'S', 'I', 'U', 'H', 'A', 'E'),
      c(25, 20, 18, 15, 5, 1, 1),
      warn_missing = FALSE) %>%
    as.numeric(.)
  
  # converts the dominance codes into codes recognised by MAVIS
  tree[['DOM_CODE']] <- mapvalues(tree[['DOM_CODE']],
                                 c('C', 'S', 'I', 'U', 'H', 'A', 'E'),
                                 c('(c)', '(c)', '(c)', '(c)', '(s)', '(g)', '(g)'),
                                 warn_missing = FALSE)

  # adding back the new correct dominance codes
  tree[['DESC_LATIN']] <- paste(tree[['DESC_LATIN']], tree[['DOM_CODE']], sep=' ') %>%
  # some of the dom codes are missing so remove the subsequent NA
    sub(' NA', '', .) %>%
  # removes the dom codes after spp/seedling
    map_chr(., rem_dom) %>%
    mapvalues(., 
              name_swap[['wrong_name']], 
              name_swap[['right_name']], 
              warn_missing = FALSE)

  # removing the superfluous dom code column now
  tree <- tree[ , !(names(tree) %in% 'DOM_CODE')] %>%
  # find unique values
    .[!duplicated(.), ]

  tree_list[[ii]] <- tree
  
  small_tib <- full_join(small_tib, tree, by = c("PLOT_ID", "DESC_LATIN", "PERCENT_COVER")) %>%
    arrange(PLOT_ID)
  
  ##############################################################################
  # changing the plot ID and making a dictionary of changes
  #

  # Getting the plot IDs and making a dictionary of name swaps
  # adding a temp string to the plot ids so they have unique swaps
  small_tib$PLOT_ID <- paste(small_tib$PLOT_ID, 'temp', sep='-')

  plotids <- unique(small_tib[['PLOT_ID']])
  name_dict <- setNames(plotids, 1:length(plotids))

  small_tib$PLOT_ID<- mapvalues(small_tib$PLOT_ID, 
                                   unlist(name_dict), 
                                   names(name_dict), 
                                   warn_missing = FALSE)

  # SAving the dictionary
  saveRDS(name_dict, file=paste(dat_dir, file_root, '.RData', sep=''))
  
  test_df <- small_tib

  ##############################################################################
  # Fixing errors with names
  #

  small_tib$DESC_LATIN<- mapvalues(small_tib$DESC_LATIN, 
                                   name_swap[['wrong_name']], 
                                   name_swap[['right_name']], 
                                   warn_missing = FALSE)
  
  # Getting the names that can't be fixed into a df to be combined later
  diag_dat_miss_names <- data.frame(broke_names =  
                    map_chr(small_tib$DESC_LATIN, missing_names, MAVIS_species))
  dlis_miss_names[[ii]] <- diag_dat_miss_names

  # Fixing the names, converts them to nearest name in a list of accepted names
  mis_name_pre <- small_tib$DESC_LATIN
  small_tib$DESC_LATIN <- map_chr(small_tib$DESC_LATIN, rename, MAVIS_species)
  mis_name_post <- small_tib$DESC_LATIN
  
  # Getting the lists of changed species
  diag_dat_names <- data.frame(pre_name_lst = mis_name_pre,
                               post_name_lst = mis_name_post)
  dlis_names[[ii]] <- diag_dat_names

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

  # turning FREQ into mavis values I-V
  small_tib$new_freq <- small_tib$FREQ2 %>%
    map_dbl(., convert_freq)


  # FIXING THE PERCENT COVER COLUMNS
  
  
  # Calculating how many species dont have a percentage cover
  diag_dat_pc <- data.frame(site = file_root,
                            pc_cover_miss = sum(is.na(small_tib$PERCENT_COVER)),
                            total_inst = dim(small_tib)[1])
  dlis_pc[[ii]] <- diag_dat_pc

  ##############################################################################
  # Creating the output for the nvc MAVIS input
  #
  df_nvc <- select(small_tib, contains(c('PLOT_ID', 'DESC', 'new_freq')))
  # Ading the G to the plot ID col
  df_nvc$PLOT_ID <- paste0("G", df_nvc$PLOT_ID)

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

  # Removing all the quotation marks in the output file
  tx  <- readLines(paste(nvc_dir, nvc_file_name, sep='')) %>%
    gsub(pattern = "\"", replace = "", x = .) %>%
    writeLines(., con=paste(nvc_dir, nvc_file_name, sep=''))

  ##############################################################################
  # Creating the output for the grimes and ellenberg MAVIS input
  #
  df_ge <-select(small_tib, contains(c('PLOT_ID', 'DESC', 'PERCENT')))
  # Making the file name
  
  ge_file_name <- paste(file_root, 'MAVIS_plots.txt', sep='_')
  # Writing the ge input file
  write_delim(
    df_ge,
    file = paste(ge_dir, ge_file_name, sep=''),
    delim = " ",
    na = "NA",
    col_names = FALSE
  )

  # Removing all the quotation marks in the output file
  tx  <- readLines(paste(ge_dir, ge_file_name, sep='')) %>%
    gsub(pattern = "\"", replace = "", x = .) %>%
    writeLines(., con=paste(ge_dir, ge_file_name, sep=''))

}

################################################################################
# Diagnositcs output
################################################################################

diagnostics_pc <- bind_rows(dlis_pc)
diagnostics_names <- bind_rows(dlis_names)
diagnostics_miss_names <- bind_rows(dlis_miss_names)

# this part finds all the tree names that aren't in the list of species
all_trees <- bind_rows(tree_list)
tree_df <- data.frame(wrong_name = sort(setdiff(unique(all_trees[['DESC_LATIN']]), MAVIS_species)),
                      right_name = NA)
write.csv(tree_df, 'new_tree_names.csv', row.names = FALSE)


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
