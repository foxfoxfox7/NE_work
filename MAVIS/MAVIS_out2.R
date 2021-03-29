library(tidyverse)
library(tokenizers)
library(readtext)
library(naniar)
library(readxl)
library(openxlsx)

################################################################################
# My functions
################################################################################

# Finds the competition value in the MAVIS output. there are often other 'c'
# values in the output which makes it less simple
get_comp <- function(set) {
  for (kk in 1:(length(set)-2)) {
    if ( (set[kk] == 'c') & (set[kk+1] == ':') ) {
      competition_index <- (kk+2)
      return(set[competition_index])
    }
  }
  return(NA)
}

################################################################################
# Setting up the environment
################################################################################

setwd('C:/Users/kiera/Work/NE_work/MAVIS/')
getwd()

nvc_dir <- './NVC_output/'
ge_dir <- './GE_output/'
dat_dir <- './R_dat/'
data <- './Data_test/'
MAVIS_data <- './MAVIS_data/'

nvc_files <- readtext(nvc_dir)
N_nvc <- dim(nvc_files)[1]

ge_files <- readtext(ge_dir)
N_ge <- dim(ge_files)[1]

dat_files <- list.files(dat_dir)
N_dat <- length(dat_files)

data_files <- list.files(data)
N_data <- length(data_files)

# The Dat file is the file generated from MAVIS in which changes the 
# plot id that come from mavis, back to the original plot ids
if ((N_nvc != N_ge) | (N_ge != N_dat) | (N_dat != N_data)) {
  stop('There should be the same number of NVC, GE, Dat and Survey files')
}

dir.create(MAVIS_data)
print(nvc_files)
################################################################################
# Looping through the sureys making edits
################################################################################

for (ii in 1:dim(nvc_files)[1]) {

  file_root <- gsub('.{5}$', '', data_files[ii])

  print(data_files[ii])
  print(dat_files[ii])
  print(nvc_files$doc_id[ii])
  print(ge_files$doc_id[ii])

  ##############################################################################
  # Getting the NVC info out of the mavis outputs
  #

  # Getting the paragraphs form the NVC output
  nvc_para <- tokenize_paragraphs(nvc_files$text[ii])
  # Deletes the first paragraph which is just the survey name
  nvc_para <- nvc_para[[1]][-1]

  nvc_plots_list <- list()
  for (jj in 1:length(nvc_para)) {
    # extractes the paragraphs in the nvc output
    # and turns them into a list of words
    nvc_set <- tokenize_words(nvc_para[jj])[[1]]
    
    plot_nvc <- list('PLOT_ID' = nvc_set[2])
    
    for (kk in 1:10) {
      
      # These capitalise the first letters in the NVC groups
      # but not the letters after the number
      nvc_class <- paste0(toupper(str_extract(nvc_set[3*kk+1], '^(.*?)\\D+')), 
                          gsub('^(.*?)\\D+', '', nvc_set[3*kk+1]))
      # This finds the percentage match
      nvc_conf <- nvc_set[3*kk+2]
      # and adds them both to the list as one, separated by a colon
      plot_nvc[[paste0('NVC', as.character(kk))]] <- paste(nvc_class, 
                                                           nvc_conf,
                                                           sep=':')
    }
    
    # converting the list into a df
    nvc_plots_df <- data.frame(plot_nvc)
    nvc_plots_list[[jj]] <- nvc_plots_df
  }
  nvc_survey <- bind_rows(nvc_plots_list) %>%
    tibble(.)

  # where it fails to classify, it fills the wrong column with rubbish
  nvc_survey <- nvc_survey %>%
    replace_with_na(replace = list(NVC1 = 'FAILED:to',
                                   NVC2 = 'NO:species',
                                   NVC3 = 'NANA:NA'))

  ##############################################################################
  # Getting the grimes and ellenberg info out of the mavis outputs
  #

  # Getting the paragraphs form the NVC output
  ge_para <- tokenize_paragraphs(ge_files$text[ii])
  # Deletes the first paragraph whihc is just the survey name
  ge_para <- ge_para[[1]][-1]

  ge_plots_list <- list()
  for (jj in 1:length(ge_para)) {
    ge_set <- tokenize_words(ge_para[jj],  strip_punct = FALSE)
    ge_set <- ge_set[[1]]

    ge_plots_df <- data.frame(PLOT_ID = ge_set[match('plot', ge_set) +1],
                              light = ge_set[match('light', ge_set) +1],
                              wetness = ge_set[match('wetness', ge_set) +1],
                              ph = ge_set[match('ph', ge_set) +1],
                              fertility = ge_set[match('fertility', ge_set) +1],
                              competition = get_comp(ge_set),
                              stress = ge_set[match('s', ge_set) +2],
                              ruderals = ge_set[match('r', ge_set) +2]
    )
    ge_plots_list[[jj]] <- ge_plots_df
  }
  ge_survey <- bind_rows(ge_plots_list)
  ge_survey <- tibble(ge_survey)

  ge_survey[ge_survey == '0.0'] <- NA

  ##############################################################################
  # Converting the plotids to the old ones
  #

  rdat_file <- paste(dat_dir, dat_files[ii], sep = '')
  #rdat_file <- paste(dat_dir, file_root, '.RData', sep='')
  name_dict <- readRDS(rdat_file)

  # Making the swaps
  for (jj in 1:length(name_dict)) {
    # Have to go backwards through the list to not keep changing the same one
    for (kk in dim(nvc_survey)[1]:1) {
      # Searches for only one change then breaks
      if (nvc_survey$PLOT_ID[kk] == names(name_dict[jj])) {
        nvc_survey$PLOT_ID[kk] <- name_dict[jj]
        break
      }
    }
    # Doing the same but for the GE output now
    for (kk in dim(ge_survey)[1]:1) {
      # Searches for only one change then breaks
      if (ge_survey$PLOT_ID[kk] == names(name_dict[jj])) {
        ge_survey$PLOT_ID[kk] <- name_dict[jj]
        break
      }
    }
  }

  # Removing the -temp from the end of the plot_IDs
  nvc_survey$PLOT_ID <- gsub('.{5}$', '', nvc_survey$PLOT_ID)
  ge_survey$PLOT_ID <- gsub('.{5}$', '', ge_survey$PLOT_ID)

  ##############################################################################
  # formlating the finalised dataframe for input to excel
  #

  data_file <- paste(data, data_files[ii], sep = '')
  #data_file <- paste(data, file_root, '_published_alldata.XLSX', sep='')
  wpd <- read_excel(data_file, sheet = "Whole Plot Data", na = c("NA", "N/A"))

  # Getting the names of the columns
  col_names <- names(wpd)

  # Converting plot_ids to character as some already are and they need to be
  # consistent
  wpd$PLOT_ID <- as.character(wpd$PLOT_ID)

  new_wpd <- full_join(wpd, nvc_survey, by = "PLOT_ID")
  new_wpd <- full_join(new_wpd, ge_survey, by = "PLOT_ID")

  # Creating the tibble which will populate the updates whole plot data sheet
  mavis_out <- tibble(SITECODE = new_wpd$SITECODE,
                      MCODE = new_wpd$MCODE,
                      YEAR = new_wpd$YEAR,
                      PLOT_ID = new_wpd$PLOT_ID,#paste(new_wpd$SITECODE, new_wpd$YEAR, new_wpd$PLOT_ID, sep='_'),
                      LIGHT = as.numeric(new_wpd$light),
                      WETNESS = as.numeric(new_wpd$wetness),
                      PH = as.numeric(new_wpd$ph),
                      FERTILITY = as.numeric(new_wpd$fertility),
                      COMPETITION = as.numeric(new_wpd$competition),
                      STRESS = as.numeric(new_wpd$stress),
                      RUDERALS = as.numeric(new_wpd$ruderals)) %>%
    full_join(., nvc_survey, by = 'PLOT_ID')
  

  ##############################################################################
  # replacing the sheet in excel
  #

  write.xlsx(list('MAVIS output' = mavis_out), 
             file = paste0(MAVIS_data, file_root, '_MAVIS_output.XLSX'))
}
