library(tidyverse)
library(readxl)
library(plyr)
library(matrixStats)
library(diverse)
library(openxlsx)

# change this to your working direstory (the directory with the code in)
setwd('//CAM381FS/x955120$/NE_handover/NE_handover/Vegetation/Dataframe_creation')

################################################################################
# Reading in information from disk
################################################################################

# Put the survey files in here
data <- './Data/'
# Put the outputs of MAVIS_out.R in here
mavis_data_dir <- './MAVIS_data/'

list_of_files <- list.files(data)
N_files <- length(list_of_files)

list_mavis <- list.files(mavis_data_dir)
N_mavis <- length(list_mavis)
print(list_mavis)

# This will stop the code if there isn't a mavis file for each survey file
if(N_files != N_mavis) {
  stop("There should be the same number of survey files as mavis files")}

# Getting the lists of right and wrong names entered manually into a csv
# to fix tpos
name_swap <- read.csv('rename_habitat.csv')
name_swap_vector <- with(name_swap, setNames(right_name, wrong_name))

spec_num_list = list()
gf_list = list()
survey_list = list()

for (ii in 1:N_files) {
  print(list_of_files[ii])
  file_name <- paste(data, list_of_files[ii], sep='')

  st <- read_excel(file_name, sheet = "Species Template", na = c('', "NA", "N/A"))
  # Getting the names of the columns
  col_names <- names(st)

  # Getting the names of the CELL columns
  cell_names <- col_names[grep('CELL', col_names)]
  # converting any non numeric to numeric
  st[cell_names] <- lapply(st[cell_names], function(x) as.numeric(as.character(x)))
  # converting all plot_ids to character as some are and some are not
  st$PLOT_ID <- as.character(st$PLOT_ID)

  ################################################################################
  # Initial edits to species list ready for transformation
  # keeping certain columns, changing the plot ID, changing names to pre-approved
  # ones, changing missing data to holding place string, redoing freq
  ################################################################################

  # The list of columns I want to keep. only part of the string
  keep_list <- c('PLOT_ID', 'DESC', 'CELL', 'PERCENT', 'FREQ')
  # Reducing the size of the tibble to only the kept columns
  st <- st %>%
    select(contains(keep_list))

  # There are often many trailing empyty rows
  st <- st[!is.na(st$DESC_LATIN), ]

  st$PLOT_ID <- as.character(st$PLOT_ID)
  # Making the manual swaps
  for (jj in 1:length(name_swap_vector)) {
    st$DESC_LATIN <- replace(
      st$DESC_LATIN,
      st$DESC_LATIN == names(name_swap_vector[jj]), name_swap_vector[jj]
    )
  }

  # FIXING THE FREQUENCY COLUMNS
  # Changing all missing data in CELL columns to 0
  #st[cell_names][is.na(st[cell_names])] <- 0
  # Calculating FREQ from the contents of CELL
  st <- st %>%
    mutate(FREQ2 = rowSums(.[cell_names]))

  st$PERCENT_COVER <- as.numeric(st$PERCENT_COVER)
  st$FREQ2 <- as.numeric(st$FREQ2)


  # Changing all NAs in percent cover or frequency to 999 so they can be
  # changed back later after we have dealth with other NAs
  st[, c('PERCENT_COVER', 'FREQUENCY')][is.na(st[, c('PERCENT_COVER', 'FREQUENCY')])] <- 999

  ################################################################################
  # Pivotting the data for Frequency
  ################################################################################

  # The list of columns I want to keep. only part of the string
  keep_list_freq <- c('PLOT_ID', 'DESC', 'FREQ2')
  # Reducing the size of the tibble to only the kept columns
  st_freq <- st %>%
    select(contains(keep_list_freq))

  # Getting a list of the plot IDs
  plot_ids <- unique(st_freq['PLOT_ID'])
  N_plots <- length(plot_ids$PLOT_ID)

  # Making a pivot table for each plot and combining
  plot_pivots <- list()
  for (jj in 1:N_plots) {
    single_plot <- st_freq %>%
      # filters for only one plot
      filter(PLOT_ID == plot_ids$PLOT_ID[jj]) %>%
      # There are lots of instances of more than one of the same species per plot
      .[!duplicated(.$DESC_LATIN), ] %>%
      # transforms the data into short and fat
      pivot_wider(names_from = DESC_LATIN, values_from = FREQ2)

    plot_pivots[[jj]] <- single_plot
  }
  frequency <- rbind.fill(plot_pivots)

  # Converting NA to 0
  frequency[is.na(frequency)] <- 0
  # These ones should be NA, they are missing data from the start
  frequency[frequency == 999] <- NA

  ################################################################################
  # Pivotting the data for percent cover
  ################################################################################

  # The list of columns I want to keep. only part of the string
  keep_list_pc <- c('PLOT_ID', 'DESC', 'PERCENT_COVER')
  # Reducing the size of the tibble to only the kept columns
  st_pc <- st %>%
    select(contains(keep_list_pc))

  # Making a pivot table for each plot and combining
  plot_pivots_pc <- list()
  for (jj in 1:N_plots) {
    single_plot_pc <- st_pc %>%
      # filters for only one plot
      filter(PLOT_ID == plot_ids$PLOT_ID[jj]) %>%
      # There are lots of instances of more than one of the same species per plot
      .[!duplicated(.$DESC_LATIN), ] %>%
      # transforms the data into short and fat
      pivot_wider(names_from = DESC_LATIN, values_from = PERCENT_COVER)

    plot_pivots_pc[[jj]] <- single_plot_pc
  }

  # fills when the columns aren't the same and fills blanks with NA
  percent_cover <- rbind.fill(plot_pivots_pc) %>%
    as_tibble(.)

  # Converting NA to 0
  percent_cover[is.na(percent_cover)] <- 0
  # These ones should be NA, they are missing data from the start
  percent_cover[percent_cover == 999] <- NA

  ##############################################################################
  # joinging with whole plot data
  ##############################################################################

  # The first column is PLOT_ID
  spec_names <- names(frequency)[-1]

  # Calculating the species diversity from the frequency table
  # Has to be done plot by plot as for some reason it mixes the order up!
  simpson_div_plot_list = list()
  for (jj in 1:N_plots) {
    single_plot_freq <- frequency %>%
      # filters for only one plot
      filter(PLOT_ID == plot_ids$PLOT_ID[jj])

    simpson_div_plot_list[[jj]] <- diversity(as.matrix(single_plot_freq[ ,spec_names]), type = "simpson")
  }
  simpson_div_plot_total <- bind_rows(simpson_div_plot_list)

  spec_pp <- tibble(PLOT_ID = frequency$PLOT_ID,
                    Species_richness = rowSums(frequency[spec_names]!=0),
                    Species_diversity = simpson_div_plot_total$simpson.I)



  # These are the columns we want from wpd
  wpd_data_cols_old <- c('PLOT_ID', 'SITECODE', 'YEAR', 'EASTINGS' , 'NORTHINGS',
                     'SDATE', 'BAP_BROAD', 'BAP_PRIORITY', 'NVC_FIRST',
                     'LIGHT', 'WETNESS', 'PH', 'FERTILITY',
                     'COMPETITION', 'STRESS', 'RUDERALS')

  wpd_data_cols <- c('PLOT_ID', 'SITECODE', 'YEAR', 'EASTINGS' , 'NORTHINGS',
                     'SDATE', 'BAP_BROAD', 'BAP_PRIORITY')

  # Getting the data out of whole plot data to group the spec data
  wpd <- read_excel(file_name, sheet = "Whole Plot Data", na = c('', "NA", "N/A")) %>%
    select(contains(wpd_data_cols))
  # Always convert to character as some are character and some are num
  wpd$PLOT_ID <- as.character(wpd$PLOT_ID)



  mavis_cols <- c('PLOT_ID', 'SITECODE', 'YEAR', 'NVC1', 'LIGHT',
                  'WETNESS', 'PH', 'FERTILITY', 'COMPETITION', 'STRESS',
                  'RUDERALS')
  mavis_data <- read_excel(paste0(mavis_data_dir, list_mavis[ii]),
                         sheet = "MAVIS output", na = c('', "NA", "N/A")) %>%
    select(contains(mavis_cols))
  # Always convert to character as some are character and some are num
  mavis_data$PLOT_ID <- as.character(mavis_data$PLOT_ID)

  wpd_data <- full_join(wpd, mavis_data, by = c('PLOT_ID', 'SITECODE', 'YEAR'))
  # Helps with some of the typos involving capital letters
  wpd_data$BAP_BROAD <- str_to_title(wpd_data$BAP_BROAD)
  wpd_data$BAP_PRIORITY <- str_to_title(wpd_data$BAP_PRIORITY)

  # Date is used for CEH
  # some of the datse are in date format and some are in string
  #wpd_data$SDATE <- wpd_data$SDATE

  # SEPARATING OUT THE NVC COLUMN data
  wpd_data$NVC_group <- str_extract(wpd_data$NVC1, '^.*?(?=:)')
  wpd_data$NVC_groupb <- gsub('(\\d)\\D+$', '\\1', wpd_data$NVC_group)
  wpd_data$NVC_groupc <- gsub('[0-9]+', '', wpd_data$NVC_groupb)

  # Combining on PLOT_ID
  spec_pp.bapb <- full_join(spec_pp, wpd_data, by = "PLOT_ID")

  #spec_num_list[[ii]] <- spec_pp.bapb

  ##############################################################################
  # Ground features
  ##############################################################################

  gf <- read_excel(file_name, sheet = "Ground Features", na = c('', "NA", "N/A"))
  # Getting the names of the columns
  col_names_gf <- names(gf)

  # Getting the names of the CELL columns
  cell_names <- col_names_gf[grep('CELL', col_names_gf)]
  # converting any non numeric to numeric
  gf[cell_names] <- lapply(gf[cell_names], function(x) as.numeric(as.character(x)))
  # converting all plot_ids to character as some are and some are not
  gf$PLOT_ID <- as.character(gf$PLOT_ID)
  # Inconsistencies with cpitalisation across surveys
  gf$FEATURE <- tolower(gf$FEATURE)

  # basic plot data to append the ground features to
  base_df <- tibble(PLOT_ID = wpd_data$PLOT_ID,
                    SITECODE = wpd_data$SITECODE,
                    YEAR = wpd_data$YEAR)

  # FIXING THE FREQUENCY COLUMNS
  keep_cols <- c('PLOT_ID', 'YEAR', 'FEATURE')
  # Taking only the vegetaion height rows
  # these are treated separately as the data analysis is different (mean)
  veg_height <- select(filter(gf, FEATURE == 'vegetation height'),
                       append(keep_cols, cell_names))

  # Creating blank columns so that if there is no veg ehight data, we still
  # have oclumns for future steps
  veg_height$MEAN_HEIGHT <- NA
  veg_height$STD_HEIGHT <- NA

  # some surveys dont have veg height data
  try(
    {veg_height$MEAN_HEIGHT <- rowMeans(veg_height[cell_names], na.rm=TRUE)
    # rowStds uses matrixStats package and needs data to be in matrix form
    veg_height$STD_HEIGHT <- rowSds(as.matrix(veg_height[cell_names]), na.rm=TRUE)
    }
  )

  base_df <- full_join(base_df,
                       veg_height[ ,c('PLOT_ID', 'YEAR', 'MEAN_HEIGHT',
                                      'STD_HEIGHT')],
                       by = c("PLOT_ID", "YEAR"))

  # Taking all the rest of the rows
  other_rows <- select(filter(gf, FEATURE != 'vegetation height'),
                       append(keep_cols, cell_names))

  # Changing all missing data in CELL columns to 0
  other_rows[cell_names][is.na(other_rows[cell_names])] <- 0

  # Getting a list of the plot IDs
  gf_plot_ids <- unique(other_rows['PLOT_ID'])
  gf_N_plots <- length(gf_plot_ids$PLOT_ID)

  bare_tib_list = list()
  for (jj in 1:gf_N_plots) {
    # separating out the data to each plot and only rows with 'bare' in FEATURE
    single_bare <- other_rows %>%
      filter(PLOT_ID == gf_plot_ids$PLOT_ID[jj]) %>%
      .[grep("bare", .$FEATURE), ]

    # Create the row for the new bare x feature
    bare_tib <- tibble(PLOT_ID = gf_plot_ids$PLOT_ID[jj],
                       YEAR = other_rows$YEAR[1],
                       FEATURE = 'bare x')
    # Function checks for any 1s in the column
    count_bare <- function(col_name) {
      sum_col <- 0
      try(sum_col <- sum(single_bare[ ,col_name]), silent=TRUE)
      if (sum_col == 0) {
        return (0)
      } else {
        return (1)
      }
    }
    # Checking for presence in a bare row and if so counting it as 1
    # then inputting that 1 in the appropriate cell column of the new row
    for (kk in 1:length(cell_names)) {
      bare_tib[ ,cell_names[kk]] <- count_bare(cell_names[kk])
    }
    # Adding each row to a column, ready to be combined and re-added
    bare_tib_list[[jj]] <- bare_tib
  }

  bare_total <- bind_rows(bare_tib_list)

  other_rows <- rbind(other_rows, bare_total)

  # Calculating FREQ from the contents of CELL
  other_rows <- other_rows %>%
    mutate(FREQ = rowSums(.[cell_names]))

  # The list of columns I want to keep. only part of the string
  keep_list_gf <- c('PLOT_ID', 'FEATURE', 'FREQ')
  # Reducing the size of the tibble to only the kept columns
  other_rows <- other_rows %>%
    select(contains(keep_list_gf))

  # Making a pivot table for each plot and combining
  plot_pivots_gf <- list()
  for (jj in 1:gf_N_plots) {
    single_plot_gf <- other_rows %>%
      # filters for only one plot
      filter(PLOT_ID == gf_plot_ids$PLOT_ID[jj]) %>%
      # There are lots of instances of more than one of the same species per plot
      .[!duplicated(.$FEATURE), ] %>%
      # transforms the data into short and fat
      pivot_wider(names_from = FEATURE, values_from = FREQ)

    plot_pivots_gf[[jj]] <- single_plot_gf
  }
  frequency_gf <- rbind.fill(plot_pivots_gf)

  # combinigng the beg height data with the frequency data on ground types
  # all on a base df which jut containts the plot_id etc
  base_df <- full_join(base_df,
                       veg_height[ ,c('PLOT_ID', 'YEAR', 'MEAN_HEIGHT',
                                      'STD_HEIGHT')],
                       by = c("PLOT_ID", "YEAR", "MEAN_HEIGHT", "STD_HEIGHT"))
  gf_df <- full_join(base_df, frequency_gf, by = "PLOT_ID")

  # combining the ground features with the species data
  full_survey <- full_join(spec_pp.bapb, gf_df,
                           by = c("PLOT_ID", "SITECODE", "YEAR"))
  survey_list[[ii]] <- full_survey
}

################################################################################
# Combning it together ready for analysis
################################################################################

# combining it all together
survey_total <- bind_rows(survey_list)

# checking for completely blank rows in the data columns and removing them
blank_check <- c('Species_richness', 'bare x', 'NVC1', 'MEAN_HEIGHT')
blank_len <- length(blank_check)
survey_total <- survey_total[rowSums(is.na(survey_total[,blank_check]))!=blank_len,]

# Keeping only certain columns (removing all of these ones)
remove_cols <- c('bare ground', 'bare rock', 'bare soil', 'open water',
                   'bare peat', 'bare earth', 'dead wood', 'bare soil/sand',
                 'leaf litter', 'exposed stone', 'exposed rock', 'deadwood',
                 'glacial pebbles', 'NVC10')
survey_total <- survey_total[ , -which(names(survey_total) %in% remove_cols)]

# repacing NA with 0 in certain columns
replace_na_cols <- c('litter', 'bare x')
survey_total[replace_na_cols][is.na(survey_total[replace_na_cols])] <- 0


colnames(survey_total) <-c('Plot_ID', 'Species_richness', 'Species_diversity',
                      'Sitecode', 'Year', 'Eastings', 'Northings', 'Date',
                      'BAP_broad', 'BAP_priority', 'NVC1', 'Light',
                      'Wetness', 'pH', 'Fertility', 'Competition', 'Stress',
                      'Ruderals', 'NVC_subgroup', 'NVC_group', 'NVC_habitat',
                      'Vegetation_height', 'STD_HEIGHT', 'Litter',
                      'Bare_ground')

# Choose how you want to write the final dataframe
write.csv(survey_total, 'all_plots.csv', row.names = FALSE)


# Use this instead of the write.csv to make it into an excel file
# wb <- createWorkbook()
# survey_sheet <- addWorksheet(wb, 'presentation_plots')
# writeData(wb, survey_sheet, survey_total)
# saveWorkbook(wb, 'presentation_surveys.xlsx')

