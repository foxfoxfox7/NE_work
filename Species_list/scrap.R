wb <- createWorkbook()

# This is the list of all the species found in all the sites
All_species <- addWorksheet(wb, 'All_species')
writeData(wb, All_species, all_spec)

# All the unique species found on each site
Sites <- addWorksheet(wb, 'Sites')
# converts the list with different lengths to a df so it can be saved

writeData(wb, Sites, site_list_df)

# All the unique species on each survey
Surveys_unique <- addWorksheet(wb, 'Surveys_unique')

writeData(wb, Surveys_unique, survey_list_df)

# All the species as found in species template
# this will be used to feed back into species template with typos corrected
Surveys <- addWorksheet(wb, 'Surveys')

writeData(wb, Surveys, survey_list_df)

saveWorkbook(wb, 'Species_lists.xlsx')

