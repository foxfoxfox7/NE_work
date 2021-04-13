library(raster)


setwd('C:/Users/kiera/Work/NE_work/Weather/')

data_dir <- './MET/'

data_files <- list.files(data_dir)

dat1 <- brick(paste0(data_dir, data_files[[1]]))  


dat1


snap1 <- dat1[[23]]

plot(snap1)

site_perimeter <- extent(400000, 600000, 200000, 400000)

snap1.crop <- crop(snap1, site_perimeter)

plot(snap1.crop)



survey_dir <- '../Data_set/'
survey_files <- list.files(survey_dir)

file_path <- paste0(survey_dir, survey_files[[1]])
species <- read_excel(file_path, sheet = "Whole Plot Data")

print(max(species$EASTINGS))


a1 <- 0
a2 <- 359
angle = 180 - abs(abs(a1 - a2) - 180)
print(angle)