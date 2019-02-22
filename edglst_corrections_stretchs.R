
rm(list = ls())

source("Data/Netherlands/train/functions_scrap_train.R")


lijn_number <- 12

url <- paste0("http://www.stationsweb.nl/lijnlijst.asp?lijn=", lijn_number)
webpage <- read_html(url)

get_title(webpage)

table <- extract_stretch(webpage)

stations <- as.data.frame(extract_stations(webpage))
