

rm(list=ls())

library(rvest)
library(magrittr)
library(stringr)
library(RCurl)

#we start by extracting all the hyperlinks of the different train lines
url_overview <- 'http://www.stationsweb.nl/lijnoverzicht.asp'
webpage <- read_html(url_overview)

#Select the nodes tables
overview_html <- html_nodes(webpage,'table')

#keep only the last one
lines <- overview_html[[5]]
op_clo <- html_text(html_nodes(lines, "td"))
op_clo <- op_clo[seq(2, length(op_clo), 2)]
op <- as.numeric(substr(op_clo, 1, 4))
cl <- as.numeric(sub('.*\\-\\s', '', op_clo))
cl[is.na(cl)] <- 2018
lines <- data.frame(link=html_attr(html_nodes(lines, "a"), "href"),
                    line=html_text(html_nodes(lines, "a")),
                    O=op,
                    C=cl)





links_to_srap <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 
                   14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 
                   25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 
                   36, 38, 39, 40, 41, 43, 44, 45, 46, 47, 48, 
                   49, 50, 51, 52, 53, 54, 56, 57, 58, 59, 60, 
                   61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 
                   72, 73, 74, 75, 78, 79, 80, 81, 82, 83, 84, 
                   86, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 
                   98, 99, 100, 101, 103, 104, 105, 106, 107, 
                   108, 109, 110, 111, 112, 113, 114, 115, 116, 
                   117, 118, 119, 120, 121, 122)


#open the hyperlink of one of the line
for(S in links_to_srap){
  url_line <- paste("http://www.stationsweb.nl/", lines[S,1], sep = "")
  web_line <- read_html(url_line)
  
  
  stations <-html_node(web_line, ".inhoud > table:nth-child(7)")
  
  #name of stations
  stations_table <- html_table(stations, fill=T)
  stations_table <- stations_table[!is.na(stations_table$X3),]
  stations_table <- stations_table[,-c(1,4)]
  colnames(stations_table) <- c("KM", "NAME")
  
  #isolate the url of stations of a line
  stations_links <- stations %>% html_nodes("a") %>% html_attr("href")
  stations_links <- stations_links[grep(pattern = "station.asp?", x = stations_links)]
  
  #set options to avoid the timeout problem.
  myOpts <- curlOptions(connecttimeout = 200)
  
  #write the pattern to identify date of opening and closing in plein text
  pattern <- "(Geopend|Gesloten|Heropend)\\s(op)(\\s|)\\d{1,2}\\s(jan|feb|mrt|apr|mei|jun|jul|aug|sep|okt|nov|dec)\\s\\d{4}"
  
  station_date <- vector("list", length(stations_links))
  for(i in 1:length(stations_links)){
    url_station <- paste("http://www.stationsweb.nl/", stations_links[i], sep = "")
    web_station <- getURL(url_station, .opts = myOpts)
    web_station <- read_html(web_station)
    plein_text <- html_text(web_station)
    v <- substr(unlist(str_extract_all(plein_text, pattern)), nchar(unlist(str_extract_all(plein_text, pattern)))-3,
                nchar(unlist(str_extract_all(plein_text, pattern))))
    station_date[[i]] <- as.numeric(v)
  }
  
  #write a loop to store all the years the stations were open
  station_years <- vector("list", length(stations_links))
  for(i in 1:length(stations_links)){
    if (length(station_date[[i]])==1) {
      station_years[[i]] <- seq(station_date[[i]][1], 2018)
    } else if (length(station_date[[i]])==2) {
      station_years[[i]] <- seq(station_date[[i]][1], station_date[[i]][2])
    } else if (length(station_date[[i]])==3) {
      station_years[[i]] <- c(seq(station_date[[i]][1], station_date[[i]][2]),
                              seq(station_date[[i]][3], 2018))
    } else if (length(station_date[[i]])==4) {
      station_years[[i]] <- c(seq(station_date[[i]][1], station_date[[i]][2]),
                              seq(station_date[[i]][3], station_date[[i]][4]))
    } else if (length(station_date[[i]])==5) {
      station_years[[i]] <- c(seq(station_date[[i]][1], station_date[[i]][2]),
                              seq(station_date[[i]][3], station_date[[i]][4]),
                              seq(station_date[[i]][5], 2018))
    } else if (length(station_date[[i]])==6) {
      station_years[[i]] <- c(seq(station_date[[i]][1], station_date[[i]][2]),
                              seq(station_date[[i]][3], station_date[[i]][4]),
                              seq(station_date[[i]][5], station_date[[i]][6]))
    }
  }
  
  
  #give the name of the stations to list objects
  names(station_years) <- stations_table$NAME
  
  
  #convert the list into a dataframe with stations and the year they were open
  station_years_table <- data.frame(Name = as.character(rep(names(station_years), lapply(station_years, length))),
                                    Year = unlist(station_years), 
                                    row.names = NULL)
  
  #define a function to compute the length of edges based on their km
  comp_length <- function(n){
    st_y$KM[n]-st_y$KM[n-1]}  
  
  #create edgelists for all sepecific years
  years <- sort(unique(station_years_table$Year))
  years <- years[years>=lines[S,3]]
  years <- years[years<=lines[S,4]]
  edgelist_y <- vector("list", length(years))
  for(i in 1:length(years)){
    st_y <- station_years_table[station_years_table$Year==years[i],]
    st_y <- merge(st_y, stations_table, by.x = 1, by.y = 2, all.x = T, all.y = F, sort = F)
    st_y$len <- 0
    
    if(nrow(st_y)>1){
      st_y$len <- c(0, sapply(2:length(st_y$len), comp_length))
      edgelist_y[[i]] <- as.matrix(data.frame(from=st_y[1:nrow(st_y)-1,1],
                                              to=st_y[2:nrow(st_y),1],
                                              year=years[i],
                                              len=st_y[2:nrow(st_y),4]))
    }else{
      edgelist_y[[i]] <- NULL
    }
  }
  
  edgelist_y
  #rbind all these edgelist
  final_edgelist <- do.call(rbind, edgelist_y)
  
  
  file_name <- paste("D:/projets/netherlands/edgelists/",str_extract(lines[S,1], pattern = "(lijn=)\\d{1,3}"), ".csv", sep = "")
  write.csv(x = final_edgelist, file = file_name, row.names = F)
}



