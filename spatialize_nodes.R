rm(list=ls())

library("WikidataQueryServiceR")
library(stringr)
library(geosphere)

edgelist <- read.csv("D:/projets/netherlands/edgelist_train_2.csv", stringsAsFactors = F, encoding = "UTF-8")
stations <- data.frame(name=unique(c(edgelist[,1], edgelist[,2])))



#query wikidata to get the names of the stations
stations_info <- query_wikidata('SELECT DISTINCT ?station ?stationLabel ?coor ?opening ?closure WHERE {
                                ?station wdt:P31 wd:Q55488.
                                ?station ?range wd:Q55.
                                ?station wdt:P625 ?coor.
                                SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],nl". }
                                OPTIONAL { ?station wdt:P1619 ?opening. }
                                OPTIONAL { ?station wdt:P3999 ?closure. }
                                }')

#clean the names from wikidata
stations_info$name_tlw <- tolower(stations_info$stationLabel)
stations_info$name_tlw <- gsub("(station\\s|stopplaats\\s|halte\\s)", "", stations_info$name_tlw)

#separate the coordinates
stations_info$coor <- substr(stations_info$coor, 7, nchar(stations_info$coor)-1)
stations_info <- cbind(stations_info, str_split_fixed(stations_info$coor, " ", 2))
colnames(stations_info) <- c("station", "stationLabel", "coor", "opening", "closure", "name_tlw", "lon", "lat")
stations_info <- stations_info[,-3]
stations_info$lon <- as.numeric(as.character(stations_info$lon))
stations_info$lat <- as.numeric(as.character(stations_info$lat))

#stations from stationweb.nl that match wikidata
stations$match <- match(stations$name, stations_info$name_tlw)


#keep only stations that don't match with wikipedia data
st_pb <- stations[is.na(stations$match),]


#function that compute the distance between surrounging nodes at any years
closest_pair <- function(year){
  edges_year <- edges[edges$V3==year,]
  res <- as.data.frame(table(c(edges_year$V1, edges_year$V2)))
  res <- res[res$Freq==1,]
  res <- merge(res[,1], stations_info[,c(5,6,7)], by.x=1, by.y=1)
  res$x <- as.character(res$x)
  dist <- distHaversine(res[1,c(2,3)], res[2,c(2,3)])
  dist_pair <- c(res[1,1], res[2,1], dist)
  print(dist_pair)
}


#edges with a missing stations
st_pb <- as.character(st_pb$name)

mid_point_list <- vector("list", length(st_pb))
for(S in 1:length(st_pb)){
  edges <- edgelist[edgelist$V1==st_pb[S]|edgelist$V2==st_pb[S],]
  years <- unique(edges$V3)
  dist_list <- lapply(years, closest_pair)
  dist_list <- data.frame(matrix(unlist(dist_list), nrow=length(dist_list), byrow=T), stringsAsFactors = F)
  dist_list <- dist_list[dist_list$X3==min(dist_list$X3),]
  dist_list <- dist_list[!duplicated(dist_list),]
  st <- c(dist_list[1,1], dist_list[1,2]) 
  st <- stations_info[stations_info$name_tlw %in% st,]
  mid <- midPoint(st[1,c(6,7)], st[2,c(6,7)])
  final_res <- c(as.character(st_pb[S]), mid)
  mid_point_list[[S]] <- print(final_res)
}



midpoint_stations <- data.frame(matrix(unlist(mid_point_list), nrow=length(mid_point_list), byrow=T), stringsAsFactors = F)
midpoint_stations <- midpoint_stations[order(midpoint_stations$X3),]

#write.csv2(midpoint_stations, "D:/projets/netherlands/final_database/stations_midpoints.csv", row.names = F, fileEncoding = "UTF-8")



see <- stations_info[stations_info$name_tlw %in% stations$name,]
see <- see[!duplicated(see$name_tlw),]
#write.csv2(see, "D:/projets/netherlands/final_database/stations.csv", row.names = F, fileEncoding = "UTF-8")



see <- see[,c(5,6,7)]

colnames(midpoint_stations) <- colnames(see)

see$source <- "wikidata"
midpoint_stations$source <- "midpoint"

see <- rbind(see, midpoint_stations)


see <- see[!is.na(see$lon),]


see$lon <- as.numeric(see$lon)
see$lat <- as.numeric(see$lat)


manual <- read.csv("D:/projets/netherlands/final_database/stations_manual.csv", header = T, sep = ";")

colnames(manual) <- colnames(see)
see <- rbind(see, manual)

ggplot(data=see, aes(x=lon, y=lat, color=source))+
  geom_point()+coord_map()

write.csv(see, "D:/projets/netherlands/final_database/nodes_stations.csv", row.names = F, fileEncoding = 'UTF-8')

