extract_stretch <- function(webpage){
  #extract the table with O/C info
  table <- html_nodes(webpage,'table')#keep only the tables
  table <- table[[5]]#keep the fifth one
  table <- html_table(table)#convert in dataframe
  
  ###faire un tableau qui compartimente l'information
  if(all(grepl(" - ", table$X2))==T){
    table <- data.frame(
      year=as.numeric(str_extract(table$X1, '([0-9]){4}')),#colonne date
      OC=as.character(str_extract(table$X2, '(Opening|Sluiting)')),#ouverture, fermeture ou reouverture
      station_1=do.call(rbind,str_split(gsub('(Opening |Sluiting )', "", table$X2), " - "))[,1],#Station 1
      station_2=do.call(rbind,str_split(gsub('(Opening |Sluiting )', "", table$X2), " - "))[,2])
  }else if(all(grepl(" - ", table$X2))==F){
    table <- data.frame(
      year=as.numeric(str_extract(table$X1, '([0-9]){4}')),#colonne date
      OC=as.character(str_extract(table$X2, '(Opening|Sluiting)')))
  }
}

extract_stations <- function(webpage){
  stations <-html_node(webpage, ".inhoud > table:nth-child(7)")
  stations_table <- html_table(stations, fill=T)
  stations_table <- stations_table[!is.na(stations_table$X3),]
  stations_table <- as.data.frame(stations_table[,-c(1,2,4)])
  colnames(stations_table) <- "NAME"
  stations_table$NAME <- as.character(stations_table$NAME) 
}

get_title <- function(webpage){
  gsub("\r\n", " ", html_text(html_node(webpage, ".inhoud > h3:nth-child(1)")))
}



