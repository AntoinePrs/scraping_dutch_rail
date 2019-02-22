rm(list = ls())

library(qdap)
#setwd("D:/projets/netherlands/edgelists/")

#open dictionnary 
thesaurus <- read.csv("D:/projets/netherlands/thesaurus_stations.txt", stringsAsFactors = F, encoding = "UTF-8")

#open all the edgelists that have been scraped
big_edgelist <- do.call(rbind, lapply(list.files(path = "D:/projets/netherlands/edgelists/"), read.csv, sep = ",", header = F))

#remove the colomns names inside the data
big_edgelist <- big_edgelist[!big_edgelist$V1=="from",]

#clean the vertex names
big_edgelist$V1 <- gsub("(- )", "-", big_edgelist$V1)
big_edgelist$V1 <- gsub("\\s\\(oud\\)", "", big_edgelist$V1)
big_edgelist$V1 <- tolower(big_edgelist$V1)

big_edgelist$V2 <- gsub("(- )", "-", big_edgelist$V2)
big_edgelist$V2 <- gsub("\\s\\(oud\\)", "", big_edgelist$V2)
big_edgelist$V2 <- tolower(big_edgelist$V2)


#change certain names
big_edgelist$V1 <- lookup(big_edgelist$V1, thesaurus, missing = NULL)
big_edgelist$V2 <- lookup(big_edgelist$V2, thesaurus, missing = NULL)



write.csv(big_edgelist, "D:/projets/netherlands/final_database/edgelist.csv", row.names = F, fileEncoding = 'UTF-8')


