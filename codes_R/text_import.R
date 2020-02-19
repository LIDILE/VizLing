library(readr)
library(quanteda)
library(readtext)
#devtools::install_github("quanteda/quanteda.corpora")
#library(quanteda.corpora)
library(stringr)
library(spacyr)
library(tidyverse)
library(dplyr)
### Dossier ? l'archive zip de Moodle a été placée
fullpath <- paste0(project_directory, "data", sep,"zip_from_moodle")
setwd(fullpath)

### Extraction de l'archive
zipF <- paste(fullpath, list.files(), sep = sep)
outDir<- unlist(strsplit(zipF,split  = "\\.zip"))[1]         

if (!dir.exists(outDir)){
  unzip(zipF,exdir=outDir)
}


setwd(outDir)

manydirectories <- list.dirs()
directorynames <- basename(manydirectories)
directorynames <- directorynames[2:length(directorynames)]

#names <- c()
#text <- c()
#for(i in 1:length(directorynames)){
#  names[i] <- str_split(string = directorynames[i], pattern = "_")[[1]][1]
#  names[i] <- gsub(" ", "_", names[i])
#  
#  text[i] <- paste(readLines(paste(outDir, directorynames[i],"onlinetext.html",sep = sep), warn=F), collapse="")
#}

names <- c()
text <- c()
for(i in 1:length(directorynames)){
  names[i] <- str_split(string = directorynames[i], pattern = "_")[[1]][1]
  names[i] <- gsub(" ", "_", names[i])
  
  setwd(paste(outDir, directorynames[i],sep = sep))
  text[i] <- paste(readLines(paste(outDir, directorynames[i],list.files(),sep = sep), warn=F), collapse="")
  setwd(outDir)
}

base_f <- data.frame(doc_id=names,text=text)

# create folder
check_creat_directory(paste0(project_directory,corpus_from_data))

write.csv(base_f, 
          file=paste0(project_directory,corpus_from_data,sep ,name_zip_file), 
          row.names=FALSE)
setwd(project_directory)

# Import csv as data frame 

df_sampleALE <- read.csv(paste0(project_directory, corpus_from_data ,sep, name_zip_file), 
                         stringsAsFactors=FALSE,
                         encoding = 'UTF-8')
