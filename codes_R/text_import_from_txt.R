library(readr)
library(quanteda)
library(readtext)
#devtools::install_github("quanteda/quanteda.corpora")
#library(quanteda.corpora)
library(stringr)
library(spacyr)
library(tidyverse)
library(dplyr)

fullpath <- paste0(project_directory, "data", sep,"from_txt")
setwd(fullpath)

files <- list.files()

names <- c()
text <- c()
for(i in 1:length(files)){
  names[i] <- unlist(str_split(files[i], pattern = ".txt"))[1]

  text[i] <- paste(readLines(files[i], warn=F), collapse=" ")
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
