library(readr)
library(quanteda)
library(readtext)
#devtools::install_github("quanteda/quanteda.corpora")
#library(quanteda.corpora)
library(stringr)
library(spacyr)
library(tidyverse)
library(dplyr)
#Import csv as data frame 

# df_sampleALE <- read_csv(paste0(project_directory, corpusSCELVA ,sep,"CELVA.Sp_398.csv"))

df_sampleALE <- read.csv(paste0(project_directory, corpusSCELVA ,sep,"CELVA.Sp_398.csv"), 
                         stringsAsFactors=FALSE,
                         encoding = 'UTF-8')


df_sampleALE$text <- paste(df_sampleALE$Texte_etudiant_1, df_sampleALE$Texte_etudiant_2, sep='')


df_sampleALE$text <- gsub('\"\"', '\"', df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- str_squish(df_sampleALE$text) # Trim whitespace from a string



#remove independent text columns
df_sampleALE$Texte_etudiant_1  <- NULL
df_sampleALE$Texte_etudiant_2 <- NULL


#modify doc id to put student ID
fun_prenom_nom = function(mail){
  prenom_nom = unlist(strsplit(mail,"@"))[1]
  prenom_nom_vect = unlist(strsplit(prenom_nom, ".", fixed = TRUE))
  prenom = prenom_nom_vect[1]
  prenom = paste(toupper(substr(prenom, 1, 1)), substr(prenom, 2, nchar(prenom)), sep="")
  nom = prenom_nom_vect[2]
  nom = toupper(nom)
  return(paste0(prenom,"_",nom))
}
df_sampleALE$doc_id <-sapply(df_sampleALE$Adresse.de.courriel,fun_prenom_nom)

#Assign  observations with 0 ID number with a new index number
index_zeros = which(df_sampleALE$doc_id==0)
df_sampleALE$doc_id[index_zeros] <- index_zeros# 1:length(df_sampleALE$doc_id[which(df_sampleALE$doc_id==0)])

#Remove duplicated data points
df_sampleALE <-df_sampleALE[!duplicated(df_sampleALE$doc_id),]

#subset English L2 writings
df_sampleALE <-subset(df_sampleALE, df_sampleALE$L2=="Anglais")

#subset accept_data
df_sampleALE <-subset(df_sampleALE, df_sampleALE$Acceptation_donnees=="Oui")



## Correction annee de naissance
for(i in which(nchar(df_sampleALE$Annee_naissance) != 4 & nchar(df_sampleALE$Age) == 2)){
  df_sampleALE$Annee_naissance[i] <- as.character(as.numeric(substr( str_split(df_sampleALE[i,"Date.ajout"], pattern = " ")[[1]][4], 1, nchar(str_split(df_sampleALE[i,"Date.ajout"], pattern = " ")[[1]][4])-1)) - df_sampleALE$Age[i])
}
for(i in which(nchar(df_sampleALE$Annee_naissance) != 4 & nchar(df_sampleALE$Age) != 2)){
  df_sampleALE$Annee_naissance[i] <- NA
  df_sampleALE$Age[i] <- NA
}