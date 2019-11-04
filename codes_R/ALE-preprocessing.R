#This script is to do preprocessing
#Input: the MOODLE DB file (csv).  It merges the two texte_etudiant fields. It Cleans a bit. It create doc_ID corresponding to student IDs and it adds new IDs for 
#observations that have the same 0 ID. 
#Output: 
#1. .txt files for each obeservation with finename as doc_id 
#2 one csv file of all observations df_sampleALE


library(readr)
library(quanteda)
library(readtext)
#devtools::install_github("quanteda/quanteda.corpora")
#library(quanteda.corpora)
library(stringr)
library(spacyr)
library(tidyverse)
library(dplyr)


options(java.parameters = "-Xmx8000m")   # permet a R d'utiliser plus de RAM


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
df_sampleALE$doc_id <- df_sampleALE$ID_etudiant

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


## Remove tags
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}
df_sampleALE$text <- cleanFun(df_sampleALE$text)
df_sampleALE$text <- gsub("&amp;", "", df_sampleALE$text, fixed=TRUE)


## On enleve/modifie les caracteres speciaux
df_sampleALE$text <- gsub('\n', " ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub('\"', " ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("¤", " ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("§", " ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("@", " ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("+", " ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("$", " ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("£", " ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("µ", " ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("[", "(", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("]", ")", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("{", "(", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("}", ")", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("etc...", "etc.", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("etc..", "etc.", df_sampleALE$text, fixed=TRUE)
#df_sampleALE$text <- gsub("etc.", "...", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("[][]|[^[:ascii:]]", " ", df_sampleALE$text, perl=T)  # enl?ve   \u2190 \u2192 ...




## Transformation ponctuation/espace

# creer espaces autour de ponctuation, reduire espaces, supprimer espaces non desires
df_sampleALE$text <- gsub(".", " . ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub(",", " , ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub(";", " ; ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub(":", " : ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("!", " ! ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("?", " ? ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("(", " ( ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub(")", " ) ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("'", " ' ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("-", " ", df_sampleALE$text, fixed=TRUE)
#df_sampleALE$text <- gsub("/", " / ", df_sampleALE$text, fixed=TRUE)

df_sampleALE$text <- str_replace_all(string = df_sampleALE$text, pattern = "\\s+\\.\\s+\\.\\s+\\.\\s+", replacement = " \\.\\.\\. ")
df_sampleALE$text <- str_replace_all(string = df_sampleALE$text, pattern = "\\.\\s+\\.", replacement = " \\.\\.\\. ")


df_sampleALE$text <- gsub("\\s+"," ", df_sampleALE$text)

df_sampleALE$text <- gsub(" ( ", " (", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub(" . ", ". ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub(" ) ", ") ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub(" ).", ").", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub(" ' ", "'", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub(" , ", ", ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub(" .... ", " ... ", df_sampleALE$text, fixed=TRUE)
#df_sampleALE$text <- gsub(" / ", "/", df_sampleALE$text, fixed=TRUE)

df_sampleALE$text <- gsub("\\s+"," ", df_sampleALE$text)




## On enleve les eventuels espaces en fin de chaine de caracteres 
df_sampleALE$text <- trimws(df_sampleALE$text)


## Ajout d'une variable comptant le nombre de "I" n'etant pas mis en majuscule et d'une variable binaire associee
df_sampleALE$nb_i <- NA
df_sampleALE$nb_i <- str_count(df_sampleALE$text, " i ") + str_count(df_sampleALE$text, " i'")
df_sampleALE$nb_i[which(str_sub(df_sampleALE$text, start = 1, end = 2) %in% c("i ","i'"))] <- df_sampleALE$nb_i[which(str_sub(df_sampleALE$text, start = 1, end = 2) %in% c("i ","i'"))] + 1

df_sampleALE$error_i <- NA
df_sampleALE$error_i[which(df_sampleALE$nb_i==0)] <- "non"
df_sampleALE$error_i[which(df_sampleALE$nb_i >0)] <- "oui"




## Changement des "i" en "I" , "i'm" en "I'm" , "i've" en "I've"
df_sampleALE$text <- gsub(" i ", " I ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub(" i'", " I'", df_sampleALE$text, fixed=TRUE)

str_sub(df_sampleALE$text, 1, 2)[which(str_sub(df_sampleALE$text, 1, 2)=="i ")] <- "I "
str_sub(df_sampleALE$text, 1, 2)[which(str_sub(df_sampleALE$text, 1, 2)=="i'")] <- "I'"




## Changement des "can't" en "cannot"
df_sampleALE$text <- gsub(" can't", " cannot", df_sampleALE$text, fixed=TRUE)



## On enleve les eventuels accents venant d'expressions francaises pour le parseur : Samuel Hahnemann read the Traite de matiere medicale by William Cullen
unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'=' ', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

df_sampleALE$text <- chartr(paste(names(unwanted_array), collapse=''), paste(unwanted_array, collapse=''), df_sampleALE$text)

df_sampleALE$text <- gsub("\\s+"," ", df_sampleALE$text)
df_sampleALE$text <- trimws(df_sampleALE$text)


path_cleaned_texts = paste0(project_directory, corpus_SCELVA_cleaned)
check_creat_directory(path_cleaned_texts)
setwd(path_cleaned_texts)
write.csv(df_sampleALE, 
          file=name_file_sample_SCELVA, 
          row.names=FALSE)


##########################
# Export text from csv to independent .txt files. Filename is studentID. These txt files are to be used with L2SCA

path_corpusALE = paste0(project_directory, corpusALE)
check_creat_directory(path_corpusALE)
setwd(path_corpusALE)

write_files = sapply(1:nrow(df_sampleALE), function(i) {
  write.table(df_sampleALE[i,"text"], 
              file = paste0(df_sampleALE[i,"doc_id"], ".txt"),
              row.names = FALSE, col.names = FALSE,
              quote = FALSE)
}
)
cat("-->> Text files are created with success !\n")
setwd(project_directory)
