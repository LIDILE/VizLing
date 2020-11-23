
options(java.parameters = "-Xmx8000m")   # permet a R d'utiliser plus de RAM

df_sampleALE$text <- gsub('\"\"', '\"', df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- str_squish(df_sampleALE$text) # Trim whitespace from a string


## Remove tags
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", " ", htmlString))
}
df_sampleALE$text <- cleanFun(df_sampleALE$text)
df_sampleALE$text <- gsub("&amp;", " ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub("&nbsp;", " ", df_sampleALE$text, fixed=TRUE)


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
df_sampleALE$text <- gsub("[][]|[^[:ascii:]]", " ", df_sampleALE$text, perl=T)  # enlève   \u2190 \u2192 ...




## Transformation ponctuation/espace

# creer espaces autour de ponctuation, reduire espaces, supprimer espaces non desires
df_sampleALE$text <- gsub("(\\d+),(\\d+)", "\\1.\\2", df_sampleALE$text) # On remplace les virgules par des points pour les nombres decimaux
df_sampleALE$text <- gsub(pattern='((?<![0-9])\\.)|(\\.(?![0-9]))', replacement=" . ", x=df_sampleALE$text, perl=TRUE) # . hors nb decimaux
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


## Changement des "i" en "I" , "i'm" en "I'm" , "i've" en "I've"
df_sampleALE$text <- gsub(" i ", " I ", df_sampleALE$text, fixed=TRUE)
df_sampleALE$text <- gsub(" i'", " I'", df_sampleALE$text, fixed=TRUE)

str_sub(df_sampleALE$text, 1, 2)[which(str_sub(df_sampleALE$text, 1, 2)=="i ")] <- "I "
str_sub(df_sampleALE$text, 1, 2)[which(str_sub(df_sampleALE$text, 1, 2)=="i'")] <- "I'"


## Changement des "can't" en "cannot"
df_sampleALE$text <- gsub(" can't", " cannot", df_sampleALE$text, fixed=TRUE)



## On enleve les eventuels accents venant d'expressions francaises pour le parseur : Samuel Hahnemann read the Traite de matiere medicale by William Cullen
unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

df_sampleALE$text <- chartr(paste(names(unwanted_array), collapse=""), paste(unwanted_array, collapse=""), df_sampleALE$text)

df_sampleALE$text <- gsub("\\s+"," ", df_sampleALE$text)
df_sampleALE$text <- trimws(df_sampleALE$text)


## On ajoute des points à la fin des textes s'il n'y en a pas pour le parseur.

df_sampleALE$text[!(str_sub(df_sampleALE$text,-1,-1) %in% c(".","?","!"))] <- paste(
  df_sampleALE$text[!(str_sub(df_sampleALE$text,-1,-1) %in% c(".","?","!"))],".", sep = "")




path_cleaned_texts = paste0(project_directory, corpus_from_data_cleaned)
check_creat_directory(path_cleaned_texts)
setwd(path_cleaned_texts)
write.csv(df_sampleALE, 
          file=name_file_sample_CELVA, 
          row.names=FALSE)


# create text file for each student
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
setwd(project_directory)
cat("-->> Text files are created with success !\n")
