
### Dossier où l'archive zip de Moodle a été placée

fullpath <- "D:/Home/alafonta/Desktop/Nouveau dossier"
setwd(fullpath)


### Extraction de l'archive

zipF <- paste(fullpath, list.files(), sep = "/")
outDir<-str_split(zipF,pattern = ".zip")[[1]][1]
unzip(zipF,exdir=outDir)


### Importation des fichiers html

fullpath <- outDir
setwd(fullpath)

manydirectories <- list.dirs()
directorynames <- basename(manydirectories)
directorynames <- directorynames[2:length(directorynames)]

names <- c()

for(i in 1:length(directorynames)){
  names[i] <- str_split(string = directorynames[i], pattern = "_")[[1]][1]
}

texte <- c()

for(i in 1:length(directorynames)){
  texte[i] <- paste(readLines(paste(fullpath, directorynames[i],"onlinetext.html",sep = "/"), warn=F), collapse="")
}

base_f <- data.frame(Nom.complet=names,texte=texte)
