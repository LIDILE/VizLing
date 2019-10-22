project_directory = "P:\\suivi_projets\\VisLang\\VizLang_v2\\" #"P:\\suivi_projets\\VisLang\\VisLang\\" 
setwd(project_directory)

### Dossier où l'archive zip de Moodle a été placée
fullpath <- paste0(project_directory, "texts")#"D:/Home/alafonta/Desktop/Nouveau dossier"
setwd(fullpath)

# detect the os name
get_sys_name = function(){
  return(Sys.info()["sysname"])
}

os = get_sys_name()
if (os == "Windows"){
  project_directory = "P:\\suivi_projets\\VisLang\\VizLang_v2\\" #"P:\\suivi_projets\\VisLang\\VisLang\\" 
  sep = "\\"
}else{
  project_directory = "/home/knefati/Documents/MyWork-Ensai/VizLing/"
  sep = "/"
}

### Extraction de l'archive

zipF <- paste(fullpath, list.files(), sep = sep)
outDir<- unlist(strsplit(zipF,split  = ".zip"))[1]         

if (!dir.exists(outDir)){
  unzip(zipF,exdir=outDir)
}


### Importation des fichiers html

fullpath <- outDir
setwd(fullpath)

manydirectories <- list.dirs()
directorynames <- basename(manydirectories)
directorynames <- directorynames[2:length(directorynames)]

names <- c()
texte <- c()
for(i in 1:length(directorynames)){
  names[i] <- str_split(string = directorynames[i], pattern = "_")[[1]][1]
  
  texte[i] <- paste(readLines(paste(fullpath, directorynames[i],"onlinetext.html",sep = "/"), warn=F), collapse="")
}

base_f <- data.frame(Nom.complet=names,texte=texte)
