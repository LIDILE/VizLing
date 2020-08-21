# create a directory
check_creat_directory = function(path_dir){
  dir.create(file.path(path_dir), showWarnings = FALSE)
}
# detect the os name
get_sys_name = function(){
  return(Sys.info()["sysname"])
}
counter = function(out_command){
  os = get_sys_name()
  if (os == "Windows"){
    return(  as.numeric(out_command[length(out_command)]))
  }
  else {
    return(as.numeric(out_command))
  }
}

os = get_sys_name()
if (os == "Windows"){
  
  project_directory = "P:\\suivi_projets\\VisLang\\Visualisation_linguistique\\" 
  sep = "\\"
}else{
  project_directory = "/home/knefati/Documents/MyWork-Ensai/R-projects/VizLing"   #"~/Documents/Thomas/MaRecherche/SoftwareApplications/Visualisation_linguistique/"
  sep = "/"
  library(doParallel)
  library(doMC) # It works only on linux
  library(progress)
  
  registerDoMC(cores = numCores) # make a fork cluster
  
}

nch = nchar(project_directory)
last_ch = substr(project_directory, nch,nch)
if (last_ch != sep){
  project_directory = paste0(project_directory, sep)
}
setwd(project_directory)

numCores<- max(1,min(detectCores() -2, detectCores()))


corpusALE = "corpusALE" 
corpus_from_data = "corpus_from_data"
metrics_SCA = "metrics_SCA"
name_file_sample_CELVA = "sample_CELVA.csv"
corpus_from_data_cleaned = "corpus_from_data_cleaned"
name_zip_file = "raw_text_students.csv"

df_all_metrics = "df_sampleALE_allMetrics.csv"
requirements_feedbacks = "requirements_feedbacks"
path_feedbacks= "feedbacks"
parsedFiles = "ParsedFiles"

data_origine = "from_csv"# "zip_from_moodle"#"from_txt" # possible values: c("from_csv", "from_txt", "zip_from_moodle")

CELVA.sp = TRUE      # FALSE # true if .csv and from CELVA.sp
var_texte = "text"   # name of the variable containing the text if .csv and not from CELVA.sp
var_id = "ID_etudiant"         # name of the variable containing IDs if .csv and not from CELVA.sp

desc_stat_chart = "boxplot" # possible parameters: c("boxplot",  "violin",   "boxplot_point" )
check_creat_directory(path_feedbacks)

unlink(paste0(project_directory,corpusALE), recursive = TRUE)

###########################################
#####                                 #####  
#####   script0 : Query_data          #####   
#####                                 #####  
###########################################

if (data_origine == "from_csv"){
  name_csv_file = "test3.csv"
  source("codes_R/text_import_from_csv.R")
}else if (data_origine == "from_txt"){
  source("codes_R/text_import_from_txt.R")
}else if (data_origine == "zip_from_moodle") 
{
  source("codes_R/text_import.R")
}else{
  stop("inexist value about origine of data!\n thank you to select one the next values: 'from_csv', 'from_txt', 'zip_from_moodle'")
}


###########################################
#####                                 #####  
#####   script1 : ALE-preprocessing   #####   
#####                                 #####  
###########################################

source("codes_R/ALE-preprocessing.R")

###########################################
#####                                 #####  
#####   script2 : make parsed files   #####   
#####                                 #####  
###########################################

unlink(paste0(project_directory,parsedFiles), recursive = TRUE) # delete old directory if exist

source("codes_R/make_parsed_files.R")

###########################################
#####                                 #####  
#####   script3 : ALE all metrics     #####   
#####                                 #####  
###########################################

source("codes_R/ALE-all-metrics.R")

###########################################
#####                                 #####  
#####   script4 : Feedback            #####   
#####                                 #####  
###########################################
source("codes_R/create_feedback.R")
