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

  numCores<-  max(1, detectCores() -2)
  
  registerDoMC(cores = numCores) # make a fork cluster

}

nch = nchar(project_directory)
last_ch = substr(project_directory, nch,nch)
if (last_ch != sep){
  project_directory = paste0(project_directory, sep)
}
setwd(project_directory)

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

CELVA.sp =  FALSE      # FALSE # true if .csv and from CELVA.sp

desc_stat_chart = "boxplot" # possible parameters: c("boxplot",  "violin",   "boxplot_point" )
check_creat_directory(path_feedbacks)

unlink(paste0(project_directory,corpusALE), recursive = TRUE)

choice_data_origine = 0
while (!choice_data_origine %in% 1:3){
  choice_data_origine <- as.integer(readline(prompt="Specify the origin of your data, select one choice:\n
                           [1] : from CSV file\n
                           [2] : from text file\n
                           [3] : from zip moodle file"))
}

data_origine_possibilities <- c("from_csv", "from_txt", "zip_from_moodle")
data_origine = data_origine_possibilities[choice_data_origine]
###########################################
#####                                 #####  
#####   script0 : Query_data          #####   
#####                                 #####  
###########################################

if (data_origine == "from_csv"){
  name_csv_file <-readline(prompt="Enter file name (such as example.csv): ")
  while(! name_csv_file %in% list.files("data/from_csv")){
    name_csv_file <-readline(prompt="File not found!, enter file name (such as example.csv): ")
  }
  source("codes_R/text_import_from_csv.R")
}else if (data_origine == "from_txt"){
  source("codes_R/text_import_from_txt.R")
}else if (data_origine == "zip_from_moodle") {
  source("codes_R/text_import.R")
}else{
  stop("Non-existing value for the origin of your data !\n Please select one the following values: 'from_csv', 'from_txt', 'zip_from_moodle'")
}

# var_texte = "text"   # name of the variable containing the text if .csv and not from CELVA.sp
# var_id = "ID_etudiant"         # name of the variable containing IDs if .csv and not from CELVA.sp


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
