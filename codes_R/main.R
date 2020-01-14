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
  project_directory = "~/Documents/Thomas/MaRecherche/SoftwareApplications/Visualisation_linguistique/"
  sep = "/"
}

setwd(project_directory)

corpusALE = "corpusALE" 
corpusSCELVA = "corpusSCELVA"
metrics_SCA = "metrics_SCA"
name_file_sample_SCELVA = "sample_SCELVA.csv"
corpus_SCELVA_cleaned = "corpusSCELVA_cleaned"
name_zip_file = "raw_text_students.csv"
df_all_metrics = "df_sampleALE_allMetrics.csv"
requirements_feedbacks = "requirements_feedbacks"
path_feedbacks= "feedbacks"
parsedFiles = "ParsedFiles"
data_from_csv = TRUE #true if .csv and FALSE if .zip from MOODLE
check_creat_directory(path_feedbacks)

unlink(paste0(project_directory,corpusALE), recursive = TRUE)

###########################################
#####                                 #####  
#####   script0 : Query_data          #####   
#####                                 #####  
###########################################
if (data_from_csv){
  source("codes_R/text_import_from_csv.R")
}else{
  source("codes_R/text_import.R")
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
