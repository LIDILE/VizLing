# create a directory
check_creat_directory = function(path){
  dir.create(file.path(path), showWarnings = FALSE)
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
  
  project_directory = "P:\\suivi_projets\\VisLang\\Visualisation_linguistique-master\\" #"P:\\suivi_projets\\VisLang\\VisLang\\" 
  sep = "\\"
}else{
  project_directory = "/home/knefati/Documents/MyWork-Ensai/VizLing/"
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

check_creat_directory(path_feedbacks)

###########################################
#####                                 #####  
#####   script0 : Query_data          #####   
#####                                 #####  
###########################################

source("codes_R/text_import.R")

###########################################
#####                                 #####  
#####   script1 : ALE-preprocessing   #####   
#####                                 #####  
###########################################

source("codes_R/ALE-preprocessing_zip_files.R")
# source("codes_R/ALE-preprocessing.R")

###########################################
#####                                 #####  
#####   script2 : make parsed files   #####   
#####                                 #####  
###########################################

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

source("code_R/create_feedback.R")
