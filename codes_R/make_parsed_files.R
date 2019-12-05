library("readtext")
division = function(x,y){
  if (x==0 | y==0){
    return (0)
  }
  return (x/y)
}

if (os == "Windows"){
  #Stanford parser
  appli_parser="lexparser.bat"
  appli_tregrex = paste0('java -mx300m -cp "', 'stanford-tregex.jar;" edu.stanford.nlp.trees.tregex.TregexPattern ')
  
  #the following is a list of tregex patterns for various structures
  
  #sentence (S)
  s='\"ROOT\"'
  
  #verb phrase (VP)
  vp='\"VP > S|SINV|SQ\"'  
  
  "'VP > S|SINV|SQ'"
  vp_q='\"MD|VBZ|VBP|VBD > (SQ !< VP)\"'
  
  #clause (C)
  c='\"S|SINV|SQ [> ROOT <, (VP <# VB) | <# MD|VBZ|VBP|VBD | < (VP [<# MD|VBP|VBZ|VBD | < CC < (VP <# MD|VBP|VBZ|VBD)])]\"'
  
  #T-unit (T)
  t='\"S|SBARQ|SINV|SQ > ROOT | [$-- S|SBARQ|SINV|SQ !>> SBAR|VP]\"'
  
  #dependent clause (DC)
  dc='\"SBAR < (S|SINV|SQ [> ROOT <, (VP <# VB) | <# MD|VBZ|VBP|VBD | < (VP [<# MD|VBP|VBZ|VBD | < CC < (VP <# MD|VBP|VBZ|VBD)])])\"'
  
  #complex T-unit (CT)
  ct='\"S|SBARQ|SINV|SQ [> ROOT | [$-- S|SBARQ|SINV|SQ !>> SBAR|VP]] << (SBAR < (S|SINV|SQ [> ROOT <, (VP <# VB) | <# MD|VBZ|VBP|VBD | < (VP [<# MD|VBP|VBZ|VBD | < CC < (VP <# MD|VBP|VBZ|VBD)])]))\"'
  
  #coordinate phrase (CP)
  cp='\"ADJP|ADVP|NP|VP < CC\"'
  
  #complex nominal (CN)
  cn1="\"NP !> NP [<< JJ|POS|PP|S|VBG | << (NP $++ NP !$+ CC)]\""
  
  cn2='\"SBAR [<# WHNP | <# (IN < That|that|For|for) | <, S] & [$+ VP | > VP]\"'
  cn3='\"S < (VP <# VBG|TO) $+ VP\"'
  
  #fragment clause
  fc='\"FRAG > ROOT !<< (S|SINV|SQ [> ROOT <, (VP <# VB) | <# MD|VBZ|VBP|VBD | < (VP [<# MD|VBP|VBZ|VBD | < CC < (VP <# MD|VBP|VBZ|VBD)])])\"'
  
  #fragment T-unit
  ft='\"FRAG > ROOT !<< (S|SBARQ|SINV|SQ > ROOT | [$-- S|SBARQ|SINV|SQ !>> SBAR|VP])\"'
  
}else{
  #Stanford parser
  appli_parser="./lexparser.sh"
  appli_tregrex = paste0('java -mx300m -cp "', 'stanford-tregex.jar:" edu.stanford.nlp.trees.tregex.TregexPattern ')
  # the following is a list of tregex patterns for various structures
  
  # sentence (S)
  s="'ROOT'"
  
  #verb phrase (VP)
  vp="'VP > S|SINV|SQ'"
  vp_q="'MD|VBZ|VBP|VBD > (SQ !< VP)'"
  
  #clause (C)
  c="'S|SINV|SQ [> ROOT <, (VP <# VB) | <# MD|VBZ|VBP|VBD | < (VP [<# MD|VBP|VBZ|VBD | < CC < (VP <# MD|VBP|VBZ|VBD)])]'"
  
  #T-unit (T)
  t="'S|SBARQ|SINV|SQ > ROOT | [$-- S|SBARQ|SINV|SQ !>> SBAR|VP]'"
  
  #dependent clause (DC)
  dc="'SBAR < (S|SINV|SQ [> ROOT <, (VP <# VB) | <# MD|VBZ|VBP|VBD | < (VP [<# MD|VBP|VBZ|VBD | < CC < (VP <# MD|VBP|VBZ|VBD)])])'"
  
  #complex T-unit (CT)
  ct="'S|SBARQ|SINV|SQ [> ROOT | [$-- S|SBARQ|SINV|SQ !>> SBAR|VP]] << (SBAR < (S|SINV|SQ [> ROOT <, (VP <# VB) | <# MD|VBZ|VBP|VBD | < (VP [<# MD|VBP|VBZ|VBD | < CC < (VP <# MD|VBP|VBZ|VBD)])]))'"
  
  #coordinate phrase (CP)
  cp="'ADJP|ADVP|NP|VP < CC'"
  
  #complex nominal (CN)
  cn1="'NP !> NP [<< JJ|POS|PP|S|VBG | << (NP $++ NP !$+ CC)]'"
  cn2="'SBAR [<# WHNP | <# (IN < That|that|For|for) | <, S] & [$+ VP | > VP]'"
  cn3="'S < (VP <# VBG|TO) $+ VP'"
  
  #fragment clause
  fc="'FRAG > ROOT !<< (S|SINV|SQ [> ROOT <, (VP <# VB) | <# MD|VBZ|VBP|VBD | < (VP [<# MD|VBP|VBZ|VBD | < CC < (VP <# MD|VBP|VBZ|VBD)])])'"
  
  #fragment T-unit
  ft="'FRAG > ROOT !<< (S|SBARQ|SINV|SQ > ROOT | [$-- S|SBARQ|SINV|SQ !>> SBAR|VP])'"
}


#list of patterns to search for
patternlist=c(s,vp,c,t,dc,ct,
              cp,cn1,cn2,cn3,fc,ft,vp_q)

## Remark!!
## in Linux, the separator is /
## in Windows, theseparator is \\
###
# example directory windows
# project_directory = "P:\\suivi_projets\\VisLang\\VisLang\\"   
# example directory Linux
# project_directory = "/home/knefati/Documents/MyWork-Ensai/VizLing/"
setwd(project_directory)


#path to the directory or folder containing input files
path_input_text= paste0(project_directory, "corpusALE", sep)#"samples"#
#output file name
output_path_metric_SCA = paste0(project_directory, "metrics_SCA", sep)
output_path_parsed = paste0(project_directory, "ParsedFiles", sep)
metrics_SCA_filename = "ALE_metrics_SCA.csv"
# output file name
check_creat_directory(output_path_metric_SCA)
check_creat_directory(output_path_parsed)

####################
########### outputFile = open(output_path_metric_SCA + metrics_SCA_filename, "w")
####################

#write a list of 24 comma-delimited fields to the output file
fields=c("W","S","VP","C","T","DC","CT","CP","CN","MLS","MLT","MLC","C/S","VP/T",
         "C/T", "DC/C", "DC/T", "T/S", "CT/T", "CP/T", "CP/C", "CN/T", "CN/C")

metrics = data.frame()

#process text files in the directory one by one
#process text files in the directory one by one
raw_text_files = list.files(path_input_text)

# sort files with respect to increasing order
raw_name = function(u){unlist(strsplit(u, split = ".txt"))}
str_name = unname(sapply(raw_text_files, raw_name))
# num_name = unname(sapply(str_name, as.numeric))
num_name_sorted = sort(str_name)
text_files = paste0(num_name_sorted, ".txt")

files_size = length(text_files)

rownames_metrics = c()
index = 1
#filename_text = text_files[127]
for (filename_text in text_files){
  rownames_metrics = c(rownames_metrics, filename_text)
  setwd(paste0(project_directory,"stanford-parser-full-2014-01-04"))
  
  print(paste0(round(index/files_size*100,1), "%", 
               "|Processing file", filename_text, "|", 
               index, "/",  files_size, " ..."))
  
  parsedFile = paste0(output_path_parsed, unlist(strsplit(filename_text, "\\."))[1], "_Rparsed.txt")
  #parse the input file
  command = paste0(appli_parser, " ", path_input_text, filename_text, " > ", parsedFile)
  system(command)
  
  #list of counts of the patterns
  patterncount = c()
  setwd(paste0(project_directory,"stanford-tregex-2014-01-04"))
  
  for (pattern in patternlist){
    command = paste0(appli_tregrex, pattern, " ", parsedFile, " -C -o")
    output = system(command, intern = TRUE)
    count = counter(output)
    patterncount = c(patterncount, count)
  }
  
  #update frequencies of complex nominals, clauses, and T-units
  n = length(patterncount) 
  patterncount[8]=patterncount[n-3]+patterncount[n-4]+patterncount[n-5]
  patterncount[3]=patterncount[3]+patterncount[n-2]
  patterncount[4]=patterncount[4]+patterncount[n-1]
  patterncount[2]=patterncount[2]+patterncount[n]
  
  #word count
  content_infile = readtext(parsedFile)["text"]
  #infile=open(parsedFile,"r")
  #content=infile.read()
  words <- regmatches(content_infile, gregexpr("\\([A-Z]+\\$? [^\\)\\(]+\\)", content_infile, perl=TRUE))$text
  w = length(words)
  
  
  #add frequencies of words and other structures to output string
  row_metrics = c(w)
  
  for (count in patterncount[1:8]){
    row_metrics = c(row_metrics, as.numeric(count))
  }
  
  #list of frequencies of structures other than words
  s  = patterncount[1]
  vp = patterncount[2]
  c  = patterncount[3] 
  t  = patterncount[4]
  dc = patterncount[5]
  ct = patterncount[6]
  cp = patterncount[7]
  cn = patterncount[8]
  
  #compute the 14 syntactic complexity indices
  mls=division(w,s)
  mlt=division(w,t)
  mlc=division(w,c)
  c_s=division(c,s)
  vp_t=division(vp,t)
  c_t=division(c,t)
  dc_c=division(dc,c)
  dc_t=division(dc,t)
  t_s=division(t,s)
  ct_t=division(ct,t)
  cp_t=division(cp,t)
  cp_c=division(cp,c)
  cn_t=division(cn,t)
  cn_c=division(cn,c)
  
  #add syntactic complexity indices to output string
  for (ratio in c(mls,mlt,mlc,c_s,vp_t,c_t,dc_c,dc_t,t_s,ct_t,cp_t,cp_c,cn_t,cn_c)){
    row_metrics = c(row_metrics, round(ratio,4))
  }
  
  
  #write output string to output file
  metrics = rbind(metrics, row_metrics)
  index = index +1
}

colnames(metrics) = fields
rownames(metrics) = rownames_metrics
write.csv(metrics, file = paste0(project_directory,"metrics_SCA", sep,"metrics_SCA.csv"), row.names = TRUE)

cat("-->>Finish parsed files ! \n")
setwd(project_directory)
