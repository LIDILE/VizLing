library(stringr)

#Input: df_sampleALE.csv created with ALE-processing.r 
#Output: df_sample_all_metrics.csv 

# Compute and plot readability metrics 
#For non natives
df_sampleALE_readability <-textstat_readability(df_sampleALE$text, measure = c("all", "ARI", "ARI.simple",
                                                                               "Bormuth", "Bormuth.GP", "Coleman", "Coleman.C2", "Coleman.Liau",
                                                                               "Coleman.Liau.grade", "Coleman.Liau.short", "Dale.Chall",
                                                                               "Dale.Chall.old", "Dale.Chall.PSK", "Danielson.Bryan",
                                                                               "Danielson.Bryan.2", "Dickes.Steiwer", "DRP", "ELF",
                                                                               "Farr.Jenkins.Paterson", "Flesch", "Flesch.PSK", "Flesch.Kincaid", "FOG",
                                                                               "FOG.PSK", "FOG.NRI", "FORCAST", "FORCAST.RGL", "Fucks", "Linsear.Write",
                                                                               "LIW", "nWS", "nWS.2", "nWS.3", "nWS.4", "RIX", "Scrabble", "SMOG",
                                                                               "SMOG.C", "SMOG.simple",      "SMOG.de", "Spache", "Spache.old",
                                                                               "Strain", "Traenkle.Bailer", "Traenkle.Bailer.2", "Wheeler.Smith",
                                                                               "meanSentenceLength", "meanWordSyllables"), remove_hyphens = TRUE,
                                                min_sentence_length = 1, max_sentence_length = 10000,
                                                intermediate = FALSE)


df_sampleALE_readability$document <-df_sampleALE$doc_id


#Transform data frame into corpus object
corpus_sampleALE <-corpus(df_sampleALE, text_field = "text")



# compute and plot lexical complexity metrics
# A document-feature matrix object must first be created, which implies tokenisation. With the dfm we clean and compute occurrences. Finally, with occurrences we can compute metrics such as TTR.


#Tokenise
corpus_sampleALE_tokens <- tokens(corpus_sampleALE, remove_punct=TRUE)

#construct a document-feature matrix. One line per student text. 
sampleALE_dfm  <- dfm(corpus_sampleALE_tokens)

#Remove specific features from dfm (stopwords)
sampleALE_dfm <- dfm_select(sampleALE_dfm, stopwords('en'), selection = 'remove')

# sampleALE_dfm <- dfm_remove(sampleALE_dfm, c('(',')',':','.','/',',','"'))   # ; ?
measures_lex <- c("TTR", "C", "R", "CTTR", "U", "S", "Maas")
sampleALE_lexdiv <- textstat_lexdiv(sampleALE_dfm, 
                                    measure = c("all", measures_lex, "K"), 
                                    log.base = 10)

# replace NaN by min/max value
for (measure in measures_lex){
  pos_NaN <- is.nan(sampleALE_lexdiv[[measure]])
  sampleALE_lexdiv[[measure]][pos_NaN] <- 0
}
# replace k=0 by a big value
pos_zero_k = which(sampleALE_lexdiv[['K']]==0)
sampleALE_lexdiv[['K']][pos_zero_k] = 1e+6
#Syntactic complexity metrics
df_sampleALE_syntcompl <- read.csv(paste0(metrics_SCA,sep, paste0(metrics_SCA,".csv")), 
                                   row.names=1, stringsAsFactors=FALSE)


#Remove .txt extension in document id name
row.names(df_sampleALE_syntcompl) <- gsub('.txt', '', row.names(df_sampleALE_syntcompl), fixed=TRUE)
df_sampleALE_syntcompl$document <- row.names(df_sampleALE_syntcompl)



#merge all metrics in one df 
df_sampleALE_allMetrics <- merge(sampleALE_lexdiv, df_sampleALE_readability, by="document")
df_sampleALE_allMetrics <- merge(df_sampleALE_allMetrics, df_sampleALE_syntcompl, by="document")


names(df_sampleALE_allMetrics)[which(str_sub(names(df_sampleALE_allMetrics), start = -2)==".x")] <- str_sub(
  names(df_sampleALE_allMetrics)[which(str_sub(names(df_sampleALE_allMetrics), start = -2)==".x")], start = 1, end = -3
  )

names(df_sampleALE_allMetrics)[which(str_sub(names(df_sampleALE_allMetrics), start = -2)==".y")] <- paste(
  str_sub(names(df_sampleALE_allMetrics)[which(str_sub(names(df_sampleALE_allMetrics), start = -2)==".y")], start = 1, end = -3),".1",sep = ""
  )



write.csv(df_sampleALE_allMetrics, 
          file=paste0(metrics_SCA, sep, df_all_metrics), 
          row.names = FALSE)
cat("-->> Finish script of calculating metrics! \n")




