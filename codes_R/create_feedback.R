#Visualisation of NS and NNS metrics

library(readtext)
library(stringr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(ggiraphExtra) # pour coord_radar, fonction pour mettre le graphe en coordonn?es polaires et lin?ariser les segments incurv?s
library(magick)       # pour importer les pdf sous forme d'images


## Importation de la cohorte temoin et des etudiants a comparer
setwd(project_directory)

df_sampleALE_allMetrics <- read.csv(paste0(metrics_SCA,sep,"df_sampleALE_allMetrics.csv"), stringsAsFactors = FALSE)


df_control_cohort_allMetrics <-  read.csv(paste0(requirements_feedbacks,sep,"cohort_df_sampleALE_allMetrics.csv"), stringsAsFactors = FALSE)




# df_sampleALE_allMetrics$document <-as.character(df_sampleALE_allMetrics$document)
# df_control_cohort_allMetrics$document <- as.character(df_control_cohort_allMetrics$document)




#  merge avec metadata
# df_sampleALE  <-read.csv("P:/suivi_projets/VisLang/Visualisation_linguistique/data/from_csv/CELVA.Sp_398_metadata.csv",
#                                                     encoding="UTF-8")
# colnames(df_sampleALE)[1] = "id_etudiant"
# 
# 
# df_sampleALE_allMetrics_test <- merge(df_sampleALE, df_sampleALE_allMetrics, by="id_etudiant")
# 
# 


df_control_cohort_allMetrics$document <- paste("w1",c(1:nrow(df_control_cohort_allMetrics)), sep = "_")


df_sampleALE_allMetrics$type1 <- "student"
df_sampleALE_allMetrics$type2 <- "student"

df_control_cohort_allMetrics$type1 <- "control cohort"
df_control_cohort_allMetrics$type2 <- df_control_cohort_allMetrics$CECR.niveau
df_control_cohort_allMetrics <- df_control_cohort_allMetrics[which(is.na(df_control_cohort_allMetrics$type2)==F),]



## Creation NDW
df_sampleALE_allMetrics$NDW <- round(df_sampleALE_allMetrics$TTR * df_sampleALE_allMetrics$W)
df_control_cohort_allMetrics$NDW <- round(df_control_cohort_allMetrics$TTR * df_control_cohort_allMetrics$W)



## Merge final
df_sampleALE_allMetrics <- df_sampleALE_allMetrics[,c("document","CTTR","W","T",
                                                      "RIX","NDW",
                                                      "MLT",
                                                      "CN.T","CP.T","K",
                                                      "type1","type2")]

df_control_cohort_allMetrics <- df_control_cohort_allMetrics[,c("document","CTTR","W","T",
                                                                "RIX","NDW",
                                                                "MLT",
                                                                "CN.T","CP.T","K",
                                                                "type1","type2")]

df_NS_NNS_allMetrics <-rbind(df_control_cohort_allMetrics, df_sampleALE_allMetrics)


df_NS_NNS_allMetrics$type1 <-as.factor(df_NS_NNS_allMetrics$type1)
df_NS_NNS_allMetrics$type2 <-as.factor(df_NS_NNS_allMetrics$type2)


df_NS_NNS_allMetrics$K <- 1/df_NS_NNS_allMetrics$K

names(df_NS_NNS_allMetrics) <- c("document","CTTR\nText.variation.words","W\nText.size.words","T\nText.size.sentence",
                                 "RIX\nSentence.difficulty","NDW\nText.size.type","MLT\nSentence.size",
                                 "CN/T\nSentence.complex_nominals","CP/T\nSentence.coordination",
                                 "1/K\nText.repetitions","type1","type2")

#names(df_NS_NNS_allMetrics) <- c("document","CTTR","W","T","RIX","NDW","MLT",
#                                 "CN/T","CP/T","1/K","type1","type2")

#names(df_NS_NNS_allMetrics) <- c("document","CTTR","W","T","RIX","NDW","MLT",
#                                 "CN/T","CP/T","K","type1","type2")



## Import  description des variables

description <- read.csv(paste0(requirements_feedbacks,"/","Variables_and_student_feedback.csv"), fileEncoding = "UTF-8")
names(description) <- gsub(names(description), pattern = ".", replacement = " ", fixed = T)







############################################################## 
#########    Partie visualisation
##############################################################


###### Mise en forme du tableau de description des variables

theme_desc <- gridExtra::ttheme_default(
  core = list(fg_params=list(hjust=0, cex = 1, x=rep(0.01,11)),
              padding = unit(c(4, 6), "mm")),
  colhead = list(fg_params=list(cex = 1)),
  rowhead = list(fg_params=list(cex = 1)),
  base_size = 12)


###### Tableau de description des variables

gr <- tableGrob(description, rows=NULL, theme=theme_desc)



###### Mise en forme du tableau de donnees mis sous le radar

tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)),
                     base_size = 10,
                     padding = unit(c(2, 4), "mm"))

tt2 <- ttheme_default(core = list(fg_params=list(cex = 0.9)),
                      colhead = list(fg_params=list(cex = 0.9)),
                      rowhead = list(fg_params=list(cex = 0.9)))



###### Page de garde et sommaire

im_pdf <- image_read_pdf(paste0(requirements_feedbacks,"/", "page_garde_sommaire.pdf"))


###### Image si radar impossible ? realiser (image libre de droit)

img<-image_read(paste0(requirements_feedbacks, "/","man-3591573_1280.jpg"))
im_plot <- image_ggplot(img)



###### Fonction de visualisation

viz <- function(student_ID){
  
  # On ne garde que l'etudiant concerne
  df_NS_NNS_9metrics <- df_NS_NNS_allMetrics[which(str_sub(df_NS_NNS_allMetrics$document,1,2)=="w1" | df_NS_NNS_allMetrics$document==student_ID),]
  
  
  ###### Page de garde et sommaire
  
  grid.arrange(image_ggplot(im_pdf[1]))
  grid.arrange(image_ggplot(im_pdf[2]), bottom=textGrob("1", x=0.5, y=2, hjust=0, gp=gpar( fontface="italic")))
  
  
  
  ###### Texte de l'etudiant
  
  text <- as.character(df_sampleALE$text[which(df_sampleALE$doc_id == student_ID)])
  
  # Mise en page selon le nombre de mots du texte pour un meilleur affichage
  if(df_NS_NNS_9metrics$W[which(df_NS_NNS_9metrics$document==student_ID)] > 450){
    fontsize_var <- 12
    xmin <- -0.68
    var_width = 120
  } else if(df_NS_NNS_9metrics$W[which(df_NS_NNS_9metrics$document==student_ID)] <= 450 & df_NS_NNS_9metrics$W[which(df_NS_NNS_9metrics$document==student_ID)] > 200){
    fontsize_var <- 15
    xmin <- -0.8
    var_width = 120
  } else {
    fontsize_var <- 18
    xmin <- -0.85
    var_width = 100
  }
  
  text <- str_wrap(text, width = var_width)
  
  text_plot <- ggplot() +
    annotation_custom(grid::textGrob(text, gp=gpar(fontsize=fontsize_var,font=50), hjust=0 ), xmin = xmin) +
    theme_bw() +
    theme(panel.grid=element_blank(), 
          panel.background=element_rect(fill = "transparent",colour = NA),
          panel.border=element_blank(),axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  grid.arrange(top=textGrob("Your text", x=0.5, y=-0.1, gp=gpar(fontsize=30,font=50)), text_plot,
               bottom=textGrob("2", x=0.5, y=2, hjust=0, gp=gpar( fontface="italic")))
  
  
  
  ###### Graphique tableau de description des indicateurs
  
  grid.arrange(top=textGrob("Table of the indicators of linguistic richness", x=0.5, y=-0.1, gp=gpar(fontsize=30,font=50)), gr,
               bottom=textGrob("3", x=0.5, y=2, hjust=0, gp=gpar( fontface="italic")))
  
  
  
  ###### Radar chart
  
  
  ### Bornes indicateurs
  
  indic <- names(df_NS_NNS_9metrics)[2:(ncol(df_NS_NNS_9metrics)-2)]
  #          "CTTR"  "W" "T" "RIX" "NDW" "MLT" "CN.T" "CP.T"  "1/K"        K dans [0;800]  ou [0;600]  
  minimum <- c(0.5,  15,  1,  1.5,   15,    5,     0,     0,   1/800)
  maximum <- c(  9, 900, 35,   15,  900,   40,     5,     3,    1/50)
  tab_indic <- data.frame(indic, minimum, maximum)
  tab_indic$indic <- as.character(tab_indic$indic)
  tab_indic$out <- NA
  for(i in 1:nrow(tab_indic)){
    
    etud <- df_NS_NNS_9metrics[which(df_NS_NNS_9metrics$type1=="student"),tab_indic$indic[i]]
    
    contr_min <- tab_indic$minimum[i]
    contr_max <- tab_indic$maximum[i]
    
    if(etud >= contr_min & etud <= contr_max){
      tab_indic$out[i] <- "non"
    } else {
      tab_indic$out[i] <- "oui"
    }
  }
  
  
  ### Normalisation
  
  df_NS_NNS_9metrics_norm    <- df_NS_NNS_9metrics
  
  for(i in 1:nrow(tab_indic)){
    df_NS_NNS_9metrics_norm[,tab_indic$indic[i]] <- (df_NS_NNS_9metrics_norm[,tab_indic$indic[i]] - tab_indic$minimum[i]) / (tab_indic$maximum[i] - tab_indic$minimum[i])
  }
  
  
  ### Niveaux
  
  niveaux <- c("A1","A2","B1","B2","C1","C2")
  
  
  ### Pagination
  
  page <- as.character(4:9)
  
  
  
  ### Boucle
  
  
  for(n in 1:length(niveaux)){
    
    
    # Choix niveau
    
    niv <- niveaux[n]
    
    
    # Tableau correspondant
    
    tb <- df_NS_NNS_9metrics[which(df_NS_NNS_9metrics$type2==niv | df_NS_NNS_9metrics$type2=="student"),]
    
    tb$`1/K\nText.repetitions` <- 1/tb$`1/K\nText.repetitions` 
    #names(tb)[which(names(tb)=="1/K")] <- "K"
    names(tb)[which(names(tb)=="1/K\nText.repetitions")] <- "K\nText.repetitions"
    
    #tb <- aggregate(tb[,tab_indic$indic], by = list(group = tb[,"type2"]), FUN = median)
    tb <- aggregate(tb[,names(tb)[2:(ncol(tb)-2)]], by = list(group = tb[,"type2"]), FUN = median)
    tb[,2:ncol(tb)] <- round(tb[,2:ncol(tb)],2)
    levels(tb$group)[1:6] <- paste("median of", levels(tb$group)[1:6])
    
    tbl <- tableGrob(tb, rows=NULL, theme=tt2)
    
    
    
    if(all(tab_indic$out=="oui" | length(tab_indic$out[which(tab_indic$out=="non")]) <3)){
      print("Trop peu d'indicateurs pour afficher un radar (moins de 3)")
      
      if(all(tab_indic$out=="oui")){
        text = "You are off radar for all indicators"
      } else {text = "You are off radar for most indicators"}
      
      data.text <- ggplot() + 
        ggplot2::annotate("text", x = 0, y = 25, size=6, label=text) + 
        theme_bw() +
        theme(panel.grid=element_blank(), 
              panel.background=element_rect(fill = "transparent",colour = NA),
              panel.border=element_blank(),axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
      
      grid.arrange(im_plot, data.text, tbl,
                   nrow = 3, heights = c(2.75, 0.25, 0.75),
                   as.table = TRUE,
                   bottom=textGrob(page[n], x=0.5, y=2, hjust=0, gp=gpar( fontface="italic")))
      
    } else {
      
      dd_norm <- df_NS_NNS_9metrics_norm[which(df_NS_NNS_9metrics_norm$type2==niv | df_NS_NNS_9metrics_norm$type2=="student"),]
      
      radar <- aggregate(dd_norm[,tab_indic$indic[which(tab_indic$out=="non" & tab_indic$indic != "W\nText.size.words"  & tab_indic$indic != "T\nText.size.sentence" & tab_indic$indic != "NDW\nText.size.type")]], by = list(group = dd_norm[,"type2"]), FUN = median)
      
      radar_bis <- as.data.frame(t(radar))
      radar_bis$indic <- row.names(radar_bis)
      names(radar_bis) <- as.vector(t(radar_bis[1,]))
      radar_bis <- radar_bis[-1,]
      rownames(radar_bis) <- 1:nrow(radar_bis)
      radar_bis[,which(names(radar_bis)==niv)] <- as.numeric(as.character(radar_bis[,which(names(radar_bis)==niv)]))
      radar_bis$student <- as.numeric(as.character(radar_bis$student))
      
      q1 <- unlist( lapply(dd_norm[which(dd_norm$type2!="student"),tab_indic$indic[which(tab_indic$out=="non" & tab_indic$indic != "W\nText.size.words"  & tab_indic$indic != "T\nText.size.sentence" & tab_indic$indic != "NDW\nText.size.type")]], function(z){quantile(z, probs=c(0.25))}))
      
      q3 <- unlist( lapply(dd_norm[which(dd_norm$type2!="student"),tab_indic$indic[which(tab_indic$out=="non" & tab_indic$indic != "W\nText.size.words"  & tab_indic$indic != "T\nText.size.sentence" & tab_indic$indic != "NDW\nText.size.type")]], function(z){quantile(z, probs=c(0.75))}))
      
      radar_bis$lower <- q1
      radar_bis$upper <- q3
      
      radar_bis <- radar_bis[order(radar_bis$group),]
      
      add <- radar_bis[1,]
      add$group <- NA
      
      radar_bis_bis <- rbind(radar_bis,add)
      
      p <- radar_bis_bis %>%
        ggplot(aes(x = group, y = radar_bis_bis[,niv], group = 1)) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = "Q1-Q3 of control group"), alpha = 0.4) +
        scale_x_discrete(expand = c(0,0), breaks = radar_bis$group) + 
        geom_line(colour = "black") +
        geom_point(aes(colour = paste("median of",names(radar_bis_bis)[1], sep = " "))) +
        theme_light() +
        theme(panel.grid.minor = element_blank()) + 
        geom_line(aes(x = group, y = student, group = 1), colour = "orange") +
        geom_point(aes(x = group, y = student, group = 1, colour = "student")) +
        geom_ribbon(aes(ymin = -0.10, ymax = -0.01), fill = "white", alpha = 1) +
        ylim(c(-0.10,1.00)) +
        geom_text(x=1, y=-0.02, label="0", alpha=0.4) + 
        geom_text(x=1, y=1.01, label="1") + 
        coord_radar() +
        labs(x = "", y = "") +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())  + 
        ggtitle(paste0("Radar chart : Student vs. ", names(radar_bis_bis)[1])) + 
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_colour_manual("Group : ", 
                            breaks = c(paste("median of",names(radar_bis_bis)[1], sep = " "),"student"),
                            values = c("black", "orange"))  +
        scale_fill_manual("Colored strip :", 
                          breaks = c("Q1-Q3 of control group"),
                          values = c("grey70")) +
        theme(legend.position="bottom",
              #legend.box = "vertical",
              legend.text=element_text(size=12),
              plot.title=element_text(size=20),
              axis.text.x = element_text(size = 8)) +
        guides(color = guide_legend(order = 1))
      
      
      
      ddc <- df_NS_NNS_9metrics[which(df_NS_NNS_9metrics$type2==niv | df_NS_NNS_9metrics$type2=="student"),]
      ddc <- ddc[,c("document","W\nText.size.words","T\nText.size.sentence","NDW\nText.size.type","type2")]
      
      ddc_byVar <-gather(ddc,  key = "Metric", 
                         value = "Value", -c("document","type2")
      )
      
      if(desc_stat_chart=="boxplot_point"){
        p1 <- ggplot(ddc_byVar[which(ddc_byVar$type2==niv),]) +
          geom_boxplot(aes(x=Metric, y=Value, fill="")) +
          scale_fill_manual(name="",values=c("white"), labels=c("Boxplot of control group")) +
          geom_jitter(aes(x=Metric, y=Value, size ="student of control group"), position= position_jitter(0.4)) +
          geom_hline(data = ddc_byVar[which(ddc_byVar$type=="student"),], aes(yintercept = Value, color=""), size = 1) +
          facet_wrap(~Metric,scales = "free", drop = FALSE) + 
          #facet_wrap(~Metric,scales = "free", drop = FALSE, ncol=1) + 
          #theme(legend.position = "bottom") +
          ggtitle(paste0("Boxplots : Student vs. ", names(radar_bis_bis)[1])) +
          xlab("") +
          scale_colour_manual(name = "", labels = c("student"), values=c("orange")) +
          theme(plot.title = element_text(hjust = 0.5))  +
          #coord_flip() +
          guides(color = guide_legend(order = 1)) + 
          theme(legend.position="bottom",
                #legend.box = "vertical",
                legend.text=element_text(size=12),
                plot.title=element_text(size=20),
                axis.text.x = element_text(size = 13)) +
          theme(legend.title=element_blank())
      }
      
      if(desc_stat_chart=="boxplot"){
        p1 <- ggplot(ddc_byVar[which(ddc_byVar$type2==niv),]) +
          geom_boxplot(aes(x=Metric, y=Value, fill="")) +
          scale_fill_manual(name="",values=c("white"), labels=c("Boxplot of control group")) +
          #geom_jitter(aes(x=Metric, y=Value, size ="student of control group"), position= position_jitter(0.4)) +
          geom_hline(data = ddc_byVar[which(ddc_byVar$type=="student"),], aes(yintercept = Value, color=""), size = 1) +
          facet_wrap(~Metric,scales = "free", drop = FALSE) + 
          #facet_wrap(~Metric,scales = "free", drop = FALSE, ncol=1) + 
          #theme(legend.position = "bottom") +
          ggtitle(paste0("Boxplots : Student vs. ", names(radar_bis_bis)[1])) +
          xlab("") +
          scale_colour_manual(name = "", labels = c("student"), values=c("orange")) +
          theme(plot.title = element_text(hjust = 0.5))  +
          #coord_flip() +
          guides(color = guide_legend(order = 1)) + 
          theme(legend.position="bottom",
                #legend.box = "vertical",
                legend.text=element_text(size=12),
                plot.title=element_text(size=20),
                axis.text.x = element_text(size = 13))
      }
      
      if(desc_stat_chart=="violin"){
        p1 <- ggplot(ddc_byVar[which(ddc_byVar$type2==niv),]) +
          geom_violin(aes(x=Metric, y=Value, fill="")) +
          geom_boxplot(aes(x=Metric, y=Value, fill=""), width=0.1) +
          scale_fill_manual(name="",values=c("white"), labels=c("Boxplot of control group")) +
          #geom_jitter(aes(x=Metric, y=Value), position= position_jitter(0.4)) +
          geom_hline(data = ddc_byVar[which(ddc_byVar$type=="student"),], aes(yintercept = Value, color=""), size = 1) +
          facet_wrap(~Metric,scales = "free", drop = FALSE) + 
          #facet_wrap(~Metric,scales = "free", drop = FALSE, ncol=1) + 
          #theme(legend.position = "bottom") +
          ggtitle(paste0("Violin plots : Student vs. ", names(radar_bis_bis)[1])) +
          xlab("") +
          scale_colour_manual(name = "", labels = c("student"), values=c("orange")) +
          theme(plot.title = element_text(hjust = 0.5))  +
          #coord_flip() +
          guides(color = guide_legend(order = 1)) + 
          theme(legend.position="bottom",
                #legend.box = "vertical",
                legend.text=element_text(size=12),
                plot.title=element_text(size=20),
                axis.text.x = element_text(size = 13)) 
      }
      
      
      
      
      
      if(all(tab_indic$out=="non")){
        
        grid.arrange(p, p1, tbl,
                     heights = c(2, 0.5),
                     layout_matrix = rbind(c(1, 2),
                                           c(3, 3)),
                     as.table = TRUE,
                     bottom=textGrob(page[n], x=0.5, y=2, hjust=0, gp=gpar( fontface="italic")))
        
      } else {
        
        text = paste("You are off radar for the following indicators :", paste(  unlist(str_split(tab_indic$indic[which(tab_indic$out=="oui")] , pattern = "\n"))[2*(1:length(tab_indic$indic[which(tab_indic$out=="oui")]) )-1]   ,collapse=", "), sep = " ")
        data.text <- ggplot() + 
          ggplot2::annotate("text", x = 0, y = 25, size=6, label=text) + 
          theme_bw() +
          theme(panel.grid=element_blank(), 
                panel.background=element_rect(fill = "transparent",colour = NA),
                panel.border=element_blank(),axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank())
        
        grid.arrange(p, p1, data.text, tbl,
                     heights = c(2.75, 0.25, 0.75),
                     layout_matrix = rbind(c(1, 2),
                                           c(3, 3),
                                           c(4, 4)),
                     as.table = TRUE,
                     bottom=textGrob(page[n], x=0.5, y=2, hjust=0, gp=gpar( fontface="italic"))) 
        
      }
      
    }
  }
  
  
}



###### Creation des fichiers de feedback
student_ID <- df_sampleALE_allMetrics$document 


for(i in 1:length(student_ID)){
  nom <- student_ID[i]
  tfile <- paste(path_feedbacks,"/", nom, " ", Sys.Date() ,".pdf", sep="")
  pdf(tfile,width=15,height=10)
  
  ## appel fonction de visualisation
  viz(nom)
  ##
  
  dev.off()
}
