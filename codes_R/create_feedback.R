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
library(ggiraphExtra) # pour coord_radar, fonction pour mettre le graphe en coordonnées polaires et linéariser les segments incurvés
library(magick)       # pour importer les pdf sous forme d'images


## Importation de la cohorte temoin et des etudiants a comparer
setwd(project_directory)

df_sampleALE_allMetrics <- read.csv(paste0(metrics_SCA,"/","df_sampleALE_allMetrics.csv"))

if (data_from_csv){
  df_control_cohort_allMetrics <-   read.csv(paste0(metrics_SCA,"/","df_sampleALE_allMetrics.csv"))
}else{
  df_control_cohort_allMetrics <-  read.csv(paste0(metrics_SCA,"/","cohort_df_sampleALE_allMetrics.csv"))
}

# On enleve les deux etudiants qui n'ont rien ecrit ou qui ont ecrit un seul mot dans la cohorte controle
# df_control_cohort_allMetrics <- df_control_cohort_allMetrics[!(df_control_cohort_allMetrics$document %in% c(17000798,15006495)),]


df_sampleALE_allMetrics$document <-as.character(df_sampleALE_allMetrics$document)

df_control_cohort_allMetrics$document <- as.character(df_control_cohort_allMetrics$document)




## Merge de la cohorte temoin avec les annotations du CELVA
scelva <- read.csv2(file = paste0(requirements_feedbacks,"/","CELVA.Sp-full-annotation.csv"), sep = ",", na.strings = "")

# df_sampleALE2 = df_sampleALE
# colnames(df_sampleALE2)[1]="document"
# b=merge(scelva,df_sampleALE2, by="document")[c("document","doc_id", "texte","CECR.niveau")]
# colnames(b)[1:2]=c("id_etudiant", "document")
# b= b[!b$id_etudiant %in% c(17000798,15006495),]
# write.csv(b, file = "CELVA.Sp-full-annotation.csv")
# scelva <- read.csv2(file = paste0(requirements_feedbacks,"/","CELVA.Sp-full-annotation.csv"), sep = ",", na.strings = "")

scelva$CECR.niveau <- as.character(scelva$CECR.niveau)
scelva$CECR.niveau <- as.factor(scelva$CECR.niveau)

df_control_cohort_allMetrics <- merge(scelva, df_control_cohort_allMetrics, by = "document")
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

names(df_NS_NNS_allMetrics) <- c("document","CTTR","W","T","RIX","NDW","MLT",
                                 "CN/T","CP/T","K","type1","type2")



## Import  description des variables

description <- read.csv(paste0(requirements_feedbacks,"/","Variables_and_student_feedback.csv"), fileEncoding = "UTF-8")
names(description) <- gsub(names(description), pattern = ".", replacement = " ", fixed = T)







############################################################## 
#########    Partie visualisation
##############################################################


###### Mise en forme du tableau de description des variables

theme_desc <- gridExtra::ttheme_default(
  core = list(fg_params=list(hjust=0, cex = 2, x=rep(0.01,11)),
              padding = unit(c(4, 8), "mm")),
  colhead = list(fg_params=list(cex = 2)),
  rowhead = list(fg_params=list(cex = 2)),
  base_size = 10)


###### Tableau de description des variables

gr <- tableGrob(description, rows=NULL, theme=theme_desc)



###### Mise en forme du tableau de donnees mis sous le radar

tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)),
                     base_size = 10,
                     padding = unit(c(2, 4), "mm"))

tt2 <- ttheme_default(core = list(fg_params=list(cex = 1.1)),
                      colhead = list(fg_params=list(cex = 1.1)),
                      rowhead = list(fg_params=list(cex = 1.1)))



###### Page de garde et sommaire

im_pdf <- image_read_pdf(paste0(requirements_feedbacks,"/", "page_garde_sommaire.pdf"))


###### Image si radar impossible à realiser (image libre de droit)

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
  
  grid.arrange(top=textGrob("Table of the indicators of linguistic richness", x=0.5, y=-0.5, gp=gpar(fontsize=30,font=50)), gr,
               bottom=textGrob("2", x=0.5, y=2, hjust=0, gp=gpar( fontface="italic")))
  
  
  
  ###### Radar chart
  
  
  ### Bornes indicateurs
  
  indic <- names(df_NS_NNS_9metrics)[2:(ncol(df_NS_NNS_9metrics)-2)]
  #          "CTTR"  "W" "T" "RIX" "NDW" "MLT" "CN.T" "CP.T"  "K"       
  minimum <- c(0.5,  15,  1,  1.5,   15,    5,     0,     0,   0)
  maximum <- c(  9, 900, 35,   15,  900,   40,     5,     3, 800)
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
    
    tb <- aggregate(tb[,tab_indic$indic], by = list(group = tb[,"type2"]), FUN = median)
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
      
      radar <- aggregate(dd_norm[,tab_indic$indic[which(tab_indic$out=="non")]], by = list(group = dd_norm[,"type2"]), FUN = median)
      
      radar_bis <- as.data.frame(t(radar))
      radar_bis$indic <- row.names(radar_bis)
      names(radar_bis) <- as.vector(t(radar_bis[1,]))
      radar_bis <- radar_bis[-1,]
      rownames(radar_bis) <- 1:nrow(radar_bis)
      radar_bis[,which(names(radar_bis)==niv)] <- as.numeric(as.character(radar_bis[,which(names(radar_bis)==niv)]))
      radar_bis$student <- as.numeric(as.character(radar_bis$student))
      
      q1 <- unlist( lapply(dd_norm[which(dd_norm$type2!="student"),tab_indic$indic[which(tab_indic$out=="non")]], function(z){quantile(z, probs=c(0.25))}))
      
      q3 <- unlist( lapply(dd_norm[which(dd_norm$type2!="student"),tab_indic$indic[which(tab_indic$out=="non")]], function(z){quantile(z, probs=c(0.75))}))
      
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
              axis.text.x = element_text(size = 13)) +
        guides(color = guide_legend(order = 1))
      
      
      if(all(tab_indic$out=="non")){
        
        grid.arrange(p, tbl,
                     nrow = 2, heights = c(2, 0.5),
                     as.table = TRUE,
                     bottom=textGrob(page[n], x=0.5, y=2, hjust=0, gp=gpar( fontface="italic")))
        
      } else {
        
        text = paste("You are off radar for the following indicators :", paste(tab_indic$indic[which(tab_indic$out=="oui")],collapse=", "), sep = " ")
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
        
        grid.arrange(p, data.text, tbl,
                     nrow = 3, heights = c(2.75, 0.25, 0.75),
                     as.table = TRUE,
                     bottom=textGrob(page[n], x=0.5, y=2, hjust=0, gp=gpar( fontface="italic")))
        
      }
      
    }
  }
  
  
}



###### Creation des fichiers de feedback
if (data_from_csv){
  student_ID <- scelva$document
}else{
  student_ID <- df_sampleALE_allMetrics$document 
}

for(i in 1:length(student_ID)){
  nom <- student_ID[i]
  tfile <- paste(path_feedbacks,"/", nom, " ", Sys.Date() ,".pdf", sep="")
  pdf(tfile,width=15,height=10)
  
  ## appel fonction de visualisation
  viz(nom)
  ##
  
  dev.off()
}


# ###### Meta-donnees (hist / facet_wrap + boxplot)
# 
# df_sampleALE  <- read.csv2(file = "S:/DUNE-DESIR/VisLinguistique/Test_chaine/VisLang/corpusSCELVA_cleaned/sample_SCELVA.csv", sep = ",")
# 
# df_sampleALE$document <- df_sampleALE$doc_id
# 
# df_sampleALE$doc_id <-NULL
# 
# df_sampleALE_allMetrics_metadata <- merge(df_sampleALE, df_sampleALE_allMetrics, by="document")
# 
# df_sampleALE_allMetrics_metadata$Domaine_de_specialite <- as.character(df_sampleALE_allMetrics_metadata$Domaine_de_specialite)
# df_sampleALE_allMetrics_metadata$Domaine_de_specialite[which(df_sampleALE_allMetrics_metadata$Domaine_de_specialite=="M?decine")] <- "Medecine" 
# df_sampleALE_allMetrics_metadata$Domaine_de_specialite <- as.factor(df_sampleALE_allMetrics_metadata$Domaine_de_specialite)
# 
# scelva <- read.csv2(file = "S:/DUNE-DESIR/VisLinguistique/CELVA.Sp-full-annotation - CELVA.Sp-full-annotation.csv", sep = ",")
# scelva$CECR.niveau <- as.character(scelva$CECR.niveau)
# scelva$CECR.niveau[which(scelva$CECR.niveau=="")] <- NA
# scelva$CECR.niveau[which(scelva$CECR.niveau==" B1")] <- "B1"
# scelva$CECR.niveau <- as.factor(scelva$CECR.niveau)
# 
# df_sample_scelva <- merge(scelva, df_sampleALE_allMetrics_metadata, by = "document")
# #write.csv(df_sample_scelva, 
# #          file=paste0(metrics_SCA, sep, "df_sample_scelva.csv"), 
# #          row.names = FALSE)
# 
# df_sample_scelva_bis <- df_sample_scelva[,c("ID_etudiant","CECR.niveau","Lecture_regularite","Lang_exposition")]
# 
# 
# df_sample_scelva_bis$Lang_exposition <- as.numeric(df_sample_scelva_bis$Lang_exposition)
# 
# df_sample_scelva_bis <- df_sample_scelva_bis[which(is.na(df_sample_scelva_bis$CECR.niveau)==F),]
# 
# 
# # Lang_exposition
# 
# library(RColorBrewer)
# 
# ggplot(df_sample_scelva_bis[which(df_sample_scelva_bis$ID_etudiant!="student"),], aes(x=CECR.niveau, y=Lang_exposition, color=CECR.niveau)) +
#   geom_boxplot(fill="white",outlier.colour = NA) +
#   geom_point(position=position_jitterdodge(dodge.width=0.9, jitter.width = 0.4), alpha=0.5) +
#   scale_color_manual(name="CECR levels",values=brewer.pal(8,"Set1")[-6]) +
#   theme(legend.position = "bottom") +
#   ggtitle("Lang exposition among students with different CECR levels") + xlab("") + ylab("Lang exposition in months") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# 
# 
# 
# # Lecture_regularite
# 
# df_sample_scelva_bis$Lecture_regularite <- as.character(df_sample_scelva_bis$Lecture_regularite)
# df_sample_scelva_bis$Lecture_regularite <- factor(df_sample_scelva_bis$Lecture_regularite, levels = c("jamais","mensuelle","hebdomadaire","quotidienne"))
# 
# 
# proportion <- df_sample_scelva_bis[,c("CECR.niveau", "Lecture_regularite")] %>%
#   group_by(CECR.niveau, Lecture_regularite) %>%
#   tally() %>%
#   group_by(CECR.niveau) %>%
#   mutate(pct = n / sum(n))
# 
# proportion2 <- proportion %>%
#   mutate(y_label = paste0(round(pct*100, 1), "%"))
# 
# levels(proportion2$Lecture_regularite) <- c("never","monthly","weekly","daily")
# 
# ggplot(proportion2, aes(x = Lecture_regularite, y = pct, fill = factor(CECR.niveau))) +
#   geom_bar(stat = "identity", position = "dodge", color = "grey40") +
#   scale_fill_manual("CECR levels", values = rev(brewer.pal(7, "Set1")[-6])) +
#   geom_text(aes(label = y_label), position = position_dodge(0.85), vjust = 1.5, color = "black") +
#   geom_vline(xintercept = 1.5, col='grey', lwd=1.2, linetype="dotted") +
#   geom_vline(xintercept = 2.5, col='grey', lwd=1.2, linetype="dotted") +
#   geom_vline(xintercept = 3.5, col='grey', lwd=1.2, linetype="dotted") +
#   ggtitle("Reading regularity among students with different CECR levels") + xlab("Reading regularity") + ylab("Percentages") +
#   theme(plot.title = element_text(hjust = 0.5))

