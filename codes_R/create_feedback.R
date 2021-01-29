cat("\n Creating personalized feedbacks \n")

# Visualisation of NS and NNS metrics

library(readtext)
library(stringr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
#library(ggiraphExtra) # pour coord_radar, fonction pour mettre le graphe en coordonnees polaires et lineariser les segments incurves
library(magick)       # pour importer les pdf sous forme d'images


## Importation de la cohorte temoin et des etudiants a comparer
setwd(project_directory)

df_sampleALE_allMetrics <- read.csv(paste0(metrics_SCA,sep, df_all_metrics), stringsAsFactors = FALSE)


df_control_cohort_allMetrics <-  read.csv(paste0(requirements_feedbacks,sep,"cohort_df_sampleALE_allMetrics.csv"), stringsAsFactors = FALSE)






df_control_cohort_allMetrics$document <- paste("w1",c(1:nrow(df_control_cohort_allMetrics)), sep = "_")



names(df_sampleALE_allMetrics)[which(names(df_sampleALE_allMetrics) %in% c("C.S","VP.T","C.T","DC.C","DC.T","T.S","CT.T","CP.T","CP.C","CN.T","CN.C"))] <- c("C/S","VP/T","C/T","DC/C","DC/T","T/S","CT/T","CP/T","CP/C","CN/T","CN/C")

names(df_control_cohort_allMetrics)[which(names(df_control_cohort_allMetrics) %in% c("C.S","VP.T","C.T","DC.C","DC.T","T.S","CT.T","CP.T","CP.C","CN.T","CN.C"))] <- c("C/S","VP/T","C/T","DC/C","DC/T","T/S","CT/T","CP/T","CP/C","CN/T","CN/C")




df_sampleALE_allMetrics$type1 <- "student"
df_sampleALE_allMetrics$type2 <- "student"

df_control_cohort_allMetrics$type1 <- "control cohort"
df_control_cohort_allMetrics$type2 <- df_control_cohort_allMetrics$CECR.niveau
df_control_cohort_allMetrics <- df_control_cohort_allMetrics[which(!is.na(df_control_cohort_allMetrics$type2)),]




## Merge final : W has to be kept for visualisation
df_sampleALE_allMetrics <- df_sampleALE_allMetrics[,c("document","CTTR","NDW","W","K","T","FOG.NRI","DC/C",
                                                      "Coleman.C2","Dickes.Steiwer","MLT","DC/T","CT",
                                                      "type1","type2")]

df_control_cohort_allMetrics <- df_control_cohort_allMetrics[,c("document","CTTR","NDW","W","K","T","FOG.NRI","DC/C",
                                                                "Coleman.C2","Dickes.Steiwer","MLT","DC/T","CT",
                                                                "type1","type2")]

df_NS_NNS_allMetrics <-rbind(df_control_cohort_allMetrics, df_sampleALE_allMetrics)


df_NS_NNS_allMetrics$type1 <-as.factor(df_NS_NNS_allMetrics$type1)
df_NS_NNS_allMetrics$type2 <-as.factor(df_NS_NNS_allMetrics$type2)




#list_metrics <- names(df_NS_NNS_allMetrics)[-c(1,length(names(df_NS_NNS_allMetrics))-1,length(names(df_NS_NNS_allMetrics)))]

if("K" %in% names(df_NS_NNS_allMetrics)){
  df_NS_NNS_allMetrics$K <- 1/df_NS_NNS_allMetrics$K
  names(df_NS_NNS_allMetrics)[which(names(df_NS_NNS_allMetrics)=="K")] <- "1/K"
}
if("Dickes.Steiwer" %in% names(df_NS_NNS_allMetrics)){
  df_NS_NNS_allMetrics$Dickes.Steiwer <- 1/df_NS_NNS_allMetrics$Dickes.Steiwer
  names(df_NS_NNS_allMetrics)[which(names(df_NS_NNS_allMetrics)=="Dickes.Steiwer")] <- "1/Dickes.Steiwer"
}

list_metrics <- names(df_NS_NNS_allMetrics)[-c(1,length(names(df_NS_NNS_allMetrics))-1,length(names(df_NS_NNS_allMetrics)))]



### If you don't want to add some description of the indicators in the labels, please comment lines 98 to 113

names(df_NS_NNS_allMetrics) <- c("document",
                                 "Word diversity\n(CTTR)",
                                 "Text size\n(NDW)",
                                 "Text size\n(W)",
                                 "Word repetition\n(1/K)",
                                 "Text size\n(T)",
                                 "Word morphology + Sentence size\n(FOG.NRI)",
                                 "Clause hypotaxis\n(DC/C)",
                                 "Word size&morphology\n(Coleman.C2)",
                                 "Word diversity&size + Sentence size\n(1/Dickes.Steiwer)",
                                 "Sentence size \n(MLT)",
                                 "Sentence hypotaxis\n(DC/T)",
                                 "Text hypotaxis\n(CT)",
                                 "type1","type2")

# Here you have to choose which variables will be displayed in box plots
var_boxplot <- c("Text size\n(W)","Text size\n(T)","Text size\n(NDW)","Text hypotaxis\n(CT)")





## Bornes des indicateurs
indic <- names(df_NS_NNS_allMetrics)[!(names(df_NS_NNS_allMetrics) %in% c("document","type1","type2"))]

#          "CTTR"  "NDW"  "W"  "1/K"  "T"  "FOG.NRI"  "DC/C"  "Coleman.C2"  "1/Dickes.Steiwer"  "MLT"  "DC/T"   "CT"
minimum <- c(0.5,    15,   15, 1/800,   1,        0,      0,            20,             -0.005,    5,      0,      1)
maximum <- c(  9,   900,  900,  1/50,  35,      150,      1,            80,                  0,   40,      2,     35)

# These borns have to be carefully chosen and in harmony with your reference dataset



## Import  description des variables

description <- read.csv(paste0(requirements_feedbacks,"/","Variables_and_student_feedback.csv"), fileEncoding = "UTF-8", stringsAsFactors = F)
names(description) <- gsub(names(description), pattern = ".", replacement = " ", fixed = T)







############################################################## 
#########    Partie visualisation
##############################################################



###### Function from the package 'ggiraphExtra'

coord_radar <- function (theta = "x", start = 0, direction = 1)
{
        theta <- match.arg(theta, c("x", "y"))
        r <- if (theta == "x")
                "y"
        else "x"
        ggproto("CoordRadar", ggplot2::CoordPolar, theta = theta, r = r, start = start,
                direction = sign(direction),
                is_linear = function(coord) TRUE)
}


###### Mise en forme du tableau de description des variables

theme_desc <- gridExtra::ttheme_default(
  core = list(fg_params=list(hjust=0, cex = 1, x=rep(0.01,11)),
              padding = unit(c(4, 6), "mm")),
  colhead = list(fg_params=list(cex = 1)),
  rowhead = list(fg_params=list(cex = 1)),
  base_size = 9.5)


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
  require(stringr)
  require(gridExtra)
  require(magick)
  # On ne garde que l'etudiant concerne
  df_NS_NNS_9metrics <- df_NS_NNS_allMetrics[which(str_sub(df_NS_NNS_allMetrics$document,1,2)=="w1" | df_NS_NNS_allMetrics$document==student_ID),]
  
  
  ###### Page de garde et sommaire
  
  grid.arrange(image_ggplot(im_pdf[1]))
  grid.arrange(image_ggplot(im_pdf[2]), bottom=textGrob("1", x=0.5, y=2, hjust=0, gp=gpar( fontface="italic")))
  
  
  
  ###### Texte de l'etudiant
  
  text <- as.character(df_sampleALE$text[which(df_sampleALE$doc_id == student_ID)])
  
  W_name <- names(df_NS_NNS_allMetrics)[ which(list_metrics=="W") +1]
  
  # Mise en page selon le nombre de mots du texte pour un meilleur affichage
  if(df_NS_NNS_9metrics[,W_name][which(df_NS_NNS_9metrics$document==student_ID)] > 450){
    fontsize_var <- 12
    xmin <- -0.68
    var_width = 120
  } else if(df_NS_NNS_9metrics[,W_name][which(df_NS_NNS_9metrics$document==student_ID)] <= 450 & df_NS_NNS_9metrics[,W_name][which(df_NS_NNS_9metrics$document==student_ID)] > 200){
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
               bottom=textGrob("3", x=0.5, y=1.25, hjust=0, gp=gpar( fontface="italic")))
  
  
  
  ###### Radar chart
  
  
  ### Hors radar ou non pour les differents indicateurs
  
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
    
    #if( any(grepl("1/K", names(tb))) ){
    #  tb[,names(tb)[grepl("1/K", names(tb))]] <- 1/tb[,names(tb)[grepl("1/K", names(tb))]]
    #  names(tb)[grepl("1/K", names(tb))] <- gsub(pattern = "1/K", replacement = "K", x = names(tb)[grepl("1/K", names(tb))])
    #}
    #if( any(grepl("1/Dickes.Steiwer", names(tb))) ){
    #  tb[,names(tb)[grepl("1/Dickes.Steiwer", names(tb))]] <- 1/tb[,names(tb)[grepl("1/Dickes.Steiwer", names(tb))]]
    #  names(tb)[grepl("1/Dickes.Steiwer", names(tb))] <- gsub(pattern = "1/Dickes.Steiwer", replacement = "Dickes.Steiwer", x = names(tb)[grepl("1/Dickes.Steiwer", names(tb))])
    #}
    

    tb <- aggregate(tb[,names(tb)[2:(ncol(tb)-2)]], by = list(group = tb[,"type2"]), FUN = median)
    levels(tb$group)[1:6] <- paste("median of", levels(tb$group)[1:6])
    names(tb)[-1] <- list_metrics
    
    #tb[,2:ncol(tb)] <- round(tb[,2:ncol(tb)],2)
    
    for(j in 2:ncol(tb)){
      if( any(grepl("1/", names(tb)[j])) ){
        tb[,j] <- 1/tb[,j]
        tb[,j] <- round(tb[,j],2)
        
        if(any(tb[,j] < 0)){
          tb[which(tb[,j] >= 0),j] <- paste0("1/", tb[which(tb[,j] >= 0),j])
          tb[which(tb[,j]  < 0),j] <- paste0("-1/", -tb[which(tb[,j]  < 0),j])
        } else { tb[,j] <- paste0("1/", as.character(tb[,j]))}
        
      
      } else{tb[,j] <- round(tb[,j],2)}
    }
    
    
    
    
    
    
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
      
      #radar <- aggregate(dd_norm[,tab_indic$indic[which(tab_indic$out=="non" & tab_indic$indic != "W\nText.size.words"  & tab_indic$indic != "T\nText.size.sentence" & tab_indic$indic != "NDW\nText.size.type")]], by = list(group = dd_norm[,"type2"]), FUN = median)
      radar <- aggregate(dd_norm[,tab_indic$indic[which(tab_indic$out=="non" & !(tab_indic$indic %in% var_boxplot)  )]], by = list(group = dd_norm[,"type2"]), FUN = median)

      
      radar_bis <- as.data.frame(t(radar))
      radar_bis$indic <- row.names(radar_bis)
      names(radar_bis) <- as.vector(t(radar_bis[1,]))
      radar_bis <- radar_bis[-1,]
      rownames(radar_bis) <- 1:nrow(radar_bis)
      radar_bis[,which(names(radar_bis)==niv)] <- as.numeric(as.character(radar_bis[,which(names(radar_bis)==niv)]))
      radar_bis$student <- as.numeric(as.character(radar_bis$student))
      
      #q1 <- unlist( lapply(dd_norm[which(dd_norm$type2!="student"),tab_indic$indic[which(tab_indic$out=="non" & tab_indic$indic != "W\nText.size.words"  & tab_indic$indic != "T\nText.size.sentence" & tab_indic$indic != "NDW\nText.size.type")]], function(z){quantile(z, probs=c(0.25))}))
      q1 <- unlist( lapply(dd_norm[which(dd_norm$type2!="student"),tab_indic$indic[which(tab_indic$out=="non" &  !(tab_indic$indic %in% var_boxplot) )]], function(z){quantile(z, probs=c(0.25))}))

      #q3 <- unlist( lapply(dd_norm[which(dd_norm$type2!="student"),tab_indic$indic[which(tab_indic$out=="non" & tab_indic$indic != "W\nText.size.words"  & tab_indic$indic != "T\nText.size.sentence" & tab_indic$indic != "NDW\nText.size.type")]], function(z){quantile(z, probs=c(0.75))}))
      q3 <- unlist( lapply(dd_norm[which(dd_norm$type2!="student"),tab_indic$indic[which(tab_indic$out=="non" &  !(tab_indic$indic %in% var_boxplot) )]], function(z){quantile(z, probs=c(0.75))}))

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
              #axis.text.x = element_text(size = 8)) +   
              axis.text.x = element_text(size = 6, face = "bold")) +
        guides(color = guide_legend(order = 1))
      
      
      
      ddc <- df_NS_NNS_9metrics[which(df_NS_NNS_9metrics$type2==niv | df_NS_NNS_9metrics$type2=="student"),]
      #ddc <- ddc[,c("document","W\nText.size.words","T\nText.size.sentence","NDW\nText.size.type","type2")]
      ddc <- ddc[,c("document",var_boxplot,"type2")]
      
      ddc_byVar <-gather(ddc,  key = "Metric", 
                         value = "Value", -c("document","type2")
      )
      
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
                strip.text = element_text(face = "bold"),
                axis.text.x=element_blank())
      }else if(desc_stat_chart=="boxplot_point"){
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
                legend.text=element_text(size=12, face = "bold"),
                plot.title=element_text(size=20),
                axis.text.x = element_text(size = 13)) +
          theme(legend.title=element_blank())
      }else if(desc_stat_chart=="violin"){
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
      
      
      
      
      
      #if(all(tab_indic$out[!(tab_indic$indic %in% c("W\nText.size.words","T\nText.size.sent","NDW\nText.size.type"))]=="non")){
      if(all(tab_indic$out[!(tab_indic$indic %in% var_boxplot)]=="non")){  
        
        grid.arrange(p, p1, tbl,
                     heights = c(2, 0.5),
                     layout_matrix = rbind(c(1, 2),
                                           c(3, 3)),
                     as.table = TRUE,
                     bottom=textGrob(page[n], x=0.5, y=2, hjust=0, gp=gpar( fontface="italic")))
        
      } else {
        
        off_radar <- list_metrics[which(tab_indic$out=="oui")]
        off_radar <- off_radar[!(tab_indic$indic[which(tab_indic$out=="oui")] %in% var_boxplot)]
        
        
        if(length(off_radar) ==1){
          text = paste("You are off radar for the following indicator :", off_radar, ".", sep = " ")
        } else { text = paste("You are off radar for the following indicators :", paste(off_radar,collapse=", "), ".", sep = " ") }
        
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


student_ID <- df_sampleALE_allMetrics$document 
iterations <- length(student_ID)

fun_viz <- function(i){
  nom <- student_ID[i]
  tfile <- paste(path_feedbacks,"/", nom, " ", Sys.Date() ,".pdf", sep="")
  
  print(paste0(round(i/iterations*100,1), "%", 
               "| Creating feedback for : ", nom, "|", 
               i, "/",  iterations, " ..."))
  pdf(tfile,width=15,height=10)
  
  ## appel fonction de visualisation
  viz(nom)
  ##
  
  dev.off()
}



if (os == "Windows"){
  for(i in 1:iterations){
    fun_viz(i)
  }
}else{
  foreach(i=1:iterations) %dopar% {
    fun_viz(i)
  }
}







