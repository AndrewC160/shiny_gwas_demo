library(shiny)
library(ggplot2)
library(ggrepel)
library(magrittr)
library(DT)
library(scales)
library(tidyverse)

dirs      <- list(base=file.path("/data/projects/shiny_demo"))
dirs$data <- file.path(dirs$base,"data")

mutate  <- dplyr::mutate
arrange <- dplyr::arrange

#Build "base" data object; this is not changed again, and is the reference for gwas_dat() when it updates.
dat   <- readRDS(file.path(dirs$data,"results_ADD_filt_1.rds")) %>%
          as_tibble() %>%
          mutate(nLog10P=-log10(P),
                 selected=FALSE,
                 signif_class=factor(case_when(P <= 5E-8 ~ "Significant",
                                               P <= 1E-5 ~ "Suggestive",
                                               TRUE ~ "None"),
                                     levels = c("Significant","Suggestive","None"))) %>%
          filter(P <= 0.05)

#The plotting function. 
plot_gwas   <- function(dat_in,
                        select_seqname=NULL,
                        select_xrange=NULL,
                        select_yrange=NULL,
                        sug_color = "orange",sug_size=3,
                        sig_color = "red",sig_size=5){
  #Default behavior when no seqnames or selection ranges provided.
  if(is.null(select_xrange)){ select_xrange=c(Inf,-Inf)}
  if(is.null(select_yrange)){ select_yrange=c(Inf,-Inf)}
  if(is.null(select_seqname)){select_seqname=1000}
  
  #Update input data object (dat_in) to reflect selection.
  dat_in  <- dat_in %>%
              mutate(selected=
                       between(BP,select_xrange[1],select_xrange[2]) &
                       between(nLog10P,select_yrange[1],select_yrange[2]) &
                       CHR == select_seqname)
  #Manually inlude 0 on scale, otherwise obscures the fact that ~4million points were cut out.
  y_scale <- c(0,max(dat_in$nLog10P)*1.1)
  
  #Build base plot.
  p <- ggplot(dat_in,aes(x=BP,y=nLog10P,color=selected,fill=signif_class,size=signif_class)) +
        facet_wrap(.~CHR,ncol=1,strip.position = 'right') +
        scale_x_continuous(name="Coordinates (bp)",expand = c(0,0),labels = comma) +
        scale_y_continuous(name="-log10(P-value)",expand = c(0,0),limits = y_scale) +
        scale_color_manual(values=c(`TRUE`="darkgreen",`FALSE`="black")) +
        scale_size_manual(values= c(Significant=sig_size,Suggestive=sug_size,None=2)) +
        scale_fill_manual(values =c(Significant=sig_color,Suggestive=sug_color,None="black"))+
        geom_point(pch=21,show.legend = FALSE) +
        theme(plot.title = element_blank(),
              panel.border = element_rect(size=0.5,color="black",fill=NA),
              panel.background = element_blank(),
              panel.grid.major.x = element_line(size=0.5,color="lightgray",linetype = "dotted"),
              panel.grid.major.y = element_line(size=0.5,color="lightgray"),
              strip.background = element_blank(),
              strip.text.y = element_text(angle=0,size=16),
              axis.title = element_text(size=20),
              axis.text = element_text(size=16))
  #Don't bother with labels if more than 5 facets.
  if(length(unique(dat_in$CHR)) <= 5){
    lab_df  <- dat_in %>% 
                filter(selected) %>%
                top_n(20,nLog10P) #Don't let more than 20 labels be drawn, can crash R.
    p   <- p + geom_text_repel(data = lab_df,mapping=aes(label=SNP),show.legend = FALSE,
                               min.segment.length = 0.01,size=8)
  }
  return(p)
}

#demo
#plot_gwas(dat_in = gwas_dat(), select_seqname = 8,select_xrange = c(1E8,1.5E8),select_yrange = c(5,10),thresh_min = 51)
#Debugging: 
# 1) check console
# 2) use test text output
# 3) reactives can be emulated with functions
