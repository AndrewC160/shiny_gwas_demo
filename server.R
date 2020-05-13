server      <- function(input, output) {
  
  gwas_dat  <- reactive(
    {
      chroms  <- input$seqname_select
      if(is.null(chroms)){
        chroms<- unique(dat$CHR)
      }
      bp_min  <- 0
      bp_max  <- max(dat$BP)
      dat %>% 
        filter(CHR %in% chroms) %>%
        filter(between(BP,input$gwas_coords[1],input$gwas_coords[2])) %>%
        filter(nLog10P >= input$nlog10P_thresh)
    }
  )
  
  output$gwas_plot  <- renderPlot(width = 1200,height = 900,{
    plot_gwas(gwas_dat(),
              sug_color = input$sug_color,sug_size=as.numeric(input$sug_size),
              sig_color = input$sig_color,sig_size=as.numeric(input$sig_size),
              select_seqname = input$gwas_brush$panelvar1,
              select_xrange = c(input$gwas_brush$xmin,input$gwas_brush$xmax),
              select_yrange = c(input$gwas_brush$ymin,input$gwas_brush$ymax))
  })
  
  output$tabulate_selected  <- renderDataTable(options=list(pageLength=100),{
    if(is.null(input$gwas_brush)){
      return(tibble(Selection="No SNPs selected"))
    }else{
      tb  <- gwas_dat() %>%
              filter(CHR == input$gwas_brush$panelvar1) %>%
              filter(between(BP,input$gwas_brush$xmin,input$gwas_brush$xmax)) %>%
              filter(between(nLog10P,input$gwas_brush$ymin,input$gwas_brush$ymax)) %>%
              select(CHR,SNP,BP,A1,BETA,L95,U95,P,nLog10P)
      return(tb)
    }
  })
  
  output$nlog10P_Pval   <- renderText({paste0("(P-value <= ",
                                              formatC(10^-input$nlog10P_thresh,format = "e",digits = 2),")")})
}

