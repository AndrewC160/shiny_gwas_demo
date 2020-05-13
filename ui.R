ui <- fluidPage(
  titlePanel("Shiny GWAS plot example"),
  sidebarLayout(
    sidebarPanel(
      h3("Plot options"),
      fluidRow(
        column(4,selectInput(inputId = "seqname_select",label = "Chromosomes",
                             choices = unique(dat$CHR),multiple = TRUE)),
        column(8,sliderInput(inputId = "gwas_coords",label = "Coordinates",
                             min = min(dat$BP),max = max(dat$BP),
                             value = c(min(dat$BP),max(dat$BP))))
      ),
      fluidRow(
        column(4,sliderInput(inputId="nlog10P_thresh",label="-log10P",value=2,min=1,max=9,step = 0.5),
                 textOutput(outputId = "nlog10P_Pval")),
        column(6,
          fluidRow(column(6,selectInput(inputId= "sug_color",label = "Suggestive color",selected = "orange",
                                        choices = colors(),multiple = FALSE)),
                   column(6,selectInput(inputId= "sug_size", label = "Size",
                                        selected = 4,choices = c(1:10)))
          ),
          fluidRow(column(6,selectInput(inputId= "sig_color", label = "Significant color",selected = "red",
                                        choices = colors(),multiple = FALSE)),
                   column(6,selectInput(inputId= "sig_size", label = "Size",
                                        selected = 5,choices = c(1:10)))
          )
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Manhattan plot",
          plotOutput("gwas_plot",brush = "gwas_brush") #See below.
        ),
        tabPanel("SNP table",
          dataTableOutput("tabulate_selected")
        )
      )
    )
  )
)

#Providing a brush argument to a plotOutput() results in an input object with that name, in this case "gwas_brush".
# This object is a named list which stores any information about the user's clickng on the plot (in this case x and 
# y min/max, as well as "panelvar1" as the facet name, specifically chromosomes). 
# More info on interactive plots here: https://shiny.rstudio.com/articles/plot-interaction.html