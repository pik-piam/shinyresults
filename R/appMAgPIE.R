#' @title appMAgPIE
#' @description appMAgPIE allows to explore and visualize time series of modelling results.
#' 
#' @param file Overview file in rds format containing an overview about all available runs.
#' @param resultsfolder folder in which MAgPIE run results are stored. File must come with a overview list called "files" 
#' @param valfile validation data. Can be a CSV/MIF file or rds file with a quitte object (saved with saveRDS). NULL by default; in this case the user can upload files directly in the tool
#' @param username username to be used to access file and resultsfolder
#' @param password password to access file and resultsfolder
#' @param readFilePar read report data files in parallel (faster) (TRUE) or in sequence (FALSE). Current default is FALSE.
#' @author Florian Humpenoeder, Jan Philipp Dietrich
#' @importFrom shiny appendTab tagList isolate div insertTab reactiveValues observeEvent updateTextInput observe updateSelectInput reactive hoverOpts uiOutput sliderInput
#' renderPrint renderDataTable downloadHandler fluidPage navbarPage tabPanel sidebarLayout sidebarPanel
#' fileInput tags selectInput mainPanel tabsetPanel wellPanel fluidRow column radioButtons conditionalPanel
#' checkboxInput checkboxGroupInput numericInput textInput downloadButton dataTableOutput h2 verbatimTextOutput
#' shinyApp renderPlot plotOutput renderUI HTML nearPoints updateCheckboxInput updateSliderInput hideTab showTab
#' @importFrom utils write.csv
#' @importFrom data.table fread setcolorder as.data.table data.table setnames
#' @importFrom trafficlight trafficlight
#' @importFrom magclass as.magpie
#' @importFrom mip mipLineHistorical theme_mip mipArea
#' @importFrom reshape2 dcast
#' @importFrom ggplot2 ggsave
#' @importFrom plotly renderPlotly ggplotly plotlyOutput
#' @export
#'
appMAgPIE <- function(file="https://rse.pik-potsdam.de/data/magpie/results/rev1/overview.rds", 
                      resultsfolder="https://rse.pik-potsdam.de/data/magpie/results/rev1/", 
                      valfile="https://rse.pik-potsdam.de/data/magpie/results/rev1/validation.rds", 
                      username=getOption("appMAgPIE_username"), 
                      password=getOption("appMAgPIE_password"),
                      readFilePar=FALSE) {
  
  if(grepl("https://",file)) {
    tmp <- try(curl(file,"r",new_handle(username=username,password=password)))
    if("try-error" %in% class(tmp)) stop("Access denied! Please check username and password!")
  }
  
  #client-sided function
  ui <- fluidPage(
    div(style = "position:absolute;right:1em;", 
        #actionButUI("append_tab", "Add LinePlot")
        actionButton("button", label = "Add LinePlot")
    ),
    tabsetPanel(id="append_tab", type="tabs",
            tabPanel("Select Data",sidebarLayout(
              sidebarPanel(tags$div(id="navigation",
                                    selectInput(inputId = "xaxis",
                                                label = "Choose X-Axis",
                                                choices = "revision_date"),
                                    selectInput(inputId = "yaxis",
                                                label = "Choose Y-Axis",
                                                choices = "runtime"),
                                    selectInput(inputId = "color",
                                                label = "Choose Colorkey",
                                                choices = "revision"),
                                    tags$p(),tags$hr(),tags$p(),
                                    modRunSelectUI("select"))),
              mainPanel(plotlyOutput("stats", height="600px", width="auto")))),
            tabPanel("LinePlot1",
              sidebarLayout(
                sidebarPanel(modLinePlotUI("lineplot1")),
                mainPanel(plotOutput("lineplot1",height = "800px",width = "auto"))
              )
            ),
            tabPanel("AreaPlot",
                     sidebarLayout(
                       sidebarPanel(modAreaPlotUI("areaplot")),
                       mainPanel(plotlyOutput("areaplot",height = "800px",width = "auto"))
                     )
            )
      )
  )
  
  #limit for file upload set to 300 MB
  options(shiny.maxRequestSize = 300*1024^2)

  #server function
  server <- function(input,output,session) {
    
    #initialize reactive value
    val <- reactiveValues()
    
    # read valfile
    if(!is.null(valfile)) {
      if(grepl("\\.mif$",valfile)) {
        if (!requireNamespace("quitte", quietly = TRUE)) {
          stop("Package \"quitte\" needed to handle mif files. Validation file will be ignored!",
               call. = FALSE)
        } 
        isMIF <- TRUE
      } else {
        isMIF <- FALSE
      }
      
      if(grepl("https://",valfile)) {
        valfile <- gzcon(curl(valfile, handle=new_handle(username=username, password=password)))
      }
      
      if(isMIF) {
        val_full <- quitte::read.quitte(valfile, check.duplicates=FALSE)
      } else {
        val_full <- readRDS(valfile) 
      }
      
      val_full <- val_full[!is.na(val_full$value),]
      levels(val_full$region) <- sub("World","GLO",levels(val_full$region))
      val_full <- val_full[val_full$period > 1950,] #show validation data only for years > 1950
    }
    
    counter <- reactiveValues(Plot = 1)

    rep_full <- callModule(modRunSelect,"select",file=file, resultsfolder=resultsfolder, username=username, password=password, readFilePar=readFilePar)
    
    #In development. Works at the moment only if the LinePlots are added before loading the data.
    observeEvent(input$button, {
      counter$Plot <- counter$Plot+1
      appendTab(inputId = "append_tab", 
                tabPanel(paste0("LinePlot",counter$Plot),
                         sidebarLayout(
                           sidebarPanel(modLinePlotUI(paste0("lineplot",counter$Plot))),
                           mainPanel(plotOutput(paste0("lineplot",counter$Plot),height = "800px",width = "auto")))))
                #target = paste0("LinePlot",counter$Plot-1), position = "after")
    })
    
    observeEvent(counter$Plot,{
      output[[paste0("lineplot",counter$Plot)]] <- callModule(modLinePlot,paste0("lineplot",counter$Plot),report=rep_full,validation=reactive(val_full))
    })

    output$areaplot <- callModule(modAreaPlot,"areaplot",report=rep_full)
    
    observeEvent(rep_full$variables(),{
      if(!setequal(val$variables,rep_full$variables())) {
        val$variables <- rep_full$variables()
        updateSelectInput(session, "xaxis",  choices=val$variables, selected = "revision_date")
        updateSelectInput(session, "yaxis",  choices=val$variables, selected = "runtime")
        updateSelectInput(session, "color",  choices=val$variables, selected = "user")   
      }
    })
    
    output$stats <- renderPlotly({
      start <- Sys.time()
      message("Create OverviewPlot in appMAgPIE..")
      cset <- function(i,check) {
        if(i %in% check) return(i)
        return(check[1])
      }
      theme <- mip::theme_mip(size=10)
      p <- ggplot2::ggplot(rep_full$selection()$x) + ggplot2::theme(legend.direction="vertical") +
        ggplot2::geom_point(ggplot2::aes_string(y=cset(input$yaxis,rep_full$variables()),
                                                x=cset(input$xaxis,rep_full$variables()),
                                                color=cset(input$color,rep_full$variables())), na.rm=TRUE) +
        theme
      message("  ..finished preparing OverviewPlot in appMAgPIE (",round(as.numeric(Sys.time()-start,units="secs"),4),"s)")
      ggplotly(p)
    })
    
  }
  
  #start the app
  shinyApp(ui = ui, server = server)
  }  
