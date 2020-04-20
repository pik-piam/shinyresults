#' @title appResults
#' @description appResults allows to explore and visualize time series of modelling results.
#' 
#' @param cfg config-file containing information about one or more models on the items listed below. Usually you provide these settings 
#'            in your .Rprofile as a list constructed as follows (to explore REMIND results please replace all occurrences of magpie with remind): 
#'            options(appResults=list("MAgPIE"= list(file="https://rse.pik-potsdam.de/data/magpie/results/rev1/overview.rds",
#'            resultsfolder="https://rse.pik-potsdam.de/data/magpie/results/rev1/",
#'            valfile="https://rse.pik-potsdam.de/data/magpie/results/rev1/validation.rds",
#'            username="xxx",
#'            password="yyy")))
#'                        
#'            file - Overview file in rds format containing a list of all runs available.
#'            resultsfolder - folder in which model results are stored in rds format.
#'            valfile - validation data. Can be a CSV/MIF file or rds file with a quitte object (saved with saveRDS). NULL by default; in this case the user can upload files directly in the tool.
#'            username - username to access "file" and "resultsfolder". 
#'            password - password to access "file" and "resultsfolder".
#' @param readFilePar read report data files in parallel (faster) (TRUE) or in sequence (FALSE). Current default is FALSE.
#' @param ... additional information to overwrite one of the settings from the cfg directly: file, resultsfolder, valfile, username or password.
#' @author Florian Humpenoeder, Jan Philipp Dietrich, Lavinia Baumstark
#' @importFrom shiny appendTab tagList isolate div insertTab reactiveValues observeEvent updateTextInput observe updateSelectInput reactive hoverOpts uiOutput sliderInput
#' renderPrint renderDataTable downloadHandler fluidPage navbarPage tabPanel sidebarLayout sidebarPanel
#' fileInput tags selectInput mainPanel tabsetPanel wellPanel fluidRow column radioButtons conditionalPanel
#' checkboxInput checkboxGroupInput numericInput textInput downloadButton dataTableOutput h2 verbatimTextOutput
#' shinyApp renderPlot plotOutput renderUI HTML nearPoints updateCheckboxInput updateSliderInput hideTab showTab runApp Progress
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
appResults <- function(cfg=getOption("appResults"),readFilePar=FALSE,...) {
  
  # If config for only one model is provided by getOption("appResults") use this one. 
  # If information for more models exists the user can choose the model.
  if(length(cfg)==1) {
    cfgModel <- cfg[[1]]
  } else{
    cat("Please choose a model:\n\n")
    models <- names(cfg)
    cat(paste(1:length(models), models, sep=": " ),sep="\n")
    cat("\nNumber: ")
    number   <- readline()
    cfgModel <- cfg[[as.numeric(number)]]   
  } 
  # check if user provides some information on settings that should be used directly, 
  # if not use information from cfg
  addInfo <- list(...)   # store additional information if provided
  if (!is.null(addInfo$file))      {
          file          <- addInfo$file 
  } else {file          <- cfgModel$file }
  if (!is.null(addInfo$resultsfolder)) {
          resultsfolder <- addInfo$resultsfolder 
  } else {resultsfolder <- cfgModel$resultsfolder  }
  if (!is.null(addInfo$valfile))       {
          valfile       <- addInfo$valfile 
  } else {valfile       <- cfgModel$valfile  }
  if (!is.null(addInfo$username))      {
          username      <- addInfo$username 
  } else {username      <- cfgModel$username  }
  if (!is.null(addInfo$password))      {
          password      <- addInfo$password 
  } else {password      <- cfgModel$password  }

  
  if(grepl("https://",file)) {
    tmp <- try(curl(file,"r",new_handle(username=username,password=password)))
    if("try-error" %in% class(tmp)) stop("Access denied! Please check username and password!")
  }
  
  #client-sided function
  ui <- fluidPage(
    div(style = "position:absolute;right:1em;", 
        actionButton("LineButton", label = "Add LinePlot"),
        actionButton("AreaButton", label = "Add AreaPlot")
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
                                                choices = "user"),
                                    tags$p(),tags$hr(),tags$p(),
                                    modRunSelectUI("select"))),
              mainPanel(plotlyOutput("stats", height="600px", width="auto")))),
            tabPanel("LinePlot1",
              sidebarLayout(
                sidebarPanel(modLinePlotUI("LinePlot1")),
                mainPanel(plotOutput("LinePlot1",height = "800px",width = "auto"))
              )
            ),
            tabPanel("AreaPlot1",
                     sidebarLayout(
                       sidebarPanel(modAreaPlotUI("AreaPlot1")),
                       mainPanel(plotlyOutput("AreaPlot1",height = "800px",width = "auto"))
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
      
      if("connection" %in% class(valfile)) close(valfile)
      
      val_full <- val_full[!is.na(val_full$value),]
      levels(val_full$region) <- sub("World","GLO",levels(val_full$region))
      val_full <- val_full[val_full$period > 1950,] #show validation data only for years > 1950
    }
    
    counter <- reactiveValues(LinePlot = 1, AreaPlot = 1)

    rep_full <- callModule(modRunSelect,"select",file=file, resultsfolder=resultsfolder, username=username, password=password, readFilePar=readFilePar)
  
    addtab <- function(type="Line",counter,plot=plotOutput) {
      if(type=="Line") {
        ui <- modLinePlotUI
      } else if(type=="Area") {
        ui <- modAreaPlotUI
      } else {
        stop("Unknown type ",type,"!")
      }
      tabname <- paste0(type,"Plot",counter)
      if(counter>1) {
        insertTab(inputId = "append_tab", 
                  tabPanel(tabname,
                           sidebarLayout(
                             sidebarPanel(ui(tabname)),
                             mainPanel(plot(tabname,height = "800px",width = "auto")))),
                  target = paste0(type,"Plot",counter-1),
                  position = "after")
      }
      if(type=="Line") {
        output[[tabname]] <- callModule(modLinePlot,tabname,report=rep_full,validation=reactive(val_full))
      } else if(type=="Area") {
        output[[tabname]] <- callModule(modAreaPlot,tabname,report=rep_full)  
      }
    }
    
    observeEvent(input$LineButton, {counter$LinePlot <- counter$LinePlot+1})
    observeEvent(counter$LinePlot, addtab("Line",counter$LinePlot, plot=plotOutput))
    
    observeEvent(input$AreaButton, {counter$AreaPlot <- counter$AreaPlot+1})
    observeEvent(counter$AreaPlot, addtab("Area",counter$AreaPlot, plot=plotlyOutput))
    
    observeEvent(rep_full$variables(),{
      if(!setequal(val$variables,rep_full$variables())) {
        val$variables <- rep_full$variables()
        updateSelectInput(session, "xaxis",  choices=val$variables, selected = "revision_date")
        updateSelectInput(session, "yaxis",  choices=val$variables, selected = "runtime")
        updateSelectInput(session, "color",  choices=val$variables, selected = "user")   
      }
    })
    
    output$stats <- renderPlotly({
      progress <- Progress$new(session, min=1, max=10)
      on.exit(progress$close())
      progress$set(message = 'Prepare overview plot',
                   detail = 'This may take a while...',
                   value = 2)
      start <- Sys.time()
      message(".:|appResults|:. Create OverviewPlot..", appendLF = FALSE)
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
      progress$set(message = 'Make it interactive',
                   detail = 'This should be quick...',
                   value = 6)
      p <- ggplotly(p)
      message("done! (",round(as.numeric(Sys.time()-start,units="secs"),2),"s)")
      progress$set(message = 'Send to plotter',
                   detail = 'This should be quick...',
                   value = 10)
      p
    })
    
  }
  
  #start the app
  runApp(shinyApp(ui = ui, server = server),launch.browser = TRUE)
  }  
