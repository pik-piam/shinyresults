#' @title appResults
#' @description appResults allows to explore and visualize time series of modelling results.
#' 
#' @param cfg config-file containing informations for one or more models on: 
#'            file - Overview file in rds format containing an overview about all available runs; 
#'            resultsfolder - folder in which MAgPIE run results are stored. File must come with a overview list called "files"; 
#'            valfile - validation data. Can be a CSV/MIF file or rds file with a quitte object (saved with saveRDS). NULL by default; in this case the user can upload files directly in the tool;
#'            username - username to be used to access file and resultsfolder; 
#'            password - password to access file and resultsfolder.
#' @param readFilePar read report data files in parallel (faster) (TRUE) or in sequence (FALSE). Current default is FALSE.
#' @param ... additional information to adjust one of the settings from the cfg directly: file, resultsfolder, valfile, username or password  
#' @author Florian Humpenoeder, Jan Philipp Dietrich, Lavinia Baumstark
#' @importFrom shiny appendTab tagList isolate div insertTab reactiveValues observeEvent updateTextInput observe updateSelectInput reactive hoverOpts uiOutput sliderInput
#' renderPrint renderDataTable downloadHandler fluidPage navbarPage tabPanel sidebarLayout sidebarPanel
#' fileInput tags selectInput mainPanel tabsetPanel wellPanel fluidRow column radioButtons conditionalPanel
#' checkboxInput checkboxGroupInput numericInput textInput downloadButton dataTableOutput h2 verbatimTextOutput
#' shinyApp renderPlot plotOutput renderUI HTML nearPoints updateCheckboxInput updateSliderInput hideTab showTab runApp
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
                                                choices = "user"),
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
      message("Create OverviewPlot in appResults..")
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
      message("  ..finished preparing OverviewPlot in appResults (",round(as.numeric(Sys.time()-start,units="secs"),4),"s)")
      ggplotly(p)
    })
    
  }
  
  #start the app
  runApp(shinyApp(ui = ui, server = server),launch.browser = TRUE)
  }  
