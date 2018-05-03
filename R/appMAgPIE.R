#' @title appMAgPIE
#' @description appToolMAgPIE allows to explore and visualize time series of modelling results.
#' 
#' @param file report data. Can be a CSV/MIF file or rds file with a quitte object (saved with saveRDS). file can also be a vector of rds files. NULL by default; in this case the user can upload files directly in the tool
#' @param resultsfolder folder in which MAgPIE run results are stored. File must come with a overview list called "files" 
#' @param valfile validation data. Can be a CSV/MIF file or rda/RData file with a quitte object (saved with saveRDS). NULL by default; in this case the user can upload files directly in the tool
#' @param username username to be used to access file and resultsfolder
#' @param password password to access file and resultsfolder
#' @author Florian Humpenoeder, Jan Philipp Dietrich
#' @importFrom shiny reactiveValues observeEvent updateTextInput observe updateSelectInput reactive hoverOpts uiOutput sliderInput
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
#' @export
#'
appMAgPIE <- function(file="https://www.pik-potsdam.de/rd3mod/magpie.rds", resultsfolder="https://www.pik-potsdam.de/rd3mod/magpie/", valfile="https://www.pik-potsdam.de/rd3mod/validation.rds", username=NULL, password=NULL) {
  
  #client-sided function
  ui <- fluidPage(
          tabsetPanel(id="full", type="tabs",
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
              mainPanel(plotOutput("stats")))),
            tabPanel("LinePlot",
              sidebarLayout(
                sidebarPanel(modLinePlotUI("lineplot")),
                mainPanel(plotOutput("lineplot",height = "800px",width = "auto"))
              )
            ),
            tabPanel("AreaPlot",
                     sidebarLayout(
                       sidebarPanel(modAreaPlotUI("areaplot")),
                       mainPanel(plotOutput("areaplot",height = "800px",width = "auto"))
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
    
    if(!is.null(valfile)) {
      if(grepl("https://",valfile)) {
        val_full <- readRDS(gzcon(url(valfile)))
      } else {
        val_full <- readRDS(valfile)
      }
      val_full <- val_full[!is.na(val_full$value),]
      levels(val_full$region) <- sub("World","GLO",levels(val_full$region))
    }
    
    rep_full <- callModule(modRunSelect,"select",file=file, resultsfolder=resultsfolder, username=username, password=password)
    output$lineplot <- callModule(modLinePlot,"lineplot",report=rep_full,validation=reactive(val_full))
    output$areaplot <- callModule(modAreaPlot,"areaplot",report=rep_full)
    
    observeEvent(rep_full()$variables,{
      #hideTab("full","Show Data")
      updateSelectInput(session, "xaxis",  choices=rep_full()$variables, selected = "revision_date")
      updateSelectInput(session, "yaxis",  choices=rep_full()$variables, selected = "title")
      updateSelectInput(session, "color",  choices=rep_full()$variables, selected = "user")   
    })
    
    output$stats <- renderPlot({
      cset <- function(i,check) {
        if(i %in% check) return(i)
        return(check[1])
      }
      theme <- mip::theme_mip(size=14)
      
      ggplot2::ggplot(rep_full()$selection()$x) + ggplot2::theme(legend.direction="vertical") +
        ggplot2::geom_point(ggplot2::aes_string(y=cset(input$yaxis,rep_full()$variables),
                                                x=cset(input$xaxis,rep_full()$variables),
                                                color=cset(input$color,rep_full()$variables)),size=5, na.rm=TRUE) +
        theme
    }, height = 700)
    
  }
  
  #start the app
  shinyApp(ui = ui, server = server)
  }  
