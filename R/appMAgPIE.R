#' @title appMAgPIE
#' @description appToolMAgPIE allows to explore and visualize time series of modelling results.
#' 
#' @param file report data. Can be a CSV/MIF file or rds file with a quitte object (saved with saveRDS). file can also be a vector of rds files. NULL by default; in this case the user can upload files directly in the tool
#' @param resultsfolder folder in which MAgPIE run results are stored. File must come with a overview list called "files" 
#' @param valfile validation data. Can be a CSV/MIF file or rda/RData file with a quitte object (saved with saveRDS). NULL by default; in this case the user can upload files directly in the tool
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
appMAgPIE <- function(file="https://www.pik-potsdam.de/rd3mod/magpie.rds", resultsfolder="https://www.pik-potsdam.de/rd3mod/magpie/", valfile="https://www.pik-potsdam.de/rd3mod/validation.rds") {
  
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
            mainPanel(
              plotOutput("lineplot",height = "800px",width = "auto")
            ))),
            tabPanel("Show Data",
              sidebarLayout(
              sidebarPanel(
                             selectInput('scenario', 'Scenario', "Pending upload",multiple = TRUE),
                             selectInput('region', 'Region', "Pending upload",multiple = TRUE),
                             sliderInput('year', 'Year',min=2000,max=2100,value=c(2000,2100),step=10),
                             selectInput('variable', 'Variable', "Pending upload",multiple = FALSE),
                             tags$hr(),
                             checkboxInput('update_plot', 'Update Plot', value = TRUE, width = NULL),
                             conditionalPanel(condition = "input.valfile != NULL", checkboxInput('show_val', 'Show Validation', value = TRUE, width = NULL))
        ),
      mainPanel(
        tabsetPanel(id = "main",type = "tabs",
                    tabPanel("AreaPlot",
                             plotOutput("areaplot",height = "800px",width = "auto"),
                             wellPanel(
                               fluidRow(
                                 column(4,
                                        checkboxInput('exclude_world', 'Exclude Region "World"', value = TRUE, width = NULL)
                                 )
                               )
                             ),
                             wellPanel(downloadButton('downloadAreaPlot', 'Download Plot'))
                    ),
                    tabPanel("Table", 
                             dataTableOutput("data"),
                             wellPanel(downloadButton('downloadData', 'Download Data'))
                    ),
                    tabPanel("Trafficlight",
                             plotOutput("tf",height = "200px",width = "auto")
                    ),
                    tabPanel("Info", 
                             h2("Summary"),
                             verbatimTextOutput("summary"),
                             h2("General information about the dataset"),
                             verbatimTextOutput("info")
                    )
        )
      )
    )
    )
  )
  )
  
  
  
  model <- scenario <- region <- year <- period <- variable <- unit <- NULL
  
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
      levels(val_full$region) <- sub("World","GLO",levels(val_full$region))
    }
    
    rep_full <- callModule(modRunSelect,"select",file=file, resultsfolder=resultsfolder)
    output$lineplot <- callModule(modLinePlot,"lineplot",report=rep_full,validation=reactive(val_full))
    
    observeEvent(rep_full()$variables,{
      #hideTab("full","Show Data")
      updateSelectInput(session, "xaxis",  choices=rep_full()$variables, selected = "revision_date")
      updateSelectInput(session, "yaxis",  choices=rep_full()$variables, selected = "title")
      updateSelectInput(session, "color",  choices=rep_full()$variables, selected = "user")   
    })
    

    tf <- reactive({
      if(is.null(val$val_sel)) stop("Validation file needed for trafficlights!")
      else trafficlight(x=as.magpie(val$rep_sel,spatial="region",temporal="period",tidy=TRUE),xc=as.magpie(val$val_sel,spatial="region",temporal="period",tidy=TRUE),detailed=FALSE)
    })
    
    areaplot <- reactive({
      p <- mipArea(x=if(input$exclude_world) val$rep_sel[val$rep_sel$region!="World",] else val$rep_sel)
      return(p)
    })
    
    output$stats <- renderPlot({
      cset <- function(i,check) {
        if(i %in% check) return(i)
        return(check[1])
      }
      theme <- mip::theme_mip(size=14)
      
      ggplot2::ggplot(rep_full()$selection()) + ggplot2::theme(legend.direction="vertical") +
        ggplot2::geom_point(ggplot2::aes_string(y=cset(input$yaxis,rep_full()$variables),
                                                x=cset(input$xaxis,rep_full()$variables),
                                                color=cset(input$color,rep_full()$variables)),size=5, na.rm=TRUE) +
        theme
    }, height=700)
    
    output$areaplot <- renderPlot({
      areaplot()},res = 120)#height = 400, width = 500
    
    output$tf <- renderPlot({
      tf()},res = 120)#height = 400, width = 500
    
    output$summary <- renderPrint({
      summary(val$rep_sel$value)
    })
    output$info <- renderPrint({
      val$rep_full <- rep_full()$report
      cat(paste(length(levels(val$rep_full$model)),"Model(s)"),
          paste(length(levels(val$rep_full$scenario)),"Scenario(s)"),
          paste(length(levels(val$rep_full$region)),"Region(s)"),
          paste(length(unique(val$rep_full$period)),"Year(s)"),
          paste(length(levels(val$rep_full$variable)),"Variable(s)"),sep="\n")
    })
    output$data <- renderDataTable({
      val$rep_sel
    }, options = list(pageLength = 10))
    
    output$downloadAreaPlot <- downloadHandler(
      filename = function() { paste("export", '.pdf', sep='') },
      content = function(file) {
        ggsave(file, plot = areaplot(), device = "pdf",scale=1,width=20,height=13,units="cm",dpi=150)
      }
    )
    output$downloadData <- downloadHandler(
      filename = function() { paste("export", '.csv', sep='') },
      content = function(file) {
        out <- val$rep_sel
        out <- dcast(out, model + scenario + region + variable + unit ~ period, value.var="value")
        write.csv(out, file ,row.names = FALSE,quote = FALSE)
      }
    )
    
  }
  
  
  #start the app
  shinyApp(ui = ui, server = server)
  }  
