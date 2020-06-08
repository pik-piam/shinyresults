#' Analyze Model Statistics
#'
#' Shiny app to analyze statistics collected with \code{\link[lucode2]{runstatistics}}
#' and merged with \code{\link[lucode2]{mergestatistics}}
#'
#' @param files path to rds-files from which statistics should be read
#' @param resultsfolder path to a folder containing model results of the corresponding runs
#' @author Jan Philipp Dietrich
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel selectInput shinyApp renderPlot mainPanel plotOutput column actionButton reactive removeUI
#' reactiveValues observeEvent insertUI tags fluidRow sliderInput titlePanel radioButtons textOutput renderText updateSelectInput callModule
#' @importFrom ggplot2 ggplot theme geom_point aes_string
#' @export


appModelstats <- function(files=c("https://www.pik-potsdam.de/rd3mod/magpie.rds","https://www.pik-potsdam.de/rd3mod/remind.rds"), resultsfolder=NULL) {
  
  names(files) <- basename(files)
  
  ui <- fluidPage(
    tags$div(id="title", titlePanel(paste0("Model run statistics from ",basename(files[1])))),
    tags$div(id="titleend"),
    sidebarLayout(
      sidebarPanel(radioButtons(inputId = "file",
                                label = "Choose input", selected = NULL,
                                inline = TRUE, choices = files),
                   tags$div(id="navigation",
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
                            modFilterUI("runfilter"))),
      mainPanel(plotOutput("stats"))
    )
  )
  
  server <- function(input, output, session) {
    readdata <- function(file) {
      if(grepl("https://",file)) {
        con <- gzcon(url(file))
        out <- readRDS(con)
        close(con)
      } else {
        out <- readRDS(file)
      }
      if(!is.null(out$date)) out$date <- as.POSIXct(out$date, origin="1970-01-01")
      if(!is.null(out$revision_date)) out$revision_date <- as.POSIXct(out$revision_date, origin="1970-01-01")
      return(out)
    }
    
    x <- reactiveValues()
    
    observeEvent(input$file, {
      x$data <- readdata(input$file)
      if(!is.null(resultsfolder)) {
        ids <- as.numeric(sub("\\.rds$","",readLines(url(paste0(resultsfolder,"/files")))))
        x$data$with_results <- (x$data$.id %in% ids)
      }
      removeUI(selector = "#title")
      insertUI(
        selector = "#titleend",
        where = "beforeBegin",
        ui = tags$div(id="title", titlePanel(paste0("Model run statistics from ",basename(input$file))))
      )
      x$variables <- names(x$data)[!(names(x$data)==".id")]
      updateSelectInput(session, "xaxis",  choices=x$variables, selected = "date")
      updateSelectInput(session, "yaxis",  choices=x$variables, selected = "user")
      updateSelectInput(session, "color",  choices=x$variables, selected = "user")
    })
    
    selection <- callModule(modFilter,"runfilter",data=reactive(x$data),exclude=".id")
    
    output$stats <- renderPlot({
      cset <- function(i,check) {
        if(i %in% check) return(i)
        return(check[1])
      }
      # fvec is the filter vector to be applied on
      # out to select the chosen entries
      if (!requireNamespace("mip", quietly = TRUE)) {
        theme <- NULL
      } else {
        theme <- mip::theme_mip(size=14)
      }
      
      ggplot2::ggplot(selection()$x) + ggplot2::theme(legend.direction="vertical") +
        ggplot2::geom_point(ggplot2::aes_string(y=cset(input$yaxis,x$variables),
                                                x=cset(input$xaxis,x$variables),
                                                color=cset(input$color,x$variables)),size=5, na.rm=TRUE) +
        theme
    }, height=700)
  }
  
  shinyApp(ui=ui, server=server)
}