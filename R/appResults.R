#' @title appResults
#' @description appResults allows to explore and visualize time series of modelling results.
#'
#' @param cfg config-file containing information about one or more models on the items listed below.
#'            Usually you provide these settings in your .Rprofile as a list constructed as follows
#'            (to explore REMIND results please replace all occurrences of magpie with remind):
#'            url <- https://rse.pik-potsdam.de/data/magpie/results/rev1
#'            options(appResults = list(MAgPIE = list(file = paste0(url, "/reduced_overview.rds"),
#'                                                    resultsfolder=url,
#'                                                    valfile=paste0(url, "/validation.rds"),
#'                                                    username="xxx",
#'                                                    password="yyy")))
#'
#'            file - Overview file in rds format containing a list of all runs available.
#'                   To get access to all available filters use overview.rds (takes 15s longer to load)
#'                   instead of reduced_overview.rds.
#'            resultsfolder - folder in which model results are stored in rds format.
#'            valfile - validation data. Can be a CSV/MIF file or rds file with a quitte object
#'                      (saved with saveRDS). NULL by default; in this case the user can upload
#'                      files directly in the tool.
#'            username - username to access "file" and "resultsfolder".
#'            password - password to access "file" and "resultsfolder".
#' @param readFilePar read report data files in parallel (faster) (TRUE) or in sequence (FALSE)
#' @param variableConfig Path to a YAML configuration file with variable presets, or NULL to use
#'                       default config. The config defines presets for the Dashboard tab and
#'                       quick-load options in plot tabs. See \code{\link{loadVariableConfig}}.
#' @param port Port number for the Shiny app (default: 3838). Using a fixed port allows
#'             consistent URLs for bookmarking.
#' @param ... additional information to overwrite one of the settings from the cfg directly:
#'            file, resultsfolder, valfile, username or password.
#' @author Florian Humpenoeder, Jan Philipp Dietrich, Lavinia Baumstark, Pascal Sauer
#' @importFrom shiny tagList div insertTab reactiveValues observeEvent updateTextInput
#' observe updateSelectInput reactive hoverOpts uiOutput sliderInput
#' renderPrint renderDataTable downloadHandler fluidPage navbarPage tabPanel sidebarLayout sidebarPanel
#' fileInput tags selectInput mainPanel tabsetPanel wellPanel fluidRow column radioButtons conditionalPanel
#' checkboxInput checkboxGroupInput numericInput textInput downloadButton dataTableOutput h2 verbatimTextOutput
#' shinyApp renderPlot plotOutput renderUI HTML nearPoints updateCheckboxInput
#' updateSliderInput hideTab runApp Progress bookmarkButton setBookmarkExclude onBookmark onRestore
#' @importFrom utils write.csv head
#' @importFrom data.table fread setcolorder as.data.table data.table setnames
#' @importFrom trafficlight trafficlight
#' @importFrom magclass as.magpie
#' @importFrom mip mipLineHistorical theme_mip mipArea
#' @importFrom reshape2 dcast
#' @importFrom ggplot2 ggsave
#' @importFrom plotly renderPlotly ggplotly plotlyOutput
#' @export
appResults <- function(cfg = getOption("appResults"), readFilePar = FALSE, variableConfig = NULL, port = 3838, ...) {
  # If config for only one model is provided by getOption("appResults") use this one.
  # If information for more models exists the user can choose the model.
  if (length(cfg) == 1) {
    cfgModel <- cfg[[1]]
  } else {
    cat("Please choose a model:\n\n")
    models <- names(cfg)
    cat(paste(seq_along(models), models, sep = ": "), sep = "\n")
    cat("\nNumber: ")
    cfgModel <- cfg[[as.numeric(readline())]]
  }
  # check if user provides some information on settings that should be used directly,
  # if not use information from cfg
  addInfo <- list(...)   # store additional information if provided
  if (!is.null(addInfo$file)) {
    file <- addInfo$file
  } else {
    file <- cfgModel$file
  }

  if (!is.null(addInfo$resultsfolder)) {
    resultsfolder <- addInfo$resultsfolder
  } else {
    resultsfolder <- cfgModel$resultsfolder
  }

  if (!is.null(addInfo$valfile)) {
    valfile <- addInfo$valfile # nolint: object_usage_linter.
  } else {
    valfile <- cfgModel$valfile
  }

  if (!is.null(addInfo$username)) {
    username <- addInfo$username
  } else {
    username <- cfgModel$username
  }

  if (!is.null(addInfo$password)) {
    password <- addInfo$password
  } else {
    password <- cfgModel$password
  }

  # Load variable configuration for presets and dashboard
 varConfig <- loadVariableConfig(variableConfig)

  if (grepl("https://", file)) {
    tmp <- try(curl(file, "r", new_handle(username = username, password = password)))
    if ("try-error" %in% class(tmp)) {
      stop("Access denied! Please check username and password!")
    }
  }

  #client-sided function
  ui <- function(request) {
    fluidPage(
      div(style = "position:absolute;right:1em;",
          actionButton("LineButton", label = "Add LinePlot"),
          actionButton("AreaButton", label = "Add AreaPlot"),
          bookmarkButton(label = "Share/Save State")),
                  tabsetPanel(id = "append_tab", type = "tabs",
                              tabPanel("Select Data",
                                       sidebarLayout(sidebarPanel(tags$div(id = "navigation",
                                                                           selectInput(inputId = "xaxis",
                                                                                       label = "Choose X-Axis",
                                                                                       choices = "date"),
                                                                           selectInput(inputId = "yaxis",
                                                                                       label = "Choose Y-Axis",
                                                                                       choices = "runtime"),
                                                                           selectInput(inputId = "color",
                                                                                       label = "Choose Colorkey",
                                                                                       choices = "user"),
                                                                           tags$p(),
                                                                           tags$hr(),
                                                                           tags$p(),
                                                                           modRunSelectUI("select"))),
                                                     mainPanel(plotlyOutput("stats",
                                                                            height = "600px", width = "auto")))),
                              tabPanel("Dashboard",
                                       modDashboardUI("dashboard", presets = varConfig$presets)),
                              tabPanel("LinePlot1",
                                       sidebarLayout(sidebarPanel(modLinePlotUI("LinePlot1", presets = varConfig$presets)),
                                                     mainPanel(plotOutput("LinePlot1",
                                                                          height = "800px", width = "auto")))),
                              tabPanel("AreaPlot1",
                                       sidebarLayout(sidebarPanel(modAreaPlotUI("AreaPlot1")),
                                                     mainPanel(plotlyOutput("AreaPlot1",
                                                                            height = "800px", width = "auto"))))))
  }

  #limit for file upload set to 300 MB
  withr::local_options(shiny.maxRequestSize = 300 * 1024^2)

  server <- function(input, output, session) {
    message("server called")

    # Set up bookmark exclusions (exclude large reactive outputs and filter inputs)
    # Exclude filter-related inputs to prevent Shiny's auto-restore from interfering with our ID-based restore
    setBookmarkExclude(c("stats", "LinePlot1", "AreaPlot1",
                         "select-load", "select-runfilter-scenario", "select-runfilter-model",
                         "select-runfilter-user", "select-runfilter-year", "select-runfilter-date"))

    # Reactive to hold restored run IDs
    restoredRunIds <- reactiveVal(NULL)

    # Bookmark handler - save state including loaded run IDs
    onBookmark(function(state) {
      state$values$activeTab <- input$append_tab
      # Save dashboard filter selections
      state$values$dashboardRegion <- input[["dashboard-region_filter"]]
      state$values$dashboardScenarios <- input[["dashboard-scenario_filter"]]
      # Save loaded run IDs
      ids <- repFull$loadedIds()
      if (!is.null(ids) && length(ids) > 0) {
        state$values$runIds <- compressIds(ids)
        message("Bookmarking: saving ", length(ids), " run IDs: ", paste(head(ids, 3), collapse=", "),
                if(length(ids) > 3) "..." else "")
      } else {
        message("Bookmarking: no run IDs to save")
      }
    })

    # Reactive values for restored dashboard state
    restoredDashboardState <- reactiveValues(region = NULL, scenarios = NULL)

    # Restore handler - restore state from bookmark
    onRestore(function(state) {
      message("Restoring bookmarked state")
      # Restore run IDs
      if (!is.null(state$values$runIds) && state$values$runIds != "") {
        ids <- decompressIds(state$values$runIds)
        message("Restoring ", length(ids), " run IDs: ", paste(head(ids, 3), collapse=", "),
                if(length(ids) > 3) "..." else "")
        restoredRunIds(ids)
      } else {
        message("No run IDs in bookmark to restore")
      }
      # Store dashboard filter state for later restoration
      restoredDashboardState$region <- state$values$dashboardRegion
      restoredDashboardState$scenarios <- state$values$dashboardScenarios
    })

    # Restore dashboard filters after data is loaded
    observe({
      req(repFull$ready())
      if (!is.null(restoredDashboardState$region)) {
        updateSelectInput(session, "dashboard-region_filter", selected = restoredDashboardState$region)
        restoredDashboardState$region <- NULL
      }
      if (!is.null(restoredDashboardState$scenarios)) {
        updateSelectInput(session, "dashboard-scenario_filter", selected = restoredDashboardState$scenarios)
        restoredDashboardState$scenarios <- NULL
      }
    })

    val <- reactiveValues()
    valFull <- NULL

    if (!is.null(valfile)) {
      if (grepl("\\.mif$", valfile)) {
        if (!requireNamespace("quitte", quietly = TRUE)) {
          stop("Package \"quitte\" needed to handle mif files. Validation file will be ignored!",
               call. = FALSE)
        }
        isMIF <- TRUE
      } else {
        isMIF <- FALSE
      }

      if (grepl("https://", valfile)) {
        valfile <- gzcon(curl(valfile, handle = new_handle(username = username, password = password)))
      }

      if (isMIF) {
        valFull <- quitte::read.quitte(valfile, check.duplicates = FALSE)
      } else {
        valFull <- readRDS(valfile)
      }

      if ("connection" %in% class(valfile)) {
        close(valfile)
      }

      valFull <- valFull[!is.na(valFull$value), ]
      levels(valFull$region) <- sub("GLO", "World", levels(valFull$region))
      valFull <- valFull[valFull$period > 1950, ] #show validation data only for years > 1950
    }

    counter <- reactiveValues(LinePlot = 1, AreaPlot = 1)

    repFull <- callModule(modRunSelect, "select", file = file, resultsfolder = resultsfolder,
                          username = username, password = password, readFilePar = readFilePar,
                          restoreIds = restoredRunIds)

    # Initialize Dashboard module
    callModule(modDashboard, "dashboard", report = repFull, validation = reactive(valFull), config = varConfig)

    addtab <- function(type = "Line", counter, plot = plotOutput) {
      tabname <- paste0(type, "Plot", counter)
      if (counter > 1) {
        if (type == "Line") {
          uiElement <- modLinePlotUI(tabname, presets = varConfig$presets)
        } else if (type == "Area") {
          uiElement <- modAreaPlotUI(tabname)
        } else {
          stop("Unknown type ", type, "!")
        }
        insertTab(inputId = "append_tab",
                  tabPanel(tabname,
                           sidebarLayout(sidebarPanel(uiElement),
                                         mainPanel(plot(tabname, height = "800px", width = "auto")))),
                  target = paste0(type, "Plot", counter - 1),
                  position = "after")
      }
      if (type == "Line") {
        output[[tabname]] <- callModule(modLinePlot, tabname, report = repFull, validation = reactive(valFull))
      } else if (type == "Area") {
        output[[tabname]] <- callModule(modAreaPlot, tabname, report = repFull)
      }
    }

    observeEvent(input$LineButton, {
      counter$LinePlot <- counter$LinePlot + 1
    })
    observeEvent(counter$LinePlot, addtab("Line", counter$LinePlot, plot = plotOutput))

    observeEvent(input$AreaButton, {
      counter$AreaPlot <- counter$AreaPlot + 1
    })
    observeEvent(counter$AreaPlot, addtab("Area", counter$AreaPlot, plot = plotlyOutput))

    observeEvent(repFull$variables(), {
      if (!setequal(val$variables, repFull$variables())) {
        val$variables <- repFull$variables()
        updateSelectInput(session, "xaxis",  choices = val$variables, selected = "date")
        updateSelectInput(session, "yaxis",  choices = val$variables, selected = "runtime")
        updateSelectInput(session, "color",  choices = val$variables, selected = "user")
      }
    })

    output$stats <- renderPlotly({
      progress <- Progress$new(session, min = 1, max = 10)
      withr::defer({
        progress$close()
      })
      progress$set(message = "Prepare overview plot",
                   detail = "This may take a while...",
                   value = 2)
      start <- Sys.time()
      message(".:|appResults|:. Create OverviewPlot..", appendLF = FALSE)
      cset <- function(i, check) {
        if (i %in% check) {
          return(i)
        }
        return(check[1])
      }
      theme <- mip::theme_mip(size = 10)

      # only make scatter plot of runstatistics if less than 30000 runs are selected
      if (is.null(repFull$selection()$x)) {
        p <- ggplot() +
          annotate("text", x = 1, y = 1, label = "Data not yet loaded...") +
          theme_void()
      } else if (nrow(repFull$selection()$x) > 30000) {
        p <- ggplot() +
          annotate("text", x = 1, y = 1, label = "Too many data points (>30000)! Please filter data!") +
          theme_void()
      } else {
        # Prepare data - convert runtime from hours to minutes
        plotData <- as.data.frame(repFull$selection()$x)
        if ("runtime" %in% names(plotData)) {
          plotData$runtime <- plotData$runtime * 60  # Convert hours to minutes
        }

        # Get axis selections
        xVar <- cset(input$xaxis, repFull$variables())
        yVar <- cset(input$yaxis, repFull$variables())
        colorVar <- cset(input$color, repFull$variables())

        p <- ggplot2::ggplot(plotData) +
          ggplot2::theme(legend.direction = "vertical") +
          ggplot2::geom_point(ggplot2::aes_string(y = yVar, x = xVar, color = colorVar), na.rm = TRUE) +
          theme

        # Add informative axis labels
        axisLabels <- list(
          runtime = "Runtime (minutes)",
          date = "Date",
          user = "User",
          scenario = "Scenario",
          model = "Model"
        )
        yLabel <- if (yVar %in% names(axisLabels)) axisLabels[[yVar]] else yVar
        xLabel <- if (xVar %in% names(axisLabels)) axisLabels[[xVar]] else xVar
        p <- p + ggplot2::labs(x = xLabel, y = yLabel)
      }

      progress$set(message = "Make it interactive",
                   detail = "This should be quick...",
                   value = 6)
      p <- ggplotly(p)
      message("done! (", round(as.numeric(Sys.time() - start, units = "secs"), 2), "s)")
      progress$set(message = "Send to plotter",
                   detail = "This should be quick...",
                   value = 10)
      p
    })
  }

  runApp(shinyApp(ui = ui, server = server, enableBookmarking = "url"), launch.browser = TRUE, port = port)
}
