# Declare global variables to avoid R CMD check NOTEs (used in ggplot2 aes())
utils::globalVariables(c("period", "label", "value", "scenario_clean", "min", "max", "mean"))

#' modDashboard Server Module
#'
#' Shiny module which works together with \code{\link{modDashboardUI}} to produce a dashboard
#' with pre-configured plots based on variable presets.
#'
#' @param input,output,session Default input, output and session objects coming from shiny
#' @param report A reactive list containing the report data (from modRunSelect)
#' @param validation A reactive containing validation data to be shown
#' @param config Variable configuration list from loadVariableConfig()
#' @author Florian Humpenoeder
#' @seealso \code{\link{modDashboardUI}}, \code{\link{appResults}}, \code{\link{loadVariableConfig}}
#' @importFrom ggplot2 ggplot theme_void annotate ggsave theme element_text unit guides guide_legend aes geom_line geom_point geom_ribbon labs theme_minimal
#' @importFrom plotly renderPlotly ggplotly plotlyOutput layout
#' @importFrom mip mipLineHistorical mipArea theme_mip
#' @importFrom data.table as.data.table
#' @importFrom graphics plot.new text
#' @importFrom grDevices dev.off pdf png
#' @export
modDashboard <- function(input, output, session, report, validation, config) {

  # Collect ALL variables from ALL presets into a single list
  allVariables <- reactive({
    if (is.null(config$presets) || length(config$presets) == 0) {
      return(list())
    }
    # Flatten all variables from all presets
    vars <- list()
    for (presetName in names(config$presets)) {
      preset <- config$presets[[presetName]]
      if (!is.null(preset$variables)) {
        for (v in preset$variables) {
          vars[[length(vars) + 1]] <- v
        }
      }
    }
    vars
  })


  # Update scenario filter choices when data is loaded
  observe({
    tryCatch({
      req(report$ready(), report$report())
      data <- report$report()
      req(is.data.frame(data), nrow(data) > 0, "scenario" %in% names(data))
      scenarios <- unique(as.character(data$scenario))
      updateSelectInput(
        session,
        "scenario_filter",
        choices = sort(scenarios),
        selected = scenarios  # Select all by default
      )
    }, error = function(e) {
      message("Dashboard: Error updating scenario filter: ", e$message)
    })
  })

  # Update region filter choices when data is loaded
  observe({
    tryCatch({
      req(report$ready(), report$report())
      data <- report$report()
      req(is.data.frame(data), nrow(data) > 0, "region" %in% names(data))
      regions <- unique(as.character(data$region))
      # Default to "World" if available, otherwise first region
      defaultRegion <- if ("World" %in% regions) "World" else regions[1]
      updateSelectInput(
        session,
        "region_filter",
        choices = sort(regions),
        selected = defaultRegion
      )
    }, error = function(e) {
      message("Dashboard: Error updating region filter: ", e$message)
    })
  })

  # Filter data for a specific variable
  # exactMatch: if TRUE, only match the exact variable name (for line plots)
  # directChildrenOnly: if TRUE, only get direct children (variables with |+|) not all descendants (for area plots)
  filterDataForVariable <- function(data, varName, scenarioFilter = NULL, regionFilter = NULL,
                                    exactMatch = FALSE, directChildrenOnly = FALSE) {
    if (is.null(data) || nrow(data) == 0) return(NULL)

    if (directChildrenOnly) {
      # Only get direct children: match "varName|+|something" but not deeper levels
      # Pattern matches: varName|+|Child but not varName|+|Child|+|Grandchild
      escapedVar <- gsub("\\|", "\\\\|", varName)
      pattern <- paste0("^", escapedVar, "\\|\\+\\|[^|]+$")
      filtered <- data[grepl(pattern, as.character(data$variable)), ]
    } else if (exactMatch) {
      # Exact match only - for line plots showing a single variable
      filtered <- data[as.character(data$variable) == varName, ]
    } else {
      # Filter by variable (partial match for hierarchical variables)
      filtered <- data[grepl(paste0("^", gsub("\\|", "\\\\|", varName), "(\\||$)"),
                             as.character(data$variable)), ]

      # If no exact match, try without the hierarchy
      if (nrow(filtered) == 0) {
        filtered <- data[data$variable == varName, ]
      }
    }

    # Apply scenario filter if specified
    if (!is.null(scenarioFilter) && length(scenarioFilter) > 0) {
      filtered <- filtered[as.character(filtered$scenario) %in% scenarioFilter, ]
    }

    # Apply region filter if specified
    if (!is.null(regionFilter) && regionFilter != "") {
      filtered <- filtered[filtered$region == regionFilter, ]
    }

    return(filtered)
  }

  # Maximum number of plots to support (pre-create all output slots)
  # This should be large enough to accommodate all variables in the config

  MAX_PLOTS <- 50

  # Pre-create all plot outputs to avoid dynamic creation issues
  lapply(seq_len(MAX_PLOTS), function(i) {
    local({
      plotId <- paste0("plot_", i)
      areaPlotId <- paste0("area_plot_", i)

      # Line plot output (interactive with plotly)
      output[[plotId]] <- renderPlotly({
        vars <- allVariables()
        req(length(vars) > 0, report$ready(), report$report())

        # Check if this plot index is within the available variables
        if (i > length(vars)) {
          return(NULL)
        }

        varConfig <- vars[[i]]
        varName <- varConfig$variable
        plotType <- if (!is.null(varConfig$plot_type)) varConfig$plot_type else "line"

        # Skip if this is an area plot
        if (plotType == "area") {
          return(NULL)
        }

        # Get filtered data - use exactMatch for line plots
        plotData <- filterDataForVariable(
          report$report(),
          varName,
          input$scenario_filter,
          input$region_filter,
          exactMatch = TRUE
        )

        # Validate and clean plotData
        if (is.null(plotData) || !is.data.frame(plotData) || nrow(plotData) == 0) {
          p <- ggplot() +
            annotate("text", x = 1, y = 1, label = paste("No data for:", varName)) +
            theme_void()
          return(ggplotly(p))
        }

        # Check required columns exist
        requiredCols <- c("period", "value", "scenario")
        missingCols <- setdiff(requiredCols, names(plotData))
        if (length(missingCols) > 0) {
          p <- ggplot() +
            annotate("text", x = 1, y = 1, label = paste("Missing columns:", paste(missingCols, collapse = ", "))) +
            theme_void()
          return(ggplotly(p))
        }

        # Remove rows with NA values in critical columns
        plotData <- plotData[!is.na(plotData$value) & !is.na(plotData$period) & !is.na(plotData$scenario), ]

        if (nrow(plotData) == 0) {
          p <- ggplot() +
            annotate("text", x = 1, y = 1, label = paste("No valid data for:", varName)) +
            theme_void()
          return(ggplotly(p))
        }

        if (nrow(plotData) > 20000) {
          p <- ggplot() +
            annotate("text", x = 1, y = 1, label = "Too many data points (>20000)") +
            theme_void()
          return(ggplotly(p))
        }

        # Get validation data if available (only historical, not projections)
        valData <- NULL
        tryCatch({
          if (!is.null(validation())) {
            valData <- filterDataForVariable(validation(), varName, NULL, input$region_filter, exactMatch = TRUE)
            # Filter to only include historical data (scenario == "historical")
            if (!is.null(valData) && nrow(valData) > 0 && "scenario" %in% names(valData)) {
              isHistorical <- grepl("^historical$", as.character(valData$scenario), ignore.case = TRUE)
              valData <- valData[isHistorical, ]
            }
          }
        }, error = function(e) {
          message("Warning: Could not load validation data for ", varName, ": ", e$message)
          valData <<- NULL
        })

        tryCatch({
          # For dashboard: show historical data + MAgPIE projections

          unitLabel <- if (!is.null(plotData$unit) && length(plotData$unit) > 0) as.character(plotData$unit[1]) else ""

          # Clean up scenario names (remove metadata patterns like ",1,NA" and timestamps)
          cleanScenarioName <- function(x) {
            x <- as.character(x)
            x <- gsub(",\\d+,NA\\)", "", x)
            x <- gsub(",\\d+,NA$", "", x)
            x <- gsub(",\\d+\\)$", "", x)
            x <- gsub(",\\d+$", "", x)
            x <- gsub(",NA\\)$", "", x)
            x <- gsub(",NA$", "", x)
            x <- gsub("\\s+\\d{2}:\\d{2}:\\d{2}", "", x)
            # Only remove unbalanced parentheses (vectorized)
            nOpen <- nchar(gsub("[^(]", "", x))
            nClose <- nchar(gsub("[^)]", "", x))
            # Remove leading ( if more opening than closing
            x <- ifelse(nOpen > nClose, sub("^\\(", "", x), x)
            # Remove trailing ) if more closing than opening
            x <- ifelse(nClose > nOpen, sub("\\)$", "", x), x)
            trimws(x)
          }

          # Safely clean scenario names
          plotData$scenario_clean <- tryCatch({
            factor(cleanScenarioName(plotData$scenario))
          }, error = function(e) {
            factor(as.character(plotData$scenario))
          })

          # Start plot
          p <- ggplot2::ggplot() +
            ggplot2::theme_minimal() +
            ggplot2::labs(y = unitLabel, x = "Year", color = "Scenario") +
            ggplot2::theme(
              legend.position = "bottom",
              legend.title = ggplot2::element_text(size = 10),
              legend.text = ggplot2::element_text(size = 9),
              axis.title = ggplot2::element_text(size = 10),
              axis.text = ggplot2::element_text(size = 9)
            )

          # Add historical data
          if (!is.null(valData) && is.data.frame(valData) && nrow(valData) > 0) {
            valData <- valData[!is.na(valData$value) & !is.na(valData$period), ]
            if (nrow(valData) > 0) {
              # Count unique data sources - use 'model' column if available, otherwise 'scenario'
              if ("model" %in% names(valData)) {
                histSources <- unique(as.character(valData$model))
              } else {
                histSources <- unique(as.character(valData$scenario))
              }
              nSources <- length(histSources)

              # Aggregate to min/max/mean range by period (handles multiple regions/data points)
              histAgg <- stats::aggregate(value ~ period, data = valData,
                                          FUN = function(x) c(min(x, na.rm = TRUE), max(x, na.rm = TRUE), mean(x, na.rm = TRUE)))
              histAgg <- cbind(histAgg[, 1, drop = FALSE], as.data.frame(histAgg$value))
              names(histAgg) <- c("period", "min", "max", "mean")

              # Legend label - always show dataset count
              histLabel <- paste0("Historical (", nSources, " dataset", if(nSources > 1) "s" else "", ")")
              histAgg$label <- histLabel

              # Show as shaded range with mean line
              p <- p +
                ggplot2::geom_ribbon(data = histAgg,
                                     ggplot2::aes(x = period, ymin = min, ymax = max, fill = label),
                                     alpha = 0.3) +
                ggplot2::geom_line(data = histAgg,
                                   ggplot2::aes(x = period, y = mean, linetype = label),
                                   color = "gray40", linewidth = 0.8) +
                ggplot2::scale_fill_manual(name = "", values = stats::setNames("gray60", histLabel)) +
                ggplot2::scale_linetype_manual(name = "", values = stats::setNames("solid", histLabel))
            }
          }

          # Add MAgPIE projection data
          p <- p +
            ggplot2::geom_line(data = plotData, ggplot2::aes(x = period, y = value, color = scenario_clean),
                               linewidth = 1) +
            ggplot2::geom_point(data = plotData, ggplot2::aes(x = period, y = value, color = scenario_clean),
                                size = 2)

          # Convert to plotly and clean trace names
          pl <- ggplotly(p)

          # Clean trace names in plotly object
          for (i in seq_along(pl$x$data)) {
            if (!is.null(pl$x$data[[i]]$name)) {
              pl$x$data[[i]]$name <- cleanScenarioName(pl$x$data[[i]]$name)
            }
            if (!is.null(pl$x$data[[i]]$legendgroup)) {
              pl$x$data[[i]]$legendgroup <- cleanScenarioName(pl$x$data[[i]]$legendgroup)
            }
          }

          plotly::layout(pl,
            legend = list(
              orientation = "h",
              yanchor = "top",
              y = -0.22,
              xanchor = "center",
              x = 0.5,
              font = list(size = 9),
              title = list(text = "")
            ),
            margin = list(b = 60, t = 10),
            xaxis = list(title = list(standoff = 5)),
            yaxis = list(title = list(standoff = 5))
          )
        }, error = function(e) {
          p <- ggplot() +
            annotate("text", x = 1, y = 1, label = paste("Error:", e$message)) +
            theme_void()
          ggplotly(p)
        })
      })

      # Area plot output (using static plot for better rendering)
      output[[areaPlotId]] <- renderPlot({
        vars <- allVariables()
        req(length(vars) > 0, report$ready(), report$report())

        # Check if this plot index is within the available variables
        if (i > length(vars)) {
          return(NULL)
        }

        varConfig <- vars[[i]]
        varName <- varConfig$variable
        plotType <- if (!is.null(varConfig$plot_type)) varConfig$plot_type else "line"

        # Skip if this is a line plot
        if (plotType != "area") {
          return(NULL)
        }

        # Get filtered data - use directChildrenOnly for cleaner area plots
        plotData <- tryCatch({
          filterDataForVariable(
            report$report(),
            varName,
            input$scenario_filter,
            input$region_filter,
            directChildrenOnly = TRUE
          )
        }, error = function(e) {
          message("Error filtering data for ", varName, ": ", e$message)
          return(NULL)
        })

        # Validate plotData
        if (is.null(plotData) || !is.data.frame(plotData) || nrow(plotData) == 0) {
          p <- ggplot() +
            annotate("text", x = 1, y = 1, label = paste("No data for:", varName)) +
            theme_void()
          return(p)
        }

        # Check required columns
        requiredCols <- c("period", "value", "variable")
        missingCols <- setdiff(requiredCols, names(plotData))
        if (length(missingCols) > 0) {
          p <- ggplot() +
            annotate("text", x = 1, y = 1, label = paste("Missing columns:", paste(missingCols, collapse = ", "))) +
            theme_void()
          return(p)
        }

        # Remove NA values
        plotData <- plotData[!is.na(plotData$value) & !is.na(plotData$period), ]

        if (nrow(plotData) == 0) {
          p <- ggplot() +
            annotate("text", x = 1, y = 1, label = paste("No valid data for:", varName)) +
            theme_void()
          return(p)
        }

        if (nrow(plotData) > 5000) {
          p <- ggplot() +
            annotate("text", x = 1, y = 1, label = "Too many data points (>5000)") +
            theme_void()
          return(p)
        }

        # Add group information for area plots
        tmp <- tryCatch({
          extractVariableGroups(levels(plotData$variable))
        }, error = function(e) {
          data.frame()
        })
        if (nrow(tmp) > 0) {
          plotData <- merge(as.data.table(plotData), as.data.table(tmp), by = "variable", all.x = TRUE)
        }

        tryCatch({
          suppressMessages(mipArea(x = plotData) + theme_mip(size = 8))
        }, error = function(e) {
          ggplot() +
            annotate("text", x = 1, y = 1, label = paste("Error:", e$message)) +
            theme_void()
        })
      }, res = 96)
    })
  })

  # Generate dashboard UI dynamically - organized by groups
  output$dashboard_plots <- renderUI({
    if (!report$ready()) {
      return(
        tags$div(
          class = "text-center",
          style = "padding: 50px;",
          tags$h4("Load data to view dashboard"),
          tags$p("Use the 'Select Data' tab to load MAgPIE runs.")
        )
      )
    }

    if (is.null(config$presets) || length(config$presets) == 0) {
      return(
        tags$div(
          class = "text-center",
          style = "padding: 50px;",
          tags$h4("No variables configured"),
          tags$p("No variable presets found in configuration.")
        )
      )
    }

    # Build UI grouped by presets
    groupedUI <- list()
    plotIndex <- 0  # Global plot index for output IDs

    for (presetName in names(config$presets)) {
      preset <- config$presets[[presetName]]
      if (is.null(preset$variables) || length(preset$variables) == 0) next

      # Check if we've hit MAX_PLOTS
      if (plotIndex >= MAX_PLOTS) break

      # Group header
      groupName <- if (!is.null(preset$name)) preset$name else presetName
      groupedUI[[length(groupedUI) + 1]] <- fluidRow(
        column(12,
          tags$h4(groupName, style = "margin-top: 20px; margin-bottom: 10px; border-bottom: 1px solid #ddd; padding-bottom: 5px;")
        )
      )

      # Variables in this group (3 per row)
      vars <- preset$variables
      nvars <- min(length(vars), MAX_PLOTS - plotIndex)
      nRows <- ceiling(nvars / 3)

      for (row in seq_len(nRows)) {
        idx1 <- (row - 1) * 3 + 1
        idx2 <- (row - 1) * 3 + 2
        idx3 <- (row - 1) * 3 + 3

        cols <- list()

        # First plot
        if (idx1 <= nvars) {
          plotIndex <- plotIndex + 1
          varConfig1 <- vars[[idx1]]
          varName1 <- varConfig1$variable
          plotType1 <- if (!is.null(varConfig1$plot_type)) varConfig1$plot_type else "line"

          cols[[1]] <- column(
            width = 4,
            wellPanel(
              tags$h5(varName1, style = "font-size: 13px; font-weight: bold; margin-bottom: 5px;"),
              if (plotType1 == "area") {
                plotOutput(session$ns(paste0("area_plot_", plotIndex)), height = "280px")
              } else {
                plotlyOutput(session$ns(paste0("plot_", plotIndex)), height = "280px")
              }
            )
          )
        }

        # Second plot
        if (idx2 <= nvars) {
          plotIndex <- plotIndex + 1
          varConfig2 <- vars[[idx2]]
          varName2 <- varConfig2$variable
          plotType2 <- if (!is.null(varConfig2$plot_type)) varConfig2$plot_type else "line"

          cols[[2]] <- column(
            width = 4,
            wellPanel(
              tags$h5(varName2, style = "font-size: 13px; font-weight: bold; margin-bottom: 5px;"),
              if (plotType2 == "area") {
                plotOutput(session$ns(paste0("area_plot_", plotIndex)), height = "280px")
              } else {
                plotlyOutput(session$ns(paste0("plot_", plotIndex)), height = "280px")
              }
            )
          )
        }

        # Third plot
        if (idx3 <= nvars) {
          plotIndex <- plotIndex + 1
          varConfig3 <- vars[[idx3]]
          varName3 <- varConfig3$variable
          plotType3 <- if (!is.null(varConfig3$plot_type)) varConfig3$plot_type else "line"

          cols[[3]] <- column(
            width = 4,
            wellPanel(
              tags$h5(varName3, style = "font-size: 13px; font-weight: bold; margin-bottom: 5px;"),
              if (plotType3 == "area") {
                plotOutput(session$ns(paste0("area_plot_", plotIndex)), height = "280px")
              } else {
                plotlyOutput(session$ns(paste0("plot_", plotIndex)), height = "280px")
              }
            )
          )
        }

        groupedUI[[length(groupedUI) + 1]] <- do.call(fluidRow, cols)
      }
    }

    tagList(groupedUI)
  })

}
