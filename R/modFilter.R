#' modFilter Module
#'
#' Shiny module which works together with \code{\link{modFilterUI}} to filter a data set based on user input
#'
#' @param id Module ID string, must match the id used in \code{\link{modFilterUI}}
#' @param data A reactive returning a data.table with observations in rows and filter options in columns
#' @param exclude names of columns that should be not used as filter
#' @param showAll FALSE | If set to TRUE all available filter are shown and the filter selector is hidden
#' @param multiple vector with booleans for each filter defining whether multiple selections are allowed
#' or not. If information is not provided it is assumed that multiple selection is allowed
#' @param xdata additional data.tables which should be filtered by the same rules as data. If provided
#' the format of the return value changes
#' @param xdataExclude similar to exclude a vector of filters that should be ignored for xdata. Useful if
#' xdata should only filtered for a subset of filters applied to data
#' @param order order the filter should be listed (provided as a vector of filter names). Filter not
#' listed here will be shown after the ones mentioned.
#' @param name name used to identify the filter in the log
#' @param preselectYear if provided the year filter will be preselected with this value
#' @param preselectMinDate if provided the date filter will be preselected with this as lower value
#' @param selectionSets named list of selection sets per filter column. Each element is a named list
#' mapping set labels to character vectors of values. When a set is selected, all its member values
#' are added to the selection. Only sets where all members are present in the data are shown.
#' Example: \code{list(region = list("All EUR regions" = c("EUC", "EUS", "EUW", "DEU")))}
#' @return  a reactive list with x as the filtered data and xdata containing the list of additional,
#' filtered data element.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{modFilterUI}}, \code{\link{appModelstats}}
#' @importFrom shiny moduleServer updateSliderInput debounce
#' @importFrom data.table uniqueN
#' @export

modFilter <- function(id, # nolint: cyclocomp_linter.
                      data, exclude = NULL, showAll = FALSE,
                      multiple = NULL, xdata = NULL, xdataExclude = NULL, order = NULL,
                      name = NULL, preselectYear = NULL, preselectMinDate = NULL,
                      selectionSets = NULL) {
  moduleServer(id, function(input, output, session) {

  if (!is.null(name)) name <- paste0(".:|", name, "|:. ")

  for (i in names(xdata)) xdata[[i]] <- as.data.table(xdata[[i]])

  x <- reactiveValues()
  x$initialized <- FALSE
  x$activefilter <- NULL

  # The following two functions implement the logic to convert between a list of raw choices
  # and an augmented list that includes raw choices and selection sets.

  # Build grouped choices for selectize dropdown, adding selection sets as an optgroup
  augmentChoices <- function(choices, filter, selectionSets) {
    sets <- selectionSets[[filter]]
    if (is.null(sets)) return(choices)
    # Only include sets where ALL members are present in current choices
    validSets <- Filter(function(members) all(members %in% choices), sets)
    if (length(validSets) == 0) return(choices)
    setChoices <- stats::setNames(paste0("__SET__", names(validSets)), names(validSets))
    return(list(Values = choices, "Selection Sets" = setChoices))
  }

  # When a selection set was selected, expand __SET__ entries in a selection to their
  # member values
  expandSets <- function(selected, filter, selectionSets) {
    sets <- selectionSets[[filter]]
    if (is.null(sets) || is.null(selected)) return(selected)
    isSet <- grepl("^__SET__", selected)
    if (!any(isSet)) return(selected)
    setNames <- sub("^__SET__", "", selected[isSet])
    expanded <- unlist(sets[setNames], use.names = FALSE)
    return(unique(c(selected[!isSet], expanded)))
  }

  selectdata <- function(data, input, filter, xdata, xdataExclude) {
    start <- Sys.time()
    if (is.null(data)) {
      return(NULL)
    }
    message(name, " Run selectdata in modFilter..", appendLF = FALSE)
    if (!("data.table" %in% class(data))) data <- as.data.table(data)
    for (f in filter) {
      slf <- paste0("slider", f)
      if (!is.null(input[[slf]])) {
        # Skip if no valid data for this filter
        validData <- data[[f]][!is.na(data[[f]])]
        if (length(validData) == 0) next

        slmax <- max(validData)
        slmin <- min(validData)
        if (x[[slf]]["max"] != slmax || x[[slf]]["min"] != slmin) {
          updateSliderInput(session, slf,
            min = slmin - 60, max = slmax + 60,
            value = input[[slf]]
          )
          x[[slf]]["max"] <- slmax
          x[[slf]]["min"] <- slmin
        }

        tmp <- function(data, f, min, max) {
          if (is.factor(data[[f]])) {
            tmp <- as.numeric(levels(data[[f]]))[data[[f]]]
          } else {
            tmp <- data[[f]]
          }
          fvec <- ((tmp >= min) & (tmp <= max))
          fvec[is.na(fvec)] <- FALSE
          return(data[fvec, ])
        }
        data <- tmp(data, f, input[[slf]][1], input[[slf]][2])
        if (!(f %in% xdataExclude)) {
          for (n in names(xdata)) {
            xdata[[n]] <- tmp(xdata[[n]], f, input[[slf]][1], input[[slf]][2])
          }
        }
      } else {
        sf <- paste0("select", f)
        if (!is.null(input[[sf]])) {
          # Get unique sorted choices, dropping unused factor levels
          slchoices <- sort(unique(as.character(data[[f]])))
          # Expand any selection set entries to their member values
          rawSelected <- expandSets(input[[sf]], f, selectionSets)
          # Compute valid selection (intersection of expanded selection and available choices)
          validSelected <- intersect(rawSelected, slchoices)
          if (length(validSelected) == 0) validSelected <- NULL
          # Update UI if there are choices, sets were expanded, or choices changed
          setsExpanded <- any(grepl("^__SET__", input[[sf]]))
          if (length(slchoices) > 0 && (setsExpanded || !setequal(slchoices, x[[sf]]))) {
            augmented <- augmentChoices(slchoices, f, selectionSets)
            updateSelectizeInput(session, sf, choices = augmented, selected = validSelected)
            x[[sf]] <- slchoices
          }
          tmp2 <- function(data, f, selection) {
            # If selection is empty, don't filter - include all available values
            if (length(selection) == 0) return(data)
            fvec <- (data[[f]] %in% selection)
            fvec[is.na(fvec)] <- FALSE
            return(data[fvec, ])
          }
          # Use validSelected for filtering (not input which may have invalid values)
          data <- tmp2(data, f, validSelected)
          if (!(f %in% xdataExclude)) {
            for (n in names(xdata)) {
              xdata[[n]] <- tmp2(xdata[[n]], f, validSelected)
            }
          }
        }
      }
    }
    out <- list(x = data)
    if (!is.null(xdata)) out$xdata <- xdata

    message("done! (", round(as.numeric(Sys.time() - start, units = "secs"), 2), "s)")
    return(out)
  }

  selectUI <- function(session, filter, data, class, multiple, preselectYear, preselectMinDate,
                       selectionSets = NULL) {
    if (filter == "") {
      return(NULL)
    }
    if (is.na(class)) class <- "NA"
    if (class == "POSIXct") {
      min <- min(data, na.rm = TRUE) - 60
      max <- max(data, na.rm = TRUE) + 60
      id <- paste0("slider", filter)
      x[[id]] <- c(min = min, max = max)
      if (filter == "date" && !is.null(preselectMinDate)) {
        value <- c(max(preselectMinDate, min(data, na.rm = TRUE) - 60), max(data, na.rm = TRUE) + 60)
      } else {
        value <- c(min(data, na.rm = TRUE) - 60, max(data, na.rm = TRUE) + 60)
      }
      return(tags$div(
        id = session$ns(paste0("div", filter)),
        sliderInput(
          inputId = session$ns(id),
          label = filter,
          min = min,
          max = max,
          value = value,
          ticks = FALSE,
          step = 6*3600,
          timeFormat = "%F %H:%M"
        )
      ))
    } else if (class %in% c("integer", "numeric")) {
      min <- floor(min(data, na.rm = TRUE))
      max <- ceiling(max(data, na.rm = TRUE))
      id <- paste0("slider", filter)
      x[[id]] <- c(min = min, max = max)
      return(tags$div(
        id = session$ns(paste0("div", filter)),
        sliderInput(
          inputId = session$ns(id),
          label = filter,
          min = min,
          max = max,
          value = c(min(data, na.rm = TRUE), max(data, na.rm = TRUE)),
          ticks = FALSE,
          sep = ""
        )
      ))
    } else {
      choices <- sort(unique(data))
      id <- paste0("select", filter)
      x[[id]] <- choices
      if (filter == "year") {
        selected <- preselectYear
      } else {
        selected <- NULL
      }
      augmented <- augmentChoices(choices, filter, selectionSets)
      # Suppress warning about large number of options - we use maxOptions to handle this
      return(tags$div(
        id = session$ns(paste0("div", filter)),
        suppressWarnings(
          selectizeInput(
            inputId = session$ns(id),
            label = filter,
            choices = augmented,
            selected = selected,
            multiple = multiple,
            options = list(maxOptions = 5000, maxItems = if (multiple) 1000 else 1)
          )
        )
      ))
    }
  }


  initialize <- function(input, session, data, x, exclude, order, multiple, showAll, preselectYear, preselectMinDate,
                         selectionSets) {

    if (!is.null(data())) {
      start <- Sys.time()
      message(name, " Initialize modFilter..", appendLF = FALSE)
      x$data <- data()

      # get all filters that actually make sense (because there is more than one choice) and save them in x$filter
      multipleChoices <- function(x) {
        x <- x[!is.na(x)]
        if (length(x) < 2) {
          return(FALSE)
        }
        return(any(x != x[1], na.rm = TRUE))
      }
      multipleChoices <- as.vector(sapply(x$data, multipleChoices)) # nolint: undesirable_function_linter.
      x$filter <- names(x$data)[!(names(x$data) %in% exclude) & multipleChoices]

      # order the filter
      if (!is.null(order)) x$filter <- c(intersect(order, x$filter), setdiff(x$filter, order))

      # get filter class to be used as input to selectUI
      x$filterclass <- sapply(x$data, function(x) class(x)[1]) # nolint: undesirable_function_linter.

      # multiple choices allowed?
      x$filtermultiple <- multiple
      x$filtermultiple[x$filter[!(x$filter %in% names(multiple))]] <- TRUE

      # remove active filter
      for (f in x$activefilter) {
        removeUI(
          selector = paste0("#", session$ns(paste0("div", escapeRegex(f))))
        )
      }
      x$activefilter <- NULL

      # here we actually initialize the filter
      if (showAll) {
        removeUI(selector = paste0("#", session$ns("filterselector")))
        tmpfunc <- function(xf, x) {
          return(selectUI(session, xf, x$data[[xf]], x$filterclass[xf], x$filtermultiple[xf],
                          preselectYear, preselectMinDate, selectionSets))
        }
        uiList <- lapply(x$filter, tmpfunc, x)
        output$moreFilters <- renderUI(tagList(uiList))
        x$activefilter <- x$filter
      } else {
        updateSelectInput(session, "filter", choices = x$filter)
        if ("year" %in% x$filter) {
          insertUI(
            selector = paste0("#", session$ns("filterend")),
            where = "beforeBegin",
            ui = selectUI(session, "year", x$data[["year"]],  x$filterclass["year"], x$filtermultiple["year"],
                          preselectYear, preselectMinDate, selectionSets)
          )

          x$activefilter <- c(x$activefilter, c("year"))
        }
        if (length(x$filter) > 0 && input$filter == x$filter[1]) {
          insertUI(
            selector = paste0("#", session$ns("filterend")),
            where = "beforeBegin",
            ui = selectUI(session, input$filter, x$data[[input$filter]], x$filterclass[input$filter],
                          x$filtermultiple[input$filter], preselectYear, preselectMinDate, selectionSets)
          )
          x$activefilter <- c(x$activefilter, input$filter)
        }
      }
      x$initialized <- TRUE
      message("  done! (", round(as.numeric(Sys.time() - start, units = "secs"), 2), "s)")
    }
  }

  updatefilter <- function(input, x, preselectYear, preselectMinDate, selectionSets) {
    if (!(input$filter %in% x$activefilter)) {
      insertUI(
        selector = paste0("#", session$ns("filterend")),
        where = "beforeBegin",
        ui = selectUI(session, input$filter, x$out$x[[input$filter]], x$filterclass[input$filter],
                      x$filtermultiple[input$filter], preselectYear, preselectMinDate, selectionSets)
      )
      x$activefilter <- c(x$activefilter, input$filter)
    }
    for (f in setdiff(x$activefilter, input$filter)) {
      if (f == "year") next # why would this even be removed?
      if (!is.null(input[[paste0("slider", f)]])) {
        id <- paste0("slider", f)
        removeUI <- ((input[[id]][1] <= x[[id]]["min"]) &
                       (input[[id]][2] >= x[[id]]["max"]))
      } else {
        removeUI <- ifelse(is.null(input[[paste0("select", f)]]), TRUE, FALSE)
      }
      if (removeUI) {
        removeUI(
          selector = paste0("#", session$ns(paste0("div", escapeRegex(f))))
        )
        x$activefilter <- setdiff(x$activefilter, f)
      }
    }
  }


  observeEvent(data(), {
    initialize(input, session, data, x, exclude, order, multiple, showAll, preselectYear, preselectMinDate,
               selectionSets)
  })

  observe({
    x$out <- selectdata(data(), input, x$activefilter, xdata, xdataExclude)
  })

  observeEvent(input$filter, if (!showAll) updatefilter(input, x, preselectYear, preselectMinDate, selectionSets))

  output$observations <- renderText(paste0(dim(x$out$x)[1], " observations"))

  return(debounce(reactive(x$out), 500))
  })
}
