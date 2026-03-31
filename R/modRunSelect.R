#' modRunSelect Module
#'
#' Corresponding server logic to \code{\link{modRunSelectUI}} to select modules runs for further analysis
#'
#' @param id Module ID string, must match the id used in \code{\link{modRunSelectUI}}
#' @param file report data. Can be a CSV/MIF file or rds file with a quitte object (saved with saveRDS).
#' file can also be a vector of rds files. NULL by default; in this case the user can upload files directly in the tool
#' @param resultsfolder folder in which MAgPIE run results are stored.
#' File must come with a overview list called "files"
#' @param username username to be used to access file and resultsfolder
#' @param password password to access file and resultsfolder
#' @param readFilePar read report data files in parallel (faster) (TRUE) or in sequence (FALSE)
#' @param restoreIds reactive containing run IDs to restore from a bookmarked URL (optional)
#' @param lassoIds reactive containing run IDs selected via lasso/box selection on the stats plot (optional)
#' @return a reactive containing a merged data.frame containing results of selected runs
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{modFilterUI}}, \code{\link{appModelstats}}
#' @importFrom shiny updateSliderInput withProgress incProgress Progress renderUI tags icon
#' checkboxGroupInput updateCheckboxGroupInput actionLink
#' @importFrom tools file_path_sans_ext
#' @importFrom data.table is.data.table rbindlist
#' @importFrom curl curl new_handle
#' @importFrom parallel detectCores
#' @importFrom snow makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach %dopar%
#' @importFrom mip shorten_legend
#' @importFrom shiny moduleServer
#' @importFrom utils head
#' @keywords internal

modRunSelect <- function(id, file, resultsfolder,
                         username = NULL, password = NULL, readFilePar = FALSE,
                         restoreIds = NULL, lassoIds = NULL) {
  moduleServer(id, function(input, output, session) {
    readdata <- function(file, username = NULL, password = NULL, addfilename = FALSE) {
      if (grepl("https://", file)) {
        con <- gzcon(curl(file, handle = new_handle(username = username, password = password)))
        out <- readRDS(con)
        close(con)
      } else {
        out <- readRDS(file)
      }
      if (addfilename) out$filename <- as.factor(file)

      convertDate <- function(x) {
        tmp <- try(as.POSIXct(x, origin = "1970-01-01"), silent = TRUE)
        if ("try-error" %in% class(tmp)) {
          return(x)
        } else {
          return(tmp)
        }
      }

      if ("date" %in% names(out)) {
        out$date <- convertDate(out$date)
        if (is.null(out$year)) try(out$year <- format(out$date, "%Y"))
      }
      if ("revision_date" %in% names(out)) {
        out$revision_date <- convertDate(out$revision_date)
      }
      if (!is.data.table(out)) {
        out <- as.data.table(out)
      }
      reorder <- union(intersect("year", names(out)), names(out))
      return(out[, reorder, with = FALSE])
    }

    readreports <- function(ids, resultsfolder, username = NULL, password = NULL) {
      files <- paste0(resultsfolder, "/", ids, ".rds")
      withProgress(message = "Read selected data", value = 0, {
        if (readFilePar) {
          nCores <- detectCores() - 1
          cl <- makeCluster(nCores)
          registerDoSNOW(cl)
          fout <- foreach(file = files, .combine = rbind, .export = "readdata",
                          .options.snow = list(progress = incProgress(1 / length(files),
                                                                      detail = basename(file)))) %dopar% {
            readdata(file, username = username, password = password, addfilename = TRUE)
          }
        } else {
          fout <- list()
          for (file in files) {
            incProgress(1 / (length(files) + 1), detail = basename(file))
            fout[[file]] <- readdata(file, username = username, password = password, addfilename = TRUE)
          }
          incProgress(1, detail = "merge data")
          fout <- rbindlist(fout)
        }
      })

      # Handle duplicate scenario names by adding date suffix
      if (nlevels(fout$scenario) != nlevels(fout$filename)) {
        option1 <- mip::shorten_legend(gsub("/", " ", levels(fout$filename), fixed = TRUE), identical_only = TRUE)
        basePath <- file_path_sans_ext(basename(levels(fout$filename)))
        option2 <- suppressWarnings(format(as.POSIXct(as.numeric(basePath) / 100000, origin = "1970-01-01")))
        option2[is.na(option2)] <- option1[is.na(option2)]
        levels(fout$filename) <- option2
        tmpsep <- " #TMPSEPARATOR# "
        fout$scenario <- as.factor(paste0(fout$scenario, tmpsep, fout$filename))
        short <- sub(paste0(tmpsep, ".*$"), "", levels(fout$scenario))
        levels(fout$scenario) <- sub(tmpsep, " ", levels(fout$scenario))
        unique <- (!duplicated(short) & !duplicated(short, fromLast = TRUE))
        levels(fout$scenario)[unique] <- short[unique]
      }
      fout$filename <- NULL

      return(fout)
    }

    # Transform scenario names based on options (applied reactively, no reload needed)
    transformScenarios <- function(data, includeFolder, shortenNames, folderMapping, loadedIds) {
      if (is.null(data) || nrow(data) == 0) return(data)

      # Work on a copy
      out <- data.table::copy(data)

      # Include folder name in scenario
      if (includeFolder && !is.null(folderMapping) && length(folderMapping) > 0) {
        # Create ID to folder mapping
        idToFolder <- stats::setNames(basename(folderMapping), loadedIds)
        # Get original scenarios (without folder prefix)
        origScenarios <- levels(out$scenario)
        # We need to map scenarios back to IDs - use the order they were loaded
        # Each unique scenario corresponds to one ID in order
        if (length(origScenarios) == length(loadedIds)) {
          newLevels <- paste0(idToFolder[as.character(loadedIds)], " | ", origScenarios)
          levels(out$scenario) <- newLevels
        }
      }

      # Shorten identical parts of scenario names
      if (shortenNames && nlevels(out$scenario) > 1) {
        shortened <- shorten_legend(levels(out$scenario), identical_only = TRUE)
        levels(out$scenario) <- shortened
      }

      return(out)
    }

    readtextfile <- function(file, username = NULL, password = NULL) {
      if (grepl("https://", file)) {
        out <- readLines(curl(file, handle = new_handle(username = username, password = password)))
      } else {
        out <- readLines(file)
      }
      return(out)
    }
    progress <- Progress$new(session, min = 1, max = 10)
    withr::defer(progress$close())
    progress$set(message = "Read in run overview",
                 detail = "This may take a while...",
                 value = 2)
    data <- readdata(file, username = username, password = password)
    progress$set(message = "Check for corresponding model run outputs",
                 detail = "That should be quick...",
                 value = 7)
    if (grepl("https://", file)) {
      ids <- suppressWarnings(as.numeric(sub("\\.rds$", "", readtextfile(paste0(resultsfolder, "/fileListForShinyresults"),
                                                                         username = username, password = password))))
      ids <- ids[!is.na(ids)]
      data <- data[(data$.id %in% ids), ]
    } else {
      data <- data[file.exists(paste0(resultsfolder, "/", data$.id, ".rds")), ]
    }
    if (!is.data.table(data)) {
      data <- as.data.table(data)
    }

    progress$set(message = "Load filter module",
                detail = "That should be quick...",
                value = 9)

    # preselect current year, and previous year if current month is January, February or March
    preselectYear <- format(as.POSIXct(Sys.time()), "%Y")
    if (format(Sys.time(), "%m") %in% c("01", "02", "03")) {
      preselectYear <- c(as.character(as.integer(preselectYear) - 1), preselectYear)
    }

    # preselect last 3 months
    preselectMinDate <- as.POSIXct(Sys.time()) - 3 * 60 * 60 * 24 * 31
    selection <- modFilter("runfilter", data = reactive(data), exclude = ".id", name = "RunSelect",
                           order = "date", preselectYear = preselectYear, preselectMinDate = preselectMinDate)

    x <- reactiveValues(rawOut = NULL, ready = FALSE, loadedIds = NULL, folderMapping = NULL)

    # Helper to get folder mapping for given IDs from selection data
    getFolderMapping <- function(ids, selData) {
      if (is.null(selData)) return(NULL)
      # Look for folder column (magpie_folder, remind_folder, or similar)
      folderCols <- grep("_folder$", names(selData), value = TRUE)
      if (length(folderCols) == 0) return(NULL)
      folderCol <- folderCols[1]  # Use first matching column
      # Get folder values for the requested IDs in order
      idx <- match(ids, selData$.id)
      folders <- as.character(selData[[folderCol]][idx])
      return(folders)
    }

    fullReport <- reactive({
      ids <- selection()$x[[".id"]]
      readreports(ids, resultsfolder, username = username, password = password)
    })

    # Load from restored IDs if provided
    observe({
      # Check if restoreIds is a reactive function
      if (is.null(restoreIds) || !is.function(restoreIds)) {
        return()
      }
      ids <- restoreIds()
      if (!is.null(ids) && length(ids) > 0 && !x$ready) {
        start <- Sys.time()
        message(".:|RunSelect|:.  Restoring data from bookmark (", length(ids), " runs): ",
                paste(head(ids, 3), collapse = ", "), if (length(ids) > 3) "..." else "", appendLF = FALSE)
        tryCatch({
          x$rawOut <- readreports(ids, resultsfolder, username = username, password = password)
          x$ready <- TRUE
          x$loadedIds <- ids
          x$folderMapping <- getFolderMapping(ids, data)
          message("done! (", round(as.numeric(Sys.time() - start, units = "secs"), 2), "s)")
        }, error = function(e) {
          message("failed: ", e$message)
        })
      }
    })

    # Load from lasso selection if provided
    # Track last processed lasso IDs to avoid reprocessing
    lastLassoIds <- reactiveVal(NULL)

    observe({
      if (is.null(lassoIds) || !is.function(lassoIds)) {
        return()
      }
      ids <- lassoIds()
      if (!is.null(ids) && length(ids) > 0) {
        # Check if these IDs were already processed to avoid infinite loop
        lastIds <- isolate(lastLassoIds())
        if (!is.null(lastIds) && length(lastIds) == length(ids) && all(sort(lastIds) == sort(ids))) {
          return()  # Already processed these IDs
        }
        lastLassoIds(ids)  # Mark as processed

        start <- Sys.time()
        message(".:|RunSelect|:.  Loading lasso selection (", length(ids), " runs): ",
                paste(head(ids, 3), collapse = ", "), if (length(ids) > 3) "..." else "", appendLF = FALSE)
        tryCatch({
          x$rawOut <- readreports(ids, resultsfolder, username = username, password = password)
          x$ready <- TRUE
          x$loadedIds <- ids
          x$folderMapping <- getFolderMapping(ids, data)
          message("done! (", round(as.numeric(Sys.time() - start, units = "secs"), 2), "s)")
        }, error = function(e) {
          message("failed: ", e$message)
        })
      }
    })

    observeEvent(input$load, {
      start <- Sys.time()
      # Always load what the filters show
      selectedIds <- selection()$x[[".id"]]
      message(".:|RunSelect|:.  Read data (", length(selectedIds), " runs): ",
              paste(head(selectedIds, 3), collapse = ", "), if (length(selectedIds) > 3) "..." else "", appendLF = FALSE)
      x$rawOut <- readreports(selectedIds, resultsfolder, username = username, password = password)
      x$ready <- TRUE
      x$loadedIds <- selectedIds
      x$folderMapping <- getFolderMapping(selectedIds, selection()$x)
      message("done! (", round(as.numeric(Sys.time() - start, units = "secs"), 2), "s)")
    })

    # Reload button removed - scenario options now apply instantly without reloading

    # Display currently loaded runs info
    output$loaded_runs_info <- renderUI({
      if (!x$ready || is.null(x$rawOut) || nrow(x$rawOut) == 0) {
        return(NULL)
      }

      # Get scenario names from loaded data
      scenarios <- unique(as.character(x$rawOut$scenario))
      nRuns <- length(x$loadedIds)

      tags$div(
        style = "background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px; padding: 10px; margin-bottom: 15px;",
        tags$strong(paste0("Currently loaded: ", nRuns, " run(s)")),
        tags$br(),
        tags$small(
          style = "color: #155724;",
          paste(head(scenarios, 5), collapse = ", "),
          if (length(scenarios) > 5) paste0(" ... and ", length(scenarios) - 5, " more") else ""
        )
      )
    })

    out <- reactiveValues()
    out$report <- reactive({
      if (is.null(x$rawOut) || nrow(x$rawOut) == 0) return(x$rawOut)

      # Apply scenario transformations based on checkbox options (no reload needed)
      includeFolder <- isTRUE(input$include_folder)
      shortenNames <- isTRUE(input$shorten_names)
      transformScenarios(x$rawOut, includeFolder, shortenNames, x$folderMapping, x$loadedIds)
    })
    out$ready <- reactive(x$ready)
    out$variables <- reactive(names(selection()$x)[-1])
    out$selection <- selection
    out$loadedIds <- reactive(x$loadedIds)
    progress$set(message = "Run selection ready",
                 detail = "Move on to the next step...",
                 value = 10)
    return(out)
  })
}
