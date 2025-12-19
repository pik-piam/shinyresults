#' Bookmark State Utilities
#'
#' Functions for encoding and decoding application state for URL bookmarking.
#' These utilities compress run IDs and application state to keep URLs manageable.
#'
#' @name bookmarkState
#' @author Florian Humpenoeder
NULL

#' Compress IDs for URL
#'
#' Compresses a vector of numeric IDs into a shorter string representation
#' for use in URL query parameters.
#'
#' @param ids Vector of numeric IDs
#' @return Compressed string representation of IDs
#' @author Florian Humpenoeder
#' @importFrom base64enc base64encode
#' @export
compressIds <- function(ids) {
  if (length(ids) == 0) return("")

  # Convert to comma-separated string
  compressed <- paste(ids, collapse = ",")

  # If the string is too long, use base64 encoding
  if (nchar(compressed) > 500) {
    compressed <- paste0("b64:", base64enc::base64encode(charToRaw(compressed)))
  }

  return(compressed)
}

#' Decompress IDs from URL
#'
#' Decompresses a string back into a vector of IDs.
#'
#' @param str Compressed string from URL
#' @return Vector of numeric IDs
#' @author Florian Humpenoeder
#' @importFrom base64enc base64decode
#' @export
decompressIds <- function(str) {
  if (is.null(str) || str == "") return(character(0))

  tryCatch({
    # Check if base64 encoded
    if (grepl("^b64:", str)) {
      decoded <- rawToChar(base64enc::base64decode(sub("^b64:", "", str)))
      ids <- strsplit(decoded, ",")[[1]]
    } else {
      # Plain comma-separated
      ids <- strsplit(str, ",")[[1]]
    }

    # Try to convert to numeric if they look like numbers (preserves large timestamps)
    numIds <- suppressWarnings(as.numeric(ids))
    if (!any(is.na(numIds))) {
      return(numIds)
    }
    return(ids)
  },
  error = function(e) {
    warning("Failed to decompress IDs: ", e$message)
    return(character(0))
  })
}

#' Encode Application State
#'
#' Encodes the essential application state into a compact JSON string.
#'
#' @param state List containing application state
#' @return JSON string representation of state
#' @author Florian Humpenoeder
#' @importFrom base64enc base64encode
#' @export
encodeAppState <- function(state) {
  # Only encode essential state to keep URL short
  essential <- list(
    tab = state$activeTab,
    preset = state$dashboardPreset
  )

  # Convert to JSON and optionally compress
  json <- paste0(
    "{",
    paste(
      sapply(names(essential), function(n) {
        v <- essential[[n]]
        if (is.null(v)) return(NULL)
        sprintf('"%s":"%s"', n, as.character(v))
      }),
      collapse = ","
    ),
    "}"
  )

  # Remove NULL entries
  json <- gsub(",NULL|NULL,|NULL", "", json)
  json <- gsub(",,", ",", json)

  return(json)
}

#' Decode Application State
#'
#' Decodes a JSON string back into application state.
#'
#' @param str JSON string from URL
#' @return List containing application state
#' @author Florian Humpenoeder
#' @importFrom base64enc base64decode
#' @export
decodeAppState <- function(str) {
  if (is.null(str) || str == "") {
    return(list())
  }

  tryCatch({
    # Simple JSON parsing for our limited structure
    # Extract key-value pairs
    str <- gsub("^\\{|\\}$", "", str)
    pairs <- strsplit(str, ",")[[1]]

    state <- list()
    for (pair in pairs) {
      if (grepl(":", pair)) {
        parts <- strsplit(pair, ":")[[1]]
        key <- gsub('"', "", parts[1])
        value <- gsub('"', "", parts[2])
        state[[key]] <- value
      }
    }

    return(state)
  }, error = function(e) {
    warning("Failed to decode app state: ", e$message)
    return(list())
  })
}

#' Create Bookmark URL
#'
#' Creates a shareable URL with the current application state encoded.
#'
#' @param session Shiny session object
#' @param state List containing application state to bookmark
#' @return Full URL string with state encoded in query parameters
#' @author Florian Humpenoeder
#' @export
createBookmarkUrl <- function(session, state) {
  # Get base URL
  baseUrl <- session$clientData$url_protocol
  if (!is.null(baseUrl)) {
    baseUrl <- paste0(
      session$clientData$url_protocol, "//",
      session$clientData$url_hostname,
      if (!is.null(session$clientData$url_port) && session$clientData$url_port != "")
        paste0(":", session$clientData$url_port) else "",
      session$clientData$url_pathname
    )
  } else {
    baseUrl <- ""
  }

  # Encode state
  stateStr <- encodeAppState(state)
  encodedState <- utils::URLencode(stateStr, reserved = TRUE)

  # Build URL
  url <- paste0(baseUrl, "?_state=", encodedState)

  return(url)
}

#' Parse URL for Bookmarked State
#'
#' Extracts and decodes application state from URL query parameters.
#'
#' @param session Shiny session object
#' @return List containing decoded application state, or empty list if none
#' @author Florian Humpenoeder
#' @export
parseBookmarkUrl <- function(session) {
  query <- shiny::parseQueryString(session$clientData$url_search)

  if (!is.null(query[["_state"]])) {
    return(decodeAppState(utils::URLdecode(query[["_state"]])))
  }

  return(list())
}
