#' Read and parse .INI file
#'
#' @param filepath .INI file to parse
#' @param encoding Encoding of filepath parameter, will default to system
#' encoding if not specifield
#'
#' @seealso \code{\link{write.ini}}
#'
#' @return List with length equivalent to number of [sections], each [section]
#' will be a named vector
#' @export
#'
read.ini <- function(filepath, encoding = getOption("encoding")) {

  sectionREGEXP <- '^\\s*\\[\\s*(.+?)\\s*]'
  # match section and capture section name

  keyValueREGEXP <- '^\\s*[^=]+=.+'
  # match "key = value" pattern

  ignoreREGEXP <- '^\\s*[;#]'
  # match lines with ; or # at start

  ini <- list()
  con <- file(filepath, open = 'r', encoding = encoding)
  on.exit(close(con))

  while ( TRUE ) {

    line <- readLines(con, n = 1, encoding = encoding, warn = F)
    if ( length(line) == 0 ) {
      break
    }

    if ( grepl(ignoreREGEXP, line) ) {
      next
    }

    if ( grepl(sectionREGEXP, line) ) {
      matches <- regexec(sectionREGEXP, line)
      lastSection <- regmatches(line, matches)[[1]][2]
    }

    if ( grepl(keyValueREGEXP, line) ) {
      tempKeyValue <- trimws(unlist(strsplit(line, "=")))
      key <- tempKeyValue[1]
      value <- tempKeyValue[2]

      ini[[ lastSection ]] <- c(ini[[ lastSection ]], list(key = value))
      names(ini[[ lastSection ]])[ match('key', names(ini[[ lastSection ]])) ] <- key
    }

  }

  ini
}

#' Parse list structure to .INI file
#'
#' @param x List with structure to be write at .INI file.
#'
#' @param filepath .INI file to write
#' @param encoding Encoding of filepath parameter, will default to system
#' encoding if not specifield
#'
#' @seealso \code{\link{read.ini}}
#'
#' @examples
#' \dontrun{
#' ## Create a new list holding our INI
#' newini = list()
#' newini[[ "Section Name" ]][[ "Key name" ]] = "Key value"
#' newini[[ "Section Name 2" ]][[ "Key name" ]] = "Key value"
#' ## Write structure to file
#' write.ini(newini, "written.ini")
#' }
#'
#'
#' @export
#'
write.ini <- function(x, filepath, encoding = getOption("encoding")) {

  con <- file(filepath, open = 'w', encoding = encoding)
  on.exit(close(con))

  for(section in names(x) ) {
    writeLines( paste0('[', section, ']'), con)
    for (key in x[ section ]) {
      writeLines( paste0(names(key), ' = ', key), con)
    }
    writeLines("", con)
  }

}
