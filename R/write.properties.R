#' Write a Java Properties File
#' @inheritParams read.properties
#' @param properties list with key-value pairs
#' @return nothing; the properties are written to file
#' @seealso \code{\link{read.properties}} for reading properties files
#' @examples
#' tf <- tempfile()
#' write.properties(file = tf,
#'     properties = list(key1 = "value1", key2 = "value2", key3 = "value3"),
#'     fields = c("key2", "key3"))
#' unlink(tf)
#' @author Daan Seynaeve
#' @export
write.properties <- function(file, properties, fields = NULL,
    encoding = "UTF-8") {
  
  if (is.character(file)) {
    file <- file(file, "w", encoding = encoding)
    on.exit(close(file))
  }
  
  if (!inherits(file, "connection")) {
    stop("'file' must be a character string or connection")
  }
  
  if (!inherits(properties, "list")
      || is.null(names(properties))
      || any(names(properties) == "")) {
    stop("'properties' must be a named list with names that differ from the empty string")
  }
  
  if (any(sapply(properties, length) != 1)) {
    stop("'properties' list values must be of length 1")
  }
  
  properties <- if (!is.null(fields)) properties[fields] else properties
  
  writeLines(paste(names(properties), unlist(properties), sep = "="), file)
  
  invisible()
  
}