#' Hello World
#'
#' This is an example of how to create and document exported functions.
#'
#' @param input you should always document the paramters.
#'              Including the expected data type.
#'
#' @export
hello_recon <- function(input) {
  "Hello RECON"
}

#' You can also document internal package function with roxygen
#'
#' Just make sure you add 'noRd' so no latex help files are being created.
#' @noRd
some_internal_function <- function() {
  NA
}

