.onLoad <- function(...) {
  op <- options()
  if (!"linelist_dictionary" %in% names(op)) {
    invisible(reset_dictionary())
  }
  op.linelist <- list(
    linelist_guess_orders = list(
      world_named_months = c("Ybd", "dby"),
      world_digit_months = c("dmy", "Ymd"), 
      US_formats         = c("Omdy", "YOmd")
    )
  )
  toset <- !names(op.linelist) %in% names(op)
  if (any(toset)) options(op.linelist[toset])
}
