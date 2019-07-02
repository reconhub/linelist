.onLoad <- function(...) {
  op <- options()
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
