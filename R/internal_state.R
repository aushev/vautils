# package-private environment for internal state
.vautils_state <- new.env(parent = emptyenv()) # not rlang::env() ?
.vautils_state$flexread_counter <- 0L

#' Increment/peek/reset the flexread call counter
#' @keywords internal
flexread_counter <- function(reset = FALSE, peek = FALSE) {
  if (isTRUE(reset)) {
    .vautils_state$flexread_counter <- 0L
    return(invisible(0L))
  }
  if (isTRUE(peek)) {
    return(.vautils_state$flexread_counter)
  }
  .vautils_state$flexread_counter <- (.vautils_state$flexread_counter %||% 0L) + 1L
  .vautils_state$flexread_counter
}
