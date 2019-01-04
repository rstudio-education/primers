#' Check that a condition holds on the columns of data between pipe stages.
#'
#' @param .data Incoming data (omitted if in pipe).
#' @param cond Column-wise condition expression to test.
#' @param msg User message to display if test fails.
#' @param active Is this check turned on (default TRUE). Set FALSE to disable test (e.g., in production).
#' @param logger Function to call with message (e.g., `warning` or `stop`).
#'
#' @return Input data without modification.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data %>% mutate(...) %>%
#'
#'   # Always check that age is greater than or equal to 18.
#'   nickr_col(age >= 18) %>%
#'
#'   # Only check that age lies below maximum when not in production.
#'   nickr_col(age <= max_age, active = !IN.PRODUCTION) %>%
#'
#'   # Generate a warning if person_id doesn't match the expected pattern.
#'   nickr_col(str_detect(person_id, "[A-Z][A-Z]"),
#'             msg = "Person ID must be 2 letters", logger = warning) %>%
#'
#'   # Further processing on unmodified data.
#'   filter(...)
#' }

nickr_col <- function(.data, cond, msg = "nickr_col", active = TRUE, logger = stop) {

  # Only run check if active.
  if (active) {

    # Augment data with row index.
    augmented <- tibble::rowid_to_column(.data, ".r")

    # Check (cond is positive, so negate separately to make logic clearer).
    cond <- rlang::enquo(cond)
    passes <- rlang::eval_tidy(cond, augmented)
    failures <- !passes

    # Accumulate error messages.
    if (any(failures)) {
      cond_txt <- deparse(rlang::quo_get_expr(cond))
      msg <- paste0(msg, " with '", cond_txt, "' rows: ",
                    paste(augmented$.r[failures], collapse = " "))
      logger(msg)
    }
  }

  # Always return data for the next stage in the pipe.
  invisible(.data)
}
