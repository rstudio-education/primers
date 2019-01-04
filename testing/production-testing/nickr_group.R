#' Check that a condition holds on groups of data between pipe stages.
#'
#' Expressions can use special functions like \code{n()} that are allowed in
#' \code{summarize} calls. This function should only be put in pipes after
#' \code{group_by} has been called (so that groups exist).
#'
#' @param .data Incoming data (omitted if in pipe).
#' @param cond Condition written using column names and aggregation functions.
#' @inheritParams nickr_col
#'
#' @return Input data without modification.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data %>%
#'   # Preliminary processing.
#'   mutate(...) %>%
#'
#'   # Aggregation.
#'   group_by(person_id) %>%
#'
#'   # Warn if there are less than 5 records in any group in production.
#'   nickr_group(n() >= 5, msg = "Should have at least 5 records",
#'               active = IN.PRODUCTION, logger = warning)
#'
#'   # Further processing on unmodified data.
#'   summarize(...)
#' }

nickr_group <- function(.data, cond, msg = "nickr_row", active = TRUE, logger = stop) {

  if (active) {
    cond <- rlang::enquo(cond)
    passes <- dplyr::pull(dplyr::summarize(.data, !!cond))
    if (!all(passes)) {
      cond_txt <- deparse(rlang::quo_get_expr(cond))
      num_failures <- sum(!passes)
      msg <- paste0(msg, " with '", cond_txt, "' ",
                    length(num_failures), " group(s) failed out of ", length(passes))
      logger(msg)
    }
  }

  # Return data for the next stage in the pipe.
  invisible(.data)
}
