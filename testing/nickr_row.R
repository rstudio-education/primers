#' Check that a condition holds on the rows of data between pipe stages.
#'
#' This is used much less frequently than \code{nickr_col}, since most
#' tests are both easier to express and more efficient to run on columns,
#' but it does have occasional uses.  See \code{nickr_col} for examples
#' of using the \code{active} and \code{logger} arguments.
#'
#' Each row is augmented with \code{.ri} for the row index and \code{.row}
#' to hold the entire original row as a list.
#'
#' @param .data Incoming data (omitted if in pipe).
#' @param cond Condition written using column names, \code{.ri}, and \code{.row}.
#' @inheritParams nickr_col
#'
#' @return Input data without modification.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data %>% mutate(...) %>%
#'
#'   # Check that at most one field is zero in any row.
#'   nickr_row(sum(as.double(.row) == 0) <= 1,
#'             msg = "at most one zero per row") %>%
#'
#'   # Further processing on unmodified data.
#'   filter(...)
#' }

nickr_row <- function(.data, cond, msg = "nickr_row", active = TRUE, logger = stop) {

  # Only run check if active.
  if (active) {

    # Augment with row index as `.ri`.
    augmented <- tibble::rowid_to_column(.data, ".r")

    # Check by row (cond is positive, so negate separately to make logic clearer).
    cond <- rlang::enquo(cond)
    passes <- purrr::pmap_lgl(augmented, function(...) {
      # Augment with original row as `.row`.
      func_args <- list(...)
      func_args$.row <- func_args
      func_args$.row$.r <- NULL

      # Check.
      rlang::eval_tidy(cond, func_args)
    })
    failures <- !passes

    # Report.
    if (any(failures)) {
      cond_txt <- deparse(rlang::quo_get_expr(cond))
      msg <- paste0(msg, " with '", cond_txt, "' rows: ",
                    paste(augmented$.r[failures], collapse = " "))
      logger(msg)
    }
  }

  # Return data for the next stage in the pipe.
  invisible(.data)
}
