#' Detect if factor entry is level, specified by numeric order
#'
#' Useful when filtering by ordered factors.
#'
#' @param .f A factor.
#' @param n Numeric order of desired factor level.
#'   If negative, count backwards along levels.
#'   I.e. `n = -1` denotes the highest factor level.
#' @return A logical vector
#' @export
#' @examples
#' f_letters <- fct_relevel(c("a", "b", "b", "c), "a", "b", "c")
#' fct_nlevel(f_letters, 1)
#' fct_nlevel(f_letters, -1)
fct_nlevel <- function(.f, n) {
  levels <- fct_unique(.f)
  if (n < 0) {
    n <- length(levels) + n + 1
  }

  .f == levels[n]
}

#' Detect if factor entry is lowest level, specified by numeric order
#'
#' Useful when filtering by ordered factors.
#'
#' @param .f A factor.
#' @return A logical vector
#' @export
#' @examples
#' f_letters <- fct_relevel(c("a", "b", "b", "c), "a", "b", "c")
#' fct_lo(f_letters)
#' fct_hi(f_letters)
fct_lo <- function(.f) {
  fct_nlevel(.f, n = 1)
}

#' Detect if factor entry is highest level, specified by numeric order
#'
#' Useful when filtering by ordered factors.
#'
#' @param .f A factor.
#' @return A logical vector
#' @export
#' @examples
#' f_letters <- fct_relevel(c("a", "b", "b", "c), "a", "b", "c")
#' fct_lo(f_letters)
#' fct_hi(f_letters)
fct_hi <- function(.f) {
  fct_nlevel(.f, n = -1)
}
