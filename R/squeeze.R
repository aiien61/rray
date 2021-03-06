#' Squeeze an rray
#'
#' `rray_squeeze()` is conceptually similar to [base::drop()], but it allows
#' for the specification of specific dimensions to squeeze.
#'
#' @details
#'
#' The dimension name handling of `rray_squeeze()` is essentially identical to
#' `drop()`, but some explanation is always helpful:
#'
#' - Dimension names are removed from the axes that are squeezed. So squeezing
#' a `(2, 1, 2)` object results in a `(2, 2)` object using the dimension names
#' from the original first and third dimensions.
#'
#' - When all dimensions are squeezed, as in the case of `(1, 1, 1)`, then
#' the first dimension names that are found are the ones that are used in the
#' `(1)` result.
#'
#' @param x A vector, matrix, array or rray.
#'
#' @param axes An integer vector specifying the size 1 dimensions to drop. If
#' `NULL`, all size 1 dimensions are dropped.
#'
#' @examples
#' # (10, 1) -> (10)
#' x <- rray(1:10, c(10, 1))
#' rray_squeeze(x)
#'
#' # Multiple squeezed dimensions
#' # (10, 1, 1) -> (10)
#' y <- rray_reshape(x, c(10, 1, 1))
#' rray_squeeze(y)
#'
#' # Use `axes` to specify dimensions to drop
#' # (10, 1, 1) -> drop 2 -> (10, 1)
#' rray_squeeze(y, axes = 2)
#'
#' # Dimension names are kept here
#' # (10, 1) -> (10)
#' x <- set_row_names(x, letters[1:10])
#' rray_squeeze(x)
#'
#' # And they are kept here
#' # (1, 10) -> (10)
#' rray_squeeze(t(x))
#'
#' @export
rray_squeeze <- function(x, axes = NULL) {

  if (is.null(axes)) {
    dim <- vec_dim(x)
    axes <- which(dim == 1L)

    # No axes are length 1
    if (length(axes) == 0L) {
      return(x)
    }
  }

  axes <- vec_cast(axes, integer())
  validate_axes(axes, x)

  res <- squeeze_impl(x, axes)

  new_dim_names <- squeeze_dim_names(x, axes)

  res <- set_full_dim_names(res, new_dim_names)

  vec_restore(res, x)
}

squeeze_impl <- function(x, axes) {
  rray_op_unary_one_cpp("squeeze", x, as_cpp_idx(axes))
}

squeeze_dim_names <- function(x, axes) {

  x_dim_names <- dim_names(x)

  # Generally, names will come from the non `axes` axes
  new_dim_names <- x_dim_names[-axes]

  # If squeezing every axis, that means they all had size 1.
  # Take the names from the first dimension with names
  if (vec_size(axes) == vec_dims(x)) {

    non_null_dim_names <- discard(x_dim_names, is.null)

    if (is_empty(non_null_dim_names)) {
      new_dim_names <- new_empty_dim_names(1L)
    }
    else {
      new_dim_names <- non_null_dim_names[1L]
    }

  }

  new_dim_names
}
