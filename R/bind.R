#' Combine many arrays together into one array
#'
#' These functions bind multiple vectors, matrices, arrays, or rrays together
#' into one, combining along the `axis`.
#'
#' @param ... Vectors, matrices, arrays, or rrays.
#'
#' @param axis A single integer. The axis to bind along.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' a <- matrix(1:4, ncol = 2)
#' b <- matrix(5:6, ncol = 1)
#'
#' # Bind along columns
#' rray_bind(a, b, axis = 2)
#'
#' # Bind along rows
#' # Broadcasting is done automatically
#' rray_bind(a, b, axis = 1)
#'
#' # You can bind "up" to a new dimension
#' # to stack matrices into an array
#' rray_bind(a, b, axis = 3)
#'
#' # ---------------------------------------------------------------------------
#' # Dimension name example
#'
#' x <- matrix(
#'  1:6,
#'  ncol = 3,
#'  dimnames = list(c("a_r1", "a_r2"), c("a_c1", "a_c2", "a_c3"))
#' )
#'
#' y <- matrix(
#'  7:8,
#'  ncol = 1,
#'  dimnames = list(NULL, c("b_c1"))
#' )
#'
#' # Dimension names come along for the ride
#' # following rray name handling
#' rray_bind(x, y, axis = 2)
#'
#' # But, if any inputs are named
#' # along the dimension you are binding on
#' # then all of them must be. This errors
#' # because `y` doesn't have row names
#' \dontrun{
#' rray_bind(x, y)
#' }
#'
#' # You can add "outer" names to the
#' # axis you are binding along
#' rray_bind(outer = x, y, axis = 2)
#'
#' # They are added to existing names with `..`
#' # Outer names can be used to get around the
#' # fact that `y` isn't named.
#' rray_bind(outer = x, outer_y = y, axis = 1)
#'
#' @export
rray_bind <- function(..., axis = 1L) {

  axis <- vec_cast(axis, integer())
  validate_axis(axis, x = numeric(), dims = Inf)

  args <- compact(list2(...))

  if (length(args) == 0L) {
    return(NULL)
  }

  # Allow for going up in dimension
  dims <- max(rray_dims_common(!!!args), axis)

  # Finalize partial types (including unspecified)
  # (have to do it again after calling vec_type())
  args <- map(args, vec_type_finalise)
  args <- rray_cast_inner_common(!!!args)

  # We have to reshape each arg so that they all have the same dims before combining
  # TODO - Would be nice to be able to do this at the c++ level
  lst_of_arg_reshape_dim <- map(args, function(x) dim_extend(vec_dim(x), dims))

  axis_sizes <- map_int(args, pull_axis_dim, axis = axis)
  out_axis_size <- sum(axis_sizes)

  out_info <- build_out_info(args, dims, axis, out_axis_size)

  # TODO - do this at the c++ level? then you could create
  # lst_of_range_lsts at that level too. you'd just have to pass
  # `axis_sizes` along
  # Build the positions where xt::all() will be used
  alls <- build_alls(dims, axis)

  pos <- 1L
  lst_of_range_lsts <- rlang::new_list(length(args))

  # TODO - with the above todo, we should be able to move this to c++
  for (i in seq_along(args)) {
    arg_axis_size <- axis_sizes[i]

    if (arg_axis_size == 0L) {
      next
    }

    # `range` controls where we update `out` at
    range <- c(pos, pos + arg_axis_size - 1L)
    range <- as_cpp_idx(range)

    lst_of_range_lsts[[i]] <- c(alls$before, list(range), alls$after)

    pos <- pos + arg_axis_size
  }

  out <- rray_bind_assign_cpp(out_info$partial, out_info$dim, args, lst_of_range_lsts, lst_of_arg_reshape_dim)

  new_dim_names <- rray_dim_names_common_along_axis(!!!args, axis = axis, dim = out_info$dim)
  out <- set_full_dim_names(out, new_dim_names)

  vec_restore(out, out_info$partial)
}

#' @rdname rray_bind
#' @export
rray_rbind <- function(...) {
  rray_bind(..., axis = 1L)
}

#' @rdname rray_bind
#' @export
rray_cbind <- function(...) {
  rray_bind(..., axis = 2L)
}

# ------------------------------------------------------------------------------
# Helpers

build_out_info <- function(args, dims, axis, out_axis_size) {

  # Get types, expand to the correct dimensions, and set axis dim to 0
  arg_types <- map(args, vec_type)
  arg_types <- map(arg_types, vec_type_finalise)
  arg_types <- map(arg_types, rray_dims_match, dims = dims)
  arg_types <- map(arg_types, set_axis_to_zero, axis = axis)

  # `axis` is currently 0, `size` is also 0 (could be the same axis)
  out_partial <- reduce(arg_types, vec_type2)

  # Now we set `axis`, so only `size` is 0
  if (axis != 1L) {
    dim(out_partial)[axis] <- out_axis_size
  }

  out_dim <- dim(out_partial)

  if (axis == 1L) {
    out_dim[1] <- out_axis_size
  }
  else {
    out_dim[1] <- vec_size_common(!!! args)
  }

  list(partial = out_partial, dim = out_dim)
}

pull_axis_dim <- function(x, axis) {
  if (vec_dims(x) < axis) {
    1L
  }
  else {
    vec_dim(x)[axis]
  }
}

set_axis_to_zero <- function(x, axis) {
  dim(x)[axis] <- 0L
  x
}

build_alls <- function(dims, axis) {

  needs_null <- seq_len(dims)[-axis]

  times_before <- sum(needs_null < axis)
  times_after  <- sum(needs_null > axis)

  before <- rep(list(NULL), times = times_before)
  after  <- rep(list(NULL), times = times_after)

  list(before = before, after = after)
}

# ------------------------------------------------------------------------------
# Names related helpers

rray_dim_names_common_along_axis <- function(..., axis, dim) {

  args <- compact(list2(...))

  dims <- max(rray_dims_common(!!!args), axis)
  axis_sizes <- map_int(args, pull_axis_dim, axis = axis)

  arg_dim_names <- map(args, dim_names)
  arg_dim_names <- map(arg_dim_names, dim_names_extend, dims = dims)

  axis_meta_names <- map(arg_dim_names, get_meta_names, axis = axis)
  axis_meta_names <- reduce(axis_meta_names, coalesce_meta_dim_names)

  axis_outer_names <- names2(args)

  axis_dim_names <- map(arg_dim_names, get_axis_names, axis = axis)
  axis_dim_names <- pmap(list(axis_outer_names, axis_dim_names, axis_sizes), outer_names)
  axis_dim_names <- discard(axis_dim_names, axis_sizes == 0L)
  axis_dim_names <- combine_axis_dim_names(axis_dim_names, axis = axis)

  non_axis_dim_names <- map(arg_dim_names, delete_axis_names, axis = axis)
  non_axis_dim_names <- map(non_axis_dim_names, restore_dim_names, to_dim = dim[-axis])
  non_axis_dim_names <- reduce(non_axis_dim_names, coalesce_dim_names)

  out <- rray_expand_dim_names(non_axis_dim_names, axis)

  if (!is.null(axis_dim_names)) {
    out[[axis]] <- axis_dim_names
  }

  if (!is.null(axis_meta_names)) {
    names(out)[axis] <- axis_meta_names
  }

  out
}

combine_axis_dim_names <- function(axis_dim_names, axis) {
  axis_namedness <- map_lgl(axis_dim_names, axis_is_fully_named)
  ok <- all(axis_namedness) || !any(axis_namedness)

  if (!ok) {
    glubort("If any elements along axis {axis} are named, all must be named.")
  }

  vec_c(!!! axis_dim_names)
}

axis_is_fully_named <- function(names) {
  (!is.null(names)) && all(names != "" & !is.na(names))
}

get_meta_names <- function(x_names, axis) {
  names(x_names)[axis]
}

get_axis_names <- function(x_names, axis) {
  x_names[[axis]]
}

delete_axis_names <- function(x_names, axis) {
  x_names[-axis]
}

outer_names <- function(outer, names, n) {

  has_outer <- !is.null(outer) && !outer %in% c("", NA)

  if (!has_outer) {
    return(names)
  }

  has_inner <- !is.null(names)

  if (has_inner) {
    paste0(outer, "..", names)
  }
  else {
    if (n == 1) {
      outer
    }
    else {
      paste0(outer, seq_len(n))
    }
  }

}
