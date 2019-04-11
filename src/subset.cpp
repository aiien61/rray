#include <rray_types.h>
#include <xtensor/xview.hpp>
#include <xtensor/xdynamic_view.hpp>
#include <xtensor/xio.hpp>
#include "xtensor/xadapt.hpp"
#include <tools/errors.hpp>
#include <Rcpp.h>
using namespace Rcpp;
using namespace rray;

template <typename T>
SEXP rray_bind_assign_impl(xt::rarray<T> out, List args, List dims, List slice_indices_list) {

  RObject arg;
  IntegerVector dim;
  List slice_indices;
  std::vector<std::size_t> shape;

  using inner_type = typename xt::r_detail::get_underlying_value_type_r<T>::type;

  for (int j = 0; j < args.size(); ++j) {
    arg = args[j];
    dim = dims[j];
    shape = as<std::vector<std::size_t>>(dim);
    slice_indices = slice_indices_list[j];

    xt::xdynamic_slice_vector sv({});

    for (int i = 0; i < slice_indices.size(); ++i) {

      if (Rf_isNull(slice_indices[i])) {
        sv.push_back(xt::all());
      }
      else {
        // Only go to std::vector<int> NOT to std::vector<std::size_t>> as
        // that allocates for some reason
        std::vector<int> slice = slice_indices[i];
        sv.push_back(xt::keep(slice));
      }

    }

    auto out_view = xt::dynamic_view(out, sv);

    const std::vector<inner_type>& arg_cpp = as<std::vector<inner_type>>(arg);
    auto replacement = xt::adapt<xt::layout_type::column_major>(arg_cpp, shape);
    out_view = replacement;
  }

  return out;
}

// [[Rcpp::export]]
SEXP rray_bind_assign_cpp(SEXP out, List args, List dims, List slice_indices_list) {

  if (Rf_isNull(out)) {
    return R_NilValue;
  }

  if (MAYBE_REFERENCED(out)) {
    out = Rf_shallow_duplicate(out);
  }

  // Switch on out
  switch(TYPEOF(out)) {

  case REALSXP: {
    return rray_bind_assign_impl(xt::rarray<double>(out), args, dims, slice_indices_list);
  }

  case INTSXP: {
    return rray_bind_assign_impl(xt::rarray<int>(out), args, dims, slice_indices_list);
  }

  case LGLSXP: {
    return rray_bind_assign_impl(xt::rarray<rlogical>(out), args, dims, slice_indices_list);
  }

  default: {
    error_unknown_type();
  }

  }

}

template <typename T>
SEXP rray_subset_assign_impl(xt::rarray<T> x, List arg, IntegerVector dim, List slice_indices) {

  xt::xdynamic_slice_vector sv({});

  for (int i = 0; i < slice_indices.size(); ++i) {

    if (Rf_isNull(slice_indices[i])) {
      sv.push_back(xt::all());
    }
    else {
      std::vector<int> slice = slice_indices[i];
      sv.push_back(xt::keep(slice));
    }

  }

  auto x_view = xt::dynamic_view(x, sv);

  std::vector<int> shape = as<std::vector<int>>(dim);

  using inner_type = typename xt::r_detail::get_underlying_value_type_r<T>::type;

  std::vector<inner_type> arg_cpp = arg[0];

  //const std::vector<inner_type>& arg_cpp = as<std::vector<inner_type>>(arg);

  // auto replacement = xt::adapt<xt::layout_type::column_major>(arg_cpp, shape);
  //
  // x_view = replacement;

  return x;
}

// [[Rcpp::export]]
SEXP rray_subset_assign_cpp(SEXP x, List arg, IntegerVector dim, List slice_indices) {

  if (Rf_isNull(x)) {
    return R_NilValue;
  }

  if (MAYBE_REFERENCED(x)) {
    x = Rf_shallow_duplicate(x);
  }

  // Switch on X
  switch(TYPEOF(x)) {

  case REALSXP: {
    return rray_subset_assign_impl(xt::rarray<double>(x), arg, dim, slice_indices);
  }

  case INTSXP: {
    return rray_subset_assign_impl(xt::rarray<int>(x), arg, dim, slice_indices);
  }

  case LGLSXP: {
    return rray_subset_assign_impl(xt::rarray<rlogical>(x), arg, dim, slice_indices);
  }

  default: {
    error_unknown_type();
  }

  }

}
