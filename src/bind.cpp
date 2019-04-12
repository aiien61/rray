#include <rray_types.h>
#include <xtensor/xstrided_view.hpp>
#include <xtensor/xarray.hpp>
#include <tools/errors.hpp>
#include <xtensor/xio.hpp>
#include <Rcpp.h>

template <typename T>
SEXP rray_bind_assign_impl(xt::rarray<T> out,
                           std::vector<std::size_t> dim,
                           Rcpp::List args,
                           Rcpp::List lst_of_range_lsts,
                           Rcpp::List lst_of_arg_dim) {

  // Resize out prototype to the actual dim size. This allocates
  // but is more efficient than using a vec_na()
  out.resize(dim);

  using vec_of_size_t = typename std::vector<std::size_t>;

  // Assign each arg to it's location in `out`
  for (int i = 0; i < args.size(); ++i) {

    if (Rf_length(args[i]) == 0) {
      continue;
    }

    xt::xstrided_slice_vector sv({});
    Rcpp::List range_lst = lst_of_range_lsts[i];

    xt::rarray<T> arg = SEXP(args[i]);
    const vec_of_size_t& reshape_dim = Rcpp::as<vec_of_size_t>(lst_of_arg_dim[i]);
    auto arg_view = xt::reshape_view(arg, reshape_dim);

    // Build the strided view
    for (int j = 0; j < range_lst.size(); ++j) {

      Rcpp::RObject range = range_lst[j];

      if (range.isNULL()) {
        sv.push_back(xt::all());
      }
      else {
        // xt::range(min, max) with the semantics [min, max)
        // So we really want to assign to max+1 to fill correctly
        vec_of_size_t range_cpp = Rcpp::as<vec_of_size_t>(range);
        sv.push_back(xt::range(range_cpp[0], range_cpp[1] + 1));
      }
    }

    xt::strided_view(out, sv) = arg_view;
  }

  return out;
}

// [[Rcpp::export]]
SEXP rray_bind_assign_cpp(SEXP out,
                          std::vector<std::size_t> dim,
                          Rcpp::List args,
                          Rcpp::List lst_of_range_lsts,
                          Rcpp::List lst_of_arg_dim) {

  if (Rf_isNull(out)) {
    return R_NilValue;
  }

  // Switch on out
  switch(TYPEOF(out)) {

  case REALSXP: {
    return rray_bind_assign_impl(xt::rarray<double>(out), dim, args, lst_of_range_lsts, lst_of_arg_dim);
  }

  case INTSXP: {
    return rray_bind_assign_impl(xt::rarray<int>(out), dim, args, lst_of_range_lsts, lst_of_arg_dim);
  }

  case LGLSXP: {
    return rray_bind_assign_impl(xt::rarray<rlogical>(out), dim, args, lst_of_range_lsts, lst_of_arg_dim);
  }

  default: {
    rray::error_unknown_type();
  }

  }

}
