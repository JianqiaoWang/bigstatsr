/******************************************************************************/

// [[Rcpp::depends(bigmemory, BH, RcppArmadillo)]]
#include <RcppArmadillo.h>
#define NDEBUG 1 // https://github.com/RcppCore/RcppArmadillo/issues/116

using namespace Rcpp;

#include <bigmemory/MatrixAccessor.hpp>

/******************************************************************************/

#ifndef UTILS_H
#define UTILS_H

// also defined in R/utils.R
const char* const ERROR_TYPE =
  "unknown type detected for big.matrix object!";

const char* const ERROR_SUB =
  "you can't use this function on a sub.big.matrix!";

#endif // UTILS_H

/******************************************************************************/

template<typename T>
class SubMatrixAccessor {
public:
  typedef T value_type;

public:
  SubMatrixAccessor(BigMatrix &bm,
                    const IntegerVector &row_ind,
                    const IntegerVector &col_ind) {
    if (bm.is_submatrix()) throw Rcpp::exception(ERROR_SUB);

    int n = row_ind.size();
    std::vector<index_type> row_ind2(n);
    for (int i = 0; i < n; i++)
      row_ind2[i] = static_cast<index_type>(row_ind[i]);

    int m = col_ind.size();
    std::vector<index_type> col_ind2(m);
    for (int j = 0; j < m; j++)
      col_ind2[j] = static_cast<index_type>(col_ind[j]);

    _pMat = reinterpret_cast<T*>(bm.matrix());
    _totalRows = bm.total_rows();
    _row_ind = row_ind2;
    _col_ind = col_ind2;
    _nrow = row_ind.size();
    _ncol = col_ind.size();
  }

  inline T operator() (int i, int j) {
    return *(_pMat + _totalRows * _col_ind[j] +_row_ind[i]);
  }

  int nrow() const {
    return _nrow;
  }

  int ncol() const {
    return _ncol;
  }

protected:
  T *_pMat;
  index_type _totalRows;
  int _nrow;
  int _ncol;
  std::vector<index_type> _row_ind;
  std::vector<index_type> _col_ind;
};

/******************************************************************************/
