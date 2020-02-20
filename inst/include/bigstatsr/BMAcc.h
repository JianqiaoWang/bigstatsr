#ifndef BM_ACC_H
#define BM_ACC_H

/******************************************************************************/

#include <bigstatsr/FBM.h>

using namespace Rcpp;
using std::size_t;

#define NA_FLOAT __FLT_MIN__

/******************************************************************************/

inline std::vector<size_t> vec_int_to_size(const IntegerVector& vec_ind,
                                           size_t limit,
                                           int sub = 0) {

  int n = vec_ind.size();
  std::vector<size_t> vec_ind2(n);

  for (int i = 0; i < n; i++) {
    size_t ind = static_cast<size_t>(vec_ind[i] - sub);
    myassert_bounds(ind, limit);
    vec_ind2[i] = ind;
  }

  return vec_ind2;
}

/******************************************************************************/

template <typename T>
class BMAcc_RW {
public:
  BMAcc_RW(FBM_RW * xpBM) {
    _pMat = static_cast<T*>(xpBM->matrix());
    _nrow = xpBM->nrow();
    _ncol = xpBM->ncol();
  }

  inline T& operator()(size_t i, size_t j) {
    return _pMat[i + j * _nrow];
  }

  inline T& operator[](size_t k) {
    return _pMat[k];
  }

  size_t nrow() const { return _nrow; }
  size_t ncol() const { return _ncol; }
  size_t size() const { return _nrow * _ncol; }

protected:
  T* _pMat;
  size_t _nrow;
  size_t _ncol;
};

/******************************************************************************/

template <typename T>
class SubBMAcc_RW : public BMAcc_RW<T> {
public:
  SubBMAcc_RW(FBM_RW * xpBM,
              const IntegerVector& row_ind,
              const IntegerVector& col_ind,
              int sub = 0)
    : BMAcc_RW<T>(xpBM) {
      _row_ind = vec_int_to_size(row_ind, xpBM->nrow(), sub);
      _col_ind = vec_int_to_size(col_ind, xpBM->ncol(), sub);
    }

  inline T& operator()(size_t i, size_t j) {
    // https://stackoverflow.com/a/32087373/6103040
    return BMAcc_RW<T>::operator()(_row_ind[i], _col_ind[j]);
  }

  // WARNING: operator[] is not redefined

  size_t nrow() const { return _row_ind.size(); }
  size_t ncol() const { return _col_ind.size(); }

protected:
  std::vector<size_t> _row_ind;
  std::vector<size_t> _col_ind;
};

/******************************************************************************/

template <typename T>
class BMAcc {
public:
  BMAcc(FBM * xpBM) {
    _pMat = static_cast<const T*>(xpBM->matrix());
    _nrow = xpBM->nrow();
    _ncol = xpBM->ncol();
  }

  inline T operator()(size_t i, size_t j) {
    return _pMat[i + j * _nrow];
  }

  inline T operator[](size_t k) {
    return _pMat[k];
  }

  size_t nrow() const { return _nrow; }
  size_t ncol() const { return _ncol; }
  size_t size() const { return _nrow * _ncol; }

protected:
  const T* _pMat;
  size_t _nrow;
  size_t _ncol;
};

/******************************************************************************/

template <typename T>
class SubBMAcc : public BMAcc<T> {
public:
  SubBMAcc(FBM * xpBM,
           const IntegerVector& row_ind,
           const IntegerVector& col_ind,
           int sub = 0)
    : BMAcc<T>(xpBM) {
      _row_ind = vec_int_to_size(row_ind, xpBM->nrow(), sub);
      _col_ind = vec_int_to_size(col_ind, xpBM->ncol(), sub);
    }

  inline T operator()(size_t i, size_t j) {
    // https://stackoverflow.com/a/32087373/6103040
    return BMAcc<T>::operator()(_row_ind[i], _col_ind[j]);
  }

  // WARNING: operator[] is not redefined

  size_t nrow() const { return _row_ind.size(); }
  size_t ncol() const { return _col_ind.size(); }

protected:
  std::vector<size_t> _row_ind;
  std::vector<size_t> _col_ind;
};

/******************************************************************************/

#endif // BM_ACC_H
