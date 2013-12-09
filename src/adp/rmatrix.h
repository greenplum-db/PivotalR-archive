
// a wrapper for R 2D array
// allow to set values

#ifndef R_ARRAY_2D_
#define R_ARRAY_2D_

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <stdexcept>
#include <typeinfo>

namespace adp
{
    template<typename T>
        class Rmatrix
    {
    public:
        Rmatrix(SEXP rArray);
        ~Rmatrix();

        T& operator() (int i, int j);
        const T& operator() (int i, int j) const;
        int rowSize() const;
        int colSize() const;

    private:
        T** array_;
        int rowSize_;
        int colSize_;
    };

    template<typename T>
        Rmatrix<T>::Rmatrix(SEXP rArray)
    {
        SEXP dim = GET_DIM(rArray);
        rowSize_ = INTEGER(dim)[0];
        colSize_ = INTEGER(dim)[1];

        array_ = new T*[colSize_];
    
        if (typeid(T).name()[0] == 'i' && TYPEOF(rArray) == INTSXP)
        {
            for (int i = 0; i < colSize_; i++)
                array_[i] = (T*)(INTEGER(rArray) + i*rowSize_);
        }
        else if (typeid(T).name()[0] == 'd' && TYPEOF(rArray) == REALSXP)
        {
            for (int i = 0; i < colSize_; i++)
                array_[i] = (T*)(REAL(rArray) + i*rowSize_);
        }
        /* else */
        /* { */
        /*     throw std::invalid_argument("The typeName must be either int or double!"); */
        /* } */
    }

    template<typename T>
        Rmatrix<T>::~Rmatrix()
    {
        for (int i = 0; i < colSize_; i++) array_[i] = 0;
        delete [] array_;
    }

    template<typename T>
        inline T& Rmatrix<T>::operator() (int i, int j)
    {
        return array_[j][i];
    }

    template<typename T>
        inline const T& Rmatrix<T>::operator() (int i, int j) const
    {
        return array_[j][i];
    }

    template<typename T>
        inline int Rmatrix<T>::rowSize() const
    {
        return rowSize_;
    }

    template<typename T>
        inline int Rmatrix<T>::colSize() const
    {
        return colSize_;
    }
}

#endif
