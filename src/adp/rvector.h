
// a wrapper for R vectors

#ifndef R_VECTOR_
#define R_VECTOR_

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <stdexcept>
#include <typeinfo>

namespace adp
{
    template<typename T>
        class Rvector
    {
    public:
        Rvector(SEXP rVector);
        ~Rvector();
    
        T& operator() (int i);
        const T& operator() (int i) const;
        int size() const;

    private:
        T* vector_;
        int size_;
    };

    template<typename T>
        Rvector<T>::Rvector(SEXP rVector)
    {
        if (typeid(T).name()[0] == 'i' && TYPEOF(rVector) == INTSXP)
            //vector_ = (T*)INTEGER(ASINTEGER_(rVector));
            vector_ = (T*)INTEGER(rVector);
        else if (typeid(T).name()[0] == 'd' && TYPEOF(rVector) == REALSXP)
            //vector_ = (T*)REAL(ASNUMERIC_(rVector));
            vector_ = (T*)REAL(rVector);
        /* else */
        /*     throw std::invalid_argument("The typeName must be either int or double!"); */
    
        size_ = LENGTH(rVector);
    }

    template<typename T>
        Rvector<T>::~Rvector()
    {
        vector_ = 0;
    }

    template<typename T>
        inline T& Rvector<T>::operator() (int i)
    {
        //if (i<0 || i>=size_) throw outof_range_("The index is out of range!");
        return vector_[i];
    }


    template<typename T>
        inline const T& Rvector<T>::operator() (int i) const
    {
        return vector_[i];
    }

    template<typename T>
        inline int Rvector<T>::size() const
    {
        return size_;
    }
}

#endif
