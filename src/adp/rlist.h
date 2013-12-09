
// a wrapper for R list

#ifndef R_LIST_
#define R_LIST_

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <stdexcept>
#include <typeinfo>

namespace adp
{
    template<typename T>
        class Rlist
    {
    public:
        Rlist(SEXP rList);
        ~Rlist();

        T& operator() (int i, int j);
        const T& operator() (int i, int j) const;
        int size() const;
        int itemSize(int i) const;
    
    private:
        T** list_;
        int* size_;
        int n_; // how many rows
    };

    template<typename T>
        Rlist<T>::Rlist(SEXP rList)
    {
        n_ = LENGTH(rList);
        list_ = new T*[n_];
        size_ = new int[n_];

        bool isInt = false;
        if (typeid(T).name()[0] == 'i')
            isInt = true;
        else if (typeid(T).name()[0] == 'd')
            isInt = false;
        /* else */
        /*     throw std::invalid_argument("The typeName must be either int or double!"); */

    
        for (int i = 0; i < n_; i++)
        {
            SEXP tmp = VECTOR_ELT(rList, i);
            size_[i] = LENGTH(tmp);

            // check whether typename is int or double
            // will support more types in the future
            if (isInt && TYPEOF(tmp) == INTSXP) 
                list_[i] = (T*)INTEGER(tmp);
            else if (!isInt && TYPEOF(tmp) == REALSXP)
                list_[i] = (T*)REAL(tmp);
            /* else */
            /*     throw std::invalid_argument("The typeName must be either int or double!"); */
	
            tmp = 0;
        }
    }

    template<typename T>
        Rlist<T>::~Rlist()
    {
        for (int i = 0; i < n_; i++) list_[i] = 0; 
        delete [] list_; 
        delete [] size_; 
    }
    
    template<typename T>
        inline T& Rlist<T>::operator() (int i, int j)
    {
        // if (i < 0 || i >= n_) throw std::outof_range_("The first index is out of range!");
        // if (j < 0 || j >= size_[i]) throw std::outof_range_("The second index is out of range!");
        return list_[i][j];
    }
    
    template<typename T>
        inline const T& Rlist<T>::operator() (int i, int j) const
    {
        return list_[i][j];
    }
    
    template<typename T>
        inline int Rlist<T>::size() const
    {
        return n_;
    }

    template<typename T>
        inline int Rlist<T>::itemSize(int i) const
    {
        // if (i < 0 || i >= n_) throw std::outof_range_("Row size is out of range!");
        return size_[i];
    }
}

#endif
