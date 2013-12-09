
#include "rmix_list.h"
#include <stdexcept>

namespace adp
{
    RmixList::RmixList(SEXP rList)
    {
	n_ = LENGTH(rList);
	intList_ = new int*[n_];
	doubleList_ = new double*[n_];
	itemType_ = new int[n_];
	size_ = new int[n_];

	for (int i = 0; i < n_; i++)
	{
	    SEXP tmp = VECTOR_ELT(rList, i);

	    size_[i] = LENGTH(tmp);

	    itemType_[i] = TYPEOF(tmp);

	    if (itemType_[i] == INTSXP)
	    {
		intList_[i] = INTEGER(tmp);
		doubleList_[i] = 0;
	    }
	    else if (itemType_[i] == REALSXP)
	    {
		intList_[i] = 0;
		doubleList_[i] = REAL(tmp);
	    }
	    // else
	    //	throw std::invalid_argument("The item type of the list must be either int or double!");

	    tmp = 0;
	}
    }

    RmixList::~RmixList()
    {
	for (int i = 0; i < n_; i++)
	{
	    intList_[i] = 0;
	    doubleList_[i] = 0;
	}

	delete [] intList_;
	delete [] doubleList_;
	delete [] itemType_;
	delete [] size_;
    }

    double RmixList::operator() (int i, int j) const
    {
	// if (i < 0 || i >= n_) throw std::outof_range_("The first index is out of range!");
	// if (j < 0 || j >= size_[i]) throw std::outof_range_("The second index is out of range!");
	if (itemType_[i] == INTSXP)
	    return (double)intList_[i][j];
	else
	    return doubleList_[i][j];
    }
}
