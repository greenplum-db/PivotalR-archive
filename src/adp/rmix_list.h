
// a list with mixed types, usually integer and double
// support double and integer mix

#ifndef R_MIX_LIST_
#define R_MIX_LIST_

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

// list item is vector with uniform type

namespace adp
{
    class RmixList
    {
    public:
	RmixList(SEXP rList);
	~RmixList();
	
	double operator() (int i, int j) const;
	
	int size() const { return n_; }
	int itemSize(int i) const { return size_[i]; }
	int itemType(int i) const { return itemType_[i]; }
	
    private:
	int** intList_;
	double** doubleList_;
	int* itemType_;
	int* size_;
	int n_;    
    };
}

#endif
