# source files.
SRC = rmix_list.cpp

OBJ = $(SRC:.cpp=.o)

OUT = libadp.a

.SUFFIXES: .cpp

.cpp.o:
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

$(OUT): $(OBJ)
	$(AR) rcs $(OUT) $(OBJ)

clean:
	rm -f $(OBJ) $(OUT) Makefile.bak 