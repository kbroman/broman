all: rmobj doc
.PHONY: doc rmobj

# build package documentation
doc:
	R -e 'library(devtools);document()'

# remove .o and .so files
rmobj:
	touch src/avoid_no_such_file_or_directory.o
	touch src/avoid_no_such_file_or_directory.so
	rm src/*.o src/*.so
