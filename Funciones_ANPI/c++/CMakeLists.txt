cmake_minimum_required(VERSION 3.10.0)
project(metodospp VERSION 0.1.0 DESCRIPTION "Métodos en C++")

if(WIN32)
	set(CMAKE_SYSTEM_LIBRARY_PATH "${CMAKE_SYSTEM_LIBRARY_PATH};../../../armadillo-11.0.1/examples/lib_win64")
	find_library(openblas_LIBRARY NAMES openblas)
	add_subdirectory(../../../armadillo-11.0.1 [EXCLUDE_FROM_ALL])
	set(ARMADILLO_LIBRARIES armadillo)
else()
	find_package(Armadillo REQUIRED)
	include_directories(${ARMADILLO_INCLUDE_DIRS})
endif()

add_library(metodospp src/ffi.cpp src/thomas.cpp src/gradiente_conjugado.cpp src/steffensen.cpp)
target_include_directories(metodospp PUBLIC include)
target_link_libraries(metodospp ${ARMADILLO_LIBRARIES})

configure_file(metodospp.pc.in metodospp.pc @ONLY)

install(TARGETS metodospp ARCHIVE PUBLIC_HEADER)
install(DIRECTORY include DESTINATION .)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/metodospp.pc DESTINATION share/pkgconfig)
