cmake_minimum_required(VERSION 3.0)
project(parallel_arrays)

find_package(MPI)

include_directories(include)
if(MPI_FOUND)
	include_directories(${MPI_INCLUDE_PATH})
else(MPI_FOUND)
	message("MPI not found by find_package, try to find MPI by your paths")
	set(MPI_INCLUDE_PATH "Enter your path here")
	set(MPI_LIB_PATH "Enter your path to lib here (include .lib file)")
	include_directories(${MPI_INCLUDE_PATH})
	add_library(MPI_LIB STATIC IMPORTED)
	set_property(TARGET MPI_LIB PROPERTY IMPORTED_LOCATION ${MPI_LIB_PATH})
endif()
	
set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Configs" FORCE)
if(NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE Release)
endif()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/Release)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_BINARY_DIR}/Debug)

add_subdirectory(samples)
add_subdirectory(src)