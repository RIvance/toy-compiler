cmake_minimum_required(VERSION 3.28)
project(toylang)

#set(CMAKE_CXX_STANDARD 20)

#set(CMAKE_C_COMPILER clang)
#set(CMAKE_CPP_COMPILER clang++)
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++20")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Werror")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-error=unused-private-field")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-error=unused-variable")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-variable")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-function")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-reorder-ctor")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-dynamic-class-memaccess")

# LLVM

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs support)

# ANTLR

find_package(antlr4-runtime REQUIRED)
find_package(antlr4-generator REQUIRED)

set(ANTLR_GENERATED_DIR ${PROJECT_BINARY_DIR}/generated/parser)

file(GLOB ANTLR_GRAMMAR_FILES ${PROJECT_SOURCE_DIR}/*.g4)

add_custom_target(GenerateParser DEPENDS ${ANTLR_GENERATED_SRC})
add_custom_command(TARGET GenerateParser
    COMMAND
    ${CMAKE_COMMAND} -E make_directory ${ANTLR_GENERATED_DIR}
    COMMAND
    "antlr4" -Werror -Dlanguage=Cpp -listener -visitor -o ${ANTLR_GENERATED_DIR}/ -package ${PROJECT_NAME} ${ANTLR_GRAMMAR_FILES}
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
    DEPENDS ${ANTLR_GRAMMAR_FILES}
)

file(GLOB_RECURSE ANTLR_GENERATED_SRC ${ANTLR_GENERATED_DIR}/*.cpp)

set(ANTLR_GENERATED_SRC ${ANTLR_GENERATED_SRC} CACHE INTERNAL "Antlr4 generated source code")

include_directories(${ANTLR4_INCLUDE_DIR})
include_directories(${PROJECT_SOURCE_DIR})
include_directories(${PROJECT_BINARY_DIR}/generated/parser)

add_executable(toylang main.cpp ${ANTLR_GENERATED_SRC})

add_dependencies(toylang GenerateParser)

# Link against LLVM libraries
target_link_libraries(toylang antlr4_shared ${llvm_libs} LLVM)
