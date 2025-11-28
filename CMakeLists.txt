cmake_minimum_required(VERSION 3.5)

project(qxf2qif LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(qxf2qif qxf2qif.cpp)

include(GNUInstallDirs)
install(TARGETS qxf2qif
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
