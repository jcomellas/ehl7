#
# Project file for the HL7 Parser library.
#

TEMPLATE                        = lib
CONFIG                         -= qt
CONFIG                         += dll thread console warn_on release

# Options common to all platforms/compilers.
DEFINES                        += HL7PARSER_DLL
# INCLUDEPATH                    += ../include
DEPENDPATH                     += .
DESTDIR                         = ../priv
VERSION                         = 1.0.0

# Options for Win32.
win32:DEFINES                  += -D_DLL

# Options for the debug version.
debug {
    TARGET                      = ehl7d
    OBJECTS_DIR                 = .obj/debug
}
release {
    # Options for the release version.
    DEFINES                    += NDEBUG
    TARGET                      = ehl7
    OBJECTS_DIR                 = .obj/release
    # Don't remove debug symbols in release mode
    QMAKE_CXXFLAGS_RELEASE     += -g
    QMAKE_CFLAGS_RELEASE       += -g
    QMAKE_LFLAGS_RELEASE        =
    QMAKE_STRIP                 =
}

SOURCES                         = $$files(*.c)
# HEADERS                         = $$files(../include/*.h)

