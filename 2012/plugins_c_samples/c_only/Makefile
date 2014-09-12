# Separate flags for building objects for DSOs and for the main
# executable.
#
CC = gcc
CCFLAGS = -pedantic -g -Wall -std=c99
CCOPT_SO = $(CCFLAGS) -fpic
CCOPT_EXE = $(CCFLAGS)

CLIB_DIR = clib
PLUGIN_DIR = plugins


all: htmlize_main $(PLUGIN_DIR)/tt.so $(PLUGIN_DIR)/narcissist.so


#
# Compiling clib into objects and the clib.a archive
#
dstring.o: $(CLIB_DIR)/dstring.c
	$(CC) -c $(CCOPT_EXE) $(CLIB_DIR)/dstring.c

mem.o: $(CLIB_DIR)/mem.c $(CLIB_DIR)/mem.h
	$(CC) -c $(CCOPT_EXE) $(CLIB_DIR)/mem.c

die.o: $(CLIB_DIR)/die.c $(CLIB_DIR)/die.h
	$(CC) -c $(CCOPT_EXE) $(CLIB_DIR)/die.c

clib.a: die.o mem.o dstring.o
	ar rs clib.a dstring.o mem.o die.o

#
# The main program
#
HTMLIZE_SRC = $(wildcard *.c)
HTMLIZE_H = $(wildcard *.h)

%.o: %.c $(HTMLIZE_H)
	$(CC) -c $< $(CCOPT_EXE)

# Link with --export-dynamic, so that public symbols will be exposed
# to DSOs.
htmlize_main: htmlize_main.o htmlize.o db.o clib.a plugin_manager.o \
			  plugin_discovery.o
	$(CC) -o $@ $^ -ldl -Wl,--export-dynamic

#
# Plugins
#
$(PLUGIN_DIR)/tt.o: $(PLUGIN_DIR)/tt.c $(HTMLIZE_H)
	$(CC) -c $< -o $@ $(CCOPT_SO) -I.

$(PLUGIN_DIR)/tt.so: $(PLUGIN_DIR)/tt.o
	$(CC) -o $@ $^ -shared

$(PLUGIN_DIR)/narcissist.o: $(PLUGIN_DIR)/narcissist.c $(HTMLIZE_H)
	$(CC) -c $< -o $@ $(CCOPT_SO) -I.

$(PLUGIN_DIR)/narcissist.so: $(PLUGIN_DIR)/narcissist.o
	$(CC) -o $@ $^ -shared

clean:
	rm -rf core *.o *.a htmlize_main plugins/*.o plugins/*.so 

