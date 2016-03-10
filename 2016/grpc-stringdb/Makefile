# Makefile for building the sample on Linux, with protoc and the gRPC plugins
# installed on the system following the instructions in gRPC's INSTALL file.
#
# Parts of are taken verbatim from examples/cpp/helloworld/Makefile, so if you
# want to be strict about these things - gRPC's (3-clause BSD) license applies
# to these parts.
# 
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.

CXX = g++
CPPFLAGS += -I/usr/local/include -pthread
CXXFLAGS += -std=c++11
LDFLAGS += -L/usr/local/lib -lgrpc++_unsecure -lgrpc -lprotobuf -lpthread -ldl
PROTOC = protoc
GRPC_CPP_PLUGIN = grpc_cpp_plugin
GRPC_CPP_PLUGIN_PATH ?= `which $(GRPC_CPP_PLUGIN)`
GRPC_PYTHON_PLUGIN = grpc_python_plugin
GRPC_PYTHON_PLUGIN_PATH ?= `which $(GRPC_PYTHON_PLUGIN)`

EXECUTABLES = db_server

all: $(EXECUTABLES)

.PRECIOUS: %.grpc.pb.cc
%.grpc.pb.cc: %.proto
	$(PROTOC) --grpc_out=. --plugin=protoc-gen-grpc=$(GRPC_CPP_PLUGIN_PATH) $<

.PRECIOUS: %.pb.cc
%.pb.cc: %.proto
	$(PROTOC) --cpp_out=. $<

db_server: stringdb.pb.o stringdb.grpc.pb.o db_server.o
	$(CXX) $^ $(LDFLAGS) -o $@

stringdb_pb2.py: stringdb.proto
	$(PROTOC) --python_out=. --grpc_out=. \
		--plugin=protoc-gen-grpc=$(GRPC_PYTHON_PLUGIN_PATH) $<

clean:
	rm -f $(EXECUTABLES) *.pb.cc *.pb.h *.pb.o *.o *_pb2.py *.pyc
