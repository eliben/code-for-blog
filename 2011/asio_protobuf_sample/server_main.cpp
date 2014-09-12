//
// server_main.cpp: Main server implementation
//
// Eli Bendersky (eliben@gmail.com)
// This code is in the public domain
//
#include <iostream>
#include <string>
#include <cstdio>
#include <cstdlib>
#include <boost/shared_ptr.hpp>
#include <boost/asio.hpp>
#include "db_server.h"
#include "packedmessage.h"
#include "stringdb.pb.h"


using namespace std;
namespace asio = boost::asio;


int main(int argc, const char* argv[])
{
    unsigned port = 4050;
    if (argc > 1)
        port = atoi(argv[1]);
    cout << "Serving on port " << port << endl;

    try {
        asio::io_service io_service;
        DbServer server(io_service, 4050);
        io_service.run();
    }
    catch (std::exception& e) {
        cerr << e.what() << endl;
    }

    return 0;
}
