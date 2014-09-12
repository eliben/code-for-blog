//
// db_server.h: DbServer interface
//
// Eli Bendersky (eliben@gmail.com)
// This code is in the public domain
//
#ifndef DB_SERVER_H
#define DB_SERVER_H

#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>


// Database server. The constructor starts it listening on the given
// port with the given io_service.
//
class DbServer 
{
public:
    DbServer(boost::asio::io_service& io_service, unsigned port);
    ~DbServer();

private:
    DbServer();
    void start_accept();

    struct DbServerImpl;
    boost::scoped_ptr<DbServerImpl> d;
};

#endif /* DB_SERVER_H */

