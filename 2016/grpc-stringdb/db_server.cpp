// Sample server code implementing the string database, using the gRPC service
// defined in stringdb.proto
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <iostream>
#include <map>
#include <memory>
#include <mutex>
#include <string>

#include <grpc++/grpc++.h>
#include "stringdb.grpc.pb.h"

// Implementation of the StringDb service. We have to implement all of the
// service's methods - these will be invoked by the running gRPC server.
class StringDbImpl final : public stringdb::StringDb::Service {
public:
  grpc::Status GetValue(grpc::ServerContext* context,
                        const stringdb::GetValueRequest* request,
                        stringdb::GetValueReply* reply) override {
    std::lock_guard<std::mutex> lock(db_mutex_);

    auto it = string_db_.find(request->key());
    if (it == string_db_.end()) {
      reply->set_value("");
    } else {
      reply->set_value(it->second);
    }
    return grpc::Status::OK;
  }

  grpc::Status SetValue(grpc::ServerContext* context,
                        const stringdb::SetValueRequest* request,
                        stringdb::SetValueReply* reply) override {
    std::lock_guard<std::mutex> lock(db_mutex_);

    string_db_[request->key()] = request->value();
    reply->set_value(request->value());
    return grpc::Status::OK;
  }

  grpc::Status CountValue(grpc::ServerContext* context,
                          const stringdb::CountValueRequest* request,
                          stringdb::CountValueReply* reply) override {
    std::lock_guard<std::mutex> lock(db_mutex_);

    auto it = string_db_.find(request->key());
    if (it == string_db_.end()) {
      reply->set_count(-1);
    } else {
      reply->set_count(it->second.size());
    }
    return grpc::Status::OK;
  }

private:
  // The actual database.
  std::map<std::string, std::string> string_db_;

  // Mutex serializing access to the map.
  std::mutex db_mutex_;
};

void RunServer() {
  // This parts is taken from the "hello world" gRPC sample.
  std::string server_address("0.0.0.0:4050");
  StringDbImpl service;

  grpc::ServerBuilder builder;
  // Listen on the given address without any authentication mechanism.
  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  // Register "service" as the instance through which we'll communicate with
  // clients. In this case it corresponds to an *synchronous* service.
  builder.RegisterService(&service);
  // Finally assemble the server.
  std::unique_ptr<grpc::Server> server(builder.BuildAndStart());
  std::cout << "Server listening on " << server_address << std::endl;

  // Wait for the server to shutdown. Note that some other thread must be
  // responsible for shutting down the server for this call to ever return.
  server->Wait();
}

int main(int argc, char** argv) {
  RunServer();

  return 0;
}
