# Sample Python client that sends commands to a server.
#
# Eli Bendersky [http://eli.thegreenplace.net]
# This code is in the public domain.
from grpc.beta import implementations

import stringdb_pb2

PORT = 4050
TIMEOUT_SECONDS = 3


def set_value(stub, key, value):
    request = stringdb_pb2.SetValueRequest(key=key, value=value)
    response = stub.SetValue(request, TIMEOUT_SECONDS)
    return response.value

def get_value(stub, key):
    request = stringdb_pb2.GetValueRequest(key=key)
    response = stub.GetValue(request, TIMEOUT_SECONDS)
    return response.value

def count_value(stub, key):
    request = stringdb_pb2.CountValueRequest(key=key)
    response = stub.CountValue(request, TIMEOUT_SECONDS)
    return response.count


def main():
  channel = implementations.insecure_channel('localhost', PORT)
  stub = stringdb_pb2.beta_create_StringDb_stub(channel)

  # some sample data for testing
  print 'Running sample data...'
  set_value(stub, 'foo', 'bar')
  set_value(stub, 'baz', 'anaconda is here')
  print get_value(stub, 'foo')
  print count_value(stub, 'baz')


if __name__ == '__main__':
  main()
