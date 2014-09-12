//
// csocket - a serial port to socket bridge application
//
// Copyright (C) <2006> Eli Bendersky
// License: LGPL (http://www.gnu.org/licenses/lgpl.html)
//
#include <windows.h>
#include <iostream>
#include <locale>
#include <fstream>
#include <cstdlib>
#include <string>
#include <vector>
#include "Serial.h"

using namespace std;


// A simple and robust string tokenizer, extracts a vector of 
// tokens from a string (str) delimited by delims
//
vector<string> tokenize(string str, string delims)
{
    string::size_type start_index, end_index;
    vector<string> ret;

    // Skip leading delimiters, to get to the first token
    start_index = str.find_first_not_of(delims);

    // While found a beginning of a new token
    //
    while (start_index != string::npos)
    {
		// Find the end of this token
		end_index = str.find_first_of(delims, start_index);
		
		// If this is the end of the string
		if (end_index == string::npos)
			end_index = str.length();

		ret.push_back(str.substr(start_index, end_index - start_index));

		// Find beginning of the next token
		start_index = str.find_first_not_of(delims, end_index);
    }

    return ret;
}


inline int str_atoi(const string &str) 
{ 
	return atoi(str.c_str());                   
}


void cleanup_and_die(SOCKET mysocket_in, SOCKET mysocket_out, CSerial serial_port)
{
	WSACleanup();
	closesocket(mysocket_in);
	closesocket(mysocket_out);

	if (serial_port.IsOpen())
		serial_port.Close();

	exit(0);
}


// Returns the Windows error message implied by the provided error code 
//
string get_error_str(LONG last_error_code)
{
	LPTSTR lpMsgBuf;

	if (!FormatMessage( 
		FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		last_error_code,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPTSTR) &lpMsgBuf,
		0,
		0))
	{
		return "unknown error";
	}

	string ret = lpMsgBuf;
	return ret;
}


struct listener_data
{
	SOCKET mysocket_out;
	CSerial serial_port;
};


// This function is executed in the listener thread. It 'listens' on the serial
// port and whenever new data arrives, transmits it to the socket.
//
DWORD WINAPI serial_listener(LPVOID lpArg)
{
	listener_data* listener = (listener_data*) lpArg;
	const unsigned BUF_SIZE = 10000;
	char buf[BUF_SIZE];
	int rc;
	DWORD nread;
	CSerial::EError serial_error;

	cerr << "[c] started listener thread" << endl;

	// Endless loop until we get killed
	//
	while (1)
	{
		if ((rc = listener->serial_port.Read(buf, BUF_SIZE, &nread)) != ERROR_SUCCESS)
		{
			cerr << "[c] **** Error reading from serial port: " << rc << endl;
			continue;
		}

		if ((serial_error = listener->serial_port.GetError()) != 0)
		{
			string reason;

			switch (serial_error)
			{
			case CSerial::EErrorBreak:		reason = "Break";				break;
			case CSerial::EErrorFrame:		reason = "Framing";				break;
			case CSerial::EErrorIOE:		reason = "IO device";			break;
			case CSerial::EErrorMode:		reason = "Unsupported mode";	break;
			case CSerial::EErrorOverrun:	reason = "Buffer overrun";		break;
			case CSerial::EErrorRxOver:		reason = "RX buffer overflow";	break;
			case CSerial::EErrorParity:		reason = "Parity";				break;
			case CSerial::EErrorTxFull:		reason = "TX buffer full";		break;
			default:						reason = "Unknown";				break;
			}

			cerr << "[c] **** COM receive error: " << reason << endl;
			continue;
		}

		if (nread > 0)
		{
			cerr << "[c] received " << nread << " bytes" << endl;
			send(listener->mysocket_out, buf, nread, 0);
		}
		else
		{
			// we don't want this thread to eat up CPU cycles while it's
			// waiting, so sleep a little
			//
			SleepEx(100, FALSE);
		}
	}

	return 0;
}


int main(int argc, char** argv)
{
	WSADATA WSAdata;
	int rc;
	ofstream log_file;

	// The required command-line argument is a configuration string of 
	// the form:
	// socket_port_in,socket_port_out,serial_port_name,baudrate,parity,stops
	//
	// * socket_port_in is the socket port number in which csocket will
	// receive bytes to transmit to the serial port.
	// * socket_port_out is the socket port number to which csocket will
	// transmit bytes received from the serial port.
	//
	if (argc < 2)
	{
		cerr << "[c] **** Expected a command-line argument" << endl;
		return 1;
	}
	else if (argc >= 3)
	{
		// The second argument is the file name to redirect the logging
		// messages into
		//
		log_file.open(argv[2]);
		cerr.rdbuf(log_file.rdbuf());
	}

	string arg = argv[1];
	vector<string> params = tokenize(arg, ",");

	if (params.size() != 6)
	{
		cerr << "[c] **** Expected 6 comma separated values" << endl;
		return 1;
	}
	
	int portnum_in = str_atoi(params[0]);
	int portnum_out = str_atoi(params[1]);
	string serial_port_name = params[2];
	string baudrate = params[3];
	string parity = params[4];
	string stops = params[5];	

	// Initialize Winsock 
	//
	if (rc = WSAStartup(MAKEWORD(2, 0), &WSAdata))
	{
		cerr << "[c] **** WSAStartup error #" << rc << endl;
		return 1;
	}

	cerr << "[c] Started up " << WSAdata.szDescription << endl;
	
	SOCKET mysocket_in = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

	if (mysocket_in == INVALID_SOCKET)
	{
		cerr << "[c] **** Socket in creation error #" << WSAGetLastError() << endl;
		WSACleanup();
		return 1;
	}

	sockaddr_in mysockaddr_in;
	mysockaddr_in.sin_family			= AF_INET;
	mysockaddr_in.sin_port				= htons(portnum_in);
	mysockaddr_in.sin_addr.S_un.S_addr	= inet_addr("127.0.0.1");

	rc = connect(mysocket_in, (SOCKADDR*) &mysockaddr_in, sizeof(sockaddr_in));

	if (rc == SOCKET_ERROR)
	{
		cerr << "[c] **** Socket in connect error #" << WSAGetLastError() << endl;
		closesocket(mysocket_in);
		WSACleanup();
		return 1;
	}

	cerr << "[c] Connected socket_in at port " << portnum_in << endl;

	SOCKET mysocket_out = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

	if (mysocket_out == INVALID_SOCKET)
	{
		cerr << "[c] **** Socket out creation error #" << WSAGetLastError() << endl;
		WSACleanup();
		return 1;
	}

	sockaddr_in mysockaddr_out;
	mysockaddr_out.sin_family			= AF_INET;
	mysockaddr_out.sin_port				= htons(portnum_out);
	mysockaddr_out.sin_addr.S_un.S_addr	= inet_addr("127.0.0.1");

	rc = connect(mysocket_out, (SOCKADDR*) &mysockaddr_out, sizeof(mysockaddr_out));

	if (rc == SOCKET_ERROR)
	{
		cerr << "[c] **** Socket out connect error #" << WSAGetLastError() << endl;
		closesocket(mysocket_out);
		WSACleanup();
		return 1;
	}

	cerr << "[c] Connected socket_out at port " << portnum_out << endl;

	CSerial::EParity cs_parity;
	CSerial::EStopBits cs_stopbits;
	CSerial serial_port;

	if (parity == "odd") cs_parity = CSerial::EParOdd;
	else if (parity == "even") cs_parity = CSerial::EParEven;
	else cs_parity = CSerial::EParNone;

	if (stops == "1.5") cs_stopbits = CSerial::EStop1_5;
	else if (stops == "2") cs_stopbits = CSerial::EStop2;
	else cs_stopbits = CSerial::EStop1;
	
	if ((rc = serial_port.Open(TEXT(serial_port_name.c_str()))) != ERROR_SUCCESS)
	{
		cerr << "[c] **** Error opening port " << serial_port_name << ": " << rc << endl;
		cleanup_and_die(mysocket_in, mysocket_out, serial_port);
	}

	if ((rc = serial_port.Setup(CSerial::EBaudrate(atoi(baudrate.c_str())), CSerial::EData8, cs_parity, cs_stopbits)) != ERROR_SUCCESS)
	{
		cerr << "[c] **** Error serial setup: " << arg << ": " << rc << endl;
		cleanup_and_die(mysocket_in, mysocket_out, serial_port);
	}

	// We need the non-blocking reads, so that the listening thread won't block
	// the whole application while it's waiting to receive data.
	//
	if ((rc = serial_port.SetupReadTimeouts(CSerial::EReadTimeoutNonblocking)) != ERROR_SUCCESS)
	{
		cerr << "[c] **** Error read timeout setup: " << rc << endl;
		cleanup_and_die(mysocket_in, mysocket_out, serial_port);
	}

	DWORD listener_thread_id;
	HANDLE listener_thread_handle;

	// Create a listener thread for the 'receive' operation. This thread forwards
	// data received from the serial port to the out socket
	//
	listener_data data_for_listener;
	data_for_listener.mysocket_out = mysocket_out;
	data_for_listener.serial_port = serial_port;

	listener_thread_handle = CreateThread(0, 0, serial_listener, &data_for_listener, 0, &listener_thread_id);

	if (listener_thread_handle == NULL)
	{
		cerr << "[c] **** Error creating listener thread: " << get_error_str(GetLastError()) << endl;
	}

	const unsigned RECBUF_SIZE = 10000;
	char recbuf[RECBUF_SIZE] = {'a', 'b', 'c', 'd'};

	// This is the 'send' loop. Wait for a send request from the master,
	// and transmit the data to the serial port.
	//
	while (1)
	{
		// Receive data from the socket. This call blocks until new data arrives.
		//
		int datalen = recv(mysocket_in, recbuf, RECBUF_SIZE, 0);

		if (datalen == SOCKET_ERROR)
		{
			cerr << "[c] **** Socket recv error #" << WSAGetLastError() << endl;
			cleanup_and_die(mysocket_in, mysocket_out, serial_port);
		}

		if (datalen)
		{
			cerr << "[c] sending " << datalen << " bytes\n";

			serial_port.Write((void*) recbuf, datalen);
		}
	}

	return 0;
}
