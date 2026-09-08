#!/usr/bin/env python3
import sys
import socket
import fcntl

def main():
    interface = sys.argv[-1]
    sock = socket.socket(family=socket.AF_PACKET, type=socket.SOCK_RAW)
    print(socket.ETH_P_ALL)
    sock.bind((interface, socket.ETH_P_ALL))
    while True:
        data = sock.recv(4096)
        print(data)

if __name__ == "__main__":
    main()
