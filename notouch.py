from __future__ import division, print_function

import serial

with serial.Serial(port='/dev/ttyACM0', baudrate=115200, parity=serial.PARITY_NONE, stopbits=serial.STOPBITS_ONE, bytesize=serial.EIGHTBITS, timeout=None) as f:
    while True:
        print(
            list(map(
                int,
                f.readline().decode('utf-8').strip().split(",")
            ))
        )

