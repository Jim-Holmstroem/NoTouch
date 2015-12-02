
stty -F /dev/ttyACM0 cs8 115200 ignbrk -brkint -imaxbel -opost -onlcr -isig -icanon -iexten -echo -echoe -echok -echoctl -echoke noflsh -ixon -crtscts

parity=serial.PARITY_NONE
stopbits=serial.STOPBITS_ONE
bytesize=serial.EIGHTBITS
timeout=None


# Photodiod model
* http://www.thorlabs.de/tutorials.cfm?tabID=31760
