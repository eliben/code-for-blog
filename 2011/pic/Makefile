CFLAGS = -Wall -std=c99 -g -O0

all: mlpic mlpic_dataonly driver

mlpic: ml_main.c ml_main.h
	gcc $(CFLAGS) -fpic -c ml_main.c -o ml_mainpic.o
	gcc -shared -o libmlpic.so ml_mainpic.o

mlpic_dataonly: ml_pic_dataonly.c
	gcc $(CFLAGS) -fpic ml_pic_dataonly.c -o ml_pic_dataonly.o -c
	gcc -shared -o libmlpic_dataonly.so ml_pic_dataonly.o

driver: driver.c mlpic mlpic_dataonly
	gcc $(CFLAGS) -c driver.c -o driver.o
	gcc -o driver driver.o -L. -lmlpic

clean:
	rm -f *.o
	rm -f *.so
	rm -f *.a
	rm -f driver driver_s
