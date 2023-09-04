TARGET = twoCrystalBall
CC = gcc
CFLAGS = -Wall -pedantic -g
LIBS = -lm

.PHONY: default all clean

default: $(TARGET).o
	$(CC) $(TARGET).o -o $(TARGET) $(LIBS)
all: default

$(TARGET).o: $(TARGET).c
	$(CC) -c $(TARGET).c $(CFLAGS)
clean:
	rm *.o
	rm $(TARGET)
