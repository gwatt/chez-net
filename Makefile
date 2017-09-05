
srcs = net.c
objs = net.o
lib = libnet.so

scmver ?= 9.4.1
machine ?= ta6le
scmdir ?= /usr/lib64/csv$(scmver)/$(machine)

cflags = -I$(scmdir) -I.  -shared -fPIC

.PHONY: clean default

default: $(lib)

$(lib): $(objs)
	$(CC) $(cflags) $^ -o $@

%.o: %.c
	$(CC) $(cflags) -c -o $@ $^

clean:
	rm -f *.so *.o
