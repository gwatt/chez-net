
srcs = net.c
objs = net.o
lib = libnet.so

cflags = -I/usr/lib64/csv9.4.1/ta6le -I.  -shared -fPIC

.PHONY: clean default

default: $(lib)

$(lib): $(objs)
	$(CC) $(cflags) $^ -o $@

%.o: %.c
	$(CC) $(cflags) -c -o $@ $^

clean:
	rm -f *.so *.o
