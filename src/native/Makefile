TARGET = pam_cloudway.so
OBJS = $(patsubst %.c,%.o,$(wildcard *.c))
LOBJS = $(patsubst %.c,%.lo,$(wildcard *.c))
CFLAGS ?= -Wall -W -Wundef -Wshadow -Wmissing-noreturn -Wmissing-format-attribute -Wno-unused-parameter
override CFLAGS += -I. -D_GNU_SOURCE

all: $(TARGET)

$(TARGET): $(LOBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -shared -o $@ $^ -Wl,-soname,$(TARGET),-z,defs -lselinux -lpam

%.o: %.c
	$(CC) $(CFLAGS) -fPIC -c -o $@ $<

%.lo: %.c
	$(CC) $(CFLAGS) -fPIC -DSHARED -c -o $@ $<

clean:
	-rm -f $(OBJS) $(LOBJS) $(TARGET)
