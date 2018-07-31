CC=g++
CFLAGS=
LIBS=-lm

EXES=hyperon_decay hyperon-gen

all: $(EXES)

%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS)

%: %.o
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)

.PHONY: clean
clean:
	rm -f *.o *~ core *~ $(EXES)
