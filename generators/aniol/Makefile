CC=g++
CFLAGS=
LIBS=-lm

EXES=hyperon_decay hyperon-gen lambda_decay

all: $(EXES)

%.o: %.c %.h
	$(CC) -c -o $@ $< $(CFLAGS)

%: %.o %.h
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)

.PHONY: clean
clean:
	rm -f *.o *~ core *~ $(EXES)
