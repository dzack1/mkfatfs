# KallistiOS ##version##
#
# mkfatfs/Makefile.nonkos
#

all: mkfatfs.kos
CFLAGS += -Wall -Wextra -Werror -std=gnu99

mkfatfs.kos: mkfatfs.c
	$(CC) $(CFLAGS) -g -o mkfatfs.kos mkfatfs.c

clean:
	-rm -f mkfatfs.kos
	-rm -rf mkfatfs.kos.dSYM
