CFLAGS=-Wall -Wextra --std=c99 -O2
CUPSDIR=$(shell cups-config --serverbin)
CUPSDATADIR=$(shell cups-config --datadir)

all:	carps-decode rastertocarpsmf3200 ppd/*.ppd

carps-decode:	carps-decode.c carps.h
	gcc $(CFLAGS) carps-decode.c -o carps-decode

rastertocarps:	rastertocarps.c carps.h
	gcc $(CFLAGS) rastertocarps.c -o rastertocarps -lcupsimage -lcups

rastertocarpsmf3200:	rastertocarpsmf3200.c carps.h
	gcc $(CFLAGS) rastertocarpsmf3200.c -o rastertocarpsmf3200 -lcupsimage -lcups

ppd/*.ppd: carps.drv
	ppdc carps.drv

clean:
	rm -f carps-decode rastertocarpsmf3200

install: rastertocarpsmf3200
	install -s rastertocarpsmf3200 $(CUPSDIR)/filter/
	install -m 644 carps.drv $(CUPSDATADIR)/drv/
	install -m 644 carps.usb-quirks $(CUPSDATADIR)/usb/
