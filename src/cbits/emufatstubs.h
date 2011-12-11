#ifndef __emufatsubs_h
#define __emufatsubs_h

typedef struct {
	void (*fn)(void*, char *);
	void *clos;
} emufat_cb;

#endif
