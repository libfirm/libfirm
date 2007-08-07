#include <string.h>
#include <stdlib.h>

#define ZONEID  0x1d4a11

typedef struct memblock_s {
    int     size;           // including the header and possibly tiny fragments
    int     tag;            // a tag of 0 is a free block
    struct memblock_s       *next, *prev;
    int     id;             // should be ZONEID
#ifdef ZONE_DEBUG
    zonedebug_t d;
#endif
} memblock_t;

typedef struct {
    int     size;           // total bytes malloced, including header
    int     used;           // total bytes used
    memblock_t  blocklist;  // start / end cap for linked list
    memblock_t  *rover;
} memzone_t;

typedef char byte;

static void Z_ClearZone( memzone_t *zone, int size ) {
    memblock_t  *block;

    // set the entire zone to one free block

    zone->blocklist.next = zone->blocklist.prev = block =
        (memblock_t *)( (byte *)zone + sizeof(memzone_t) );
#if 0
    zone->blocklist.tag = 1;    // in use block
    zone->blocklist.id = 0;
    zone->blocklist.size = 0;
    zone->rover = block;
    zone->size = size;
    zone->used = 0;
#endif

    block->prev = block->next = &zone->blocklist;
    block->tag = 0;         // free block
    block->id = ZONEID;
    block->size = size - sizeof(memzone_t);
}

void Com_InitSmallZoneMemory( void ) {
	int s_smallZoneTotal = 512 * 1024;
	// bk001205 - was malloc
	memzone_t *smallzone = calloc( s_smallZoneTotal, 1 );
	if ( !smallzone ) {
		abort();
	}
	Z_ClearZone( smallzone, s_smallZoneTotal );

	return;
}

int main(void)
{
	Com_InitSmallZoneMemory();
	return 0;
}
