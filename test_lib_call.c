
#include <stdio.h>
#include <string.h>
#include <stdint.h>

extern int32_t hash_memory_( void *mem, size_t *len );

int
main( int argc, char *argv[] )
{
  char  *str = "this is a test-string";
  size_t len = strlen(str);
  printf( "test %d\n", hash_memory_( str, &len ) );
  return 0;
}

