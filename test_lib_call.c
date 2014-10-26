
#include <stdio.h>
#include <string.h>
#include <stdint.h>

extern int32_t hash_memory_( void *mem, size_t *len );
extern int32_t hash_crc32_( int32_t *crc, void *mem, size_t *len );
extern int32_t crc32( int32_t crc, void *mem, size_t len );

int
main( int argc, char *argv[] )
{
  char  *str = "this is a test-string";
  size_t len = strlen(str), crc = 0;
  int i, j, code;

  printf( "test %d\n", hash_memory_( str, &len ) );
  printf( "crc32 %0x\n", hash_crc32_( &crc, str, &len ) );

  for (i = 0; i < 4000; i++)
    for (j = 0; j < 4000; j++)
      code = crc32( 0, str, len );
      //code = hash_crc32_( &crc, str, &len );

  return 0;
}

