#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

int64_t read_int(void) {
  int64_t i;
  scanf("%" SCNd64, &i);
  return i;
}
