#include <limits.h>
#include <assert.h>

#include "irmode.h"
#include "tv.h"

int main() {
  ir_init();

  ir_tarval *one = new_tarval_from_long(1, mode_LLs);
  ir_tarval *longmax = new_tarval_from_long(LONG_MAX, mode_LLs);
  ir_tarval *longmaxp = tarval_add(longmax, one);
  assert (!tarval_is_long(longmaxp));
  ir_tarval *longmax2 = tarval_sub(longmaxp, one, mode_LLs);
  assert (tarval_is_long(longmax2));
  return 0;
}
