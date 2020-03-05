#!/usr/bin/env python

UINT64_MAX = 2**64 - 1

REGION_BITS = 32 # The amount of least significant bits used by a region.
REGION_SIZE = 1 << REGION_BITS # The amount bytes of each region
TOTAL_REGIONS = 2**(48 - REGION_BITS) # The max amount of regions possible

MAX_SIZE = 2**30 # The max size a region can allocate
START_REGION = 0x100000000 # Start of the first region

sizes = []

#SIZES CONFIGURATION
# 16 byte steps to 8kB
for i in range(0, 8 * 1024, 16):
    sizes.append(i);
sizes.append(8 * 1024);
sizes[0] = UINT64_MAX #region 0 is not used

# pow2 stesp
s = 16 * 1024
while s <= MAX_SIZE:
    sizes.append(s)
    s *= 2

region_count = len(sizes);

# remaining filling with UINT64_MAX
sizes += [UINT64_MAX] * (TOTAL_REGIONS - len(sizes)) # Remaining possible regions are not used.

sizes = list(map(lambda x: hex(x), sizes))


# C-style printing
print('#ifndef GEN_LFASAN_SIZES_H')
print('#define GEN_LFASAN_SIZES_H')
print('#include <stdint.h>')
print('#define REGION_BITS '   + hex(REGION_BITS))
print('#define REGION_SIZE '   + hex(REGION_SIZE))
print('#define START_REGION '  + hex(START_REGION))
print('#define TOTAL_REGIONS ' + hex(TOTAL_REGIONS))
print('#define MAX_SIZE '      + hex(MAX_SIZE))
print('#define REGION_COUNT '  + hex(region_count));
print('const uint64_t SIZES[' + hex(len(sizes)) + '] = {\n' + ',\n'.join(sizes) + '};')

print('#define MEMORY_WRITE 0')
print('#define MEMORY_READ 1')
print('#define FUNCTION_ESCAPE 2')
print('#define MEMORY_ESCAPE 3')
print('#define RETURN_ESCAPE 4')
print('#endif')
