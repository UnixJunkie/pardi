#!/bin/bash

set -x

# rm -f data/decoys_std.smi
# time (standardiser -i data/decoys.smi -o data/decoys_std.smi 2>&1) > /dev/null
# wc -l data/decoys_std.smi
printf "real\t0m6.363s\n"

rm -f data/decoys_std_pardi.smi
time ./pardi -c 100 -i data/decoys.smi -o data/decoys_std_pardi.smi \
     -w '(standardiser -i %IN -o %OUT 2>&1) > /dev/null'
wc -l data/decoys_std_pardi.smi

rm -f data/test_out.types
./pardi -n 1 -i data/test_in.types -o data/test_out.types \
        -d 'r:^#atoms:' -w 'cat %IN > %OUT'
diff data/test_in.types data/test_out.types
./pardi -c 6 -i data/test_in.types -o data/test_out.types \
        -d 'r:^#atoms:' -w 'cat %IN > %OUT'
diff data/test_in.types data/test_out.types
