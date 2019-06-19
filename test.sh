#!/bin/bash

set -x

rm -f data/decoys_std.smi
time (standardiser -i data/decoys.smi -o data/decoys_std.smi 2>&1) > /dev/null
wc -l data/decoys_std.smi

rm -f data/decoys_std_pardi.smi
time ./pardi -c 100 -i data/decoys.smi -o data/decoys_std_pardi.smi \
     -w '(standardiser -i %IN -o %OUT 2>&1) > /dev/null'
wc -l data/decoys_std_pardi.smi
