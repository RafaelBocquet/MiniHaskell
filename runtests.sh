#!/bin/sh
# A Script to run tests
mkdir test-out -p
echo "SPIM Version 8.0 of January 8, 2010
Copyright 1990-2010, James R. Larus.
All Rights Reserved.
See the file README for a full copyright notice.
Loaded: /usr/lib/spim/exceptions.s" > test-out/header
for test in `ls tests/exec/*.hs`; do
    echo "$test : ";
    timeout 3s ./dist/build/POPHaskell/POPHaskell $test Base.hs > test-out/`basename -s.hs $test`.s 2> /dev/null;
    if [ $? -eq 0 ];
    then
        echo "Compilation ok : $test";
        echo "spim -file test-out/`basename -s.hs $test`.s > test-out/`basename -s.hs $test`.out 2> /dev/null";
        timeout 3s spim -file test-out/`basename -s.hs $test`.s > test-out/`basename -s.hs $test`.out 2> /dev/null;
        if [ $? -eq 0 ]
        then
           tail -n +6 test-out/`basename -s.hs $test`.out > test-out/`basename -s.hs $test`.out2;
           echo "Run ok : $test";
           if diff test-out/`basename -s.hs $test`.out2 tests/exec/`basename -s.hs $test`.out > /dev/null;
           then
               echo "Test ok : $test";
           else
               echo "Test failed : $test";
           fi
        else
            echo "Run failed : $test";
        fi
    else
        echo "Compilation failed : $test"
    fi;
done;

