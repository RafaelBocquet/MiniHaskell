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
    testfile=`basename $test .hs`
    timeout 1s ./dist/build/MiniHaskell/MiniHaskell $test Base.hs > test-out/$testfile.s 2> /dev/null;
    if [ $? -eq 0 ];
    then
        echo "Compilation ok : $test";
        echo "spim -file test-out/$testfile.s > test-out/$testfile.out 2> /dev/null";
        timeout 1s spim -ldata 10000000 -file test-out/$testfile.s > test-out/$testfile.out 2> /dev/null;
        if [ $? -eq 0 ]
        then
           tail -n +6 test-out/$testfile.out > test-out/$testfile.out2;
           echo "Run ok : $test";
           if diff test-out/$testfile.out2 tests/exec/$testfile.out > /dev/null;
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

