#!/bin/bash

echo "Testing all tests"

test_dir=Tests

total=0
passed=0

for i in $test_dir/Test*.java;
do
    echo "=== Testing $i";
    ((total++))

    test_class=$(basename $i .java)
    test_letter=$(echo $test_class | cut -c5- -);

    supposed_output_file=$test_dir/out${test_letter}.txt
    our_output=$(java -cp classes:javassist.jar ist.meic.pa.KeyConstructors $test_class)

    res=$(diff -a <(echo "$our_output") $supposed_output_file)
    echo "$res"

    if [ "$res" == "" ]; then ((passed++)); fi
done

echo "Passed $passed/$total"
echo "Done."
