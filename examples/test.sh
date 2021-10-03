#!/bin/sh

EXAMPLES_DIR="$(dirname "$0")"

compile () {
    FILENAME="$EXAMPLES_DIR/$1.crp"
    shift
    echo crpc "$FILENAME" "$@"
    crpc "$FILENAME" "$@"
}

nonterminating () {
    echo timeout "$@"
    timeout "$@"
}

terminating () {
    echo "./$1"
    "./$1"
    echo "status code $?"
}

if ! [ $CHIRP_TEST_NO_RECOMPILE ]; then
    for n in genrec mutrec factrecur ackermann io recursive_type hello_world \
                    cons wave cat branches calculator ffi lambda globalroots float_arithmetic \
                    factiter fizz_buzz church printf
    do
        compile $n
    done

    cc -c "$EXAMPLES_DIR/gctest.c" -o "gctest.o"
    compile gctest -l "gctest.o"
fi

for strategy in Noop MarkAndSweep Generational
do
    export CHIRP_COLLECTOR_STRATEGY="$strategy"

    echo "Collector: $CHIRP_COLLECTOR_STRATEGY"

    for n in mutrec factrecur ackermann io recursive_type hello_world \
                cons wave branches ffi lambda globalroots float_arithmetic \
                factiter church printf
    do
        terminating $n
    done

    echo "catting!" | terminating cat
    echo "1\n2\n+" | terminating calculator
    nonterminating 0.1 ./genrec
    terminating fizz_buzz | tee /dev/null | head
    terminating gctest

done
