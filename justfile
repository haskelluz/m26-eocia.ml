build file:
    gcc -o {{file}} {{file}}.s

build-rt file:
    gcc -o {{file}} {{file}}.s runtime.c

run file: (build file)
    ./{{file}}; echo "exit: $?"

run-rt file: (build-rt file)
    ./{{file}}

clean file:
    rm -f {{file}} {{file}}.o
