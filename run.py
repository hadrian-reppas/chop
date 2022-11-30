import os
import sys


def run(command):
    code = os.system(command)
    if code != 0:
        exit(code)


run(f"cargo run -- {sys.argv[1]}")
run("clang out.c -o out")
run(f"./out {' '.join(sys.argv[2:])}")
