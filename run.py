import os
import sys

if os.system(f"cargo run -- {sys.argv[1]}") == 0:
    if os.system("rustc out.rs -o out") == 0:
        os.system(f"./out {' '.join(sys.argv[2:])}")
