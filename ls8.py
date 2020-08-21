#!/usr/bin/env python3

"""Main."""

import os
import sys
from cpu import *

# Check argument count
if len(sys.argv) != 2:
    print("Usage: ls8.py filename")

# Load the program into a list
try:
    program = []
    with open(sys.argv[1]) as f:
        for line in f:
            # Ignore empty lines and comments
            if len(line) > 0 and line[0] != '#' and line[0] != '\n':
                line = line.split('#')  # remove any trailing comments
                program.append(int(line[0], 2))  # append it as a base 2 int
except FileNotFoundError:
    print(f"File '{sys.argv[1]}' not found")

# Run the program on our CPU
cpu = CPU()
cpu.load(program)
cpu.run()