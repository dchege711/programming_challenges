#!/usr/bin/env python

from glob import glob
from os import path

num_files = 0
for filepath in glob("/Users/dchege711/blog/content/programming-challenges/**/*.md", recursive=True):
    
    original_lines = []
    with open(filepath, "r") as f:
        original_lines = f.readlines()

    if original_lines and original_lines[0].strip() == "---":
        print(f"Skipping {path.basename(filepath)}")
        continue

    with open(filepath, "w") as f:
        f.write("---\ndraft: true\n----\n\n")
        f.writelines(original_lines)

    num_files += 1
