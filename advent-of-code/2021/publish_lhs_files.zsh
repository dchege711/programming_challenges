#!/bin/zsh

working_dir="$(dirname $0)"
printf "Looking for .LHS files in $working_dir\n"

lhs_files=($working_dir/**/*.lhs)

for f in $lhs_files ; do
    output_file=${f/.lhs/.md} # Replace .lhs with .md
    lhs2tex --markdown src/Lanternfish/Lanternfish.lhs | sed '1,2d' > output_file
    printf "Published $output_file\n"
done
