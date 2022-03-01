#!/bin/zsh

working_dir="$(dirname $0)"
lhs_files=($working_dir/**/*.lhs)

for lhs_file in $lhs_files ; do
    output_file=${lhs_file/.lhs/.md} # Replace .lhs with .md
    lhs2tex --markdown $lhs_file | sed '1,2d' > $output_file # Delete first 2 lines
    printf "Published $output_file\n"
done
