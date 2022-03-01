#!/bin/zsh

working_dir="$(dirname $0)"
lhs_files=($working_dir/**/*.lhs)

for lhs_file in $lhs_files ; do
    output_file=${lhs_file/.lhs/.md} # Replace .lhs with .md
    # Delete the first 2 lines which only have a `%` character.
    # The 2nd expression replaces `\#` with `#`. The former is needed for .lhs
    # to compile. As an SO user said, when in doubt, add another backslash.
    lhs2tex --markdown $lhs_file | sed -e '1,2d' -e s.^\\\\\#.\#. > $output_file
    printf "Published $output_file\n"
done
