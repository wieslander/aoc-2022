#!/bin/sh
#
# init-day.sh - set up boilerplate for today's solution
#
# USAGE
#     ./init-day.sh [DAY]
#
#   Copy the solution template file to src/aoc_2022/day{DAY}.clj and use
#   the `aoc' command to download the input file for the given day to
#   resources/day{DAY}.txt.
#
#   If DAY is omitted, use today's date.
#
# DEPENDENCIES
#   Uses the `aoc' command from https://github.com/scarvalhojr/aoc-cli
#   to download input files.

if [ -z "$1" ]; then
  day=`date '+%d'`
else
  day="$1"
fi

# Copy template to solution file and update date in docstring
day_padded=`printf %02d $day`
solution_filename="src/aoc_2022/day${day_padded}.clj"
sed "s/<DAY>/${day_padded}/g" src/aoc_2022/template.clj.txt > "$solution_filename"

# Download today's input file
input_filename="resources/day${day_padded}.txt"
aoc -y 2022 -d "$day" -f "$input_filename" download

# Open the solution file and input file in vertical split
nvim "$solution_filename" "$input_filename" -O
