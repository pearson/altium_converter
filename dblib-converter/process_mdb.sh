#!/bin/bash

# Generates a KiCad schematic symbol library using an .mdb file as input.
# gen_kicad_lib.py requires a set of templates created with sch_converter.pas
# Altium script.

IFS=$'\n'
for table in $(mdb-tables -d ';' "$1" | tr ';' '\n'); do
    #filename=$(echo "$table.csv" | tr ' ' '_')
    libpath=${1//.mdb/}
    #dbname=$(echo "$libpath" | rev | cut -d '/' -f 1 | rev)
    filename="$table.csv"
    mkdir -p "$libpath"
    mdb-export "$1" "$table" > "$libpath/$filename"
    python3 ./gen_kicad_lib.py "$libpath/$filename"
done
