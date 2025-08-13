#!/bin/bash

if [ ! -d tmp ]; then
    mkdir tmp
fi

for i in cbmbasic1 cbmbasic2 kbdbasic osi kb9 applesoft microtan aim65 sym1 eater; do
    echo "Processing target: $i"
    
    # Compile with ca65
    ca65 -D $i msbasic.s -o tmp/$i.o --feature labels_without_colons --listing tmp/$i.lst -g
    if [ $? -ne 0 ]; then
        echo "Error compiling $i with ca65."
        continue
    fi
    
    # Link with ld65
    ld65 -o tmp/$i.bin tmp/$i.o -C $i.cfg --mapfile tmp/$i.map -vm -Ln tmp/$i.lbl --dbgfile tmp/$i.dbg
    if [ $? -ne 0 ]; then
        echo "Error linking $i with ld65."
        continue
    fi
    
    echo "Successfully processed $i"
done
