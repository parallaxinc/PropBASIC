#!/bin/bash

PBASFILE=$1

if [ -z $PBASFILE ] ; then
    echo "Error: Must pass a file name!"
    exit 1
fi

propbasic ./$PBASFILE

if [ $? -ne 0 ] ; then
    echo "Error: Failed to compile PropBASIC code"
    exit 1
fi

SPINFILE=${PBASFILE%.pbas}.spin
BINFILE=`echo $SPINFILE | sed -e 's/.spin$//'`

bstc -b $SPINFILE -o $BINFILE

if [ $? -ne 0 ] ; then
    echo "Error: Failed to compile Spin code"
    exit 1
fi
