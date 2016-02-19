#!/usr/bin/sh

echo $REDO_TARGET $@ 1>&2

for dep in $@; do
	mkdir -p ".redo/$REDO_TARGET"
	md5sum $dep | awk '{print $1}'> ".redo/$REDO_TARGET/$dep" 
done