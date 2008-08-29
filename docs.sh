#! /bin/sh
mkdir -p documentation
exec naturaldocs -o HTML documentation -i . -p NaturalDocs
