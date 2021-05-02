#!/bin/sh
if [ -e .git ]
then
	git describe --abbrev=1 --dirty=+ --long
	exit 0
fi
if [ ! -z "$VERSION" ]
then
	echo $VERSION
	exit 0
fi
echo '>0.6.12'

