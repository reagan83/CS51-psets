#!/bin/bash -e

if test -f /usr/local/bin/submit; then
	rm -rf ps6-submit
	mkdir ps6-submit
	/usr/local/bin/submit lib51 3 `pwd`/ps6-submit
	cp -p streams.ml ps6-submit
	cp -p refs.ml ps6-submit
	cp -p music.ml ps6-submit
	/usr/local/bin/submit lib51 6 `pwd`/ps6-submit
	echo "Done!"
else
    echo "You must run this on one of the nice.fas machines"
fi
