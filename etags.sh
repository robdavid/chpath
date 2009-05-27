#!/bin/sh
find src -name \*.hs -not -name .\* | xargs hasktags -e
