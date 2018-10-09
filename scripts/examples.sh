#!/bin/bash

for d in example/* ; do
  if [ -d "$d" ] ; then
    dune build "$d/"{index.html,main.bc.js}
  fi
done
