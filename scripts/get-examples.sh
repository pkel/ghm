#!/bin/bash

svn export https://github.com/janestreet/incr_dom/trunk/example --force
svn export https://github.com/janestreet/incr_dom_widgets/trunk/example --force
echo "*" > example/.gitignore
