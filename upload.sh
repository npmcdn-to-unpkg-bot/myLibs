#!/bin/bash
find . -name .DS_Store -print0 | xargs -0 git rm -f --ignore-unmatch
git add -A
git commit -m "Add file"
git push origin master
git pull
