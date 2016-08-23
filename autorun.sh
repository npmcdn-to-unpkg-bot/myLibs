#!/bin/bash
for dir in bower/*; do (cd "$dir" && composer install); done
