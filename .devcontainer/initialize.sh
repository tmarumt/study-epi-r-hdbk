#!/bin/bash

volumes=("renv" "pip" "julia" "TinyTeX" "fonts")

for vol in ${volumes[@]}; do
  if [ ! "$(docker volume ls -q -f name="$vol")" ]; then
    docker volume create $vol
  fi
done
