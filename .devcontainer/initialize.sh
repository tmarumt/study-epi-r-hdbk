#!/bin/bash

volumes=("renv" "pip" "julia" "TinyTeX" "fonts" "pydrive2fs")

for vol in ${volumes[@]}; do
  if [ -z "$(docker volume ls -q -f name="$vol")" ]; then
    docker volume create $vol
  fi
done

configurations=("user.name" "user.email")

for config in ${configurations[@]}; do
  cur_config="$(git config $config)"
  global_config="$(git config --global $config)"
  local_config="$(git config --local $config)"

  if [ -z "$local_config" ] && [ -n "$cur_config" ] && [ "$cur_config" != "$global_config" ]; then
    git config --local $config "$cur_config"
  fi
done
