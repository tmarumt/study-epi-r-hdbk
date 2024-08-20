#!/bin/bash

# Install R packages
if [ -s renv.lock ]; then
  Rscript -e 'options(renv.config.external.libraries = "/usr/local/lib/R/site-library"); renv::restore()'
fi

# Install Python packages
if [ -s requirements.txt ]; then
  python3 -m pip install --user -r requirements.txt
fi

# Install Julia packages
if [ -s Project.toml ]; then
  julia -e 'using Pkg; Pkg.activate("."); Pkg.instantiate()'
fi
