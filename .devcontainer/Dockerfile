FROM rocker/tidyverse

RUN apt-get update && apt-get install -y --no-install-recommends \
    openssh-client \
    gnupg2 \
    pipx

RUN install2.r --error --skipinstalled -n -1 \
    --repos https://manuelhentschel.r-universe.dev --repos getOption \
    renv \
    languageserver \
    httpgd \
    vscDebugger

RUN /rocker_scripts/install_python.sh && \
    /rocker_scripts/install_julia.sh

USER rstudio
WORKDIR /home/rstudio

RUN mkdir -p .cache/R/renv .cache/pip .julia .local/bin .cache/pydrive2fs && \
    Rscript -e 'tinytex::install_tinytex(force = TRUE)'

RUN python3 -m pip config set global.break-system-packages true && \
    pipx install --include-deps radian && \
    pipx install --include-deps dvc[gdrive]

USER root
