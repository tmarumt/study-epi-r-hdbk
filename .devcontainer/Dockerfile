FROM rocker/tidyverse

RUN apt-get update && apt-get install -y \
    openssh-client \
    gnupg

RUN /rocker_scripts/install_python.sh
RUN /rocker_scripts/install_julia.sh

RUN install2.r --error --skipinstalled -n -1 \
    renv \
    languageserver \
    httpgd
RUN installGithub.r ManuelHentschel/vscDebugger

RUN python3 -m pip install --no-cache-dir \
    radian \
    dvc

USER rstudio
WORKDIR /home/rstudio

RUN mkdir -p .cache/R/renv .cache/pip .julia .local/bin && \
    Rscript -e 'tinytex::install_tinytex()'

USER root

RUN python3 -m pip install --no-cache-dir dvc-gdrive
