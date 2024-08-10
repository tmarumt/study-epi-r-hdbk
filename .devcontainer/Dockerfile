FROM rocker/geospatial:4.4.1

RUN apt-get update && apt-get install -y \
    openssh-client \
    gnupg \
    && rm -rf /var/lib/apt/lists/*

USER rstudio
WORKDIR /home/rstudio

RUN mkdir -p .cache/R/renv .cache/pip .julia .local/bin .cache/pydrive2fs && \
    Rscript -e 'tinytex::install_tinytex(force = TRUE)'

USER root

RUN install2.r --error --skipinstalled -n -1 \
    --repos https://manuelhentschel.r-universe.dev --repos getOption \
    renv \
    languageserver \
    httpgd \
    vscDebugger \
    && rm -rf /tmp/downloaded_packages \
    && strip /usr/local/lib/R/site-library/*/libs/*.so

RUN /rocker_scripts/install_julia.sh && \
    /rocker_scripts/install_python.sh && \
    python3 -m pip install --no-cache-dir \
    radian \
    dvc

RUN python3 -m pip install --no-cache-dir dvc-gdrive
