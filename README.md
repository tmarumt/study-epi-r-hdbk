## Prerequisites

- Docker on WSL
- VS Code with Dev Containers

## Background

1. This repository was generated from the template repository and cloned
2. Docker image was built, and container was started by opening this repository cloned in VS Code with Dev Containers
3. R project would have been prepared with RStudio on `localhost:8787` if `.Rproj` file exists
4. R package management with renv would have been prepared by executing `renv::init(settings = list(external.libraries = "/usr/local/lib/R/site-library"))` in R REPL if `renv.lock` file exists
5. Julia environment with some package installed would have been prepared by executing `pkg> activate .; add <Package name>` or `julia> using Pkg; Pkg.activate("."); Pkg.add("<Package name>")` in Julia REPL if `Project.toml` file exists
6. If `.dvc` directory exists, DVC and remote storage (e.g., Google Drive) would have been prepared as below:
   1. Prepare Google Drive folder, Google Cloud project for Google Drive API to connect to it and its OAuth credentials
   2. Execute `dvc init && dvc remote add -d myremote gdrive://<Google Drive folder ID>` in shell
   3. Also execute `dvc remote modify --local myremote gdrive_client_id '<Client ID>' && dvc remote modify --local myremote gdrive_client_secret '<Client secret>'` in shell
   4. Share the Google Drive folder with the collaborators as needed

## Getting Started

1. Clone this repository
2. Open this repository cloned in VS Code with Dev Containers so that Docker image is built, container is started, and the following process is performed automatically:
   - Install R packages if `renv.lock` file exists
   - Install Python packages if `requirements.txt` file exists
   - Install Julia packages if `Project.toml` file exists
3. Download data from the remote storage by executing `dvc pull` in shell after preparing the credentials in the same manner as the initiator as needed if `.dvc` directory exists
4. Open R project with RStudio on `localhost:8787` if `.Rproj` file exists (Optional)

## Workflow

1. Develop, conduct analysis and report
2. Execute `renv::snapshot()` in R REPL to record installed R packages as appropriate (at least before pushing your commits)
3. Execute `pip freeze --user > requirements.txt` in shell to record installed Python packages as appropriate (at least before pushing your commits)
4. Manage source codes, outputs, reports and data with Git and DVC (as well as remote storage)

Note that each installed Julia package will automatically be recorded in `Project.toml` file whenever to be installed if Julia environment is activated by executing `pkg> activate .` or `julia> using Pkg; Pkg.activate(".")` in Julia REPL.
