# Introduction

This document describes how the docker container was created and how to use it.

# Dockerfile

The `Dockerfile` was created with `golem::add_dockerfile()`. This generates a `Dockerfile` with install commands for (almost) all R packages. The generated `Dockerfile` was used as basis to create the final `Dockerfile`.

## Python environment

To create the python environment a small R script was developed (based on installation instruction of `omicser`). This file is copied into the container and executed during build of the container.

To make sure the correct python environment is used, a `.Renviron` file was created which uses an environment from `reticulate` to force the correct python environment. The `.Renviron` file is copied and copied to the corrrect location during build of the container.

## App configuration

The `omicser` app needs a config file (`app_config.yml`) to run. For now a config file is already prepared and copied into the container and copied to the correct location.

# Databases

The current setup leaves the database files outside the container. During startup of the container the location of the database files need to be coupled to the database folder inside the container.

# Usage

## Build docker container

The location of the `Dockerfile` needs to be the root of your project folder. Build the container with:

```
docker build --tag omicser_container .
```

## Run the docker container

Run the container as follows:

```
docker run -d -p 3939:3939 -v /my_location/of/databases/:/root/DB/ omicser_container
```
