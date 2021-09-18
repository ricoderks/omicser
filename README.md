
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NDCN -Omics Browser (name TBC)

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Introduction

This package allows creation of a browser app for exploring a variety of
*post-QC* -omics data. Support for *transcript*-omics, *prote*-omics,
*metabol*-omics and *lipid*-omics has been tested.

The goal is to create a “playground” for exploring -omics data, with an
aim towards sharing. Primary development was initiated as part of the
NDCN Open Science initiative Data Science Pilot March-Sept 2021, with
hopes of formenting ongoing open-source developmnet.

There are two types of “users” mode for this package. First, a “curator”
scientist who must prepare their dataset for browsing and perform some
configuration so the app can access teh data. Second, in “browser” mode
the curated database(s) can be explored with the intention of developing
intuition about the data and developing hypothesis to pose for the next
steps of analysis.

\*Note: although the NDCN browser is not yet officially named, there are
references to the browser/package as `omicser` or `omxr`.

## Installing this package

``` r
devtools::install_github("ergonyc/omicser")
```

## Usage

Once installed two more steps are nescessary: 1. [Data
Curation](quickstart/03_data_curation.md) 2. [Browser
Configuration](quickstart/04_configuration.md)

The app is launched from R as:

``` r
    library(omicser)
    omicser::launchApp()
```

## Quickstart

A set of examples scripts and a quickstart guide are available in the
[`quickstart/`](quickstart/) directory of the repo. The [Quickstart
Guide](quickstart/README.md) will outline the flow from installation
through data exploration. i.e.

1.  Environment Setup: underlying tools/packages from R and python.
    [link](quickstart/01_environment_setup.md)
2.  Install: creates the browser and curation functions as an R package
    from github [link](quickstart/02_install.md)
3.  Data Curation: the *-omics* data curated into a *database*
    [link](quickstart/03_data_curation.md)
4.  Configuration: connecting the *-omics database* to the browser app
    [link](quickstart/04_configuration.md)
5.  Browsing: explore the data! [link](quickstart/05_browsing.md)
6.  Share: just a couple extra curation steps to share the data with
    others [link](quickstart/06_sharing.Rmd) The setup of the browser is
    5 steps:

## Background

The NDCN Browser (`Omicser`) was developing using
[`{golem}`](https://github.com/ThinkR-open/golem): “an opinionated
framework for building production-grade shiny applications.”

## ETC. Caveats

Several “big” updates are planned.

-   data table visualizaiton
-   QC report tab
-   Consistent `roxygen2` headers. Currently importing functions
    (e.g. `require(“package”)`) or using the `package::function` syntax
    is inconsistent.  
-   Migrate all dplyr:: tidyverse table manipulation to data.frame
    syntax and/or dtdplyr.  
    (test speed improvements on mac, with caveat of apple disabled
    multithreading)
-   migration to the NDCN github: `ndcn/omicser`.
