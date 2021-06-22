#!/bin/sh
set -o errexit
Rscript --default-packages=testthat,renv -e "installed.packages()"
