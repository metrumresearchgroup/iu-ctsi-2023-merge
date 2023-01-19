# acop13-merge-demo
MeRGE demonstration for ACoP 13 pre-conference workshop in Aurora, CO 30 October 2022.

This workshop focused on the packages that are showcased in the [MeRGE Expo 1 website](https://merge.metrumrg.com/expo/expo1-nonmem-foce/) and the code in this repo is a simplified version of code maintained in the [MeRGE Expo 1 GitHub Repo](https://github.com/metrumresearchgroup/expo1-nonmem-foce/).

## Directory listing

~~~
   /model = the NONMEM-formatted model files (.ctl)

   /script = the scripts that were demonstrated during the workshop
   
   /data = a simulated data file to use as example "observed" data
   
   /presentation = a pdf file with the hands-on slides for the workshop
~~~

## Installing packages with `renv::restore()`

Packages have been specified in the _renv.lock_ file and should be accessible by first running in your RStudio Console window:

~~~ 

install.packages("renv")
library(renv)
renv::restore()  

~~~

Users should respond "Yes" when asked to activate the project after calling `renv::restore()`. The packages will then begin installing. (This may take awhile.)

For further information on using renv, please see: https://rstudio.github.io/renv/articles/renv.html#reproducibility

## Installing packages with `pkgr`

This repository also includes a `pkgr.yml` file with all relevant packages. If you have `pkgr` installed, you can follow the steps below to install your packages. This will go faster than the `renv::restore()` method, because `pkgr` uses multi-threading and caching. (If you are on Metworx, you will have `pkgr` installed.) 

**In your R console**

~~~ 

install.packages("renv")
renv::init(bare = TRUE)  

~~~

**In your terminal**

~~~
# cd to this repo directory
pkgr install
~~~

Once `pkgr` finishes installing, restart your R session.
