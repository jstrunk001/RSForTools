# Before You Start
You will likely need Rtools for this package's installation. Make sure you match the version of rtools to the version of R that you have. The latest version of rtools does not work for R installations before version 4.0! If you have R 3.5.?? look for a matching older version of rtools. It may help if you update to the latest version of R,RStudio and update all of your existing packages (fun, I know).

**Windows**: [Rtools.exe](https://cran.r-project.org/bin/windows/Rtools/). 

# RSForTools automatic install
Install the latest version of RSForTools from github with devtools. You may have to manually install a few packages that aren't brought in by the package for some reason. Recently R has become picky when the local version of R does not match the version of a package on CRAN. Lubridate, for example, was a package that wouldn't import automajically when loading the package for me. 

You will need the remotes (or devtools) package to install from github.

```r
install.packages("devtools")
install.packages("remotes)
#install.packages("lubridate")
#install.packages("lubridate",type="source")

#this package is not on CRAN and needs manual install
install.packages("greenbrown", repos="http://R-Forge.R-project.org")

remotes::install_github("jstrunk001/RSForTools")
```

# RSForTools manual install

Download this git repository to a local zip file, then rename the downloaded zip archive from "RSForTools-master.zip" -> "RSForTools.zip", and then use the R remotes::install_local to install from .zip package file. 

```r
install.packages("remotes)
#your path will vary here!
remotes::install_local("c:\\temp\\RSForTools.zip")

```
