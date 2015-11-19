## Resubmission
This is a resubmission. In this version I have:

* Previous submissions was successful, but doesn't work with R-oldrelease (R-3.1.3). I fixed the incorrect test which was causing R CMD check to fail.

## Test environments
* local OS X install, R 3.2.2
* ubuntu 12.04 (on travis-ci), R 3.2.2
* win-builder (devel, release and oldrel)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Roman Tsegelskyi <roman.tsegelskyi@gmail.com>’
New submission

## Downstream dependencies
This is a new package so it doesn't have downstream dependencies yet.
