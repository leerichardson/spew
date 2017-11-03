## Resubmission 
I am re-submitting, after incorporating comments by Swetlana Herbrandt and Uwe Ligges. Specifically:

* I updated the date field. 
* I replaced the 'snow' package with 'parallel' in the Suggests description. 
* I updated the title and description, which now provide more detail on what the package does (generally and specifically). I have also included a reference to the paper our package implements. 
* I added examples in all key functions users will call. 
* No errors were introduced after re-running R CMD check, travis-ci, and win-builder after making all of these changes. 

## Test Environments 
* local ubuntu 16.04, R 3.4.1
* ubuntu 14.04 (on travis-ci), R 3.4.2
* win-builder (devel and release)

## R CMD check results 
Locally and on Travis:
Status: OK

R CMD check results
0 errors | 0 warnings | 0 notes

1 note on win-builder:
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Lee Richardson <leerichardson2013@gmail.com>'

## Downstream dependencies
First submission. 
