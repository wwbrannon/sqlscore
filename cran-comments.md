Resubmitting as per CRAN maintainers without backward compatibility code for dplyr 0.5.0.

## Test environments
* ubuntu 12.04.5 LTS (R-oldrel, R-release and R-devel) via travis-ci
* Mac OSX 10.11 (R-oldrel and R-release) via travis-ci
* Solaris 11 (R-oldrel) via VirtualBox
* win-builder (R-release and R-devel)

## R CMD check results
There were no ERRORs or WARNINGs.

While testing locally with the new versions of dbplyr/dplyr, there
was the following NOTE:

checking dependencies in R code ... NOTE
Missing or unexported objects:
  ‘dplyr::build_sql’ ‘dplyr::translate_sql’

A similar NOTE occurs if using dplyr 0.5.0. Both are unavoidable, because
they occur in backward-compatibility code that wraps around the change in
API from dplyr 0.5.0 to the current API.

## Downstream dependencies
There are currently no downstream dependencies for this package.
