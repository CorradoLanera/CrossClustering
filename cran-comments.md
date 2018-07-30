## Test environments
* local win 10 pro, R 3.5.1

* ubuntu 14.04.5 LTS, and macOS Sierra 10.12.6 (on travis-ci);
    both on R 3.4.4, R 3.5.0, and R under development (2018-06-20 r74923)
    (NOTE: R devel, only under osx, throwed a warning due to a problem 
           we were not able to manage with git2r configuration. see:
           <https://travis-ci.org/CorradoLanera/CrossClustering/jobs/409315972>,
           hence `warnings_are_errors` were set as `false` for that build)

* win-builder (on appveyor) R devel and release


## R CMD check results
R CMD check results
0 errors | 0 warnings | 0 notes

R CMD check succeeded.


## Downstream dependencies
* devtools::revdep_check() results in no errors


## Resubmission
This is a resubmission. In this version I have:

* Removed the package name from the title.
