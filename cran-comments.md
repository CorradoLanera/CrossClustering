## Test environments
* local win 11 edu, R 4.4.0

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

## Re-submission
This is a re-submission, after the package has been removed from CRAN.
In this version I have:

* Replace all `{assertive}`'s functions calls to `{checkmate}`'s ones.
* Replace `{magrittr}`'s pipe instances with native pipe (updating 
  Dependencies)
* Overall general lint of source code
* Update the documentation format
* Switch from Travis and AppVeyor to GH-actions for CMD-checks, lint, 
  and coverage.
