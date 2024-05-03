## Test environments
### Local
* Windows 11 Edu, R 4.4.0

### Remote
* R-oldrelease, and release with win-builder.r-project.org.

* 19/20 [rhub checks](https://github.com/CorradoLanera/CrossClustering/actions/runs/8927590108) passed (all but the [windows on R devel](https://github.com/CorradoLanera/CrossClustering/actions/runs/8927590108/job/24521327410#step:5:5339) one, because of a [known](https://github.com/gagolews/stringi/issues/508) setup issue w/ `{stringi}`):
  - [VM] linux          R-* (any version)                     ubuntu-latest on GitHub
  - [VM] macos          R-* (any version)                     macos-13 on GitHub
  - [VM] macos-arm64    R-* (any version)                     macos-latest on GitHub
  - [VM] windows        R-* (any version)                     windows-latest on GitHub
  - [CT] atlas          R-devel (2024-05-01 r86507)           Fedora Linux 38 (Container Image)
  - [CT] clang-asan     R-devel (2024-05-01 r86507)           Ubuntu 22.04.4 LTS
  - [CT] clang16        R-devel (2024-04-30 r86503)           Ubuntu 22.04.4 LTS
  - [CT] clang17        R-devel (2024-04-30 r86503)           Ubuntu 22.04.4 LTS
  - [CT] clang18        R-devel (2024-04-30 r86503)           Ubuntu 22.04.4 LTS
  - [CT] donttest       R-devel (2024-04-30 r86503)           Ubuntu 22.04.4 LTS
  - [CT] gcc13          R-devel (2024-05-01 r86507)           Fedora Linux 38 (Container Image)
  - [CT] intel          R-devel (2024-05-01 r86507)           Fedora Linux 38 (Container Image)
  - [CT] mkl            R-devel (2024-05-01 r86507)           Fedora Linux 38 (Container Image)
  - [CT] nold           R-devel (2024-05-01 r86507)           Ubuntu 22.04.4 LTS
  - [CT] nosuggests     R-devel (2024-05-01 r86507)           Fedora Linux 38 (Container Image)
  - [CT] ubuntu-clang   R-devel (2024-05-01 r86507)           Ubuntu 22.04.4 LTS
  - [CT] ubuntu-gcc12   R-devel (2024-05-01 r86507)           Ubuntu 22.04.4 LTS
  - [CT] ubuntu-next    R-4.4.0 (patched) (2024-04-30 r86507) Ubuntu 22.04.4 LTS
  - [CT] ubuntu-release R-4.4.0 (2024-04-24)                  Ubuntu 22.04.4 LTS
  - [CT] valgrind       R-devel (2024-05-01 r86507)           Fedora Linux 38 (Container Image)

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
