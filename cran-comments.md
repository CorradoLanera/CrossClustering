# CrossClustering v4.1.1
## Test environments
### Local
* Windows 11 Edu, R 4.4.0

### Remote (rhub)

* 20/20 [rhub checks](https://github.com/CorradoLanera/CrossClustering/actions/runs/8941514230) passed:
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


R CMD check results (rhub)
0 errors | 0 warnings | 0 notes

R CMD check succeeded.

### Remote (win-builder)
* R [oldrelease](https://win-builder.r-project.org/5IeBFD74s03V),
  and [release](https://win-builder.r-project.org/8yWOrhyD9fhG)
  with win-builder.r-project.org.

R CMD check results (rhub)
0 errors | 0 warnings | 1 notes

> New submission
> 
> Package was archived on CRAN
> 
> Possibly misspelled words in DESCRIPTION:
>   CrossClustering (15:26)
>   Tellaroli (15:53)
>   al (15:66)
>   et (15:63)
> 
> CRAN repository db overrides:
>   X-CRAN-Comment: Archived on 2020-07-31 as required archived package
>     'assertive'.


## New submission (after package archived on CRAN)
This is a new/re-submission, after the package has been removed/archived
from CRAN.

In this updated version I have:

* Replace all `{assertive}`'s functions calls to `{checkmate}`'s ones.
* Replace `{magrittr}`'s pipe instances with native pipe (updating 
  Dependencies)
* Overall general lint of source code
* Update the documentation format
* Switch from Travis and AppVeyor to GitHub-actions for CMD-checks, lint, 
  and coverage.
* removed `.Random.seed`, after request
* exported `cc_test_ari`, `cc_test_ari_permutation`,
  `consensus_cluster`, and `is_zero`
  
> File 'R/CrossClustering-internal.R' sets .Random.seed.
>    This is usually neither needed nor wanted.
> 
> Please fix and resubmit.
