# CrossClustering v4.1.3

Relaxes the attribute check in a test case `correct output class` (within `tests/testthat/test-cc-crossclustering.R`) due to for the function used for the comparison (i.e. `testthat::expect_equal`) now "comparison of these types is not implemented". Anyway, all the attributes are already checked in the previous lines of the same test case. 

## Test environments
### Local
* Windows 11 Edu, R 4.5.2

### Remote (rhub v2)

Only rchk (container issue) and macos r-devel (which is retired) failed.
see https://github.com/CorradoLanera/CrossClustering/actions/runs/20835662551 for the complete record.

All the following has passed:

 1 [VM] linux          R-* (any version)                     ubuntu-latest on Git…
 2 [VM] m1-san         R-* (any version)                     macos-15 on GitHub, …
 3 [VM] macos          R-* (any version)                     macos-13 on GitHub
 4 [VM] macos-arm64    R-* (any version)                     macos-latest on GitH…
 5 [VM] windows        R-* (any version)                     windows-latest on Gi…
 6 [CT] atlas          R-devel (2026-01-07 r89288)           Fedora Linux 38 (Con…
 7 [CT] c23            R-devel (2026-01-06 r89281)           Ubuntu 22.04.5 LTS
 8 [CT] clang-asan     R-devel (2026-01-07 r89288)           Ubuntu 22.04.5 LTS
 9 [CT] clang-ubsan    R-devel (2026-01-07 r89288)           Ubuntu 22.04.5 LTS
10 [CT] clang16        R-devel (2026-01-06 r89281)           Ubuntu 22.04.5 LTS
11 [CT] clang17        R-devel (2026-01-06 r89281)           Ubuntu 22.04.5 LTS
12 [CT] clang18        R-devel (2026-01-06 r89281)           Ubuntu 22.04.5 LTS
13 [CT] clang19        R-devel (2026-01-06 r89281)           Ubuntu 22.04.5 LTS
14 [CT] clang20        R-devel (2026-01-07 r89288)           Ubuntu 22.04.5 LTS
15 [CT] donttest       R-devel (2026-01-06 r89281)           Ubuntu 22.04.5 LTS
16 [CT] gcc-asan       R-devel (2026-01-07 r89288)           Fedora Linux 40 (Con…
17 [CT] gcc13          R-devel (2026-01-07 r89288)           Fedora Linux 38 (Con…
18 [CT] gcc14          R-devel (2026-01-07 r89288)           Fedora Linux 40 (Con…
19 [CT] gcc15          R-devel (2026-01-07 r89288)           Fedora Linux 42 (Con…
20 [CT] intel          R-devel (2026-01-07 r89288)           Fedora Linux 38 (Con…
21 [CT] lto            R-4.5.2 (2025-10-31)                  Ubuntu 24.04.3 LTS
22 [CT] mkl            R-devel (2026-01-07 r89288)           Fedora Linux 38 (Con…
23 [CT] nold           R-devel (2026-01-07 r89288)           Ubuntu 22.04.5 LTS
24 [CT] noremap        R-devel (2026-01-07 r89288)           Ubuntu 22.04.5 LTS
25 [CT] nosuggests     R-devel (2026-01-07 r89288)           Fedora Linux 38 (Con…
27 [CT] ubuntu-clang   R-devel (2026-01-07 r89288)           Ubuntu 22.04.5 LTS
28 [CT] ubuntu-gcc12   R-devel (2026-01-07 r89288)           Ubuntu 22.04.5 LTS
29 [CT] ubuntu-next    R-4.5.2 (patched) (2026-01-07 r89288) Ubuntu 24.04.3 LTS
30 [CT] ubuntu-release R-4.5.2 (2025-10-31)                  Ubuntu 24.04.3 LTS
31 [CT] valgrind       R-devel (2026-01-07 r89288)           Fedora Linux 38 (Con…

R CMD check results (rhub)
0 errors | 0 warnings | 0 notes

R CMD check succeeded.

# CrossClustering v4.1.1
## Test environments
### Local
* Windows 11 Edu, R 4.4.0

### Remote (rhub)

* 20/20 [rhub checks](https://github.com/CorradoLanera/CrossClustering/actions/runs/9065559066) passed:
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
* R [oldrelease](https://win-builder.r-project.org/bAVBF38BNXJK),
  and [release](https://win-builder.r-project.org/0nXlf3Hm2Uh8)
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
  `consensus_cluster`, and `is_zero` documented functions
* put single quotes on package description for the package name.

Note: We deliberately keep 'CrossClustering' in CammelCase style as
      package name for historical reasons.

### First CRAN request
> File 'R/CrossClustering-internal.R' sets .Random.seed.
>    This is usually neither needed nor wanted.
> 
> Please fix and resubmit.

### Second CRAN request

> Please always write package names, software names and API (application
> programming interface) names in single quotes in title and description.
> e.g: --> 'CrossClustering'
> Please note that package names are case sensitive.
> 
> "Using foo:::f instead of foo::f allows access to unexported objects.
> This is generally not recommended, as the semantics of unexported
> objects may be changed by the package author in routine maintenance."
> 
> Please omit one colon.
> 
> -> Warning: Used ::: in documentation:
>       man/cc_test_ari_permutation.Rd:
>          CrossClustering:::cc_test_ari_permutation(ground_truth, clusters)
>       man/cc_test_ari.Rd:
>          CrossClustering:::cc_test_ari(ground_truth, clusters)
>       man/consensus_cluster.Rd:
>          CrossClustering:::consensus_cluster(c(3, 4), cluster_ward,
> cluster_other)
>       man/is_zero.Rd:
>          CrossClustering:::is_zero(1)
>       man/is_zero.Rd:
>          CrossClustering:::is_zero(0)
> 
> Please fix and resubmit.
