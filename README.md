  <!-- badges: start -->
  
  [![R-CMD-check](https://github.com/RSGInc/travelSurveyTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RSGInc/travelSurveyTools/actions/workflows/R-CMD-check.yaml)
  
  <!-- badges: end -->

## Travel Survey Tools
`travelSurveyTools` is an R package that empowers users of household travel survey (HTS) data to create meaningful summaries of their data. Currently, `travelSurveyTools` is compatible with RSG, Inc. HTS datasets, but we hope to expand to any travel survey. If you would like to collaborate, please contact Erika Redding at [erika.redding@rsginc.com](mailto:erika.redding@rsginc.com?subject=TravelSurveyTools). These datasets usually contain six tables: household, person, day, trip, vehicle, and location. In the future, we may expand `travelSurveyTools` to work with other types of travel survey data or other types of surveys.

`travelSurveyTools` is in active development and is open-source; anyone can contribute 🤝. See the [CONTRIBUTING](CONTRIBUTING.md) page to learn how.

### Cloning instructions

1. Set config - 
`usethis::use_git_config(user.name = {"username"}, user.email = {your_email@email.com})`

2. Go to github page to generate token - 
`usethis::create_github_token()`

3. Paste your PAT into pop-up that follows - 
`credentials::set_github_pat()`

4. Lastly, `remotes::install_github()` will work - 
`remotes::install_github('RSGInc/travelSurveyTools')`


### What can this package do?

Some of the things this package enables include:

* Cross tabs with an unlimited number of variables 
* Summarizes numeric, categorical, date, and date-time variables
* Accepts both weighted and unweighted data
* Numeric summaries return means, medians, and summaries with binned data
* Ability to use customized datasets (e.g., filtered, binned, renamed data)
* Data labeling helper functions
* Returns sample sizes/unweighted counts
* Standard errors calculated with survey statistics
* Ability to specify custom weights
* Trip rate calculations

### Dependency Tree
```
├─dplyr 1.1.4
│ ├─cli 3.6.2
│ ├─generics 0.1.3
│ ├─glue 1.6.2
│ ├─lifecycle 1.0.4
│ │ ├─cli
│ │ ├─glue
│ │ └─rlang 1.1.2
│ ├─magrittr 2.0.3
│ ├─pillar 1.9.0
│ │ ├─cli
│ │ ├─fansi 1.0.6
│ │ ├─glue
│ │ ├─lifecycle
│ │ ├─rlang
│ │ ├─utf8 1.2.4
│ │ └─vctrs 0.6.5
│ │   ├─cli
│ │   ├─glue
│ │   ├─lifecycle
│ │   └─rlang
│ ├─R6 2.5.1
│ ├─rlang
│ ├─tibble 3.2.1
│ │ ├─fansi
│ │ ├─lifecycle
│ │ ├─magrittr
│ │ ├─pillar
│ │ ├─pkgconfig 2.0.3
│ │ ├─rlang
│ │ └─vctrs
│ ├─tidyselect 1.2.0
│ │ ├─cli
│ │ ├─glue
│ │ ├─lifecycle
│ │ ├─rlang
│ │ ├─vctrs
│ │ └─withr 2.5.2
│ └─vctrs
├─srvyr 1.2.0
│ ├─dplyr
│ ├─magrittr
│ ├─rlang
│ ├─survey 4.2-1
│ │ ├─Matrix 1.6-1.1 -> 1.6-4
│ │ │ └─lattice 0.21-9 -> 0.22-5
│ │ ├─survival 3.5-7 
│ │ │ └─Matrix
│ │ ├─lattice
│ │ ├─minqa 1.2.6
│ │ │ └─Rcpp 1.0.11
│ │ ├─numDeriv 2016.8-1.1
│ │ └─mitools 2.4
│ │   └─DBI 1.2.0
│ ├─tibble
│ ├─tidyr 1.3.0
│ │ ├─cli
│ │ ├─dplyr
│ │ ├─glue
│ │ ├─lifecycle
│ │ ├─magrittr
│ │ ├─purrr 1.0.2
│ │ │ ├─cli
│ │ │ ├─lifecycle
│ │ │ ├─magrittr
│ │ │ ├─rlang
│ │ │ └─vctrs
│ │ ├─rlang
│ │ ├─stringr 1.5.1
│ │ │ ├─cli
│ │ │ ├─glue
│ │ │ ├─lifecycle
│ │ │ ├─magrittr
│ │ │ ├─rlang
│ │ │ ├─stringi 1.8.3
│ │ │ └─vctrs
│ │ ├─tibble
│ │ ├─tidyselect
│ │ └─vctrs
│ ├─tidyselect
│ └─vctrs
└─stringr
```
