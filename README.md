  <!-- badges: start -->
  
  [![R-CMD-check](https://github.com/RSGInc/travelSurveyTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RSGInc/travelSurveyTools/actions/workflows/R-CMD-check.yaml)
  
  <!-- badges: end -->

## Travel Survey Tools
`travelSurveyTools` is an R package that empowers users of household travel survey (HTS) data to create meaningful summaries of their data. Currently, `travelSurveyTools` is compatible with HTS datasets from [RSG, Inc.](https://rsginc.com/), but we hope to expand to any travel survey. If you would like to collaborate, please contact Erika Redding at [erika.redding@rsginc.com](mailto:erika.redding@rsginc.com?subject=TravelSurveyTools). These datasets usually contain six tables: household, person, day, trip, vehicle, and location. In the future, we may expand `travelSurveyTools` to work with other types of travel survey data or other types of surveys.

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

## Creating the prepared data list

```mermaid

flowchart TD
    A(hts_prep_variable) --> B{hts_validate_variable_list}
    B --> C{remove_missing}
    C --> |TRUE| CA(hts_remove_missing_data)
    C --> |FALSE| D{hts_find_var}
    CA --> D{hts_find_var}
    D --> E(var_is_shared)
    E --> |TRUE| EA(hts_melt_vars)
    E --> |FALSE| F(summarize_var)
    EA --> F(summarize_var)
    F --> |numeric| FA(remove_outliers)
    F --> |categorical| G("summarize_by > 0")
    FA --> FB(hts_bin_var)
    FB --> G("summarize_by > 0")
    G --> |TRUE| GA(hts_find_var)
    GA --> GB(hts_prep_byvar)
    G --> H{"!is.null(strataname)"}
    GB --> H{"!is.null(strataname)"}
    H --> |TRUE| HA(hts_cbind_var)
    H --> |FALSE| I((prepped_dt_ls))
    HA --> I



```

## Creating the summary

```mermaid
flowchart TD
    A((prepped_dt_ls))
    A --> B(hts_get_ns)
    B --> |categorical| BB(hts_summary_cat)
    B --> |numeric| CB(hts_summary_num)
    BB --> BC{se}
    BC --> |TRUE| BCA(hts_to_so)
    BCA --> BD((cat_summary_ls))
    BC --> |FALSE| BD
    CB --> CC{weighted}
    CC --> |TRUE| CCA(hts_to_so)
    CCA --> CD((num_summary_ls))
    CC --> |FALSE| CD
```