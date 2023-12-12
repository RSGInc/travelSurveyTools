## Travel Suvey Tools
`newPackage` is an R package that empowers users of household travel survey (HTS) data to create meaningful summaries of their data. Currently, `newPackage` is compatible with RSG, Inc. HTS datasets. These datasets usually contain six tables: household, person, day, trip, vehicle, and location. In the future, we may expand `newPackage` to work with other types of travel survey data or other types of surveys.

`newPackage` is in active development and is open-source; anyone can contribute 🤝. See the CONTRIBUTING page to learn how.

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
