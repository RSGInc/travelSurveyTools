#' Summarize a categorical variable
#'
#' @param prepped_dt Dataset containing the summary variables and key columns in
#'  data.table format.
#' @param summarize_var Name of the categorical variable to summarize. Default is NULL.
#' @param summarize_by Name of the variable to summarize the summarize_var by.
#'  Default is NULL.
#' @param weighted Whether the data is weighted. Default is TRUE.
#' @param se Whether to calculate standard error. Default is FALSE.
#' @param wtname Name of the weight column to use. Default is NULL.
#' @param strataname  Name of strata name to bring in. Default is NULL.
#' @param checkbox_valname Name of the column with the checkbox value. Default is 'value'.
#'  Must be provided if summarize_var is a checkbox variable.
#' @param checkbox_yesval Value of checkbox_valname that indicates it was selected.
#'  Default is 1. Must be provided if summarize_var is a checkbox variable.
#' @param summarize_vartype String; one of either 'categorical' (when the
#'  variable being summarized is categorical) or 'checkbox' (when the variable being
#'  summarized is derived from a multiple response, aka select-all-that-apply question).
#' @param id_cols names of possible ids in prepped_dt to return unique counts of
#'
#' @importFrom srvyr survey_prop
#' @importFrom srvyr survey_total
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by_at
#'
#' @return List of unweighted and weighted categorical summaries including counts
#' and proportions.
#' @export
#'
#' @examples
#'
#' require(data.table)
#' require(stringr)
#' require(dplyr)
#' require(srvyr)
#' DT <- hts_prep_data(
#'   summarize_var = "age",
#'   variables_dt = variable_list,
#'   data = list(
#'     "hh" = hh,
#'     "person" = person,
#'     "day" = day,
#'     "trip" = trip,
#'     "vehicle" = vehicle
#'   )
#' )$cat
#' hts_summary_cat(
#'   prepped_dt = DT,
#'   summarize_var = "age",
#'   wtname = "person_weight"
#' )
#'
#' DT <- hts_prep_data(
#'   summarize_var = "race",
#'   summarize_by = "employment",
#'   variables_dt = variable_list,
#'   data = list(
#'     "hh" = hh,
#'     "person" = person,
#'     "day" = day,
#'     "trip" = trip,
#'     "vehicle" = vehicle
#'   )
#' )$cat
#' hts_summary_cat(
#'   prepped_dt = DT,
#'   summarize_var = "race",
#'   summarize_by = "employment",
#'   wtname = "person_weight",
#'   summarize_vartype = "checkbox"
#' )
#'
#' DT <- hts_prep_data(
#'   summarize_var = "employment",
#'   summarize_by = c("race", "income_detailed", "gender"),
#'   variables_dt = variable_list,
#'   data = list(
#'     "hh" = hh,
#'     "person" = person,
#'     "day" = day,
#'     "trip" = trip,
#'     "vehicle" = vehicle
#'   )
#' )$cat
#'
#' hts_summary_cat(
#'   prepped_dt = DT,
#'   summarize_var = "employment",
#'   summarize_by = c("race", "income_detailed", "gender"),
#'   wtname = "person_weight"
#' )$unwtd
hts_summary_cat <- function(prepped_dt,
                            summarize_var = NULL,
                            summarize_by = NULL,
                            weighted = TRUE,
                            se = FALSE,
                            wtname = NULL,
                            strataname = NULL,
                            checkbox_valname = "value",
                            checkbox_yesval = 1,
                            summarize_vartype = "categorical",
                            id_cols = c("hh_id", "person_id", "day_id", "trip_id", "vehicle_id")) {
  if (!weighted & se) {
    message("Standard errors require weighted data; setting se = FALSE.
            Set weighted = TRUE and specify a wtname if standard errors are desired.")

    se <- FALSE
  }

  if (!se & weighted & !is.null(strataname)) {
    message("Stratanames are only used in calculating standard errors; setting se = TRUE
            Set se = FALSE and remove the strataname if standard errors are not desired.")

    se <- TRUE
  }

  if (summarize_vartype != "checkbox") {
    checkbox_valname <- NULL
    checkbox_yesval <- NULL
  }

  groupbyvars <- c(
    summarize_by,
    summarize_var,
    checkbox_valname
  )

  groupbyvars <- groupbyvars[groupbyvars %in% names(prepped_dt)]

  present_ids <- intersect(names(prepped_dt), id_cols)

  ndt_ids <- prepped_dt[, present_ids, with = FALSE]

  ns_unwtd <- lapply(ndt_ids, function(x) uniqueN(x))

  id_idx <- which.max(ns_unwtd)

  id_name <- id_cols[id_idx]

  if (!is.null(checkbox_valname)) {
    if (is.null(summarize_by)) {
      if (weighted) {
        wtd_group_vars <- c(wtname, id_name)

        wtd_group_n <- prepped_dt[, .N, wtd_group_vars][, sum(get(wtname))]
      }
    } else {
      if (weighted) {
        wtd_group_vars <- c(wtname, id_name, summarize_by, checkbox_valname)

        wtd_group_n <- prepped_dt[, .N, wtd_group_vars][
          ,
          .(wtd_group_n = sum(get(wtname) * get(checkbox_valname))),
          summarize_by
        ]
      }
    }
  }


  unwtd_summary <- copy(prepped_dt)

  if (is.null(checkbox_valname)) {
    unwtd_summary <- unwtd_summary[, .(count = .N), keyby = groupbyvars]

    if (is.null(summarize_by)) {
      unwtd_summary[, prop := count / sum(count)]
    } else {
      unwtd_summary[, prop := count / sum(count), keyby = summarize_by]
    }
  } else {
    unwtd_summary <- unwtd_summary[, .(count = .N), keyby = groupbyvars]

    setnames(unwtd_summary, old = checkbox_valname, new = "checkbox_valname")

    unwtd_summary <- unwtd_summary[checkbox_valname == checkbox_yesval]
    unwtd_summary[, checkbox_valname := NULL]

    if (is.null(summarize_by)) {
      unwtd_group_n <- prepped_dt[, uniqueN(get(id_name))]

      unwtd_summary[, prop := count / unwtd_group_n]
    } else {
      unwtd_group_n <- prepped_dt[
        , .(unwtd_group_n = uniqueN(get(id_name))),
        summarize_by
      ]

      unwtd_summary <- merge(unwtd_summary, unwtd_group_n, by = summarize_by, all.x = TRUE)

      unwtd_summary[, prop := count / unwtd_group_n]

      unwtd_summary[, unwtd_group_n := NULL]
    }
  }

  if (!is.null(checkbox_valname)) {
    groupbyvars_unwtd <- groupbyvars[groupbyvars != checkbox_valname]

    setcolorder(unwtd_summary, c(groupbyvars_unwtd, "count", "prop"))
  } else {
    setcolorder(unwtd_summary, c(groupbyvars, "count", "prop"))
  }

  cat_summary_ls <- list()

  if (weighted) {
    if (se) {
      so <- hts_to_so(
        prepped_dt = prepped_dt,
        weighted = weighted,
        wtname = wtname,
        strataname = strataname
      )

      wtd_summary <-
        so |>
        group_by_at(unlist(groupbyvars)) |>
        summarize(
          count = length(get(summarize_var)),
          prop = srvyr::survey_prop(
            proportion = FALSE,
            vartype = "se"
          ),
          est = survey_total(vartype = "se")
        ) |>
        setDT()
    } else if (!se) {
      wtd_summary <-
        prepped_dt[
          , .(
            count = .N,
            est =
              sum(
                get(wtname)
              )
          ),
          groupbyvars
        ]

      if (is.null(summarize_by)) {
        wtd_summary[, prop := est / sum(est)]
      } else {
        wtd_summary[, prop := est / sum(est), summarize_by]
      }

      setcolorder(wtd_summary, c(groupbyvars, "count", "prop", "est"))
    }
  }

  # Only get "selected" rows:
  if (!is.null(checkbox_valname)) {
    if (checkbox_valname %in% groupbyvars) {
      is_checkbox <- TRUE

      if (weighted) {
        setnames(wtd_summary, old = checkbox_valname, new = "checkbox_valname")

        wtd_summary <- wtd_summary[checkbox_valname == checkbox_yesval]
        wtd_summary[, checkbox_valname := NULL]

        # recalculate prop without value == 0
        if (is.null(summarize_by)) {
          wtd_summary[, prop := est / wtd_group_n]
        } else {
          wtd_summary <- merge(wtd_summary, wtd_group_n, by = summarize_by, all.x = TRUE)

          wtd_summary[, prop := est / wtd_group_n]
        }
      }
    } else {
      stop("Only provide checkbox_valname and checkbox_yesval if summarize_var is
           a checkbox variable.")
    }
  } else {
    is_checkbox <- FALSE
  }


  # Skip reordering if var is a checkbox
  if (!is_checkbox) {
    if (is.null(summarize_by)) {
      unwtd_summary <- unwtd_summary[order(get(groupbyvars[1]))]
    } else {
      unwtd_summary <- unwtd_summary[order(
        get(groupbyvars[1]),
        get(groupbyvars[2])
      )]
    }
  }

  if (is_checkbox & !is.null(summarize_by)) {
    unwtd_summary <- unwtd_summary[order(
      get(groupbyvars[1]),
      get(groupbyvars[2])
    )]
  }

  cat_summary_ls[["unwtd"]] <- unwtd_summary[]

  if (weighted) {
    # Skip reordering if var is a checkbox
    if (!is_checkbox) {
      if (is.null(summarize_by)) {
        wtd_summary <- wtd_summary[order(get(groupbyvars[1]))]
      } else {
        wtd_summary <- wtd_summary[order(
          get(groupbyvars[1]),
          get(groupbyvars[2])
        )]
      }
    }

    if (is_checkbox & !is.null(summarize_by)) {
      wtd_summary <- wtd_summary[order(
        get(groupbyvars[1]),
        get(groupbyvars[2])
      )]
    }

    cat_summary_ls[["wtd"]] <- wtd_summary[]

    cat_summary_ls$weight_name <- wtname
  }

  return(cat_summary_ls)
}

## quiets concerns of R CMD check
utils::globalVariables(c("value_labels", "prop", "est", "count"))
