#' Prepare datasets for trip rate calculations
#'
#' @param summarize_by Name of the variable to summarize trip rates by. Default
#'  is NULL.
#' @param variables_dt List of variable locations and descriptions in data.table
#'  format.
#' @param trip_name Name of the trip dataset in hts_data.
#' @param day_name Name of the day dataset in hts_data.
#' @param ids name of unique identifier in each table in hts_data
#' @param wts name of weight column in each table in hts_data
#' @param remove_outliers Boolean whether or not to remove outliers from dataset.
#'  Default is TRUE.
#' @param threshold Threshold to define outliers. Default is 0.975.
#' @param weighted Whether the data is weighted. Default is TRUE.
#' @param hts_data List containing household, person, day, trip, and vehicle
#'  datasets in data.table format.
#'
#' @return List of binned number of trips with key columns and summarize by variable,
#' unbinned number of trips with key columns and summarize by variable, and a
#' breakdown of outliers if removed.
#' @export
#'
#' @examples
#'
#' require(data.table)
#' require(stringr)
#' hts_prep_triprate(
#'   variables_dt = variable_list,
#'   trip_name = "trip",
#'   day_name = "day",
#'   hts_data = list(
#'     "hh" = hh,
#'     "person" = person,
#'     "day" = day,
#'     "trip" = trip,
#'     "vehicle" = vehicle
#'   )
#' )
#' hts_prep_triprate(
#'   summarize_by = "age",
#'   variables_dt = variable_list,
#'   trip_name = "trip",
#'   day_name = "day",
#'   hts_data = list(
#'     "hh" = hh,
#'     "person" = person,
#'     "day" = day,
#'     "trip" = trip,
#'     "vehicle" = vehicle
#'   )
#' )
hts_prep_triprate = function(summarize_by = NULL,
                              variables_dt = variable_list,
                              trip_name = "trip",
                              day_name = "day",
                              ids = c("hh_id", "person_id", "day_id", "trip_id", "vehicle_id"),
                              wts = c("hh_weight", "person_weight", "day_weight", "trip_weight", "hh_weight"),
                              remove_outliers = TRUE,
                              threshold = 0.975,
                              weighted = TRUE,
                              hts_data) {
  # Check variable_list first
  variables_dt = hts_validate_variable_list(variables_dt, hts_data)

  tripdat = hts_data[[trip_name]]
  daydat = hts_data[[day_name]]

  trip_index = which(names(hts_data) == trip_name)
  day_index = which(names(hts_data) == day_name)

  # Get ids
  trip_id = ids[trip_index]
  day_id = ids[day_index]

  # Get weights
  trip_wt = wts[trip_index]
  day_wt = wts[day_index]

  tripratekeys = intersect(names(tripdat), ids[-trip_index])
  trip_subset_cols = intersect(names(tripdat), c(ids, wts))
  day_subset_cols = intersect(names(daydat), c(ids, wts))

  if (weighted & (!trip_wt %in% trip_subset_cols |
    !day_wt %in% day_subset_cols)) {
    stop("Trip/Day weight not found - are these data weighted?")
  }

  day_control = daydat[, day_subset_cols, with = FALSE]

  trip_control = merge(day_control,
    tripdat[, trip_subset_cols, with = FALSE],
    all.x = TRUE
  )

  if (length(summarize_by) == 0) {
    if (weighted) {
      triprate_dt = tripdat[, .(num_trips = sum(get(trip_wt))),
        by = tripratekeys
      ]
    }

    # FIXME: rename triprate_binned to num_trips?
    if (!weighted) {
      triprate_dt = tripdat[, .(num_trips = .N),
        by = tripratekeys
      ]
    }

    join_vars = names(triprate_dt)[names(triprate_dt) %in% names(day_control)]

    triprate_dt = merge(day_control,
      triprate_dt,
      all.x = TRUE,
      all.y = FALSE
    )

    # fill in with zeros for zero trips on a given day:
    triprate_dt[, `:=`(
      num_trips = nafill(num_trips, fill = 0)
    )]

    if (weighted) {
      # calculate trip rate
      triprate_dt[, trip_rate :=
        ifelse(num_trips == 0, 0, num_trips / get(day_wt))]

      triprate_dt[, num_trips := NULL]

      setnames(triprate_dt, "trip_rate", "num_trips")
    }
  }

  if (length(summarize_by) > 0) {
    byvar_dt = hts_prep_byvar(summarize_by,
      variables_dt = variables_dt,
      hts_data = hts_data,
      byvar_ids = ids,
      byvar_wts = wts
    )

    merge_cols = names(byvar_dt)[names(byvar_dt) %in% names(trip_control)]

    triprate_dt = merge(trip_control, byvar_dt, by = merge_cols)

    triprate_cols = intersect(names(triprate_dt), c(ids, wts))

    triprate_cols = triprate_cols[!triprate_cols %in% c(trip_id, trip_wt)]

    triprate_cols_all = c(triprate_cols, summarize_by)

    if (weighted) {
      triprate_dt = triprate_dt[, .(num_trips = sum(get(trip_wt))),
        by = triprate_cols_all
      ]
    }

    if (!weighted) {
      triprate_dt = triprate_dt[, .(num_trips = .N),
        by = triprate_cols_all
      ]
    }

    # fill in with zeros for zero trips on a given day:
    triprate_dt[, `:=`(
      num_trips = nafill(num_trips, fill = 0)
    )]

    # If one of the by-variables is in trip table, need to expand to
    # include all levels of the variable for every trip, and fill with zeros:
    if (trip_id %in% names(byvar_dt)) {
      # fill in with zeros for zero trips for a given level of xt_var using dcast:
      dcast_formula =
        paste0(
          paste0(triprate_cols, collapse = " + "),
          " ~ ",
          paste0(summarize_by, collapse = " + ")
        )

      triprate_cast = dcast(triprate_dt,
        dcast_formula,
        value.var = "num_trips",
        fill = 0
      )

      # Remove columns where NA levels of factors were generated during dcast:
      na_filled_cols = names(triprate_cast)[names(triprate_cast) %like% "_NA"]

      if (length(na_filled_cols) > 0) {
        triprate_cast[, c(na_filled_cols) := NULL]
      }

      # transform back to long format, with separate cols for weighted & unwt. trip rates:
      triprate_dt = data.table::melt(
        triprate_cast,
        id.vars = triprate_cols,
        value.name = "num_trips"
      )

      # Relabel xtab trip vars after melting:
      if (length(summarize_by) > 1) {
        triprate_dt[, c(summarize_by) := tstrsplit(variable, "_")]
        triprate_dt[, variable := NULL]
      }

      if (length(summarize_by) == 1) {
        setnames(triprate_dt, old = "variable", new = summarize_by)
      }

      triprate_dt = triprate_dt[]
    }

    if (weighted) {
      # calculate trip rate
      triprate_dt[, trip_rate :=
        ifelse(num_trips == 0, 0, num_trips / get(day_wt))]

      triprate_dt[, num_trips := NULL]

      setnames(triprate_dt, "trip_rate", "num_trips")
    }
  }

  # remove outliers
  if (remove_outliers) {
    out = hts_remove_outliers(triprate_dt,
      numvar = "num_trips",
      threshold = threshold
    )

    triprate_dt = out[["dt"]]

    outlier_table = out[["outlier_description"]]
  }

  # Bin trips:
  triprate_binned = hts_bin_var(
    prepped_dt = triprate_dt,
    numvar = "num_trips",
    nbins = 7
  )

  if (weighted) {
    setnames(triprate_dt, "num_trips", "num_trips_wtd", skip_absent = TRUE)
    setnames(triprate_binned, "num_trips", "num_trips_wtd", skip_absent = TRUE)
  } else {
    setnames(triprate_dt, "num_trips", "num_trips_unwtd", skip_absent = TRUE)
    setnames(triprate_binned, "num_trips", "num_trips_unwtd", skip_absent = TRUE)
  }
  prepped_dt_ls = list(
    "num" = triprate_dt,
    "cat" = triprate_binned
  )

  # Append outliers:
  if (remove_outliers) {
    prepped_dt_ls = list(
      "cat" = triprate_binned,
      "num" = triprate_dt,
      "outliers" = outlier_table
    )
  }


  return(prepped_dt_ls)
}

## quiets concerns of R CMD check
utils::globalVariables(c("trip_weight", "num_trips", "trip_rate", "day_weight"))
