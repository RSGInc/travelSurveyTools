#' Bin numeric variables
#'
#' @param prepped_dt Dataset containing variable to bin in data.table format
#' @param numvar Name of the numeric variable to bin
#' @param nbins Number of bins for variable. Defaults to 7.
#'
#' @return Inputted dataset with the specified variable binned in data.table format.
#' @export
#'
#'
#' @importFrom stats quantile
#'
#' @examples
#'
#' require(data.table)
#' require(stringr)
#' hts_bin_var(prepped_dt = trip, numvar = "speed_mph")
#'
hts_bin_var = function(
    prepped_dt,
    numvar,
    nbins = 7) {
  # TODO: Allow user to specify bins directly to cut
  prepped_dt_binned = data.table::copy(prepped_dt)

  data.table::setnames(prepped_dt_binned, old = numvar, new = "numvar")

  # Reclassify outliers:
  # TODO: Should this be a parameter.
  # TODO: Defaults should be described in description.
  min_prob = 0.05
  q_min = round(
    stats::quantile(
      x = prepped_dt_binned[, numvar],
      probs = min_prob / 2,
      na.rm = TRUE
    )
  )
  q_max = round(
    stats::quantile(
      x = prepped_dt_binned[, numvar],
      probs = 1 - min_prob / 2,
      na.rm = TRUE,
    )
  )

  prepped_dt_binned[, binned := ifelse(numvar >= q_max, q_max, numvar)]
  prepped_dt_binned[, binned := ifelse(numvar <= q_min, q_min, numvar)]


  # Create breaks:
  round_digits = 1 * (as.numeric(q_max - q_min) < 5)


  mid_breaks = seq(
    from = q_min,
    to = q_max,
    by = round((q_max - q_min) / (nbins - 2), round_digits)
  )

  min_break = ifelse(q_min == 0, -Inf, 0)

  all_breaks = c(
    min_break,
    mid_breaks,
    q_max,
    Inf
  )

  all_breaks = unique(all_breaks)

  prepped_dt_binned[, binned := cut(
    numvar,
    breaks = all_breaks,
    include.lowest = TRUE
  )]

  # Clean up bin labels:
  binlabels = levels(prepped_dt_binned$binned)
  binlabels = stringr::str_remove(string = binlabels, pattern = "[(]")
  binlabels = stringr::str_remove(string = binlabels, pattern = "[]]")
  binlabels = stringr::str_replace(
    string = binlabels,
    pattern = ",Inf|, Inf",
    replacement = " or more"
  )
  binlabels[[1]] =
    ifelse(q_min == 0,
      "Exactly 0",
      paste0(stringr::str_split_i(
        binlabels[[1]],
        i = 2, pattern = ","
      ), " or less")
    )

  # if ( prepped_dt_binned[binned %in% c('(-Inf,0]', '(-Inf, 0]'), .N] == 0 ){
  #   binlabels = binlabels[binlabels != 'Exactly 0']
  #
  #
  #   levels(prepped_dt_binned$binned) = levels(prepped_dt_binned$binned)[!levels(prepped_dt_binned$binned) %in% c('(-Inf,0]', '(-Inf, 0]')]
  #
  # }


  binlabels = stringr::str_replace(
    string = binlabels,
    pattern = ",",
    replacement = "-"
  )

  prepped_dt_binned[, binned := factor(binned,
    levels = levels(prepped_dt_binned$binned),
    labels = binlabels
  )]

  levels(prepped_dt_binned$binned) = c(
    levels(prepped_dt_binned$binned),
    "Missing"
  )

  prepped_dt_binned[is.na(binned), binned := "Missing"]

  # Return result:
  prepped_dt_binned[, numvar := NULL]
  data.table::setnames(prepped_dt_binned, old = "binned", new = numvar)

  return(prepped_dt_binned[])
}

## quiets concerns of R CMD check
utils::globalVariables(c("binned", "str_split_i"))
