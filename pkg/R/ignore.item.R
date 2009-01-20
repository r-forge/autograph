# ================================================================================
# ignore_item
# ================================================================================
# ignore_item takes values yes or no, and is for the "current question" only
#
ignore_item <- function (x)
{
  graphattr(x, "!skip")
}

is.applicable <- function (x)
{
  x <- recode(x, apply.na=TRUE, napp="NaN")
  not.applicable <- (x=="NaN" | is.nan(x))
  not.applicable[is.na(not.applicable)] <- FALSE
  return(!not.applicable)
}

is.observed <- function (x)
{
  x <- recode(x, apply.na=TRUE, napp=NA)
  return(!is.na(x))
}

applicable <- function (x)
{
  # non-applicable==NaN, unobserved but applicable==NA
  # a non-applicable data point is by default unobserved, but actually
  # observablility does not apply to non-applicable data!
  att <- attributes(x)
  x <- x[ is.applicable(x) ]
  attributes(x) <- att
  return(x)
}

ag_prop_applicable <- function (x)
{
  mean(is.applicable(x))
}

ag_resp.prop.applicable <- function (x)
{
  mean(is.observed(applicable(x)))
}
