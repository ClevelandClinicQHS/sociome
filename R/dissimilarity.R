
#' Append list columns of Gower's distances and sampling weights to a data frame
#'
#' Runs [cluster::daisy()] on a data frame, breaks up the columns of the
#' resulting dissimilarity into a list, and adds this list to the data frame as
#' a list column. In addition or instead, it adds a transformed version of the
#' dissimilarity list, which can be used as sampling weights.
#'
#' All columns are fed to [cluster::daisy()] by default, but the user can select
#' which ones using the `cols` argument.
#'
#' Once the full dissimilarity matrix is obtained, the columns are separated
#' into a list via [asplit()] and appended to `data`. Each element of the list
#' is therefore a [double] vector with [`nrow`]`(data)` values. For any given
#' row, its dissimilarity vector represents the row's dissimilarity to every
#' row.
#'
#' The optional/alternative "sampling weight" column is a transformed version of
#' the dissimilarity list: 1. All dissimilarity measures of 0 are replaced with
#' the next smallest dissimilarity value in the vector. In effect, this means
#' that a row's dissimilarity to itself (and any rows identical to it) is
#' replaced with the dissimilarity value of its next most similar row.
#' (Exception: if all elements are 0, all of them are replaced with 1). 2. Then
#' the reciprocal of each element is taken so that larger values represent
#' greater similarity. 3. Each element is divided by the sum of the vector,
#' which standardizes the elements to add to 1.
#' 
#' Requires the package `cluster` to be installed.
#'
#' @param data A data frame that has at least one row and at least one column.
#' @param cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns of `data` on
#'   which to calculate dissimilarities.
#' @param dissimilarity_measure_name,sampling_weight_name The names of the list
#'   columns that will be added to `data`. Cannot match the names of the
#'   existing columns. Make one of them `NULL` if you don't want it added, but
#'   they can't both be `NULL`.
#' @param metric,... Passed to [cluster::daisy()]. Use `...` at your own risk.
#'
#' @examples
#' # Running this on all mtcars columns
#' mtdissim <- append_dissimilarities(mtcars)
#' 
#' # Therefore, these numbers represent the dissimilarity of each row to the
#' # fifth row:
#' mtdissim$dissimilarities[[5]]
#' 
#' # And these are the dissimilarities' corresponding sampling weights:
#' mtdissim$sampling_weights[[5]]
#' 
#' # Now we run it on mtcars without the wt and qsec colums so that we purposely
#' # end up with some duplicate rows (the first and second).
#' mtdissim_dup <- append_dissimilarities(mtcars, cols = !c(wt, qsec))
#'
#' # These represent each row's dissimilarity to its first row.
#' # Since we specifically told it not to take wt and qsec into account, the
#' # first two rows are identical. Therefore, both values are zero.
#' mtdissim_dup$dissimilarities[[1]]
#' 
#' # And here are the corresponding sampling weights:
#' mtdissim_dup$sampling_weights[[1]]
#'
#' @return A data frame, specifically the `data` argument with one or two more
#'   columns added to the end.
#' @export
append_dissimilarities <- function(data,
                                   cols = dplyr::everything(),
                                   dissimilarity_measure_name =
                                     "dissimilarities",
                                   sampling_weight_name =
                                     "sampling_weights",
                                   metric = "gower",
                                   ...) {
  check_for_packages("cluster")
  if (!is.data.frame(data) || !nrow(data)) {
    stop("data must be a data frame with at least one row", call. = FALSE)
  }
  
  data2 <- dplyr::select(data, {{cols}})
  
  if (!ncol(data2)) {
    stop("cols must specifiy and at least one column in data", call. = FALSE)
  }
  
  if (anyNA(data2)) {
    warning("missing values detected among the columns in data specified by ",
            "cols", call. = FALSE)
  }
  
  validate_dissim_colnames(
    dissimilarity_measure_name,
    sampling_weight_name,
    data
  )
  
  dissim_measure <-
    cluster::daisy(x = data2, metric = metric, ...) %>% 
    as.matrix() %>% 
    unname() %>% 
    asplit(MARGIN = 2L) %>% 
    lapply(as.numeric)
  
  if (!is.null(dissimilarity_measure_name)) {
    data[[dissimilarity_measure_name]] <- dissim_measure
  }
  
  if (!is.null(sampling_weight_name)) {
    if (anyDuplicated(data2)) {
      warning("Duplicate row(s) detected among the columns of data specified ",
              "by cols.\nThese rows' results will be missing", call. = FALSE)
    }
    data[[sampling_weight_name]] <- lapply(dissim_measure, dissim_samp_wts)
  }
  
  data
}


dissim_samp_wts <- function(x, i) {
  zeros <- x == 0
  if (all(zeros)) {
    x[] <- 1
  } else {
    x[zeros] <- min(x[!zeros])
    x <- 1 / x
  }
  
  x / sum(x)
}


