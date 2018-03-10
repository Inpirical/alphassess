# Core Functions and Operators =================================================


#' Evaluate an alpha function given data (xts) and constant arguments.
#' Extracts relevant data from data argument and returns xts alpha results.
#' @param constants (numeric) vector of constants to be passed to alpha fun.
#' @param dat (named list) each entry an xts matrix (e.g. cl= closing prices,
#'   vol=volumes).
#' @param alpha_fun (function) the alpha function to be evaluated.
#' @return (xts matrix) alpha value computed for every period and every asset. 
#' @export
#' @examples
#' To evaluate alpha1 against constants (20, 5) and data contained in `d`:
#' alpha_run(c(20, 5), d, alpha1)

alpha_run <- function(constants, dat, alpha_fun) {

  # Argument checks:
  base::stopifnot(
    is.numeric(constants) | length(constants) == 0,
    is.list(dat),
    all(plyr::laply(dat, inherits, "xts")),
    length(unique(plyr::laply(x, nrow))) == 1,
    length(unique(plyr::laply(x, ncol))) == 1,
    is.function(alpha_fun))

  dat %<>% plyr::llply(head, 128) #FIXME:
  methods::formalArgs(alpha_fun) %>%
  base::setdiff("k") %>%
  dat[.] %>%
  plyr::llply(zoo::coredata) %>%
  utils::modifyList(list(k=constants)) %>%
  base::do.call(what=alpha_fun) %>%
  {dat[[1]][] <- .}

  dat[[1]]}


#' Global function definitions.
#' @param x (xts) time series matrix.
#' @param y (xts) time series matrix.

al1_param_defs <- function() {NULL}


#' Cross-sectional rank
#' Proportion of firms that have an observed value less than or equal to the
#' observed value.
#' @inheritParams al1_param_defs
#' @return (xts)
#' @export

al1_rank <- function(x) {matrixStats::rowRanks(x) / ncol(x)}


# Rolling Functions ------------------------------------------------------------

#' Time series rank
#' @inheritParams zoo::rollmeanr
#' @export

al1_roll_rank<- function(x, k) {
  RcppRoll::roll_meanr(x, k) %>%
  al1_rank
}


#' Time series argument max (rolling)
#' @inheritParams zoo::rollapplyr
#' @export

al1_arg_max <- function(data, width) {
  zoo::rollapplyr(
    fill=NA,
    data=data,
    width=width,
    FUN=function(z) {which.max(z) %>% ifelse(length(.), ., NA)})
}


#' Time series argument min (rolling)
#' @inheritParams zoo::rollapplyr
#' @export

al1_arg_min <- function(data, width) {
  zoo::rollapplyr(
    fill=NA,
    data=data,
    width=width,
    FUN=function(z) {which.min(z) %>% ifelse(length(.), ., NA)})
}



# Correlation and Covariance Functions
# --------------------------------------

FCo <- function(x, y, n, stat) {

  # Time-serial covariance / correlation of x and y for the past n days.
  # NOTE: x and y must have equal indices.

  # Arguments:
  # `x` (xts) time series with days as index.
  # `y` (xts) time series with days as index.
  # `n` (integer length 1) number of days to compute correlation over.
  # `stat` (character) suffix of the desired `TTR` run.. function. One of
  #   Cov, Cor.

  # Value: (xts) time series.

  # Pick the right running function from the `TTR` package:
  RunFun <- stringr::str_c("TTR::run", stat) %>% get
  n %<>% floor  

  1:ncol(x) %>%
  llply(function(i) {tryCatch(error=function(e) {
    xts(rep(NA, nrow(x)), order.by=index(x))}, RunFun(x[,i], y[,i], n=n))}) %>%
  Reduce(f=cbind)
}

FCov <- function(x, y, n) {FCo(x, y, n, stat="Cov")}
FCor <- function(x, y, n) {FCo(x, y, n, stat="Cor")}


#' Rescale x such that sum(abs(x)) == a, column-wise.
#' @inheritParams al1_param_defs
#' @param a (numeric) target sum(abs(.)) value of every column after scaling.
#' @return (xts)
#' @export

al1_scale <- function(x, a=1) {
  scale(x,
    center=FALSE,
    scale=abs(x) %>% colSums(na.rm=TRUE) %>% `/`(a))}


#' Neutralisation (demean) by group
#' Cross-sectionally neutralized (demeaned) by groups
#' @inheritParams al1_param_defs
#' @param groups (factor) groups allocation, one for each column.
#' @export

al1_group_neut <- function(x, groups=NULL) {
  #NOTE: this is potentially SLOW

  # If no groups are present, treat every column as a group:
  if(is.null(groups)) {groups <- 1:ncol(x)}

  # Compute rowwise means by for every group:
  unique(groups) %>%
  plyr::laply(. %>% {x[, groups == .]} %>% rowMeans(na.rm=TRUE)) %>%
  # Subtract the relevant group-means for every column:
  {x - t(.)[, match(groups, unique(groups))]}
}
