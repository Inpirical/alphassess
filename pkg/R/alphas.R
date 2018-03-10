# Alpha functions (101 in Total) ===============================================

#' Definitions of common parameters used in alpha functions.
#' @param k (numeric) vector of constants used to evaluate the alpha.
#' @param op (xts) matrix with asset open prices.
#' @param cl (xts) matrix with asset close prices.
#' @param hi (xts) matrix with asset high prices.
#' @param lo (xts) matrix with asset low prices.
#' @param vol (xts) matrix with volume traded.
#' @param dvol (xts) matrix with dollar volume traded.
#' @param r (xts) matrix with logarithmic returns.
#' @param cap (xts) matrix with market capitalisation values. #FIXME:

#' @inheritParams alpha_common_parameters
#' @export

alpha_common_parameters <- function() {}



#' @inheritParams alpha_common_parameters
#' @export

alpha1 <- function(k, cl, r) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  base::ifelse(
    test=r < 0,
    yes=RcppRoll::roll_sdr(r, k[1]),
    no=cl) %>%
  `^`(2) %>%
  al1_whichmax(k[2]) %>%
  al1_rank %>%
  `-`(0.5)}


#' @inheritParams alpha_common_parameters
#' @export

alpha2 <- function(k, vol, cl, op) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  FCor(
    x=base::diff(log(vol), k[1]) %>% al1_rank,
    y=al1_rank((cl - op) / op),
    n=k[2]) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha3 <- function(k, op, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  FCor(
    al1_rank(op),
    al1_rank(vol),
    k[1]) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha4 <- function(k, lo) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  al1_rank(lo) %>%
  al1_roll_rank(k[1]) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha5 <- function(k, op, vwap, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  RcppRoll::roll_sumr(vwap, k[1]) %>%
  `/`(k[1]) %>%
  {op - .} %>%
  al1_rank %>%
  `*`(
    (cl - vwap) %>%
    al1_rank %>%
    base::abs()) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha6 <- function(k, op, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  FCor(op, vol, k[1]) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha7 <- function(k, dvol, vol, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 3, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  base::ifelse(
    test=RcppRoll::roll_meanr(dvol, k[1]) < vol,

    yes=base::diff(cl, k[2])%>%
      base::abs() %>%
      al1_roll_rank(k[3]) %>%
      `*`(
        base::diff(cl, k[2]) %>%
        base::sign()),

    no=1) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha8 <- function(k, op, r) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  RcppRoll::roll_sumr(op, k[1]) %>%
  `*`(RcppRoll::roll_sumr(r, k[1])) %>%
  {. - stats::lag(., k[2])} %>%
  al1_rank  %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha9 <- function(k, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  base::ifelse(
    test=0 < RcppRoll::roll_minr(base::diff(cl, k[1]), k[2]),
    yes=base::diff(cl, 1),

    no=base::ifelse(
      test=RcppRoll::roll_maxr(base::diff(cl, k[1]), k[2]) < 0,
      yes=base::diff(cl, 1),
      no=(-1 * base::diff(cl, k[1]))))}


#' @inheritParams alpha_common_parameters
#' @export

alpha10 <- function(k, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  base::ifelse(
    test=0 < RcppRoll::roll_minr(base::diff(cl, k[1]), k[2]),
    yes=base::diff(cl, 1),
    no=base::ifelse(
      test=RcppRoll::roll_maxr(base::diff(cl, k[1]), k[2]) < 0,
      yes=base::diff(cl, k[1]),
      no=(-1 * base::diff(cl, k[1]))))}


#' @inheritParams alpha_common_parameters
#' @export

alpha11 <- function(k, vwap, cl, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
      al1_rank(RcppRoll::roll_maxr(vwap - cl, k[1]))%>%
  `+`(al1_rank(RcppRoll::roll_minr(vwap - cl, k[1]))) %>%
  `*`(al1_rank(base::diff(vol, k[1])))}


#' @inheritParams alpha_common_parameters
#' @export

alpha12 <- function(k, vol, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  base::diff(vol, k[1]) %>%
  base::sign %>%
  `*`(base::diff(cl, k[1])) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha13 <- function(k, cl, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  FCov(al1_rank(cl),  al1_rank(vol), k[1]) %>%
  al1_rank %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha14 <- function(k, r, op, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  base::diff(r, k[1]) %>%
  al1_rank %>%
  `*`(FCor(op, vol, k[2])) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha15 <- function(k, hi, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  FCor(al1_rank(hi), al1_rank(vol), k[1]) %>%
  al1_rank %>%
  RcppRoll::roll_sumr(k[1]) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha16 <- function(k, hi, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  FCov(al1_rank(hi), al1_rank(vol), c[1]) %>%
  al1_rank %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha17 <- function(k, cl, vol, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 4, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  al1_roll_rank(cl, k[1]) %>%
  al1_rank %>%
  `*`(
    base::diff(cl, k[2]) %>%
    base::diff(k[2]) %>%
    al1_rank) %>%
  `*`(
    (vol / RcppRoll::roll_meanr(dvol, k[3])) %>%
    al1_roll_rank(k[4]) %>%
    al1_rank) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha18 <- function(k, cl, op) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  base::abs(cl - op) %>%
  RcppRoll::roll_sdr(k[1]) %>%
  `+`(cl - op) %>%
  `+`(FCor(cl, op, k[2])) %>%
  al1_rank %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha19 <- function(k, cl, r) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  cl %>%
  `-`(stats::lag(cl, k[1])) %>%
  `+`(base::diff(cl, k[1])) %>%
  base::sign %>%
  `*`(
    (1 + RcppRoll::roll_sumr(r, k[2])) %>%
    al1_rank %>%
    `+`(1)
  ) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha20 <- function(k, op, hi, lo) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
      al1_rank(op - stats::lag(hi, k[1])) %>%
  `*`(al1_rank(op - stats::lag(cl, k[1]))) %>%
  `*`(al1_rank(op - stats::lag(lo, k[1]))) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha21 <- function(k, cl, vol, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 3, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  base::ifelse(
    test=
      (RcppRoll::roll_sumr(cl, k[1]) / k[1]) %>%
      `+`(RcppRoll::roll_sdr(cl, k[1])) %>%
      `<`(RcppRoll::roll_sumr(cl, k[2]) / k[2]),

    yes=-1,
    no=base::ifelse(
      test=
        (RcppRoll::roll_sumr(cl, k[2]) / k[2]) %>%
        `<`(
          (RcppRoll::roll_sumr(cl, k[1]) / k[1]) %>%
          `-`(RcppRoll::roll_sdr(cl, k[1]))),

      yes=1,
      no=base::ifelse(
        test=(vol / RcppRoll::roll_meanr(dvol, k[3])) >= 1,
        yes=1,
        no-1)
    )
  )}


#' @inheritParams alpha_common_parameters
#' @export

alpha22 <- function(k, hi, vol, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  FCor(hi, vol, k[1]) %>%
  base::diff(k[1]) %>%
  `*`(al1_rank(RcppRoll::roll_sdr(cl, k[2]))) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha23 <- function(k, hi) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  base::ifelse(
    test=(RcppRoll::roll_sumr(hi, k[1]) / k[1]) < hi,
    yes=-base::diff(hi, k[2]),
    no=0)}


#' @inheritParams alpha_common_parameters
#' @export

alpha24 <- function(k, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  base::ifelse( 
    test=
      (RcppRoll::roll_sumr(cl, k[1]) / k[1]) %>%
      base::diff(k[1]) %>%
      `/`(stats::lag(cl, k[1])) %>%
      `<=`(0.05),

     yes=cl - RcppRoll::roll_minr(cl, k[1]),
     no=(base::diff(cl, k[2]))) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha25 <- function(k, r, vwap, hi, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  (-r) %>%
  `*`(RcppRoll::roll_meanr(dvol, k[1])) %>%
  `*`(vwap) %>%
  `*`(hi - cl) %>%
  al1_rank}


#' @inheritParams alpha_common_parameters
#' @export

alpha26 <- function(k, vol, hi) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
 FCor(
    al1_roll_rank(vol, k[1]) %>%
    al1_roll_rank(hi, k[1]),
    k[1]) %>%
 RcppRoll::roll_maxr(k[2]) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha27 <- function(k, vol, vwap) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  FCor(
    al1_rank(vol),
    al1_rank(vwap),
    k[1]) %>%
  RcppRoll::roll_sumr(k[2]) %>%
  `/`(2)
  al1_rank %>%
  `>`(0.5) %>%
  base::ifelse(-1, 1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha28 <- function(k, dvol, lo, hi, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  RcppRoll::roll_meanr(dvol, k[1]) %>%
  FCor(lo, k[2]) %>%
  `+`((hi + lo) / 2) %>%
  `-`(cl) %>%
  al1_scale}


#' @inheritParams alpha_common_parameters
#' @export

alpha29 <- function(k, cl, r) {
  # Argument tests:
  base::stopifnot(length(k) == 7, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  base::diff(cl - 1, k[1]) %>%
  al1_rank %>%
  `*`(-1) %>%
  al1_rank %>%
  al1_rank %>%
  RcppRoll::roll_minr(k[2]) %>%
  RcppRoll::roll_sumr(k[3]) %>%
  log %>%
  al1_scale %>%
  al1_rank %>%
  al1_rank %>%
  RcppRoll::roll_prodr(k[4]) %>%
  RcppRoll::roll_minr(k[5]) %>%
  `+`(
    stats::lag(-r, k[6]) %>%
    al1_roll_rank(k[7]))}


#' @inheritParams alpha_common_parameters
#' @export

alpha30 <- function(k, cl, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 5, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
      (cl - stats::lag(cl, k[1])) %>%
      base::sign() %>%
  `+`(
    (stats::lag(cl, 1) - stats::lag(cl, k[2])) %>%
    base::sign()) %>%
  `+`(
    (stats::lag(cl, 2) - stats::lag(cl, k[3])) %>%
    base::sign()) %>%
  al1_rank %>%
  {1 - .} %>%
  `*`(RcppRoll::roll_sumr(vol, k[4])) %>%
  `/`(RcppRoll::roll_sumr(vol, k[5]))}


#' @inheritParams alpha_common_parameters
#' @export

alpha31 <- function(k, cl, dvol, lo) {
  # Argument tests:
  base::stopifnot(length(k) == 4, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  base::diff(cl, k[1]) %>%
  al1_rank %>%
  al1_rank %>%
  `*`(-1) %>%
  RcppRoll::roll_sumr(weights=1:k[1]) %>%
  al1_rank %>%
  al1_rank %>%
  al1_rank %>%
  `+`(
    (-base::diff(cl, k[2]))  %>%
    al1_rank)  %>%
  `+`(
    FCor(RcppRoll::roll_meanr(dvol, k[3]), lo, k[4]) %>%
    al1_scale %>%
    base::sign())}


#' @inheritParams alpha_common_parameters
#' @export

alpha32 <- function(k, cl) {
  # Argument tests:
  base::stopifnot(
    length(k) == 3,
    k[1] %% 1 == 0,
    k[2] %% 1 == 0,
    k[3] %% 1 == 0,
    k[1] > 0,
    k[2] > 0,
    k[3] > 0)

  # Evaulating the result:
  (RcppRoll::roll_sumr(cl, k[1]) / k[1]) %>%
  `-`(cl) %>%
  al1_scale %>%
  `+`(
    FCor(vwap, stats::lag(cl, k[2]), k[3]) %>%
    al1_scale)}


#' @inheritParams alpha_common_parameters
#' @export

alpha33 <- function(k, op, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 0, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  (-1 * ((1 - (op / cl)) ^ 1)) %>%
  al1_rank}


#' @inheritParams alpha_common_parameters
#' @export

alpha34 <- function(k, r, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 3, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
      RcppRoll::roll_sdr(r, k[1]) %>%
  `/`(RcppRoll::roll_sdr(r, k[2])) %>%
  al1_rank %>%
  {1 - .} %>%
  `+`(1 - al1_rank(base::diff(cl, k[3]))) %>%
  al1_rank}


#' @inheritParams alpha_common_parameters
#' @export

alpha35 <- function(k, vol, cl, hi, lo, r) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  al1_roll_rank(vol, k[1]) %>%
  `*`(1 - al1_roll_rank(((cl + hi) - lo), k[2])) %>%
  `*`(1 - al1_roll_rank(r, k[1]))}


#' @inheritParams alpha_common_parameters
#' @export

alpha36 <- function(k, cl, op, vol, vwap, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 11, all(k %% 1 == 0), all(k > 0))

  # Evaluating the results:
  FCor((cl - op), stats::lag(vol, k[1]), k[2]) %>%
  al1_rank %>%
  `*`(k[3]) %>%
  `+`(k[4] * al1_rank(op - cl)) %>%
  `+`(
    stats::lag(-r, k[5]) %>%
    al1_roll_rank(k[6]) %>%
    al1_rank %>%
    `*`(k[7])) %>%
  `+`(
    FCor(
      vwap,
      RcppRoll::roll_meanr(dvol, k[8]),
      k[9]) %>%
    base::abs() %>%
    al1_rank) %>%
  `+`(
    (RcppRoll::roll_sumr(cl, k[10]) / k[10]) %>%
    `-`(op) %>%
    `*`(cl - op) %>%
    al1_rank %>%
    `*`(k[11]))}


#' @inheritParams alpha_common_parameters
#' @export

alpha37 <- function(k, op, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
 (op - cl) %>%
 stats::lag(k[1]) %>%
 FCor(cl, k[2]) %>%
 al1_rank %>%
  `+`(al1_rank(op - cl))}


#' @inheritParams alpha_common_parameters
#' @export

alpha38 <- function(k, cl, op) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  al1_roll_rank(cl, k[1]) %>%
  al1_rank %>%
  `*`(-1) %>%
  `*`(al1_rank(cl / op))}


#' @inheritParams alpha_common_parameters
#' @export

alpha39 <- function(k, cl, vol, dvol, r) {
  # Argument tests:
  base::stopifnot(length(k) == 4, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  (vol / RcppRoll::roll_meanr(dvol, k[1])) %>%
  RcppRoll::roll_sumr(weights=1:k[2]) %>%
  al1_rank %>%
  {1 - .} %>%
  `*`(base::diff(cl, k[3])) %>%
  al1_rank %>%
  `*`(-1) %>%
  `*`(
    RcppRoll::roll_sumr(r, k[4]) %>%
    al1_rank %>%
    `+`(1))}


#' @inheritParams alpha_common_parameters
#' @export

alpha40 <- function(k, hi, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  RcppRoll::roll_sdr(, hi, k[1]) %>%
  al1_rank %>%
  `*`(FCor(hi, vol, k[2])) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha41 <- function(k, hi, lo, vwap) {
  # Argument tests:
  base::stopifnot(length(k) == 0, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  ((hi * lo)^0.5) - vwap}


#' @inheritParams alpha_common_parameters
#' @export

alpha42 <- function(k, vwap, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 0, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
      al1_rank(vwap - cl) %>%
  `/`(al1_rank(vwap + cl))}


#' @inheritParams alpha_common_parameters
#' @export

alpha43 <- function(k, cl, vol, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
   base::diff(x=cl, n=7) %>%
   `*`(-1) %>%
   al1_roll_rank(n=k[1]) %>%
   `*`(
     vol %>%
     `/`(FRun(dvol, n=k[2], mean)) %>%
     al1_roll_rank(k[2]))}


#' @inheritParams alpha_common_parameters
#' @export

alpha44 <- function(k, vol, hi) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  al1_rank(vol) %>%
  {FCo(hi, ., n=k[1], stat="Cor")} %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha45 <- function(k, cl, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 3, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  (RcppRoll::roll_sumr(stats::lag(cl, k[1]), k[2]) / k[2]) %>%
  `*`(FCor(cl, vol, k[3])) %>%
  `*`(
    (RcppRoll::roll_sumr(stats::lag(cl, k[1]), k[2]) / k[2]) %>%
    al1_rank %>%
    `*`(FCor(cl, vol, k[3]))) %>%
  `*`(
    FCor(
    RcppRoll::roll_sumr(cl, k[1]),
    RcppRoll::roll_sumr(cl, k[2]),
    k[3]) %>%
    al1_rank) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha46 <- function(k, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 3, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  ((stats::lag(cl, k[1]) - stats::lag(cl, k[2])) / k[2]) %>%
  `-`((stats::lag(cl, k[2]) - cl) / k[2]) %>%
  {base::ifelse(
    test=0.25 < .,
    yes=-1,
    no=base::ifelse(
      test=. < 0,
      yes=1,
      no=-(cl - stats::lag(cl, k[3]))))}}


#' @inheritParams alpha_common_parameters
#' @export

alpha47 <- function(k, cl, vol, dvol, hi, vwap) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  al1_rank(1 / cl) %>%
  `*`(vol) %>%
  `/`(RcppRoll::roll_meanr(dvol, k[1])) %>%
  `*`(
    (hi * al1_rank(hi - cl)) %>%
    `/`(RcppRoll::roll_sumr(hi, k[2]) / k[2])) %>%
  `-`(al1_rank(vwap - stats::lag(vwap, k[2])))}


#' @inheritParams alpha_common_parameters
#' @export

alpha48 <- function(k, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  FCor(
    base::diff(cl, k[1]),
    base::diff(stats::lag(cl, k[1]), k[1]),
    k[2]) %>%
  `*`(base::diff(cl, k[1])) %>%
  `/` (cl) %>%
  FIndNet(IndClass.subindustry) %>%
  `/`(
    (base::diff(cl, k[1]) / stats::lag(cl, k[1])) %>%
    `^`(2) %>%
    RcppRoll::roll_sumr(k[2]))}


#' @inheritParams alpha_common_parameters
#' @export

alpha49 <- function(k, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 3, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  ((stats::lag(cl, k[1]) - stats::lag(cl, k[2])) / k[2]) %>%
  `-`((stats::lag(cl, k[2]) - cl) / k[2]) %>%
  {base::ifelse(
    test=. < -.1,
    yes=1,
    no=-(cl - stats::lag(cl, k[3])))}}


#' @inheritParams alpha_common_parameters
#' @export

alpha50 <- function(k, vol, vwap) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  FCor(al1_rank(vol), al1_rank(vwap), k[1]) %>%
  al1_rank %>%
  RcppRoll::roll_maxr(k[1]) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha51 <- function(k, cl, r) {
  # Argument tests:
  base::stopifnot(length(k) == 3, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  ((stats::lag(cl, k[1]) - stats::lag(cl, k[2])) / k[2]) %>%
  `-`(((stats::lag(cl, k[2]) - cl) / k[2])) %>%
  {base::ifelse(
    test=. <- -0.05,
    yes=1,
    no=-(cl - stats::lag(cl, k[3])))}}


#' @inheritParams alpha_common_parameters
#' @export

alpha52 <- function(k, lo, r, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 3, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  RcppRoll::roll_minr(lo, k[1]) %>%
  `+`(
    RcppRoll::roll_minr(lo, k[1]) %>%
    stats::lag(k[1]))%>%
  `*`(
    RcppRoll::roll_sumr(r, k[2]) %>%
    `-`(RcppRoll::roll_sumr, r, k[3]) %>%
    `/`(k[4])) %>%
  al1_rank %>%
  `*`(al1_roll_rank(vol, k[1])) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha53 <- function(k, cl, lo, hi) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  (((cl - lo) - (hi - cl)) / (cl - lo)) %>%
  base::diff(k[1]) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha54 <- function(k, hi, lo, op, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 1, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  ((lo - cl) * (op^k[1])) %>%
  `/`((lo - hi) * (cl^k[1])) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha55 <- function(k, cl, lo, hi, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  FCor(
    (cl - RcppRoll::roll_minr(lo, k[1])) %>%
    `/`(
      RcppRoll::roll_maxr(hi, k[1]) %>%
      `-`(RcppRoll::roll_minr(lo, k[1]))) %>%
    al1_rank,
    al1_rank(vol),
    k[2]) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha56 <- function(k, r, cap) {
  # Argument tests:
  base::stopifnot(length(k) == 3, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  RcppRoll::roll_sumr(r, k[1]) %>%
  `/`(
    RcppRoll::roll_sumr(r, k[2]) %>%
    RcppRoll::roll_sumr(k[3])) %>%
  `*`(al1_rank(r * cap)) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha57 <- function(k, cl, vwap) {
  # Argument tests:
  base::stopifnot(length(k) == 2, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  (cl - vwap) %>%
  `/`(
    al1_whichmax(cl, k[1]) %>%
    al1_rank %>%
    RcppRoll::roll_sumr(weights=1:k[2])) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha58 <- function(k, vwap, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 3)

  # Evaulating the result:
  FCor(
    al1_group_neut(vwap, IndClass.sector),
    vol,
    k[1]) %>%
  RcppRoll::roll_sumr(weights=1:k[2]) %>%
  al1_roll_rank(k[3]) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha59 <- function(k, vwap, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 5,
    all(k[3:5] %% 1) == 0,
    all(k[3:5] > 0))

  (vwap * k[1]) %>%
  `+`(vwap * (1 - k[2])) %>%
  al1_group_neut(IndClass.industry) %>%
  FCor(vol, k[3]) %>%
  RcppRoll::roll_sumr(weights=1:k[4]) %>%
  al1_roll_rank(k[5]) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha60 <- function(k, cl, lo, hi, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 1, k[1] %% 1 == 0, k[1] > 0)

  # Evaulating the result:
  ((((cl - lo) - (hi - cl)) / (hi - lo)) * vol) %>%
  al1_rank %>%
  al1_scale %>%
  `*`(2) %>%
  `-`(
    al1_whichmax(cl, k[1]) %>%
    al1_rank %>%
    al1_scale) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha61 <- function(k, wvap, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 1, k[1] %% 1 == 0, k[1] > 0)

  # Evaulating the result:
  (vwap - RcppRoll::roll_minr(vwap, k[1])) %>%
  al1_rank %>%
  `<`(
    FCor(
      vwap,
      RcppRoll::roll_meanr(dvol, k[2]),
      k[3]) %>%
    al1_rank)}


#' @inheritParams alpha_common_parameters
#' @export

alpha62 <- function(k, dvol, op, hi, lo) {
  # Argument tests:
  base::stopifnot(length(k) == 3, all(k %% 1 == 0), all(k > 0))

  # Evaulating the result:
  FCor(
    vwap, 
    RcppRoll::roll_meanr(dvol, k[1]),
    RcppRoll::roll_sumr(k[2]),
    k[3]) %>%
  al1_rank %>%
  `<`(
    al1_rank(op) %>%
    `+`(al1_rank(op)) %>%
    `<`(
      al1_rank((hi + lo) / 2) %>%
      `+`(al1_rank(hi))) %T>%
    {storage.mode(.) <- "numeric"} %>%
    al1_rank) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha63 <- function(k, op, vwap, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 7,
    all(k[-3] %% 1 == 0),
    all(k[-3] > 0))

  # Evaulating the result:
  al1_group_neut(cl, IndClass.industry) %>%
  base::diff(k[1]) %>%
  RcppRoll::roll_sumr(weights=1:k[2]) %>%
  al1_rank %>%
  `-`(
    FCor(
      ((vwap * k[3]) + (op * (1 - k[3]))),

      RcppRoll::roll_meanr(dvol, k[4]) %>%
      RcppRoll::roll_sumr(k[5]),
      k[6]) %>%
    RcppRoll::roll_sumr(weights=1:k[7]) %>%
    al1_rank) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha64 <- function(k, op, dvol, lo, vwap, hi) {
  # Argument tests:
  base::stopifnot(length(k) == 5,
    all(k[-5] %% 1 == 0),
    all(k[-5] > 0))

  FCor(
    ((op * k[1]) + (lo * (1 - k[1]))) %>%
    RcppRoll::roll_sumr(k[2]),

    RcppRoll::roll_meanr(dvol, k[5]) %>%
    RcppRoll::roll_sumr(k[2]),
    k[3]) %>%
  al1_rank %>%
  `<`(
      (((hi + lo) / 2) * k[1]) %>%
      `+`(vwap * (1 - k[1])) %>%
    base::diff(k[4]) %>%
    al1_rank)
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha65 <- function(k, op, vwap, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 5,
    all(k[-1] %% 1 == 0),
    all(k[-1] > 0))

  FCor(
    ((op * k[1]) + (vwap * (1 - k[1]))),
    RcppRoll::roll_meanr(dvol, k[2]) %>%
    RcppRoll::roll_sumr(k[3]),
    k[4]) %>%
  al1_rank %>%
  `<`(
    (op - RcppRoll::roll_minr(op, k[5])) %>%
    al1_rank) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha66 <- function(k, vwap, lo, op, hi) {
  # Argument tests:
  base::stopifnot(length(k) == 5,
    all(k[-3] %% 1 == 0),
    all(k[-3] > 0))

  base::diff(vwap, k[1]) %>%
  RcppRoll::roll_sumr(weights=1:k[2]) %>%
  al1_rank %>%
  `+`(
    (((lo * k[3]) + (lo * (1 - k[3]))) - vwap) %>%
    `/`(op - ((hi + lo) / 2)) %>%
    RcppRoll::roll_sumr(weights=1:k[4]) %>%
    al1_roll_rank(k[5])) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha67 <- function(k, hi, vwap, dvol) {
  # Argument tests:
  base::stopifnot(
    length(k) == 3)

  # Evaulating the result:
  (hi - RcppRoll::roll_minr(hi, k[1])) %>%
  al1_rank %>%
  `^`(
    FCor(
      al1_group_neut(vwap, IndClass.sector),

      RcppRoll::roll_meanr(dvol, k[2]) %>%
      al1_group_neut(IndClass.subindustry),

      k[3]) %>%
    al1_rank) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha68 <- function(k, dvol, cl, lo) {
  # Argument tests:
  base::stopifnot(length(k) == 5,
    all(k[-4] %% 1 == 0),
    all(k[-4] > 0))

  FCor(
    al1_rank(hi),
    RcppRoll::roll_meanr(dvol, k[1]) %>%
    al1_rank,
    k[2]) %>%
  al1_roll_rank(k[3])
  `<`(
    ((cl * k[4]) + (lo * (1 - k[4]))) %>%
    base::diff(k[5]) %>%
    al1_rank) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha69 <- function(k, wvap, cl, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 6,
    all(k[-3] %% 1 == 0),
    all(k[-3] > 0))

  al1_group_neut(vwap, IndClass.industry) %>%
  base::diff(k[1]) %>%
  RcppRoll::roll_maxr(k[2]) %>%
  al1_rank %>%
  `^`(
    FCor(
      ((cl * k[3]) + (vwap * (1 - k[3]))),
      RcppRoll::roll_meanr(dvol, k[4]),
      k[5]) %>%
    al1_roll_rank(k[6])) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha70 <- function(k, vwap, cl, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 4,
    all(k %% 1 == 0),
    all(k > 0))

 base::diff(vwap, k[1])
 al1_rank %>%
 `^`(
   FCor(
     al1_group_neut(cl, IndClass.industry),
     RcppRoll::roll_meanr(dvol, k[2]),
     k[3]) %>%
   al1_roll_rank(k[4]))
 `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha71 <- function(k, cl, dvol, vwap, lo, op) {
  # Argument tests:
  base::stopifnot(length(k) == 8,
    all(k %% 1 == 0),
    all(k > 0))

  base::pmax(
    FCor(
      al1_roll_rank(cl, k[1]),
      RcppRoll::roll_meanr(dvol, k[2]) %>%
      al1_roll_rank(k[3]),
      k[4]) %>%
    RcppRoll::roll_sumr(weights=1:k[5]) %>%
    al1_roll_rank(k[6]),

    ((lo + op) - (vwap + vwap)) %>%
    `^`(2) %>%
    al1_rank %>%
    RcppRoll::roll_sumr(weights=1:k[7]) %>%
    al1_roll_rank(k[8]))}


#' @inheritParams alpha_common_parameters
#' @export

alpha72 <- function(k, hi, lo, dvol, vol, vwap) {
  # Argument tests:
  base::stopifnot(length(k) == 7,
    all(k %% 1 == 0),
    all(k > 0))

  FCor(
    ((hi + lo) / 2),
    RcppRoll::roll_meanr(dvol, k[1]),
    k[2]) %>%
  RcppRoll::roll_sumr(weights=1:k[3]) %>%
  al1_rank %>%
  `/`(
    FCor(
      al1_roll_rank(vwap, k[4]),
      al1_roll_rank(vol, k[5]),
      k[6]) %>%
    RcppRoll::roll_sumr(weights=1:k[7]) %>%
    al1_rank
  )}


#' @inheritParams alpha_common_parameters
#' @export

alpha73 <- function(k, vwap, lo, op) {
  # Argument tests:
  base::stopifnot(length(k) == 6,
    all(k[-3] %% 1 == 0),
    all(k[-3] > 0))

  base::pmax(
    base::diff(vwap, k[1]) %>%
    RcppRoll::roll_sumr(weights=1:k[2]) %>%
    al1_rank,

    ((op * k[3]) + (lo * (1 - k[3]))) %>%
    base::diff(k[4]) %>%
    `/`((op * k[3]) + (lo * (1 - k[3]))) %>%
    `*`(-1) %>%
    RcppRoll::roll_sumr(weights=1:k[5]) %>%
    al1_roll_rank(k[6])) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha74 <- function(k, dvol, hi, vwap) {
  # Argument tests:
  base::stopifnot(length(k) == 5,
    all(k[-4] %% 1 == 0),
    all(k[-4] > 0))

  FCor(
    cl,
    RcppRoll::roll_meanr(dvol, k[1]),
    RcppRoll::roll_sumr(k[2]),
    k[3]) %>%
  al1_rank %>%
  `<`(
    FCor(
      ((hi * k[4]) + (vwap * (1 - k[4]))) %>%
      al1_rank,
      al1_rank(vol),
      k[5]) %>%
    al1_rank) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha75 <- function(k, vol, lo, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 3,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  FCor(vwap, vol, k[1]) %>%
  al1_rank %>%
  `<`(
    FCor(
      al1_rank(lo),
      RcppRoll::roll_meanr(dvol, k[2]) %>% al1_rank,
      k[3]) %>%
    al1_rank)}


#' @inheritParams alpha_common_parameters
#' @export

alpha76 <- function(k, lo, wvap, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 7,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  base::pmax(
    base::diff(vwap, k[1]) %>%
    RcppRoll::roll_sumr(weights=1:k[2]) %>%
    al1_rank,

    FCor(
      al1_group_neut(lo, IndClass.sector),
      RcppRoll::roll_meanr(dvol, k[3]), k[4]) %>%
    al1_roll_rank(k[5]) %>%
    RcppRoll::roll_sumr(weights=1:k[6]) %>%
    al1_roll_rank(k[7])) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha77 <- function(k, hi, lo, vwap, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 4,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  base::pmin(
    ((((hi + lo) / 2) + hi) - (vwap + hi)) %>%
    RcppRoll::roll_sumr(weights=1:k[1]) %>%
    al1_rank,

    FCor(
      ((hi + lo) / 2),
      RcppRoll::roll_meanr(dvol, k[2]),
      k[3]) %>%
    RcppRoll::roll_sumr(weights=1:k[4]) %>% 
    al1_rank
  )}


#' @inheritParams alpha_common_parameters
#' @export

alpha78 <- function(k, vwap, lo, dvol, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 3,
    all(k[-1] %% 1 == 0),
    all(k[-1] > 0))

  # Evaulating the result:
  FCor(
    ((lo * k[1]) + (vwap * (1 - k[1]))) %>%
    RcppRoll::roll_sumr(k[2]),
 
    RcppRoll::roll_meanr(dvol, k[3]) %>%
    RcppRoll::roll_sumr(k[2]),

    k[4]) %>%
  al1_rank %>%
  `^`(
    FCor(al1_rank(vwap), al1_rank(vol), k[5]) %>%
    al1_rank)}


#' @inheritParams alpha_common_parameters
#' @export

alpha79 <- function(k, op, cl, vwap, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 6,
    all(k[-1] %% 1 == 0),
    all(k[-1] > 0))

  # Evaulating the result:
 ((cl * k[1]) + (op * (1 - k[1]))) %>%
 al1_group_neut(IndClass.sector) %>%
 base::diff(k[2]) %>%
 al1_rank %>%
 `<`(
   FCor(
     al1_roll_rank(vwap, k[3]),
     RcppRoll::roll_meanr(dvol, k[4]) %>%
     al1_roll_rank(k[5]),
     k[6]) %>%
   al1_rank)}


#' @inheritParams alpha_common_parameters
#' @export

alpha80 <- function(k, op, hi, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 5,
    all(k[-1] %% 1 == 0),
    all(k[-1] > 0))

  # Evaulating the result:
  ((op * k[1]) + (hi * (1 - k[1]))) %>%
  al1_group_neut(IndClass.industry) %>%
  base::diff(k[2]) %>%
  base::sign() %>%
  al1_rank %>%
  `^`(
    FCor(hi, RcppRoll::roll_meanr(dvol, k[3]), k[4]) %>%
    al1_roll_rank(k[5]))
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha81 <- function(k, dvol, vwap, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 5,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  FCor(
    vwap,
    RcppRoll::roll_meanr(dvol, k[1]) %>%
    RcppRoll::roll_sumr(k[2]),
    k[3]) %>%
  al1_rank %>%
  `^`(4) %>%
  al1_rank %>%
  RcppRoll::roll_prodr(k[4]) %>%
  log %>%
  al1_rank %>%
  `<`(
    FCor(al1_rank(vwap), al1_rank(vol), k[5]) %>%
    al1_rank) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha82 <- function(k, op, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 6,
    all(k[-3] %% 1 == 0),
    all(k[-3] > 0))

  # Evaulating the result:
  base::pmin(
    base::diff(op, k[1]) %>%
    RcppRoll::roll_sumr(weights=1:k[2]) %>%
    al1_rank,

    FCor(
      al1_group_neut(vol, IndClass.sector),
      ((op * k[3]) + (op * (1 - k[3]))),
      k[4]) %>%
    RcppRoll::roll_sumr(weights=1:k[5]) %>%
    al1_roll_rank(k[6])) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha83 <- function(k, vol, hi, lo, cl) {
  # Argument tests:
  base::stopifnot(length(k) == 2,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  (hi - lo) %>%
  `/`(RcppRoll::roll_sumr(cl, k[1]) / k[1]) %>%
  stats::lag(k[2]) %>%
  al1_rank %>%
  `*`(
    al1_rank(vol) %>%
    al1_rank) %>%
  `/`(
    (hi - lo) %>%
    `/`((RcppRoll::roll_sumr(cl, k[1]) / k[1])) %>%
    `/`(vwap - cl))}


#' @inheritParams alpha_common_parameters
#' @export

alpha84 <- function(k, vwap, cl) {
  # Argument tests:
  base::stopifnot(
    length(k) == 3)

  # Evaulating the result:
  (vwap - RcppRoll::roll_maxr(vwap, k[1])) %>%
  al1_roll_rank(k[2]) %>%
  `^`(base::diff(cl, k[3]))}


#' @inheritParams alpha_common_parameters
#' @export

alpha85 <- function(k, cl, hi, dvol, lo, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 2,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  FCor(
    ((hi * k[1]) + (cl * (1 - k[1]))),
    RcppRoll::roll_meanr(dvol, 30),
    k[2]) %>%
  al1_rank %>%
  `^`(FCor(
    al1_roll_rank(((hi + lo) / 2), k[3]),
    al1_roll_rank(vol, k[4]),
    k[5]) %>%
  al1_rank)}


#' @inheritParams alpha_common_parameters
#' @export

alpha86 <- function(k, dvol, cl, op, wvap) {
  # Argument tests:
  base::stopifnot(length(k) == 4,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  RcppRoll::roll_meanr(dvol, k[1]) %>%
  RcppRoll::roll_sumr(k[2]) %>%
  FCor(cl, ., k[3]) %>%
  al1_roll_rank(k[4]) %>%
   `<`(al1_rank((op + cl) - (vwap + op))) %>%
   `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha87 <- function(k, wvap, cl, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 7,
    all(k[-1] %% 1 == 0),
    all(k[-1] > 0))

  # Evaulating the result:
  base::pmax(
    ((cl * k[1]) + (vwap * (1 - k[1]))) %>%
    base::diff(k[2]) %>%
    RcppRoll::roll_sumr(weights=1:k[3]) %>%
    al1_rank,

    #FIXME:
    al1_group_neut(RcppRoll::roll_meanr(dvol, k[4]) %>%
    FCor(., IndClass.industry), cl, k[5]) %>%
    base::abs() %>%
    RcppRoll::roll_sumr(weights=1:k[6]) %>%
    al1_roll_rank(k[7])) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha88 <- function(k, op, lo, hi, cl, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 6,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  base::pmin(
    (al1_rank(op) + al1_rank(lo)) %>%
    `-`(al1_rank(hi) + al1_rank(cl)) %>%
    RcppRoll::roll_sumr(weights=1:k[1]) %>%
    al1_rank,
    FCor(
      al1_roll_rank(cl, k[2]),

      RcppRoll::roll_meanr(dvol, k[3]) %>%
      al1_roll_rank(k[4]),

      k[5]) %>%
    RcppRoll::roll_sumr(weights=1:k[6]) %>% 
    al1_roll_rank(k[7]))}


#' @inheritParams alpha_common_parameters
#' @export

alpha89 <- function(k, lo, dvol, vwap) {
  # Argument tests:
  base::stopifnot(length(k) == 8,
    all(k[-1] %% 1 == 0),
    all(k[-1] > 0))

  # Evaulating the result:
 ((lo * k[1]) + (lo * (1 - k[1]))) %>%
 FCor(RcppRoll::roll_meanr(dvol, k[2]), k[3]) %>%
 RcppRoll::roll_sumr(weights=1:k[4]) %>%
 al1_roll_rank(k[5]) %>%
 `-`(
    al1_group_neut(vwap, IndClass.industry) %>%
    base::diff(k[6]) %>%
    RcppRoll::roll_sumr(weights=1:k[7]) %>%
    al1_roll_rank(k[8]))}


#' @inheritParams alpha_common_parameters
#' @export

alpha90 <- function(k, cl, dvol, lo) {
  # Argument tests:
  base::stopifnot(length(k) == 4,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  (cl - RcppRoll::roll_maxr(cl, k[1])) %>%
  al1_rank %>%
  `^`(
    RcppRoll::roll_meanr(dvol, k[2]) %>%
    al1_group_neut(IndClass.subindustry) %>%
    FCor(lo, k[3]) %>%
    al1_roll_rank(k[4])) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha91 <- function(k, cl, vol, wvap, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 7,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  al1_group_neut(cl, IndClass.industry) %>%
  FCor(vol, k[1]) %>%
  RcppRoll::roll_sumr(weights=1:k[2]) %>%
  RcppRoll::roll_sumr(weights=1:k[3]) %>%
  al1_roll_rank(k[4]) %>%
  `-`(
    FCor(vwap, RcppRoll::roll_meanr(dvol, k[5]), k[6]) %>%
    RcppRoll::roll_sumr(weights=1:k[7]) %>%
    al1_rank) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha92 <- function(k, hi, lo, cl, op, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 6,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  base::pmin(
    ((((hi + lo) / 2) + cl) < (lo + op)) %>%
    RcppRoll::roll_sumr(weights=1:k[1]) %>%
    al1_roll_rank(k[2]),

    FCor(
      al1_rank(lo),
      al1_rank(RcppRoll::roll_meanr(dvol, k[3])),
      k[4]) %>%
    RcppRoll::roll_sumr(weights=1:k[5]) %>%
    al1_roll_rank(k[6]))}


#' @inheritParams alpha_common_parameters
#' @export

alpha93 <- function(k, cl, vwap, dvol) {
  al1_group_neut(vwap, IndClass.industry) %>%
  # Argument tests:
  base::stopifnot(length(k) == 7,
    all(k[-5] %% 1 == 0),
    all(k[-5] > 0))

  # Evaulating the result:
  {FCor(., RcppRoll::roll_meanr(dvol, k[1]), k[2])} %>%
  RcppRoll::roll_sumr(weights=1:k[3]) %>%
  al1_roll_rank(k[4]) %>%
  `/`(
     ((cl * k[5]) + (vwap * (1 - k[5]))) %>%
     base::diff(k[6]) %>%
     RcppRoll::roll_sumr(weights=1:k[7]) %>%
     al1_rank)}


#' @inheritParams alpha_common_parameters
#' @export

alpha94 <- function(k, wvap, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 6,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  (vwap - RcppRoll::roll_minr(vwap, k[1])) %>%
  al1_rank %>%
  `^`(
    {FCor(
      al1_roll_rank(vwap, k[2]) %>%
      RcppRoll::roll_meanr(dvol, k[3]) %>%
      al1_roll_rank(k[4]),
      k[5])} %>%
    al1_roll_rank(k[6])) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha95 <- function(k, op, dvol, hi, lo) {
  # Argument tests:
  base::stopifnot(length(k) == 5,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  (op - RcppRoll::roll_minr(op, k[1])) %>%
  al1_rank %>%
  `<`(
    RcppRoll::roll_sumr(((hi + lo) / 2), k[2]) %>%
    FCor(
      RcppRoll::roll_sumr(RcppRoll::roll_meanr(dvol, k[3]), k[2]),
      k[4]) %>%
    al1_rank %>%
    `^`(5) %>%
    al1_roll_rank(k[5]))}


#' @inheritParams alpha_common_parameters
#' @export

alpha96 <- function(k, vwap, vol, cl, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 10,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  base::pmax(
    FCor(
      al1_rank(vwap),
      al1_rank(vol),
      k[1]) %>%
    RcppRoll::roll_sumr(weights=1:k[2]) %>% 
    al1_roll_rank(k[3]),
    FCor(
      al1_roll_rank(cl, k[4]),
      al1_roll_rank(RcppRoll::roll_meanr(dvol, k[5]), k[6]),
      n=k[7]) %>%
    al1_whichmax(k[8]) %>%
    RcppRoll::roll_sumr(weights=1:k[9]) %>%
    al1_roll_rank(k[10])) %>%
 `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha97 <- function(k, vwap, lo, dvol) {
  # Argument tests:
  base::stopifnot(length(k) == 10,
    all(k[-1] %% 1 == 0),
    all(k[-1] > 0))

  # Evaulating the result:
  (((lo * k[1]) + (vwap * (1 - k[1]))) %>%
  al1_group_neut(IndClass.industry) %>%
  base::diff(k[2]) %>%
  RcppRoll::roll_sumr(weights=1:k[3]) %>%
  al1_rank) -

  (FCor(
    al1_roll_rank(lo, k[4]),
    al1_roll_rank(RcppRoll::roll_meanr(dvol, k[5]), k[6]),
    n=k[7]) %>%
  al1_roll_rank(k[8]) %>%
  RcppRoll::roll_sumr(weights=1:k[9]) %>%
  al1_roll_rank(k[10])) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha98 <- function(k, dvol, op) {
  # Argument tests:
  base::stopifnot(length(k) == 9,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  FCor(
    vwap,

    RcppRoll::roll_meanr(dvol, k[1]) %>%
    RcppRoll::roll_sumr(k[2]),

    k[3]) %>%
  RcppRoll::roll_sumr(weights=1:k[4]) %>%
  al1_rank %>%
  `-`(
    FCor(
      al1_rank(op),
      RcppRoll::roll_meanr(dvol, k[5]) %>% al1_rank,
      n=k[6]) %>%
    al1_whichmin(k[7]) %>%
    al1_roll_rank(k[8]) %>%
    RcppRoll::roll_sumr(weights=1:k[9]) %>%
  al1_rank)}


#' @inheritParams alpha_common_parameters
#' @export

alpha99 <- function(k, hi, lo, dvol, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 6,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  ((hi + lo) / k[1]) %>%
  RcppRoll::roll_sumr(k[2]) %>%
  FCor(
    RcppRoll::roll_sumr(RcppRoll::roll_meanr(dvol, k[3]), k[4]),
    k[5]) %>%
  al1_rank %>%
  `<`(
    FCor(lo, vol, k[6]) %>%
    al1_rank) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha100 <- function(k, cl, dvol, vol) {
  # Argument tests:
  base::stopifnot(length(k) == 5,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  ((((cl - lo) - (hi - cl)) / (hi - lo)) * vol) %>%
  al1_rank %>%
  al1_group_neut(IndClass.subindustry) %>%
  al1_group_neut(IndClass.subindustry) %>%
  al1_scale %>%
  `*`(k[1]) %>%
  `-`(
    FCor(
      cl,
      RcppRoll::roll_meanr(dvol, k[2]) %>%
      al1_rank,
      k[3]) %>%
    `-`(
      al1_whichmin(cl, k[4]) %>%
      al1_rank) %>%
    al1_group_neut(IndClass.subindustry) %>%
    al1_scale) %>%
  `*`(vol / RcppRoll::roll_meanr(dvol, k[5])) %>%
  `*`(-1)}


#' @inheritParams alpha_common_parameters
#' @export

alpha101 <- function(k, cl, op, hi, lo) {
  # Argument tests:
  base::stopifnot(length(k) == 0,
    all(k %% 1 == 0),
    all(k > 0))

  # Evaulating the result:
  (cl - op) / ((hi - lo) + k[1])}
