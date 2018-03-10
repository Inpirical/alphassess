# Data =========================================================================

#' Convert historical stock price and volume data to long format.
#' @param d (data frame) price and volume data from `quantmod::getSymbols` call.
#' @export

# Arguments: `sym` (character) ticker symbol to query.
# Value: (data frame) date, sym, var, value

sym_data_tidy <- function(d) {
  # Converting the rownames (dates) to an explicit column:
  tibble::rownames_to_column(d, var="obs_date") %>%
#  # Adding stock ticker symbol as an explicit column:
#  dplyr::mutate(sym=stringr::str_extract(colnames(.)) %>%
  # Converting the observation date column to Date class:
  dplyr::mutate_at("obs_date", lubridate::ymd) %T>%
  # Removing stock ticker symbol from column names and making lower case:
  {colnames(.) %<>%
    stringr::str_replace_all(c("^.*\\."="")) %>%
    stringr::str_to_lower()} %>%
  # Renaming the variable columns to shorthand:
  dplyr::rename(op=open, hi=high, lo=low, cl=close, vol=volume,
    adj=adjusted) %>%
  # Converting data from wide to long format (grouping by date and symbol):
  #reshape2::melt(id.vars=c("obs_date", "sym")) %>%
  # Converting to a `tbl_df` class data frame:
  dplyr::tbl_df()
}


#' Get price and volume data on several ticker symbols in one long data frame.
#' @param syms (character) ticker symbol(s) to process
#' @param ... further arguments passed to `quantmod::getSymbols`
#' @export

syms_data_get <- function(syms=test_syms, ...) {
  setNames(as.list(syms), syms) %>%
  plyr::ldply(
    .data=.,
    .id="sym",
    .progress="text",
    .fun=function(s) {
      tryCatch(error=function(e) {NULL},
      # Getting the financial data on the symbol:
      quantmod::getSymbols(
        Symbols=s,
        return.class="data.frame",
        auto.assign=FALSE,
        src="yahoo",
        ...) %>%
      # Converting the data to long form:
      sym_data_tidy)})  %>%
  # Converting to a `tbl_df` class data frame:
  dplyr::tbl_df()
}


#' Convert a data frame with historical price data to a list of xts matrices.
#'
#' @param d (data frame) historical price data from `syms_data_get`
#' @return (named list) each entry an xts time-series matrix.
#' @export

syms_data_xts <- function(d) {
  # Applying xts conversion to every non-key column name:
  colnames(d) %>%
  setdiff(c("sym", "obs_date")) %>%
  setNames(as.list(.), .) %>%
  plyr::llply(.progress="text",
    .fun=function(v) {
      dplyr::select(.data=d, "sym", "obs_date", v) %>%
      dplyr::rename_("v"=v) %>%
      # Converting the data to wide format:
      reshape2::dcast(obs_date ~ sym, value.var="v") %>%
      # Converting the data from data frame to xts matrix:
      {xts::xts(x=select(., -obs_date), order.by=.$obs_date)}
    })
}


GetVWAPData <- function(in_dir, in_file) {

  symbol <- str_extract(in_file, "(?<=/?)[^/]*(?=\\.csv$)")

  file.path(in_dir, in_file) %>%
  read.csv %>%
  mutate_each(funs(as.Date), date) %>%
  select(date, o=Open, h=High, l=Low, k=PX_Last, vwap=PR094, v=PX_VOLUME) %>% 
  mutate(sym=symbol) %>%
  tbl_df
}


GetAllVWAPS <- function(in_dir) {

  list.files(in_dir) %>%
  ldply(GetVWAPData, in_dir=in_dir) %>%
  # convert to long form from wide using `reshape2::melt`:
  melt(id.vars=c("date", "sym")) %>%
  rename(var=variable, val=value) %>%
  tbl_df
}


ParseSectors <- function(in_dir, in_file=sectors_file, syms=colnames(e[[1]])) {
  # Parse out the sector, industry and subindustry data from Rds file:
  file.path(in_dir, in_file) %>%
  readRDS %>%
  mutate_each(funs(unlist), sector, industry, subindustry) %>%
  tbl_df %>%
  .[match(syms, .$sym),]
}
