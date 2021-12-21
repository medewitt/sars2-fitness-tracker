#' Retrieve and Format Raw Data
#' @param state_use a string vector representing the state or states
#'     of interest
#' @returns  A tibble with 6 columns
#' @export

prep_sequences <- function(state_use = "North Carolina"){
	require(data.table)
	require(dplyr)
	`%>%` <- dplyr::`%>%`
	dat_url <- "https://raw.githubusercontent.com/conedatascience/cov2-variant-tracker/master/data/cdc-nc-data.csv"
	
	dat_raw <- data.table::fread(dat_url)
	
	names(dat_raw) <- c("UpdateDTS", "State", "Variant", "Perc", "N")
	
	dat_nc <- dat_raw[State %in% state_use][,Perc:=fifelse(is.na(Perc),0,Perc)]
	
	dat_nc[,Sequenced:= round(N * Perc)]
	
	
dat_nc[,VariantReduced := fcase(
  Variant == "B.1.1.7", "Alpha",
  Variant == "B.1.351", "Beta",
  Variant == "P.1", "Gamma",
  Variant == "B.1.617.2", "Delta",
  Variant == "AY.1", "Delta",
  Variant == "AY.2", "Delta-Plus",
  Variant == "AY.3", "Delta",
  Variant == "B.1.427/B.1.429", "Epsilon",
  Variant == "B.1.621", "Mu",
  Variant == "B.1.1.529", "Omicron",
  Variant == "Other", "Other lineages",
  default = "Other lineages"
)]
	
	dat_nc[,VariantReduced:=factor(VariantReduced, c("Other lineages", "Alpha", "Beta","Delta", "Delta-Plus","Gamma", "Epsilon", "Mu", "Omicron"))]
	
	dat_nc_red <- dat_nc[,.(Total = sum(Sequenced)), by = c("UpdateDTS", "VariantReduced")]
	
	
	dat_nc_red[,Perc:= Total/sum(Total), by = c("UpdateDTS")]
	
	dat_unique <- dat_nc_red %>%
		dplyr::group_by(VariantReduced,Perc) %>%
		dplyr::slice(1) %>%
		dplyr::ungroup() %>%
		tidyr::complete(UpdateDTS, VariantReduced,fill = list(Total = 0, Perc = 0)) %>%
		dplyr::arrange(UpdateDTS) %>%
		dplyr::mutate(UpdateDT = lubridate::date(UpdateDTS)) %>%
		dplyr::group_by(VariantReduced) %>%
		dplyr::mutate(t = as.numeric(UpdateDT - lag(UpdateDT,1))) %>%
		dplyr::mutate(t = ifelse(is.na(t),1,t)) %>%
		dplyr::mutate(t = cumsum(t))
	
	return(dat_unique)
}
