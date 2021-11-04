#' Fit Sequence Data to Understand Fitness
#' 
#' @param sequence_data a tibble generated from the `fit_sequence` function
#' @param gt a numeric representing the generation time days
#' @export
#' 
fit_sequences <- function(sequence_data, gt = 4.7){
	require(nnet)
	require(lme4)
	require(splines)
	require(gamm4)
	require(emmeans)
	`%>%` <- dplyr::`%>%`
	
	fit <- nnet::multinom(VariantReduced ~ ns(t, df=2),
												data = as.data.frame(sequence_data %>%
																						 	dplyr::mutate(Total = ifelse(Total==0,.1,Total))),
												weights = Total)
	
	new_t <-max(sequence_data$t)+30
	
	dses <- expand.grid(VariantReduced = unique(sequence_data$VariantReduced),
											t = 1:new_t)
	out <- predict(fit, newdata = dses, "probs")
	
	out_with_t <- cbind(t = dses[,2],
											out) %>%
		unique()
	
	out_with_dates <- out_with_t %>%
		as.data.frame() %>%
		tibble::add_column(DateDT = seq.Date(min(sequence_data$UpdateDT),
																 length.out = new_t,by = "day")) %>%
		tidyr::gather(Variant,Perc, -t, -DateDT)
	
	
	r_variants  <- data.frame(confint(emtrends(fit, trt.vs.ctrl ~ VariantReduced|1, var="t",  mode="latent"), adjust="none", df=NA)$contrasts)[,-c(3,4)]
	rownames(r_variants) = r_variants[,"contrast"]
	
	R_variants <- exp(r_variants[,-1]*gt)
	
	R_variants$contrast <- rownames(r_variants)
	
	pred_2 <- data.frame(emmeans(fit,
															 ~ VariantReduced,
															 by=c("t"),
															 at=list(t=seq(1, max(sequence_data$t)+30, by=2)), # by=2 just to speed up things a bit
															 mode="prob", df=NA))
	
	return(list(out_with_dates = out_with_dates,
							r_variants = r_variants,
							predictions = pred_2,
							R_variants=R_variants))
	
	
}
