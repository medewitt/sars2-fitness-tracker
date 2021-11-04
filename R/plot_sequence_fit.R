
plot_sequences <- function(dat){
	require(ggplot2)
	p <- dat %>%
		filter(DateDT >= as.Date("2021-07-01")) %>%
		ggplot(aes(DateDT, Perc, colour = Variant))+
		#geom_line(aes(linetype = ifelse(DateDT<max(dat_unique$UpdateDT), "dashed", "solid")))+
		geom_line()+
		geom_vline(xintercept = Sys.Date(), color = "orange")+
		labs(
			title = "Predicted Variant Trends for North Carolina",
			caption = "Using CDC Sequenced Variant Data",
			x = NULL,
			y = "Percentage of Cases"
		)+
		theme_bw()+
		ggrepel::geom_label_repel(data = . %>%
																dplyr::filter(DateDT == max(DateDT)),
															aes(label = Variant))+
		ggrepel::geom_label_repel(data = . %>%
																dplyr::filter(DateDT == Sys.Date()&
																								Variant == "Delta"),
															aes(label = scales::percent(Perc)))+
		scale_y_continuous(labels = scales::percent)+
		theme(legend.position = "none"
		)+
		theme(panel.grid.minor = element_blank(),
					panel.grid.major.x = element_blank(),
					axis.line.x = element_line(colour = "black"),
					panel.grid.major.y = element_line(size = rel(1.2)))
	
	return(p)
}

