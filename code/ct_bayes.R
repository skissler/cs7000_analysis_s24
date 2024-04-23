
fig_ct_overall <- ct_dat  %>% 
	ggplot(aes(x=TestDateIndex, y=Ct)) + 
		geom_point(alpha=0.2) + 
		scale_y_reverse() + 
		theme_classic() + 
		labs(x="Days since highest recorded test", y="Ct")

fig_ct_indiv <- ct_dat %>% 
	filter(InfectionIndex<=24) %>% 
	ggplot(aes(x=TestDateIndex, y=Ct)) + 
		geom_point(size=0.1) + 
		geom_line(alpha=0.6) + 
		scale_y_reverse() + 
		theme_classic() + 
		labs(x="Days since highest recorded test", y="Ct") + 
		facet_wrap(~InfectionIndex, ncol=4)

ct_fit_overall <- stan(
	file="code/ct_fit_overall_2.stan",
	data=list(
		n = nrow(ct_dat),
		inf = ct_dat$InfectionIndex,
		n_inf = max(ct_dat$InfectionIndex),
		t = ct_dat$TestDateIndex,
		ydiff = 40 - ct_dat$Ct
		),
	iter=1000, chains=4)

ct_fit_overall_chains <- getchains(ct_fit_overall)
ct_fit_overall_draws <- getdraws(ct_fit_overall)

# temp <- makechainfig(ct_fit_overall, grepstr="tmax\\[5")
# temp <- makehists(ct_fit_overall, grepstr="clearance_mu")

postmeans_converted <- ct_fit_overall_draws %>% 
	filter(grepl("mu",parameter) | grepl("sigma",parameter)) %>% 
	filter(!grepl("measurement",parameter)) %>% 
	separate_wider_delim(parameter, delim="_", names=c("parameter","element")) %>% 
	pivot_wider(names_from=element, values_from=value) %>% 
	mutate(mean=exp(mu+sigma^2/2)) %>% 
	mutate(mean=case_when(parameter=="peak"~40-mean, TRUE~mean)) %>% 
	select(sample, parameter, value=mean)

postmeans_converted_summary <- postmeans_converted %>% 
	group_by(parameter) %>% 
	summarise(mean=mean(value), lwr=quantile(value, 0.025), upr=quantile(value, 0.975))


fig_postmeans_converted <- postmeans_converted %>% 
	ggplot(aes(x=mean)) + 
		geom_histogram(aes(y=after_stat(density)), fill="white", col="darkgrey") + 
		geom_density(adjust=2) + 
		facet_wrap(~parameter, scales="free", ncol=1) + 
		theme_classic()

postmeans_converted_summary_toplot <- postmeans_converted_summary %>% 
	select(parameter,mean) %>% 
	pivot_wider(names_from=parameter, values_from=mean)


tmax_mean <- ct_fit_overall_draws %>% 
	filter(grepl("tmax", parameter)) %>% 
	filter(!grepl("sd", parameter)) %>% 
	summarise(tmax_mean=mean(value)) %>% 
	pull(tmax_mean)


fig_ct_overall_fit <- fig_ct_overall + 
	geom_segment(data=postmeans_converted_summary_toplot,
		aes(x=-proliferation+tmax_mean, xend=tmax_mean, y=40, yend=peak), col="blue", alpha=0.6, linewidth=1) + 
	geom_segment(data=postmeans_converted_summary_toplot,
		aes(x=tmax_mean, xend=clearance+tmax_mean, y=peak, yend=40), col="blue", alpha=0.6, linewidth=1) 


# Look at some individual fits 
draws_indiv_df <- ct_fit_overall_draws %>% 
	filter(!grepl("\\_", parameter)) %>% 
	separate_wider_delim(parameter, delim=".", names=c("parameter","InfectionIndex")) %>% 
	mutate(InfectionIndex=as.numeric(InfectionIndex)) %>% 
	filter(sample<=100, InfectionIndex<=24) 


draws_indiv_df %>% 
	pivot_wider(names_from=parameter, values_from=value) %>% 
	ggplot() + 
		geom_segment(aes(x=-14, xend=-proliferation+tmax, y=40, yend=40), alpha=0.05, col="grey") + 
		geom_segment(aes(x=-proliferation+tmax, xend=tmax, y=40, yend=40-peak), alpha=0.05, col="grey") + 
		geom_segment(aes(x=tmax, xend=clearance+tmax, y=40-peak, yend=40), alpha=0.05, col="grey") + 
		geom_segment(aes(x=clearance+tmax, xend=14, y=40, yend=40), alpha=0.05, col="grey") + 
		geom_point(data=filter(ct_dat, InfectionIndex<=24), aes(x=TestDateIndex, y=Ct), size=0.2, col="black") + 
		theme_classic() + 
		scale_y_reverse() + 
		facet_wrap(~InfectionIndex, ncol=4, scales="free")




