
ct_dat_means <- ct_dat %>% 
	group_by(TestDateIndex) %>% 
	summarise(Ct=mean(Ct))

fig_ct_dat_scatter <- ct_dat %>% 
	ggplot(aes(x=TestDateIndex, y=Ct)) + 
		geom_point(alpha=0.2) + 
		geom_line(data=ct_dat_means, col="blue", linewidth=1, alpha=0.6) + 
		scale_y_reverse() + 
		theme_classic() 


fig_ct_dat_scatter_byage <- ct_dat %>% 
	ggplot(aes(x=TestDateIndex, y=Ct)) + 
		geom_point(alpha=0.2) + 
		scale_y_reverse() + 
		theme_classic() + 
		facet_wrap(~AgeGrp) 

fig_ct_dat_scatter_indiv <- ct_dat %>% 
	filter(InfectionIndex <= 24) %>% 
	ggplot(aes(x=TestDateIndex, y=Ct)) + 
		geom_point(size=0.2, alpha=0.6) + 
		geom_line(alpha=0.6) + 
		scale_y_reverse() + 
		theme_classic() + 
		facet_wrap(~InfectionIndex, nrow=6)