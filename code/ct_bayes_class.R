

# Histogram of highest measured viral loads by age group: 
ct_dat %>% 
	group_by(InfectionIndex) %>% 
	summarise(maxCt = min(Ct), AgeGrp=first(AgeGrp)) %>% 
	ggplot(aes(x=maxCt)) + 
		geom_histogram()  +
		theme_classic() + 
		theme(text=element_text(size=25)) + 
		facet_wrap(~AgeGrp, ncol=1)

# Scatter of Ct values over time: 
ct_dat %>% 
	ggplot(aes(x=TestDateIndex, y=Ct)) + 
		geom_point(alpha=0.4) + 
		theme_classic()  + 
		scale_y_reverse() + 
		theme(text=element_text(size=25)) 


ct_dat %>% 
	filter(InfectionIndex <= 24) %>% 
	ggplot(aes(x=TestDateIndex, y=Ct)) + 
		geom_point(size=0.2, alpha=0.4) + 
		geom_line(alpha=0.4) + 
		theme_classic()  + 
		scale_y_reverse() + 
		theme(text=element_text(size=25)) + 
		facet_wrap(~InfectionIndex, ncol=4)

stan(
	file="code/ct_bayes_class.stan", 
	data=____, 
	iter=1000, chains=4)


















