
# Run a basic linear regression: ----------------------------------------------

loblolly_basic <- stan(
	file="code/loblolly_basic.stan",
	data=list(
		n=nrow(loblolly), 
		age=loblolly$Age, 
		height=loblolly$Height), 
	iter=1000, chains=4)

loblolly_basic_chains <- getchains(loblolly_basic) 
loblolly_basic_draws <- getdraws(loblolly_basic)
loblolly_basic_summaries <- getsummaries(loblolly_basic) 

fig_loblolly_basic_chains <- makechainfig(loblolly_basic)
fig_loblolly_basic_hists <- makehists(loblolly_basic)


post_trajectories_basic <- loblolly_basic_draws %>% 
	filter(parameter %in% c("b0","b1")) %>% 
	pivot_wider(names_from=parameter, values_from=value) %>% 
	filter(sample<=100) 

fig_postfit_basic <- loblolly %>% 
	ggplot(aes(x=Age, y=Height)) + 
		geom_point() + 
		geom_segment(data=post_trajectories_basic, aes(x=0, y=b0, xend=30, yend=b0+30*b1), col="grey", alpha=0.1) + 
		theme_classic() 

# Try a parabolic relationship: -----------------------------------------------

loblolly_pba <- stan(
	file="code/loblolly_pba.stan",
	data=list(
		n=nrow(loblolly), 
		age=loblolly$Age, 
		height=loblolly$Height), 
	iter=1000, chains=4)

loblolly_pba_chains <- getchains(loblolly_pba) 
loblolly_pba_draws <- getdraws(loblolly_pba)
loblolly_pba_summaries <- getsummaries(loblolly_pba) 

fig_loblolly_pba_chains <- makechainfig(loblolly_pba)
fig_loblolly_pba_hists <- makehists(loblolly_pba)

xvals <- seq(from=0, to=30, by=0.1) 
post_trajectories_pba <- loblolly_pba_draws %>% 
	filter(parameter %in% c("b0","b1","b2")) %>% 
	pivot_wider(names_from=parameter, values_from=value) %>% 
	filter(sample<=100) %>% 
	split(.$sample) %>% 
	map(~ .$b0 + (.$b1)*xvals + (.$b2)*xvals^2) %>% 
	map(~ tibble(Age=xvals, Height=.)) %>% 
	bind_rows(.id="sample")

fig_postfit_pba <- loblolly %>% 
	ggplot(aes(x=Age, y=Height)) + 
		geom_point() + 
		geom_line(data=post_trajectories_pba, aes(x=Age, y=Height, fill=sample), col="grey", alpha=0.1) + 
		theme_classic() 

# Try a parabolic relationship with individual effects: -----------------------

seedmap <- tibble(Seed=unique(loblolly$Seed)) %>% 
	mutate(index=1:n())
seedindex <- loblolly %>% left_join(seedmap, by="Seed") %>% pull(index)

loblolly_indiv <- stan(
	file="code/loblolly_indiv.stan",
	data=list(
		n=nrow(loblolly),
		n_seeds=length(unique(loblolly$Seed)), 
		seedindex=seedindex,
		age=loblolly$Age, 
		height=loblolly$Height), 
	iter=1000, chains=4)

loblolly_indiv_chains <- getchains(loblolly_indiv) 
loblolly_indiv_draws <- getdraws(loblolly_indiv)
loblolly_indiv_summaries <- getsummaries(loblolly_indiv) 

fig_loblolly_indiv_chains <- makechainfig(loblolly_indiv, "\\[1\\]")
fig_loblolly_indiv_hists <- makehists(loblolly_indiv, "\\.1$")

xvals <- seq(from=0, to=30, by=0.1) 
post_means_indiv <- loblolly_indiv_draws %>% 
	filter(grepl("^b0", parameter) | grepl("^b1", parameter) | grepl("^b2", parameter)) %>% 
	separate_wider_delim(parameter, delim=".", names=c("parameter","seed")) %>%
	pivot_wider(names_from=parameter, values_from=value) %>% 
	group_by(seed) %>% 
	summarise(b0=mean(b0), b1=mean(b1), b2=mean(b2)) %>% 
	split(.$seed) %>% 
	map(~ .$b0 + (.$b1)*xvals + (.$b2)*xvals^2) %>% 
	map(~ tibble(Age=xvals, Height=.)) %>% 
	bind_rows(.id="index") %>% 
	mutate(index=as.numeric(index)) %>% 
	left_join(seedmap, by="index") 

fig_postfit_indiv <- loblolly %>% 
	ggplot(aes(x=Age, y=Height, col=Seed)) + 
		geom_point() + 
		geom_line(data=post_means_indiv, aes(x=Age, y=Height, col=Seed), alpha=0.4) + 
		theme_classic() 

