# =============================================================================
# Run a basic linear regression
# =============================================================================

# Sample from the Stan model: 
loblolly_basic <- stan(
	file="code/loblolly_basic.stan",
	data=list(
		n=nrow(loblolly), 
		age=loblolly$Age, 
		height=loblolly$Height), 
	iter=1000, chains=4)

# Extract the ordered samples (chains), the permuted samples without burn-in (draws), and the posterior means and credible intervals for the parameters: 
loblolly_basic_chains <- getchains(loblolly_basic) 
loblolly_basic_draws <- getdraws(loblolly_basic)
loblolly_basic_summaries <- getsummaries(loblolly_basic) 

# Plot the ordered samples (chains) and histograms of the posterior distributions for the parameters: 
fig_loblolly_basic_chains <- makechainfig(loblolly_basic)
fig_loblolly_basic_hists <- makehists(loblolly_basic)

# Generate a data frame for plotting the posterior fits over the data:
post_trajectories_basic <- loblolly_basic_draws %>% 
	filter(parameter %in% c("b0","b1")) %>% 
	pivot_wider(names_from=parameter, values_from=value) %>% 
	filter(sample<=100) 

# Plot the posterior fits over the data: 
fig_postfit_basic <- loblolly %>% 
	ggplot(aes(x=Age, y=Height)) + 
		geom_point() + 
		geom_smooth(method="lm") + 
		geom_segment(data=post_trajectories_basic, aes(x=0, y=b0, xend=30, yend=b0+30*b1), col="grey", alpha=0.1) + 
		theme_classic() + 
		theme(text=element_text(size=40))

# =============================================================================
# Try a parabolic relationship
# =============================================================================

# Sample from the Stan model: 
loblolly_pba <- stan(
	file="code/loblolly_pba.stan",
	data=list(
		n=nrow(loblolly), 
		age=loblolly$Age, 
		height=loblolly$Height), 
	iter=1000, chains=4)

# Extract the ordered samples (chains), the permuted samples without burn-in (draws), and the posterior means and credible intervals for the parameters: 
loblolly_pba_chains <- getchains(loblolly_pba) 
loblolly_pba_draws <- getdraws(loblolly_pba)
loblolly_pba_summaries <- getsummaries(loblolly_pba) 

# Plot the ordered samples (chains) and histograms of the posterior distributions for the parameters: 
fig_loblolly_pba_chains <- makechainfig(loblolly_pba)
fig_loblolly_pba_hists <- makehists(loblolly_pba)

# Generate a data frame for plotting the posterior fits over the data:
xvals <- seq(from=0, to=30, by=0.1) 
post_trajectories_pba <- loblolly_pba_draws %>% 
	filter(parameter %in% c("b0","b1","b2")) %>% 
	pivot_wider(names_from=parameter, values_from=value) %>% 
	filter(sample<=100) %>% 
	split(.$sample) %>% 
	map(~ .$b0 + (.$b1)*xvals + (.$b2)*xvals^2) %>% 
	map(~ tibble(Age=xvals, Height=.)) %>% 
	bind_rows(.id="sample")

# Plot the posterior fits over the data: 
fig_postfit_pba <- loblolly %>% 
	ggplot(aes(x=Age, y=Height)) + 
		geom_point() + 
		geom_line(data=post_trajectories_pba, aes(x=Age, y=Height, fill=sample), col="grey", alpha=0.1) + 
		theme_classic()  + 
		theme(text=element_text(size=40))

# =============================================================================
# Try a parabolic relationship with individual effects
# =============================================================================

# Map the seed numbers to a seed index (1:n): 
seedmap <- tibble(Seed=unique(loblolly$Seed)) %>% 
	mutate(index=1:n())
seedindex <- loblolly %>% left_join(seedmap, by="Seed") %>% pull(index)

# Sample from the Stan model: 
loblolly_indiv <- stan(
	file="code/loblolly_indiv.stan",
	data=list(
		n=nrow(loblolly),
		n_seeds=length(unique(loblolly$Seed)), 
		seedindex=seedindex,
		age=loblolly$Age, 
		height=loblolly$Height), 
	iter=1000, chains=4)

# Extract the ordered samples (chains), the permuted samples without burn-in (draws), and the posterior means and credible intervals for the parameters: 
loblolly_indiv_chains <- getchains(loblolly_indiv) 
loblolly_indiv_draws <- getdraws(loblolly_indiv)
loblolly_indiv_summaries <- getsummaries(loblolly_indiv) 

# Plot the ordered samples (chains) and histograms of the posterior distributions for the parameters: 
fig_loblolly_indiv_chains <- makechainfig(loblolly_indiv, "\\[1\\]")
fig_loblolly_indiv_hists <- makehists(loblolly_indiv, "\\.1$")

# Generate a data frame for plotting the posterior means over the data:
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

# Plot the posterior means over the data: 
fig_postfit_indiv <- loblolly %>% 
	ggplot(aes(x=Age, y=Height, col=Seed)) + 
		geom_point() + 
		geom_line(data=post_means_indiv, aes(x=Age, y=Height, col=Seed), alpha=0.4) + 
		theme_classic() 

# =============================================================================
# Try a hierarchical parabolic relationship with individual effects
# =============================================================================

# Sample from the Stan model: 
loblolly_hierarchical <- stan(
	file="code/loblolly_hierarchical.stan",
	data=list(
		n=nrow(loblolly),
		n_seeds=length(unique(loblolly$Seed)), 
		seedindex=seedindex,
		age=loblolly$Age, 
		height=loblolly$Height), 
	iter=4000, chains=4)

# Extract the ordered samples (chains), the permuted samples without burn-in (draws), and the posterior means and credible intervals for the parameters: 
loblolly_hierarchical_chains <- getchains(loblolly_hierarchical) 
loblolly_hierarchical_draws <- getdraws(loblolly_hierarchical)
loblolly_hierarchical_summaries <- getsummaries(loblolly_hierarchical) 

# Plot the ordered samples (chains) and histograms of the posterior distributions for the parameters: 
fig_loblolly_hierarchical_chains <- makechainfig(loblolly_hierarchical, "\\[1\\]")
fig_loblolly_hierarchical_hists <- makehists(loblolly_hierarchical, "\\.1$")

fig_loblolly_hierarchical_chains_overall <- makechainfig(loblolly_hierarchical, "overall")
fig_loblolly_hierarchical_hists_overall <- makehists(loblolly_hierarchical, "overall")

makehists(loblolly_hierarchical, "^b1.", scalesval="fixed")
makehists(loblolly_hierarchical, "^eta1.", scalesval="fixed")

# Generate a data frame for plotting the seed-level posterior means over the data:
xvals <- seq(from=0, to=30, by=0.1) 
post_means_hierarchical <- loblolly_hierarchical_draws %>% 
	filter(grepl("^b0", parameter) | grepl("^b1", parameter) | grepl("^b2", parameter)) %>% 
	filter(!grepl("overall", parameter)) %>% 
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

# Generate a data frame for plotting the overall posterior mean over the data:
post_means_hierarchical_overall <- loblolly_hierarchical_draws %>% 
	filter(grepl("overall", parameter)) %>% 
	pivot_wider(names_from=parameter, values_from=value) %>% 
	summarise(b0=mean(b0_overall), b1=mean(b1_overall), b2=mean(b2_overall)) %>% 
	mutate(seed=0) %>% 
	split(.$seed) %>% 
	map(~ .$b0 + (.$b1)*xvals + (.$b2)*xvals^2) %>% 
	map(~ tibble(Age=xvals, Height=.)) %>% 
	bind_rows(.id="index") %>% 
	mutate(index=as.numeric(index)) %>% 
	left_join(seedmap, by="index") 

# Plot the posterior means over the data: 
fig_postfit_hierarchical <- loblolly %>% 
	ggplot(aes(x=Age, y=Height, col=Seed)) + 
		geom_point() + 
		geom_line(data=post_means_hierarchical, aes(x=Age, y=Height, col=Seed), alpha=0.4) + 
		geom_line(data=post_means_hierarchical_overall, aes(x=Age, y=Height), col="black", alpha=0.8, linewidth=1, linetype="dashed") + 
		theme_classic() 

# =============================================================================
# Try a hierarchical 1-exp(-x) relationship with individual effects
# =============================================================================


























