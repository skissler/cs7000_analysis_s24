# ==============================================================================
# What are the mean petal lengths by species? 
# ==============================================================================

# Generate a reduced dataset containing just the petal lengths: 
iris_ex1 <- iris %>% 
	filter(Structure=="Petal" & Metric=="Length") %>% 
	mutate(SpeciesIndex=case_when(
		Species=="setosa"~1,
		Species=="versicolor"~2,
		Species=="virginica"~3))

# Sample from the Stan model: 
iris_petallengths <- stan(
	file="code/iris_petallengths.stan",
	data=list(
		n=nrow(iris_ex1), 
		n_species=length(unique(iris_ex1$SpeciesIndex)), 
		species=iris_ex1$SpeciesIndex,
		petallength=iris_ex1$Value), 
	iter=1000, chains=4)

iris_petallengths_chains <- getchains(iris_petallengths) 
iris_petallengths_draws <- getdraws(iris_petallengths)
iris_petallengths_summaries <- getsummaries(iris_petallengths) 

# Plot the ordered samples (chains) and histograms of the posterior distributions for the parameters: 
fig_iris_petallengths_chains <- makechainfig(iris_petallengths)
fig_iris_petallengths_hists <- makehists(iris_petallengths, grepstr="mean", scalesval="fixed")

fig_postfit_iris_petallengths <- iris_petallengths_draws %>% 
	filter(grepl("mean",parameter)) %>% 
	separate_wider_delim(parameter, delim=".", names=c("parameter","speciesindex")) %>% 
	mutate(Species=case_when(
		speciesindex==1~"setosa",
		speciesindex==2~"versicolor",
		speciesindex==3~"virginica"
		)) %>% 
	select(sample, Species, Value=value) %>% 
	ggplot(aes(x=Value)) + 
		geom_histogram(aes(y=after_stat(density)), bins=100, fill="white", col="darkgrey") + 
		geom_density(adjust=2) + 
		geom_point(data=iris_ex1, aes(x=Value, y=1), alpha=0.2) + 
		theme_classic() + 
		facet_wrap(~Species, ncol=1) + 
		labs(x="Length", y=element_blank())

# ==============================================================================
# What are the mean petal widths by species? 
# ==============================================================================

# Generate a reduced dataset containing just the petal lengths: 
iris_ex2 <- iris %>% 
	filter(Structure=="Petal" & Metric=="Width") %>% 
	mutate(SpeciesIndex=case_when(
		Species=="setosa"~1,
		Species=="versicolor"~2,
		Species=="virginica"~3))

# Sample from the Stan model: 
iris_petalwidths <- stan(
	file="code/iris_petalwidths.stan",
	data=list(
		n=nrow(iris_ex2), 
		n_species=length(unique(iris_ex2$SpeciesIndex)), 
		species=iris_ex2$SpeciesIndex,
		petalwidth=iris_ex2$Value), 
	iter=1000, chains=4)

iris_petalwidths_chains <- getchains(iris_petalwidths) 
iris_petalwidths_draws <- getdraws(iris_petalwidths)
iris_petalwidths_summaries <- getsummaries(iris_petalwidths) 

# Plot the ordered samples (chains) and histograms of the posterior distributions for the parameters: 
fig_iris_petalwidths_chains <- makechainfig(iris_petalwidths)
fig_iris_petalwidths_hists <- makehists(iris_petalwidths, grepstr="mean", scalesval="fixed")

fig_postfit_iris_petalwidths <- iris_petalwidths_draws %>% 
	filter(grepl("mean",parameter)) %>% 
	separate_wider_delim(parameter, delim=".", names=c("parameter","speciesindex")) %>% 
	mutate(Species=case_when(
		speciesindex==1~"setosa",
		speciesindex==2~"versicolor",
		speciesindex==3~"virginica"
		)) %>% 
	select(sample, Species, Value=value) %>% 
	ggplot(aes(x=Value)) + 
		geom_histogram(aes(y=after_stat(density)), bins=100, fill="white", col="darkgrey") + 
		geom_density(adjust=2) + 
		geom_point(data=iris_ex2, aes(x=Value, y=1), alpha=0.2) + 
		theme_classic() + 
		facet_wrap(~Species, ncol=1) + 
		labs(x="Width", y=element_blank())

# ==============================================================================
# Some plotting 
# ==============================================================================

# Plot raw means and credible intervals: 
combined_summaries <- bind_rows(iris_petallengths_summaries, iris_petalwidths_summaries) %>% 
	separate_wider_delim(parameter, delim=".", names=c("parameter","speciesindex")) %>% 
	mutate(Species=case_when(
		speciesindex==1~"setosa",
		speciesindex==2~"versicolor",
		speciesindex==3~"virginica"
		)) %>% 
	separate_wider_delim(parameter,delim="_", names=c("Metric","Statistic")) %>%
	mutate(Metric=case_when(Metric=="length"~"Length",Metric=="width"~"Width"))%>% 
	select(Species, Metric, Statistic, mean, lwr, upr)

fig_pointswithmeans <- iris %>% 
	filter(Structure=="Petal") %>% 
	select(Species, Flower, Metric, Value) %>% 
	pivot_wider(names_from=Metric, values_from=Value) %>% 
	ggplot() + 
		geom_point(aes(x=Width, y=Length, col=Species), alpha=0.4) + 
		geom_vline(data=filter(combined_summaries, Metric=="Width" & Statistic=="mean"), aes(xintercept=mean, col=Species), alpha=0.4) + 
		geom_hline(data=filter(combined_summaries, Metric=="Length" & Statistic=="mean"), aes(yintercept=mean, col=Species), alpha=0.4) + 
		geom_rect(data=filter(combined_summaries, Metric=="Width" & Statistic=="mean"), aes(xmin=lwr, xmax=upr, ymin=-Inf, ymax=Inf, fill=Species), alpha=0.1) + 
		geom_rect(data=filter(combined_summaries, Metric=="Length" & Statistic=="mean"), aes(ymin=lwr, ymax=upr, xmin=-Inf, xmax=Inf, fill=Species), alpha=0.1) + 
		theme_classic() + 
		scale_color_manual(values=c("setosa"="black","versicolor"="blue","virginica"="red")) + 
		scale_fill_manual(values=c("setosa"="black","versicolor"="blue","virginica"="red")) 

# Plot prediction intervals: 


