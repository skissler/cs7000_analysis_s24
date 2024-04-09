# =============================================================================
# What are the mean petal lengths by species? 
# =============================================================================

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
iris_petallengths_lp <- getlp(iris_petallengths) 

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

# =============================================================================
# What are the mean petal widths by species? 
# =============================================================================

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
iris_petalwidths_lp <- getlp(iris_petalwidths) 

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

# =============================================================================
# Some plotting 
# =============================================================================

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


bestdraws <- iris_petallengths_lp %>% 
	left_join(iris_petalwidths_lp, by=c("sample"), suffix=c(".len",".wid")) %>% 
	mutate(lp=value.len + value.wid) %>% 
	arrange(desc(lp)) %>% 
	slice(1:1900)  %>% 
	pull(sample) %>% 
	sort()


iris_draws_clean <- bind_rows(iris_petalwidths_draws, iris_petallengths_draws) %>% 
	# filter(sample %in% bestdraws) %>% 
	filter(grepl("mean",parameter)) %>% 
	separate_wider_delim(parameter, delim=".", names=c("parameter","speciesindex")) %>% 
	mutate(Species=case_when(
		speciesindex==1~"setosa",
		speciesindex==2~"versicolor",
		speciesindex==3~"virginica"
		)) %>% 
	separate_wider_delim(parameter,delim="_", names=c("Metric","Statistic")) %>%
	mutate(Metric=case_when(Metric=="length"~"Length",Metric=="width"~"Width")) %>% 
	select(sample, Species, Metric, Value=value) %>% 
	pivot_wider(names_from=Metric, values_from=Value)

fig_pointswithmeans_ellipse <- fig_pointswithmeans + 
	stat_ellipse(data=iris_draws_clean, aes(x=Width, y=Length, col=Species)) 

predbound_df <- bind_rows(iris_petalwidths_summaries, iris_petallengths_summaries) %>% 
	separate_wider_delim(parameter,delim=".",names=c("parameter","speciesindex")) %>% 
	mutate(Species=case_when(
		speciesindex==1~"setosa",
		speciesindex==2~"versicolor",
		speciesindex==3~"virginica"
		)) %>% 
	select(-speciesindex) %>% 
	separate_wider_delim(parameter,delim="_",names=c("Metric","Statistic")) %>% 
	select(Species, Metric, Statistic, mean) %>% 
	pivot_wider(names_from=Statistic, values_from=mean) %>% 
	split(.$Species) %>% 
	map(~ as_tibble(as.data.frame(rmvnorm(n=1000, mean=.$mean, sigma=diag(.$sd))))
		) %>% 
	map(~ rename(., Width=V1, Length=V2)) %>% 
	bind_rows(.id="Species") 

fig_predint <- fig_pointswithmeans + 
	stat_ellipse(data=predbound_df, aes(x=Width, y=Length, col=Species))


# =============================================================================
# Try a full multivariate normal fit
# =============================================================================

# Generate a reduced dataset containing just the petal lengths: 
iris_ex3 <- iris %>% 
	filter(Structure=="Petal") %>% 
	pivot_wider(names_from=Metric, values_from=Value) %>% 
	mutate(SpeciesIndex=case_when(
		Species=="setosa"~1,
		Species=="versicolor"~2,
		Species=="virginica"~3)) %>% 
	(function(x){
		out <- list(
			n=nrow(x),
			n_species=length(unique(x$Species)),
			species=x$SpeciesIndex,
			measurement=select(x,Length,Width) %>% mutate(id=1:n()) %>% split(.$id) %>% map(~ c(.$Width, .$Length))
			)
		return(out)
	})

# Sample from the Stan model: 
iris_petalLW <- stan(
	file="code/iris_petalLW.stan",
	data=list(
		n=iris_ex3$n, 
		n_species=iris_ex3$n_species, 
		species=iris_ex3$species,
		measurement=iris_ex3$measurement), 
	iter=1000, chains=4)


iris_petalLW_chains <- getchains(iris_petalLW) 
iris_petalLW_draws <- getdraws(iris_petalLW)
iris_petalLW_summaries <- getsummaries(iris_petalLW) 
iris_petalLW_lp <- getlp(iris_petalLW) 

# Plot the ordered samples (chains) and histograms of the posterior distributions for the parameters: 
fig_iris_petalLW_chains <- makechainfig(iris_petalLW, grepstr="mu")
fig_iris_petalLW_hists <- makehists(iris_petalLW, grepstr="mu", scalesval="fixed")

postmean_df <- iris_petalLW_draws %>% 
	filter(grepl("mu",parameter)) %>% 
	separate_wider_delim(parameter, delim=".", names=c("parameter","speciesindex","wl")) %>% 
	mutate(Species=case_when(
		speciesindex==1~"setosa",
		speciesindex==2~"versicolor",
		speciesindex==3~"virginica"
		)) %>% 
	select(-speciesindex) %>% 
	mutate(Metric=case_when(wl==1~"Width", TRUE~"Length")) %>% 
	select(sample, Species, Metric, value) %>% 
	pivot_wider(names_from=Metric, values_from=value)


fig_postmean_mvn <- iris %>% 
	filter(Structure=="Petal") %>% 
	select(Species, Flower, Metric, Value) %>% 
	pivot_wider(names_from=Metric, values_from=Value) %>% 
	ggplot() + 
		geom_point(aes(x=Width, y=Length, col=Species), alpha=0.4) + 
		theme_classic() + 
		scale_color_manual(values=c("setosa"="black","versicolor"="blue","virginica"="red"))  + 
		stat_ellipse(data=postmean_df, aes(x=Width, y=Length, col=Species))


postpred_list <- iris_petalLW_summaries %>% 
	select(parameter, mean) %>% 
	separate_wider_delim(parameter, delim=".", names=c("parameter","speciesindex","row","col"), too_few="align_start") %>% 
	mutate(Species=case_when(
		speciesindex==1~"setosa",
		speciesindex==2~"versicolor",
		speciesindex==3~"virginica"
		)) %>% 
	select(-speciesindex) %>% 
	split(.$parameter) %>% 
	map(~ split(., .$Species)) %>% 
	map(~ map(., ~ arrange(., row, col))) %>% 
	map(~ map(., ~ matrix(.$mean, byrow=TRUE, ncol=2)))


postpred_draws_setosa <- as_tibble(as.data.frame(rmvnorm(n=10000, mean=postpred_list$mu$setosa, sigma=postpred_list$sigma$setosa))) %>% 
	rename(Width=V1, Length=V2) %>% 
	mutate(Species="setosa")

postpred_draws_versicolor <- as_tibble(as.data.frame(rmvnorm(n=10000, mean=postpred_list$mu$versicolor, sigma=postpred_list$sigma$versicolor))) %>% 
	rename(Width=V1, Length=V2) %>% 
	mutate(Species="versicolor")

postpred_draws_virginica <- as_tibble(as.data.frame(rmvnorm(n=10000, mean=postpred_list$mu$virginica, sigma=postpred_list$sigma$virginica))) %>% 
	rename(Width=V1, Length=V2) %>% 
	mutate(Species="virginica")


postpred_draws <- bind_rows(postpred_draws_setosa, postpred_draws_versicolor, postpred_draws_virginica)

fig_postpred_mvn <- iris %>% 
	filter(Structure=="Petal") %>% 
	select(Species, Flower, Metric, Value) %>% 
	pivot_wider(names_from=Metric, values_from=Value) %>% 
	ggplot() + 
		geom_point(aes(x=Width, y=Length, col=Species), alpha=0.4) + 
		theme_classic() + 
		scale_color_manual(values=c("setosa"="black","versicolor"="blue","virginica"="red"))  + 
		stat_ellipse(data=postpred_draws, aes(x=Width, y=Length, col=Species))

# Are the lengths and widths correlated? --------------------------------------

post_corr_df <- iris_petalLW_draws %>% 
	filter(grepl("sigma", parameter)) %>% 
	separate_wider_delim(parameter,delim=".",names=c("parameter","speciesindex","row","col")) %>% 
	mutate(
		covstat=case_when(
			row==1 & col==1 ~"var1",
			row==2 & col==2 ~"var2",
			row==1 & col==2 ~ "cov", 
			TRUE~"cut"
			)) %>% 
	filter(covstat!="cut") %>% 
	select(sample, speciesindex, covstat, value) %>% 
	mutate(Species=case_when(
		speciesindex==1~"setosa",
		speciesindex==2~"versicolor",
		speciesindex==3~"virginica"
		)) %>% 
	select(-speciesindex) %>% 
	pivot_wider(names_from=covstat, values_from=value) %>% 
	mutate(corr=cov/sqrt(var1*var2))

fig_post_corr <- post_corr_df %>% 
	ggplot(aes(x=corr)) + 
		geom_histogram(aes(y=after_stat(density)), fill="white", col="darkgrey") + 
		geom_density(adjust=2) + 
		facet_wrap(~Species, ncol=1) + 
		theme_classic() 

