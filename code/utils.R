
getchains <- function(obj){
	out <- as_tibble(extract(obj, permuted=FALSE, inc_warmup=TRUE)) %>% 
		mutate(iteration=1:n()) %>% 
		pivot_longer(-iteration) %>% 
		separate_wider_delim(name, delim=":", names=c("drop","keep")) %>% 
		select(-drop) %>% 
		separate_wider_delim(keep, delim=".", names=c("chain","parameter")) %>% 
		select(iteration, chain, parameter, value) %>% 
		filter(parameter!="lp__")
	return(out)
}

getdraws <- function(obj){
	out <- as_tibble(as.data.frame(extract(obj))) %>% 
		select(-"lp__") %>% 
		mutate(sample=1:n()) %>% 
		pivot_longer(-sample, names_to="parameter")
	return(out)
}

getsummaries <- function(obj){
	out <- getdraws(obj) %>% 
		group_by(parameter) %>% 
		summarise(mean=mean(value), lwr=quantile(value,0.025), upr=quantile(value,0.975))
}

makechainfig <- function(obj, grepstr=NULL){

	obj_chains <- getchains(obj)

	if(length(grepstr)>0){
		obj_chains <- obj_chains %>% 
			filter(grepl(grepstr, parameter))
		if(nrow(obj_chains)==0){
			stop("grepstr has removed all columns")
		}
	}

	out <- obj_chains %>%
		ggplot(aes(x=iteration, y=value, col=factor(chain))) + 
		geom_line(alpha=0.6) + 
		theme_classic() + 
		facet_wrap(~parameter, scales="free", ncol=1) 
	return(out)
}



makehists <- function(obj, grepstr=NULL, scalesval="free"){

	obj_draws <- getdraws(obj)
	obj_summaries <- getsummaries(obj)

	if(length(grepstr)>0){
		obj_draws <- obj_draws %>% 
			filter(grepl(grepstr, parameter))
		obj_summaries <- obj_summaries %>% 
			filter(grepl(grepstr, parameter))
		if(nrow(obj_draws)==0){
			stop("grepstr has removed all columns")
		}
	}

	out <- obj_draws %>% 
		ggplot(aes(x=value)) + 
			geom_histogram(aes(y=after_stat(density)), bins=50, fill="white", col="darkgrey") + 
			geom_density(adjust=2) + 
			geom_point(data=obj_summaries, aes(x=mean, y=0), col="black") + 
			geom_point(data=obj_summaries, aes(x=lwr, y=0), col="black", shape="[", size=3) + 
			geom_point(data=obj_summaries, aes(x=upr, y=0), col="black", shape="]", size=3) + 
			theme_classic() + 
			facet_wrap(~parameter, ncol=1, scales=scalesval)

	return(out)

}
