

x <- seq(from=-3, to=3, by=0.01) 
y <- dnorm(x,0,1)

tibble(x=x, y=y) %>% 
	ggplot(aes(x=x, y=y)) + 
		geom_line() + 
		theme_classic() 


x <- seq(from=-3, to=3, by=0.01) 
y <- seq(from=-3, to=3, by=0.01) 


as_tibble(data.frame(rmvnorm(n=10000, mean=c(0,0), sigma=diag(c(1,1))))) %>% 
	ggplot(aes(x=X1, y=X2)) + 
		geom_density_2d(adjust=2) + 
		theme_classic() 


as_tibble(data.frame(rmvnorm(n=10000, 
	mean=c(0,0), 
	sigma=matrix(c(1,1,1,1), ncol=2)))) %>% 
	ggplot(aes(x=X1, y=X2)) + 
		geom_density_2d(adjust=2) + 
		theme_classic() 
