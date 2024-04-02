
fig_iris_scatter_bymetric <- iris %>% 
	pivot_wider(names_from="Structure", values_from="Value") %>% 
	ggplot(aes(x=Sepal, y=Petal, col=Species)) + 
		geom_point(alpha=0.4)  + 
		theme_classic() + 
		scale_color_manual(values=c("setosa"="black","versicolor"="blue","virginica"="red")) + 
		facet_wrap(~Metric, nrow=3, scales="free")


fig_iris_scatter_bystructure <- iris %>% 
	pivot_wider(names_from="Metric", values_from="Value") %>% 
	ggplot(aes(x=Width, y=Length, col=Species)) + 
		geom_point(alpha=0.4)  + 
		theme_classic() + 
		scale_color_manual(values=c("setosa"="black","versicolor"="blue","virginica"="red")) + 
		facet_wrap(~Structure, nrow=3, scales="free")