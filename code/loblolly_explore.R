# Generate a scatter of tree heights by age, colored by seed: 

fig_loblolly_scatter <- loblolly %>% 
	ggplot(aes(x=Age, y=Height, col=factor(Seed))) + 
		geom_point() + 
		geom_line(alpha=0.4) + 
		theme_classic() + 
		theme(legend.position="none")