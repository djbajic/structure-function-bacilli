#gr <- fread('data/comm_mixes_dMMfits.csv')
#map <- fread('data/stepwise_map_PA.csv')
strfun <- fread('data/structure_function_PA.csv')

# .. sub-landscape C, E, P 
aux <- strfun[V2 %in% c('M-P-S', 'M-S', 'P-S', 'M-P', 'M', 'S', 'P')]

pdf('results/fig_3C_layer1.pdf',4,4)
ggplot(aux, aes(x = len.v2, y = Vj.real.V2)) +
    geom_point(colour = 'red3', alpha=.4, size = 1.5) +
    xlim(0,3) + ylim(0, 60) +
    geom_segment(mapping = aes(x = len.v1, xend = len.v2,
                               y = Vj.real.V1, yend = Vj.real.V2),
                 colour = 'red3', alpha=.4, size = 1) +
    geom_errorbar(aes(ymin= Vj.real.V2 - se.real.V2 ,
                      ymax= Vj.real.V2 + se.real.V2),
                  size=.5, width = 0, colour = 'red3') +
    labs(x = 'Number of species', y='Vj') +
    theme_bw() + labs(title = 'Observed landscape') +
    theme(panel.grid = element_blank())
dev.off()

pdf('results/fig_3C_layer2.pdf',4,4)
ggplot(aux, aes(x = len.v2, y = Vj.real.V2)) +
    geom_point(colour = 'gray', alpha=.4, size = 1.5) +
    xlim(0,3) + ylim(0, 60) +
    geom_segment(mapping = aes(x = len.v1, xend = len.v2,
                               y = Vj.2.V1, yend = Vj.2.V2),
                 colour = 'gray', alpha=.5, size = 1) +
    geom_errorbar(aes(ymin= Vj.2.V2 - Vj.2.V2.se ,
                      ymax= Vj.2.V2 + Vj.2.V2.se),
                  size=.5, width = 0, colour = 'gray') +
    labs(x = 'Number of species', y='Vj') +
    theme_bw() + labs(title = 'Observed landscape') +
    theme(panel.grid = element_blank())
dev.off()
