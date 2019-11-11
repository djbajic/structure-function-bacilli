source('functions.R')
d <- fread('data/filters_processed.csv')

p <- 'results/fig_S4.pdf'
pdf(p, 8,3)
ggplot(d[sample!='NO_SUP'],
       aes(variable, means, colour = cond, group=cond)) +
    geom_pointrange(aes(ymin = means-errors, ymax = means+errors), shape = 1, size=.2) +
    geom_line() + 
    facet_grid(.~sample) +
    scale_colour_brewer(palette='Set2') +
    theme_bw() + theme(panel.grid = element_blank())
dev.off()
