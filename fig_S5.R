source('functions.R')
d <- fread('data/two_sp_consortia.csv')

p <- 'results/fig_S5.pdf'
pdf(p, 5, 6, family = 'CM Sans')
ggplot(d, aes(variable, value, shape = variable)) +
geom_point(size = 3, colour = 'seagreen4') +
scale_y_log10() +
annotation_logticks(sides="l", size=.1, colour = 'gray') +
facet_grid(v1~v2) + 
scale_shape_manual(values = c(0, 1, 15, 16)) +
theme(panel.grid = element_blank()) +
theme(panel.grid.major.y = element_line(colour = 'gray95', size = .1),
      panel.grid.minor.y = element_line(colour = 'gray95', size = .1)) +
theme_bw()
dev.off()
embed_fonts(p, outfile = p)
