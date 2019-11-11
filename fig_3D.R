source('functions.R')
d <- fread('data/three_sp_consortia.csv')

p <- 'results/fig_3D.pdf'
pdf(p, 5, 4, family = 'CM Sans')
ggplot(d, aes(eps.2, d.epsilon.ABC, colour = (bg=='P' | pair %like% 'P'))) +
geom_errorbar(aes(ymin=d.epsilon.ABC - d.epsilon.ABC.se,
                  ymax=d.epsilon.ABC + d.epsilon.ABC.se),
              size=.5, width = .3, colour = 'gray') +
geom_errorbarh(aes(xmin=eps.2 - eps.2.se,
                   xmax=eps.2 + eps.2.se),
               size=.5, width = .3, colour = 'gray') +
geom_hline(yintercept = 0, col='gray', linetype = 2) +
geom_vline(xintercept = 0, col='gray', linetype = 2) +
geom_point(size = 3) +
scale_colour_manual(values = c('skyblue3', 'chocolate2')) +
theme_bw() + labs(title = 'Triads',
                  y = 'Higher order epistasis' ,
                  x = expression(paste(Sigma, epsilon[ij]^0)),
                  colour = 'P. polymyxa \nin triad') +
theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)
