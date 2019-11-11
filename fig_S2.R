source('functions.R')
d <- fread('data/three_sp_consortia.csv')

p <- 'results/fig_S2.pdf'
pdf(p, 5, 4, family = 'CM Sans')
ggplot(d, aes(Vj.1.V2, eps.2, colour = (bg=='P' | pair %like% 'P'))) +
geom_hline(yintercept = 0, col='gray', linetype = 2) +
geom_vline(xintercept = 0, col='gray', linetype = 2) +
geom_errorbar(aes(ymin=eps.2 - eps.2.se,
                  ymax=eps.2 + eps.2.se),
              size=.5, width = 0, colour = 'gray') +
geom_errorbarh(aes(xmin=Vj.1.V2 - Vj.1.V2.se,
                   xmax=Vj.1.V2 + Vj.1.V2.se),
               size=.5, width = 0, colour = 'gray') +
geom_point(size = 3) +
scale_colour_manual(values = c('skyblue3', 'chocolate2')) +
theme_bw() + labs(title = 'Triads',
                  y = 'Pairwise interaction' ,
                  x = 'Additive expectation (1st order)',
                  colour = 'P. polymyxa \nin triad') +
theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)
