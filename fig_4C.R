source('functions.R')
gr <- fread('data/comm_mixes_dMMfits.csv')

aux <- gr[comp %like% 'P' & replc==1]
p <- 'results/fig_4C.pdf'

pdf(p, 4, 4, family = 'CM Sans')
ggplot(aux, aes(x=ordered(c.len), y=as.numeric(P))) +
geom_dotplot(binaxis='y', stackdir = 'center',
             size = .6, fill = 'chocolate2') +
scale_y_log10() +
annotation_logticks(sides="l", size=.4) +
theme_bw() + labs(y = 'P. Polymyxa CFU' ,
                  x = 'Community size') +
theme(panel.grid = element_blank())
dev.off()
embed_fonts(p, outfile = p)

