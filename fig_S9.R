require('functions.R')

gr.communities <- fread('data/data_comm_mixes_growth.csv')
aux <- gr.communities[,c('C','E','M','P','S','T')]
aux <- as.matrix(aux)
aux <- apply(aux, 2, as.numeric)
aux <- melt(aux)

gr.sncond <- fread('data/data_SN_conditioning.csv')
minvals <- gr.sncond[, min(cfu.m, na.rm=TRUE), by='strain']
names(minvals) <- c('Var2', 'value')
minvals[, value:= value/2]

p <- 'results/fig_S9.pdf'
pdf(p, 6,4, family = 'CM Sans')
ggplot(aux, aes(Var2, value)) + geom_jitter(width=.1, shape=19) +
scale_y_log10() +
geom_point(data = minvals, colour = 'red') +
annotation_logticks(sides="l", size=.4) +
theme_bw() + labs(y = 'CFU in communities' ,
                  x = 'Strain') + 
theme(panel.grid = element_blank(), aspect.ratio=1)
dev.off()
embed_fonts(p)
