source('functions.R')
k <- fread('data/P_langm_data.csv')
fitline <- fread('data/P_langm_fitline.csv')

clrs <- c("#C4961A", "maroon3", "mediumseagreen", "chocolate2", "#4E84C4", "#293352")

pdf('results/P_SN_titration_langmuir.pdf', 6, 4)
ggplot(k, aes(x = cfu.m, y = fit)) +
geom_ribbon(data = fitline, aes(x = N, ymin = prop2.5, ymax = prop97.5),
            fill = 'gray90', inherit.aes=FALSE) +
geom_line(data = fitline, aes(x = N, y = Prop.Mean.1), linetype = 'dashed', 
          inherit.aes=FALSE) +
geom_point(aes(shape = cond.SN,
               color = cond.SN, alpha = 10*as.numeric(cond.SN.dil)),
           size = 3, stroke=2) +
geom_errorbar(aes(ymin = fit-fit.se, ymax = fit+fit.se,
                  color = cond.SN, alpha = 10*as.numeric(cond.SN.dil)),
              width = 0) +
geom_errorbarh(aes(xmin = cfu.m-cfu.se, xmax = cfu.m+cfu.se,
                   color = cond.SN, alpha = 10*as.numeric(cond.SN.dil)),
               width = 0) +
scale_color_manual(values = clrs[c(1, 3:6)]) +
scale_shape_manual(values = c(0:2, 5:6)) +
theme_bw() +
scale_y_log10(limits = c(0.05, 60)) + 
scale_x_continuous(labels = scales::scientific) +
theme(panel.grid = element_blank()) +
labs(x = 'CFU', y = 'V)')
dev.off()
