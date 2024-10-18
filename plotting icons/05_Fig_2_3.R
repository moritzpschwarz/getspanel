library(showtext)
library(ggimage)

font_add("Noto", "C:/Users/morit/Downloads/Noto_Emoji/NotoEmoji-VariableFont_wght.ttf")
font_add("Arial", "Arial.ttf")
showtext_auto()


### Plotting for Fig. 2 + Fig. 3

setwd("~/Github/getspanel/plotting icons")
#setwd("C:\\Users\\stecheme.PIKACCOUNTS\\Documents\\Break_detection\\OECD_project_code_first_submission\\Public_code_zenodo\\")

source('00_oecd_project_functions.R')
conflicts_prefer(ggpubr::get_legend)

policy_out <- readRDS("Policy_out.RDS")
oecd_grouped = read.csv('OECD_data_preprocessed_august_23.csv')

#set the color palette for the policies
palette <- c("#e6194b","#f58231","#f032e6","#991eb4","#ffe119","#bfef45","#3cb44b","#4363d8","#fabed4","#42d4f4","#ffd8b1","#fffac8","#aaffc3","#dcbeff","#800000","#9a6324","#808000","#000075","#469990","#000000","#a9a9a9","tan","aquamarine")
names(palette) <- sort(unique(oecd_grouped$Policy_name_fig_2_3))

color_dict = palette

#load the label_df to visualize years with EU policies we control for

label_df <- read.csv('EU_policies_label_df.csv')

## make panels for each sector
sector_plots = list()
ncol = c(6,3,3,6)
box_size = c(3,4,3,3)
ylims = list(c(0.5,3.1),c(0.2,3.5),c(0.5,3),c(0.3,3.5))
prop = c(0.65,1.05,0.9,0.6)
icon_links = c("Logos\\Buildings.png","Logos\\Electricity.png","Logos\\Industry.png","Logos\\Transport.png")
i=1
for(s in unique(policy_out$sector)){
  #s = unique(policy_out$sector)[1]
  policy_out_sub = policy_out[policy_out$sector == s,]
  out = rbind(policy_out_sub$out[[1]],policy_out_sub$out[[2]])
  policy_match = oecd_grouped
  myplots = list()
  logo <- ggdraw() +
    draw_image(icon_links[i])
  counter = 1
  myplots[[counter]] = logo
  countries = unique(out$id)
  hi_countries = unique(policy_out_sub$out[[1]]$id)
  li_countries = unique(policy_out_sub$out[[2]]$id)
  counter = counter+1
  for(c in countries){
    if(c %in% hi_countries){
      res = policy_out_sub[1,]$is[[1]]
    }else{
      res = policy_out_sub[2,]$is[[1]]
    }
    #see oecd project functions for this function
    p_out<- plot_ts_example_with_policy(c,res,out,policy_match,sector = s,label_df = label_df,ylim = ylims[[i]], symbol_size = 4,cube_size = box_size[i],policy_plot_prop = prop[i])
    myplots[[counter]] <- p_out
    counter = counter+1
  }

  sector_policies = data.frame(sector_policies = oecd_grouped[oecd_grouped$Module == s,c('Policy_name_fig_2_3')])
  sector_policies = sector_policies[!duplicated(sector_policies),]
  sector_policies = data.frame(Policy_name_fig_2_3 = sector_policies)
  p_legend <- ggplot(sector_policies,aes(x=Policy_name_fig_2_3,fill=Policy_name_fig_2_3))+
    geom_bar(color='black',size = 0.02)+
    scale_fill_manual('',values = color_dict)+
    theme(legend.key.size = unit(0.7, 'cm'),
          legend.title = element_text(size=20),
          legend.text = element_text(size=15),
          legend.margin=margin(l = 3, unit='cm'),
          rect = element_rect(fill = "transparent"),
          legend.position = 'bottom')
  legend <- ggpubr::get_legend(p_legend)

  legend_1 <- create_fig_2_3_legend()

  if(i<3){
    myplots[[counter]]<-legend_1
  }

  p <- cowplot::plot_grid(plotlist=myplots,ncol=ncol[i])
  p_final <- cowplot::plot_grid(plotlist = list(p,legend),nrow=2, rel_heights = c(1,0.15))
  sector_plots[[i]] <- p_final
  i=i+1
}


#save Fig. 2
path = paste("Figs\\",'Fig_2',".png",sep='')

png(path, width     =17,height    = 27,units     = "in",res       = 900)

p <- cowplot::plot_grid(plotlist = list(sector_plots[[2]],sector_plots[[3]]),nrow=2,rel_heights=c(0.6,1))+theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
y.grob <- textGrob("log(CO2 emissions)",
                   gp=gpar(fontface="bold", fontsize=25), rot=90)

x.grob <- textGrob("years",
                   gp=gpar(fontface="bold", fontsize=25))
right.grob <- textGrob("Adopted policies & tightenings",
                       gp=gpar(fontface="bold", fontsize=25), rot=270)
grid.arrange(arrangeGrob(p, left = y.grob, bottom = x.grob,right = right.grob))

dev.off()

#save Fig. 3
path = paste("Figs\\",'Fig_3',".png",sep='')
png(path, width     =20,height    = 27,units     = "in",res       = 900)

p <- cowplot::plot_grid(plotlist = list(sector_plots[[1]],sector_plots[[4]]),nrow=2)+theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
y.grob <- textGrob("log(CO2 emissions)",
                   gp=gpar(fontface="bold", fontsize=25), rot=90)

x.grob <- textGrob("years",
                   gp=gpar(fontface="bold", fontsize=25))
right.grob <- textGrob("Policy present",
                       gp=gpar(fontface="bold", fontsize=25), rot=270)
grid.arrange(arrangeGrob(p, left = y.grob, bottom = x.grob,right = right.grob))

dev.off()


#save Fig. 2 PDF
path = paste("Figs\\",'Fig_2_new',".pdf",sep='')

pdf(path, width = 17, height = 27) #, units = "in", res = 900

p <- cowplot::plot_grid(plotlist = list(sector_plots[[2]],sector_plots[[3]]),nrow=2,rel_heights=c(0.6,1))+theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
y.grob <- textGrob("log(CO2 emissions)",
                   gp=gpar(fontface="bold", fontsize=25), rot=90)

# x.grob <- textGrob("years",
#                    gp=gpar(fontface="bold", fontsize=25))
right.grob <- textGrob("Adopted policies & tightenings",
                       gp=gpar(fontface="bold", fontsize=25), rot=270)
grid.arrange(arrangeGrob(p, left = y.grob,
                         #bottom = x.grob,
                         right = right.grob))

dev.off()


#save Fig. 3 PDF
path = paste("Figs\\",'Fig_3_new',".pdf",sep='')
cairo_pdf(path, width     =20,height    = 27)

p <- cowplot::plot_grid(plotlist = list(sector_plots[[1]],sector_plots[[4]]),nrow=2)+theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
y.grob <- textGrob("log(CO2 emissions)",
                   gp=gpar(fontface="bold", fontsize=25), rot=90)

x.grob <- textGrob("years",
                   gp=gpar(fontface="bold", fontsize=25))
right.grob <- textGrob("Policy present",
                       gp=gpar(fontface="bold", fontsize=25), rot=270)
grid.arrange(arrangeGrob(p, left = y.grob,
                         #bottom = x.grob,
                         right = right.grob))

dev.off()

