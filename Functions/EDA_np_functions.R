# author: Roman Velez
# date: 7 May 2020

#==========================================================================
# Functions and Routines for Non Parametric EDA 
#==========================================================================
library(dplyr)
library(ggplot2)
library(lazyeval)
#### ========================= Routines ==================================
plt_dnsty <- function(x, bw = 'nrd0', kernel_list = c('epanechnikov','gaussian','rectangular')){
  # plot histogram and a series of KDA 
  x %>% hist(col = 'pink', prob = T)
  
  #kernel densities
  n <- length(kernel_list)
  if(n > 0 ){
    i <- 2
    for (k0 in kernel_list){
      x %>% density(bw = bw, kernel = k0) %>% lines(col = i, lwd = 3)
      i <- i + 1
    }
    legend('topright', legend = unlist(kernel_list), col = 2:i, lwd = rep(3, n))
  }
}


plt_bxplt <- function(df, x.var, y.var){
  #cast vars
  x.var <- rlang::sym(quo_name(enquo(x.var)))
  y.var <- rlang::sym(quo_name(enquo(y.var)))
  
  
  # graph
  g <- df %>% 
        ggplot(mapping = aes(x = !! x.var, y = !! y.var, fill = !! x.var)) +
        geom_boxplot()

  return(g)
}

plt_multiple_bxplt <- function(df, x.vars, y.var){
  
}



