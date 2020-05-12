library(readr)
library(stringr)

# principal table
clientes <- readRDS("FinalDataSet/transacciones.rds")

df1 <- clientes
varnum <- 'NumTransx'
varcat <- 'Abandono'

# dfs %>% 
#   ggplot(aes(x = factor(Abandono), y = NumTransx)) +
#     geom_boxplot() + 
#       theme_bw()

bxplt <- function(df, varcat, varnum){
  dfs <- df
  p1 <- str_c(
        'dfs %>% ggplot(aes(x = factor(',
        varcat,
        '), y = ',
        varnum,
        ')) + geom_boxplot() + theme_bw()')
  r <- eval(parse(text = p1))
  return(r)
}

g <- bxplt(diamonds, 'cut', 'price')
g


#======================mapan
install.packages('leaflet')
library(leaflet)
m <- leaflet() %>% addMarkers(lng = clientes$lon, lat = clientes$lat)  %>% addTiles()
m

