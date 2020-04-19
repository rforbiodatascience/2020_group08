set.seed(16)
?set.seed
dat = data.frame(elev = round( runif(20, 100, 500), 1),
                 resp = round( runif(20, 0, 10), 1),
                 grad = round( runif(20, 0, 1), 2),
                 slp = round( runif(20, 0, 35),1),
                 lat = runif(20, 44.5, 45),
                 long = runif(20, 122.5, 123.1),
                 nt = rpois(20, lambda = 25) )
head(dat)
response = names(dat)[1:3]
expl = names(dat)[4:7]

geomjitter = function(x, y) {
  ggplot(dat, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_jitter() +
    geom_smooth(method = "loess", se = FALSE) +
    theme_bw() +
    labs(x = x,
         y = y)
}

scatter_fun("lat", "elev")

elev_plots = map(expl, ~scatter_fun(.x, "elev") )

?theme_bw
