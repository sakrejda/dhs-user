


d <- haven::read_dta('KEIR52FL.DTA')
ds <- d %>% dplyr::group_by(v001) %>% 
  dplyr::summarize(anymethod = mean(!is.na(v313) & v313 != 0))
m <- maptools::readShapeSpatial(fn='KEGE52FL')
m@data <- dplyr::left_join(m@data, ds, by=c(DHSCLUST = "v001"))
mf <- data.frame(m@data, long=m@coords[,1], lat=m@coords[,2])
ggplot(data=mf, aes(x=long, y=lat, colour=anymethod)) +
   geom_point(size=5, alpha=0.8) +
   scale_colour_continuous("contraceptive use, any method", limits=c(0,1)) +
   coord_cartesian(xlim=c(33,43), ylim=c(-5, 5)) +
   theme_minimal()

