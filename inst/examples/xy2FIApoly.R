
#get details of function
?RSForTools::xy2FIApoly()

#laod sf to play with data
library(sf)

#build singly plot
  df1=data.frame(plot=1,x=0,y=0)

  ply1 = RSForTools::xy2FIApoly(df1)
  plot(st_geometry(ply1))

#build grid of plots
  grid1 = expand.grid(x=seq(-500,500,4*103.923),y=seq(-500,500,4*103.923))
  grid1$plot=1:nrow(grid1)

  ply2 = RSForTools::xy2FIApoly(grid1)
  ply2bf = st_buffer(ply2,24)

  plot(st_geometry(ply2))
  plot(st_geometry(ply2bf),add=T,lty=2)

#make a grid across area
  grid2 = st_make_grid(
    st_bbox(ply2),
    n = c(3, 3),
    what = "centers",
    square = F ,
    flat_topped =T
  )
  grid2_poly =  st_make_grid(
    st_bbox(ply2),
    n = c(3, 3),
    #what = "centers",
    square = F ,
    flat_topped = T
  )

  #make FIA plots in each grid cell
  ply3 = RSForTools::xy2FIApoly(data.frame(plot=1:length(grid2),st_coordinates(grid2)),names = c(plot = "plot", x = "X", y = "Y"))

  #plot hex grid and FIA plots
  plot(grid2_poly,add=F, lwd=2)
  plot(st_geometry(ply3),add=T,col="forest green",border=0)




  pl_data = compileFIA( trees = df_tree, plots = df_plots , condition = df_cond)


