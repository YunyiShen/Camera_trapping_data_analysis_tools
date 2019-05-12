relative_ab = function(event,spplist,cameraloc){
  res_p = matrix(0,nrow = length(cameraloc),ncol = length(spplist))
  res_c = res_p
  row.names(res_c) = as.character(cameraloc)
  colnames(res_c) = as.character(spplist)
  row.names(res_p) = as.character(cameraloc)
  colnames(res_p) = as.character(spplist)
  for(i in cameraloc){
    temp = event$Species[event$Location==i]
    for(j in spplist){
      res_p[i,j] = sum(temp==j)/(length(temp)+1e-20)
      res_c[i,j] = sum(temp==j)
    }
  }
  return(list(p = res_p,count = res_c))
}

clean_event = read.csv("CleanEvents.csv")
camera_site = read.csv("TrailCameraLocations.csv")

camera_site$Camname =apply(camera_site,1, function(camera_site1){ paste0("CN",camera_site1[1],sprintf("%02d",as.integer( camera_site1[2])),collapse = "")})
spplist = unique(as.character(clean_event$Species))

carn = c(1,4,5,6,8,9,12,14,16,18,25)
herb = c(3,10,11,15,20,22,29)
bird = c(7,13,17,26,27,28)

carn_list = spplist[carn]
herb_list = spplist[herb]
bird_list = spplist[bird]

carn_diversity = relative_ab(clean_event,carn_list,camera_site$Camname)
carn_SW = rowSums((-carn_diversity$p * log(carn_diversity$p+1e-20)))

carn_data = data.frame(camera_site,SW = carn_SW)

require(ggplot2)
require(grDevices)
require(RColorBrewer)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(-0, 1.3))
ggplot(data = carn_data[carn_data$Location!="MD",],aes(x=Long,y=Lat,color = SW)) +
  geom_point() + sc


unique_island = unique( camera_site$Location )
carn_SW_island = data.frame(
  gamma_diversity = matrix(0,length(unique_island)),
  alpha_diversity = matrix(0,length(unique_island)),
  size = matrix(0,length(unique_island))
  )
rownames(carn_SW_island) =as.character( unique_island)

for(i in unique_island){
  sites = camera_site$Location==i
  if(!is.null(nrow(carn_diversity$count[sites,]))){
  ctemp = colSums( carn_diversity$count[sites,])
  }
  else ctemp = carn_diversity$count[sites,]
  sum_c = sum(ctemp) + 1e-20
  p_temp = ctemp/sum_c
  carn_SW_island[i,1] = sum(-p_temp * log(p_temp+1e-20))
  
  carn_SW_island[i,2] = mean(carn_data$SW[sites])
  
  carn_SW_island[i,3] = sum(sites)
  
}

carn_SW_island$beta_diversity = carn_SW_island$ gamma_diversity/carn_SW_island$alpha_diversity

div_lm = lm(gamma_diversity ~ (size),data = carn_SW_island)
gamma_alpha = lm(gamma_diversity~alpha_diversity,data = carn_SW_island)
beta_size = lm((beta_diversity)~(size),data = carn_SW_island)

ggplot(data = carn_SW_island,aes(x=size,y=beta_diversity))+
  geom_point(color="darkred") + 
  geom_smooth(method = "lm") +
  theme(axis.text.x = element_text(size = 10, 
                                   color = "black", 
                                   vjust = 1, 
                                   hjust = 1, 
                                   angle = 0))+
  theme(axis.text.y = element_text(size = 10,
                                   color = 'black',
                                   vjust = 0.5,
                                   hjust = 0)) + 
  ylab("beta diversity")+
  xlab("Island size")



herb_diversity = relative_ab(clean_event,herb_list,camera_site$Camname)
herb_SW = rowSums((-herb_diversity$p * log(herb_diversity$p+1e-20)))
herb_abun = rowSums(herb_diversity$count)
herb_data = data.frame(camera_site,SW = herb_SW,abs_abun = herb_abun)



myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(-0, 50))
ggplot(data = herb_data,aes(x=Long,y=Lat,color = abs_abun)) +
  geom_point() + sc


bird_diversity = relative_ab(clean_event,bird_list,camera_site$Camname)
bird_SW = rowSums((-bird_diversity$p * log(bird_diversity$p+1e-20)))

bird_data = data.frame(camera_site,SW = bird_SW)

require(ggplot2)
require(grDevices)
require(RColorBrewer)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(-0, .3))
ggplot(data = bird_data,aes(x=Long,y=Lat,color = SW)) +
  geom_point() + sc

canid_data = data.frame(camera_site,
                            coyote = carn_diversity$count[,"Coyote"]>0,
                            red_fox = carn_diversity$count[,"Fox_red"]>0,
                            gray_fox = carn_diversity$count[,"Fox_gray"]>0)

ggplot(data = canid_data ,aes(x=Long,y=Lat,color = red_fox + 2 * gray_fox)) +
  geom_point()

