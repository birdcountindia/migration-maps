
#### Updated migration map function
#### For image with side panel: Species1 = "India Name of Species", SciName = "Scientific Name" 
#### rawpathPhoto = "file path of jpeg of bird", yaxis = ylim of inset graph, pointsize = size of the point

#### Removed: see line 109, I have replaced the lapply with a for loop, found it easier to code in both the map     stuff and the graph

#### Code is for single species at the moment

migrationmap = function(n=1, 
                        Species1,SciName, rawpath1, rawpath2=NA, 
                        rawpathPhoto,  res = 120, range = 30,
                        step = 10, fps = 2, col1 = "red", col2 = "blue", 
                        pointsize, yaxis, world = F, 
                        minlong = -15, minlat = -33, maxlong = 180, maxlat = 70,
                        ggp,dataall,migstatus,credit,
                        impos,grpos,
                        credit_color = "black")
{

  PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
  #PROJ = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
  
  projdat = project(cbind(data$LONGITUDE,data$LATITUDE), proj = PROJ)
  data = cbind(projdat, data)
  names(data)[1:2] = c("long","lat")
  
  data = data %>% select(long,lat,COMMON.NAME,day)
  
  
  min = project(cbind(minlong,minlat), proj = PROJ)
  max = project(cbind(maxlong,maxlat), proj = PROJ)
  
  windowsFonts("Gill Sans" = windowsFont("Gill Sans"))
  
  ##############
  
####  
  mon = c(rep("January",31),rep("February",28),rep("March",31),rep("April",30),rep("May",31),rep("June",30),
          rep("July",31),rep("August",31),rep("September",30),rep("October",31),rep("November",30),
          rep("December",31))
  
  mlabs = c("J","F","M","A","M","J","J","A","S","O","N","D")
  
  img = image_graph(width = 1080, height = 810, res = res)
  #datalist = split(data, data$fort)
  
  l = list()
  nums = c(1:365,1:(range-1))
  x = c(seq(101,365,step),seq(1,100,step))
  
  ct = 0
  for (i in x)
  {
    ct = ct + 1
    l[[ct]] = nums[i:(i+range-1)]
    if(max(l[[ct]]) == 365 & min(l[[ct]]) == 1)
      l[[ct]][l[[ct]]<=365 & l[[ct]]>(365-range)] = l[[ct]][l[[ct]]<=365 & l[[ct]]>(365-range)] - 365
  }
  
  a = image_read(rawpathPhoto)
  a = image_scale(a, "300")
  a = image_border(a, "#ffffff", "3x3")
  a = image_annotate(a, credit, font = 'Gill Sans', size = 24, location = "+8+4", color = credit_color)
  
  b = image_read("birdcountindia logo.png")
  b = image_scale(b, "300")
  b = image_background(b, "#ffffff", flatten = TRUE)
  #b = image_border(b, "black", "3x3")
  
  c = image_read("eBird India logo.png")
  c = image_scale(c, "300")
  c = image_background(c, "#ffffff", flatten = TRUE)
  #c = image_border(c, "black", "4x4")
  
  avg<-matrix(ncol = 2,nrow = 0)
  for (i in l){
    
    v1 = i
    if(min(v1) <= 0)
    {
      v1[v1<=0] = v1[v1<=0]+365
    }
    
    temp = data %>%
      filter(day %in% v1) %>%
      distinct(long,lat,COMMON.NAME)
    
    temp1 = freq1 %>%
      filter(day %in% v1) 
    
    med = floor(median(i))
    if (med == 0)
      med = 365
    if (med < 0)
      med = med + 365
    
    #cols = "#c26023"
    
    if (isTRUE(world))
    {
      p = ggp +
        if(switchs)geom_point(data = temp, aes(x = long,y = lat, col = COMMON.NAME, alpha = 0.99, stroke = 0), size = 1.5) +
        ggtitle(mon[med], size = 1) +
        theme(plot.title = element_text(hjust = 0.01, size = 10)) +
        scale_color_manual(breaks = sort(specs), values = cols) +
        theme(legend.position = "none")
    }
    else
    {
      p = ggp +
        geom_point(data = temp, aes(x = long,y = lat, col = COMMON.NAME, alpha = 0.99, stroke = 0), size = pointsize) +
        coord_cartesian(xlim = c(min[1],max[1]), ylim = c(min[2],max[2])) +
        theme(plot.title = element_text(hjust = 0.01, size = 10)) +
        scale_color_manual(breaks = sort(specs), values = cols) +
        #ggtitle(mon[med]) +
        theme(legend.position = "none")
      p1 = ggdraw(p)
    }
    
    xt = v1[floor(range/2)]
    #ggg<-c(((sum(temp1$detected)/sum(temp1$checklists))*100),xt)
    #avg<-rbind(avg,ggg, deparse.level = 0)
    #colnames(avg)<-c("frequency","day")
    #avg<-as.data.frame(avg)
    #print(avg)
    
    
    qi<-ggplot(freq1,aes(y = perc,x =  day)) + 
      #geom_point(size = 1.5)+
      #geom_line()+
      geom_line(data = spl, aes(x=x,y=y), size=1)+
      geom_vline(xintercept = xt, size = 0.1)+
      scale_x_continuous(limits = c(1,365), breaks= mdays, labels=mlabs)+
      scale_y_continuous(limits = yaxis, breaks= ybreaks)+
      xlab("months")+ ggtitle(paste("frequency in India ","(max ",round(mx,1),"%)",sep = ""))+
      theme(text=element_text(family="Gill Sans"))+
      theme(axis.title.x = element_blank(), axis.text.x = element_text(colour = "black",size = 8),
            axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(),
            plot.title = element_text(size = 10, hjust = 0.5)) +
      theme(panel.background = element_rect(fill = "#ffffff"),
            plot.margin = margin(1, 1, 1, 1, "mm"),
            plot.background = element_rect(
              fill = "#ffffff",
              colour = "black",
              size = 0.75),
            axis.line=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
    
    qj = ggplot() +
      theme(text=element_text(family="Gill Sans"))+
      theme(panel.background = element_rect(fill = "#ffffff"),
            plot.margin = margin(1, 1, 1, 1, "mm"),
            plot.background = element_rect(
              fill = "#ffffff",
              colour = "#ffffff",
              size = 0.5),
            axis.line=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
    
    qj1 = ggdraw(qj) +
      draw_label(species, 0.5, 0.59, size = 13, fontfamily="Gill Sans", fontface = 'bold', colour = col1)
    
    vv<-ggdraw(p1) + {if(impos == "R")draw_image(a, x = 1.017, y = 0.955, hjust = 1, vjust = 0.9, 
                                                 width = 0.25, height = 0.25)} +
      {if(impos == "L")draw_image(a, x = 0.234, y = 0.955, hjust = 1, vjust = 0.9, 
                                  width = 0.25, height = 0.25)} +
      {if(grpos == "R")draw_image(b, x = 0.215, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.230, height = 0.07)} +
      {if(grpos == "R")draw_image(c, x = 0.287, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.105, height = 0.07)} +
      {if(grpos == "L")draw_image(b, x = 0.923, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.230, height = 0.07)} +
      {if(grpos == "L")draw_image(c, x = 0.995, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.105, height = 0.07)}
    
    
    ## include in the function component, probably
    
    if (grpos == "L")
    {
      vp = viewport(width = 0.3, height = 0.2, x = 0.316,
                    y = unit(6.22, "lines"), just = c("right","top"))
    }
    
    if (grpos == "R")
    {
      vp = viewport(width = 0.3, height = 0.2, x = 0.685,
                    y = unit(6.22, "lines"), just = c("left","top"))
    }
    
    vq = viewport(width = wd, height = 0.04, x = 0.5,
                  y = 0.99, just = c("center","top"))
    
    
    full = function() {
      print(vv)
      print(qi, vp = vp)
      print(qj1, vp = vq)
    }
    
    full()
    
    #text1<- paste(Species1)
    #text2<-paste(SciName)
    #tgrob1 <- text_grob(text1, family = "Gill Sans", face = "bold", color = "black", vjust = 2, size = 16)
    #tgrob2 <- text_grob(text2, family = "Gill Sans", face = "italic", color = "black", vjust = -1, size = 12)
    #tgrob3 <- text_grob(mon[med], family = "Gill Sans", face = "bold", color = "black", vjust = 2, size = 18 )
    
    #r<-grid.arrange(arrangeGrob(arrangeGrob(tgrob1,tgrob2, ncol=1, nrow=2),qi, tgrob3, nrow = 3, ncol = 1),
    #arrangeGrob(vv, ncol=1, nrow=1), heights=c(100,1), widths=c(1,2))
    #print(r)
  }
  
  dev.off(which = 2)
  
###  
  
  #animation = image_animate(img, fps = fps)
  image_write_gif(img, nm, delay = 1/fps)
  
  
  #ggsave("map_draft_2.png", width=24, height=14.5, units="cm", dpi=600)
}


migrationmap2 = function(Species1, Species2, rawpath1, rawpath2, 
                         rawpathPhoto1, rawpathPhoto2,  res = 120, range = 30,
                         step = 10, fps = 2, col1 = "red", col2 = "blue", 
                         pointsize, yaxis, world = F, 
                         minlong = -15, minlat = -33, maxlong = 180, maxlat = 70,
                         ggp,dataall,migstatus1,migstatus2,credit1,credit2,
                         impos,grpos,
                         credit1_color = "black",credit2_color = "black")
{

  
  data1 = readcleanrawdata(rawpath = rawpath1)
  data2 = readcleanrawdata(rawpath = rawpath2)
  data = rbind(data1,data2)
  data1 = data1 %>% select(COMMON.NAME)
  data2 = data2 %>% select(COMMON.NAME)  
  
  afreq = create_freq(Species = Species1, data = dataall, migstatus = migstatus1)
  
  afreq1 = afreq[[1]]
  #afreq2 = afreq[[2]]
  #afreq3 = afreq[[3]]
  afreq4 = afreq[[2]]
  afreq1$checklists[afreq1$checklists == 0] = 1
  #afreq2$checklists[afreq2$checklists == 0] = 1
  #afreq3$checklists[afreq3$checklists == 0] = 1
  afreq4$checklists[afreq4$checklists == 0] = 1
  afreq1$perc = (afreq1$detected/afreq1$checklists)*100
  #afreq2$perc = (afreq2$detected/afreq2$checklists)*100
  #afreq3$perc = (afreq3$detected/afreq3$checklists)*100
  afreq4$perc = (afreq4$detected/afreq4$checklists)*100
  mdays = c(15.5,45.0,74.5,105.0,135.5,166.0,196.5,227.5,258.0,288.5,319.0,349.5)
  #mweek = rollmean(seq(0,365,7),2)
  mfort = rollmean(seq(0,365,14),2)
  #afreq2$day = mdays
  #afreq3$day = c(mweek,mweek[1]+365)
  afreq4$day = c(mfort,mfort[1]+365)
  
  aspl1 = smooth.spline(c(afreq1$day,(afreq1$day+365),(afreq1$day+730)),rep(afreq1$perc,3),nknots=30)
  #aspl2 = smooth.spline(c(afreq2$day,(afreq2$day+365),(afreq2$day+730)),rep(afreq2$perc,3),nknots=30)
  #aspl3 = smooth.spline(c(afreq3$day,(afreq3$day+365),(afreq3$day+730)),rep(afreq3$perc,3),nknots=30)
  aspl4 = smooth.spline(c(afreq4$day,(afreq4$day+365),(afreq4$day+730)),rep(afreq4$perc,3),nknots=30)
  
  aspl1a = predict(aspl1,366:730)
  aspl1a = as.data.frame(aspl1a)
  aspl1a$y[aspl1a$y<0] = 0
  
  #aspl2a = predict(aspl2,366:730)
  #aspl2a = as.data.frame(aspl2a)
  #aspl2a$y[aspl2a$y<0] = 0
  
  #aspl3a = predict(aspl3,366:730)
  #aspl3a = as.data.frame(aspl3a)
  #aspl3a$y[aspl3a$y<0] = 0
  
  aspl4a = predict(aspl4,366:730)
  aspl4a = as.data.frame(aspl4a)
  aspl4a$y[aspl4a$y<0] = 0
  
  #print(afreq[[2]])
  #print(aspl4a)
  
  aspl = aspl4a
  aspl$x = 1:365
  
  amx = max(na.omit(aspl$y))
  ayaxis = c(0,(amx+0.02))
  aybreaks = seq(0,ayaxis[2],1)
  
  bfreq = create_freq(Species = Species2, data = dataall, migstatus = migstatus2)
  
  bfreq1 = bfreq[[1]]
  #bfreq2 = bfreq[[2]]
  #bfreq3 = bfreq[[3]]
  bfreq4 = bfreq[[2]]
  bfreq1$checklists[bfreq1$checklists == 0] = 1
  #bfreq2$checklists[bfreq2$checklists == 0] = 1
  #bfreq3$checklists[bfreq3$checklists == 0] = 1
  bfreq4$checklists[bfreq4$checklists == 0] = 1
  bfreq1$perc = (bfreq1$detected/bfreq1$checklists)*100
  #bfreq2$perc = (bfreq2$detected/bfreq2$checklists)*100
  #bfreq3$perc = (bfreq3$detected/bfreq3$checklists)*100
  bfreq4$perc = (bfreq4$detected/bfreq4$checklists)*100
  mdays = c(15.5,45.0,74.5,105.0,135.5,166.0,196.5,227.5,258.0,288.5,319.0,349.5)
  #mweek = rollmean(seq(0,365,7),2)
  mfort = rollmean(seq(0,365,14),2)
  #bfreq2$day = mdays
  #bfreq3$day = c(mweek,mweek[1]+365)
  bfreq4$day = c(mfort,mfort[1]+365)
  
  bspl1 = smooth.spline(c(bfreq1$day,(bfreq1$day+365),(bfreq1$day+730)),rep(bfreq1$perc,3),nknots=30)
  #bspl2 = smooth.spline(c(bfreq2$day,(bfreq2$day+365),(bfreq2$day+730)),rep(bfreq2$perc,3),nknots=30)
  #bspl3 = smooth.spline(c(bfreq3$day,(bfreq3$day+365),(bfreq3$day+730)),rep(bfreq3$perc,3),nknots=30)
  bspl4 = smooth.spline(c(bfreq4$day,(bfreq4$day+365),(bfreq4$day+730)),rep(bfreq4$perc,3),nknots=30)
  
  bspl1a = predict(bspl1,366:730)
  bspl1a = as.data.frame(bspl1a)
  bspl1a$y[bspl1a$y<0] = 0
  
  #bspl2a = predict(bspl2,366:730)
  #bspl2a = as.data.frame(bspl2a)
  #bspl2a$y[bspl2a$y<0] = 0
  
  #bspl3a = predict(bspl3,366:730)
  #bspl3a = as.data.frame(bspl3a)
  #bspl3a$y[bspl3a$y<0] = 0
  
  bspl4a = predict(bspl4,366:730)
  bspl4a = as.data.frame(bspl4a)
  bspl4a$y[bspl4a$y<0] = 0
  
  #print(bfreq[[2]])
  #print(bspl4a)
  
  bspl = bspl4a
  bspl$x = 1:365
  
  bmx = max(na.omit(bspl$y))
  byaxis = c(0,(bmx+0.02))
  bybreaks = seq(0,byaxis[2],1)
  
  cmx<-max(amx,bmx)
  cyaxis = c(0,(cmx+0.02))
  cybreaks = seq(0,cyaxis[2],1)
  
  PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
  #PROJ = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
  
  projdat = project(cbind(data$LONGITUDE,data$LATITUDE), proj = PROJ)
  data = cbind(projdat, data)
  names(data)[1:2] = c("long","lat")
  
  data = data %>% select(long,lat,COMMON.NAME,day)
  
  
  min = project(cbind(minlong,minlat), proj = PROJ)
  max = project(cbind(maxlong,maxlat), proj = PROJ)
  
  windowsFonts("Gill Sans" = windowsFont("Gill Sans"))
  
  ##############
  
  spec1 = data1$COMMON.NAME[1]
  spec2 = data2$COMMON.NAME[1]
  specs = c(spec1,spec2)
  species = paste(specs[1],"         ",specs[2])
  
  if (sort(specs)[1] == specs[1])
  {
    cols = c(col2,col1)
  }
  if (sort(specs)[1] != specs[1])
  {
    cols = c(col1,col2)
  }
  wd1 = strwidth(spec1,family = "Gill Sans",units = 'figure')
  wd2 = strwidth(spec2,family = "Gill Sans",units = 'figure')
  wd1 = wd1 + 0.04
  wd2 = wd2 + 0.04
  
  
  mon = c(rep("January",31),rep("February",28),rep("March",31),rep("April",30),rep("May",31),rep("June",30),
          rep("July",31),rep("August",31),rep("September",30),rep("October",31),rep("November",30),
          rep("December",31))
  
  mlabs = c("J","F","M","A","M","J","J","A","S","O","N","D")
  
  img = image_graph(width = 1080, height = 810, res = res)
  #datalist = split(data, data$fort)
  
  l = list()
  nums = c(1:365,1:(range-1))
  x = c(seq(101,365,step),seq(1,100,step))
  
  ct = 0
  for (i in x)
  {
    ct = ct + 1
    l[[ct]] = nums[i:(i+range-1)]
    if(max(l[[ct]]) == 365 & min(l[[ct]]) == 1)
      l[[ct]][l[[ct]]<=365 & l[[ct]]>(365-range)] = l[[ct]][l[[ct]]<=365 & l[[ct]]>(365-range)] - 365
  }
  
  a1 = image_read(rawpathPhoto1)
  a1 = image_scale(a1, "300")
  a1 = image_border(a1, "#ffffff", "3x3")
  a1 = image_annotate(a1, credit1, font = 'Gill Sans', size = 24, color = credit1_color, location = "+8+4")
  
  a2 = image_read(rawpathPhoto2)
  a2 = image_scale(a2, "300")
  a2 = image_border(a2, "#ffffff", "3x3")
  a2 = image_annotate(a2, credit2, font = 'Gill Sans', size = 24, color = credit2_color, location = "+8+4")
  
  b = image_read("birdcountindia logo.png")
  b = image_scale(b, "300")
  b = image_background(b, "#ffffff", flatten = TRUE)
  #b = image_border(b, "black", "3x3")
  
  c = image_read("eBird India logo.png")
  c = image_scale(c, "300")
  c = image_background(c, "#ffffff", flatten = TRUE)
  #c = image_border(c, "black", "4x4")
  
  avg<-matrix(ncol = 2,nrow = 0)
  for (i in l){
    
    v1 = i
    if(min(v1) <= 0)
    {
      v1[v1<=0] = v1[v1<=0]+365
    }
    
    temp = data %>%
      filter(day %in% v1) %>%
      distinct(long,lat,COMMON.NAME)
    
    temp1 = afreq1 %>%
      filter(day %in% v1)
    
    temp2 = bfreq1 %>%
      filter(day %in% v1) 
    
    med = floor(median(i))
    if (med == 0)
      med = 365
    if (med < 0)
      med = med + 365
    
    #cols = "#c26023"
    
    if (isTRUE(world))
    {
      p = ggp +
        if(switchs)geom_point(data = temp, aes(x = long,y = lat, col = COMMON.NAME, alpha = 0.99, stroke = 0), size = pointsize) +
        #ggtitle(mon[med], size = 1) +
        theme(plot.title = element_text(hjust = 0.01, size = 10)) +
        scale_color_manual(breaks = sort(specs), values = cols) +
        theme(legend.position = "none")
      p1 = ggdraw(p)
    }
    else
    {
      p = ggp +
        geom_point(data = temp, aes(x = long,y = lat, col = COMMON.NAME, alpha = 0.99, stroke = 0), size = pointsize) +
        coord_cartesian(xlim = c(min[1],max[1]), ylim = c(min[2],max[2])) +
        theme(plot.title = element_text(hjust = 0.01, size = 10)) +
        scale_color_manual(breaks = sort(specs), values = cols) +
        #ggtitle(mon[med]) +
        theme(legend.position = "none")
      p1 = ggdraw(p)
    }
    
    xt = v1[floor(range/2)]
    #ggg<-c(((sum(temp1$detected)/sum(temp1$checklists))*100),xt)
    #avg<-rbind(avg,ggg, deparse.level = 0)
    #colnames(avg)<-c("frequency","day")
    #avg<-as.data.frame(avg)
    #print(avg)
    
    
    aqi<-ggplot(afreq1,aes(y = perc,x =  day)) + 
      #geom_point(size = 1.5)+
      #geom_line()+
      geom_line(data = aspl, aes(x=x,y=y), size=1, colour = col2 )+
      geom_line(data = bspl, aes(x=x,y=y), size=1, colour = col1 )+
      geom_vline(xintercept = xt, size = 0.1)+
      scale_x_continuous(limits = c(1,365), breaks= mdays, labels=mlabs)+
      scale_y_continuous(limits = cyaxis, breaks= cybreaks)+
      xlab("months")+ ggtitle(paste("frequency in India ","(max ",round(cmx,1),"%)",sep = ""))+
      theme(text=element_text(family="Gill Sans"))+
      theme(axis.title.x = element_blank(), axis.text.x = element_text(colour = "black",size = 8),
            axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(),
            plot.title = element_text(size = 10, hjust = 0.5)) +
      theme(panel.background = element_rect(fill = "#ffffff"),
            plot.margin = margin(1, 1, 1, 1, "mm"),
            plot.background = element_rect(
              fill = "#ffffff",
              colour = "black",
              size = 0.75),
            axis.line=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
    
    #bqi<-ggplot(bfreq1,aes(y = perc,x =  day)) + 
    #geom_point(size = 1.5)+
    #geom_line()+
    #geom_line(data = bspl, aes(x=x,y=y), size=1)+
    #geom_vline(xintercept = xt, size = 0.1)+
    #scale_x_continuous(limits = c(1,365), breaks= mdays, labels=mlabs)+
    #scale_y_continuous(limits = byaxis, breaks= bybreaks)+
    #xlab("months")+ ggtitle(paste("frequency in India ","(max ",round(bmx,1),"%)",sep = ""))+
    #theme(text=element_text(family="Gill Sans"))+
    #theme(axis.title.x = element_blank(), axis.text.x = element_text(colour = "black",size = 8),
    #     axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
    #    axis.title.y = element_blank(), 
    #   axis.text.y = element_blank(),
    #  plot.title = element_text(size = 10, hjust = 0.5)) +
    #theme(panel.background = element_rect(fill = "#008000"),
    #     plot.margin = margin(1, 1, 1, 1, "mm"),
    #    plot.background = element_rect(
    #     fill = "#008000",
    #    colour = "black",
    #   size = 0.75),
    #axis.line=element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    #panel.border = element_blank())
    
    
    qj = ggplot() +
      theme(text=element_text(family="Gill Sans"))+
      theme(panel.background = element_rect(fill = "#ffffff"),
            plot.margin = margin(1, 1, 1, 1, "mm"),
            plot.background = element_rect(
              fill = "#ffffff",
              colour = "#ffffff",
              size = 0.5),
            axis.line=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
    
    qj1 = ggdraw(qj) +
      draw_label(spec1, 0.5, 0.59, size = 13, fontfamily="Gill Sans", fontface = 'bold', colour = col2)
    
    qj2 = ggdraw(qj)+
      draw_label(spec2, 0.5, 0.59, size = 13, fontfamily="Gill Sans", fontface = 'bold', colour = col1)
    
    vv<-ggdraw(p1) + {if(impos == "R")draw_image(a1, x = 1.017, y = 0.955, hjust = 1, vjust = 0.9, 
                                                 width = 0.25, height = 0.25)} +
      {if(impos == "R")draw_image(a2, x = 0.234, y = 0.955, hjust = 1, vjust = 0.9, 
                                  width = 0.25, height = 0.25)} +
      {if(impos == "L")draw_image(a1, x = 0.234, y = 0.955, hjust = 1, vjust = 0.9, 
                                  width = 0.25, height = 0.25)} +
      {if(impos == "L")draw_image(a2, x = 1.017, y = 0.955, hjust = 1, vjust = 0.9, 
                                  width = 0.25, height = 0.25)}+
      {if(impos == "BL")draw_image(a2, x = 0.234, y = 0.6, hjust = 1, vjust = 0.9, 
                                   width = 0.25, height = 0.25)}+
      {if(impos == "BL")draw_image(a1, x = 0.234, y = 0.92, hjust = 1, vjust = 0.9, 
                                   width = 0.25, height = 0.25)}+
      {if(grpos == "R")draw_image(b, x = 0.215, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.230, height = 0.07)} +
      {if(grpos == "R")draw_image(c, x = 0.287, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.105, height = 0.07)} +
      {if(grpos == "L")draw_image(b, x = 0.923, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.230, height = 0.07)} +
      {if(grpos == "L")draw_image(c, x = 0.995, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.105, height = 0.07)}
    
    ## include in the function component, probably
    
    if (grpos == "L")
    {
      vp = viewport(width = 0.3, height = 0.2, x = 0.316,
                    y = unit(6.22, "lines"), just = c("right","top"))
    }
    
    #if (grpos2 == "L")
    #{
    #  vp2 = viewport(width = 0.3, height = 0.2, x = 0.316,
    #                 y = unit(6.22, "lines"), just = c("right","top"))
    #}
    
    if (grpos == "R")
    {
      vp = viewport(width = 0.3, height = 0.2, x = 0.685,
                    y = unit(6.22, "lines"), just = c("left","top"))
    }
    
    #if (grpos2 == "R")
    #{
    #  vp2 = viewport(width = 0.3, height = 0.2, x = 0.685,
    #                y = unit(6.22, "lines"), just = c("left","top"))
    #}
    #
    
    if(impos == "BL")
    {
      vq = viewport(width = wd1, height = 0.04, x = 0.117,
                    y = 0.99, just = c("center","top"))
    }
    else{
      vq = viewport(width = wd1, height = 0.04, x = 0.35,
                    y = 0.99, just = c("center","top"))
    }
    
    if(impos == "BL")
    {
      vr = viewport(width = wd2, height = 0.04, x = 0.117,
                    y = 0.67, just = c("center","top"))
    }
    else{vr = viewport(width = wd2, height = 0.04, x = 0.65,
                       y = 0.99, just = c("center","top"))
    }
    
    
    full = function() {
      print(vv)
      print(aqi, vp = vp)
      #print(bqi,vp = vp2)
      print(qj1, vp = vq)
      print(qj2, vp = vr)
    }
    
    full()
    
    #text1<- paste(Species1)
    #text2<-paste(SciName)
    #tgrob1 <- text_grob(text1, family = "Gill Sans", face = "bold", color = "black", vjust = 2, size = 16)
    #tgrob2 <- text_grob(text2, family = "Gill Sans", face = "italic", color = "black", vjust = -1, size = 12)
    #tgrob3 <- text_grob(mon[med], family = "Gill Sans", face = "bold", color = "black", vjust = 2, size = 18 )
    
    #r<-grid.arrange(arrangeGrob(arrangeGrob(tgrob1,tgrob2, ncol=1, nrow=2),qi, tgrob3, nrow = 3, ncol = 1),
    #arrangeGrob(vv, ncol=1, nrow=1), heights=c(100,1), widths=c(1,2))
    #print(r)
  }
  
  dev.off(which = 2)
  
  
  nm1 = spec1
  nm2 = spec2
  nm = paste(nm1,"_",nm2,"_",minlong,"_",minlat,"_",maxlong,"_",maxlat,".gif",sep = "")
  
  
  #animation = image_animate(img, fps = fps)
  image_write_gif(img, nm, delay = 1/fps)
  
  #ggsave("map_draft_2.png", width=24, height=14.5, units="cm", dpi=600)
}

migrationmapS = function(n=1, 
                         Species1,SciName, rawpath1, rawpath2=NA, 
                         rawpathPhoto,  res = 120, range = 30,
                        step = 10, fps = 2, col1 = "red", col2 = "blue", 
                        pointsize, yaxis, world = F, 
                        minlong = -15, minlat = -33, maxlong = 180, maxlat = 70,
                        namepos = "centre",
                        ggp,credit,
                        impos,grpos, 
                        credit_color = "black")
{

 
  
  avg<-matrix(ncol = 2,nrow = 0)
  for (i in l){
    
  
    
    
    # label boxes for species names
    
    qj = ggplot() +
      theme(text=element_text(family="Gill Sans"))+
      theme(panel.background = element_rect(fill = "#ffffff"),
            plot.margin = margin(1, 1, 1, 1, "mm"),
            plot.background = element_rect(
              fill = "#ffffff",
              colour = "#ffffff",
              size = 0.5),
            axis.line=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
    
    qk = ggplot() +
      theme(text=element_text(family="Gill Sans"))+
      theme(panel.background = element_rect(fill = "#ffffff"),
            plot.margin = margin(1, 1, 1, 1, "mm"),
            plot.background = element_rect(
              fill = "#ffffff",
              colour = "#ffffff",
              size = 0.3),
            axis.line=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
  

    if (namepos == "centre")
    {
      qj1 = ggdraw(qj) +
        draw_label(species, 0.5, 0.59, size = 13, fontfamily="Gill Sans", fontface = 'bold', colour = col1)
      
    }
    
    if (namepos == "image")
    {
      qj1 = ggdraw(qj) +
        draw_label(species, 0.5, 0.59, size = 11, fontfamily="Gill Sans", fontface = 'bold', colour = col1)
    }
     
      
    qj2 = ggdraw(qk) +  
        draw_label(mon[med], 0.5, 0.59, size = 10, fontfamily="Gill Sans", fontface = 'bold', colour = "black")
    
    
    
    
    vv<-ggdraw(p1) + {if(impos == "R")draw_image(a, x = 1.017, y = 0.955, hjust = 1, vjust = 0.9, 
                                                 width = 0.25, height = 0.25)} +
      {if(impos == "L")draw_image(a, x = 0.234, y = 0.955, hjust = 1, vjust = 0.9, 
                                  width = 0.25, height = 0.25)} +
      # image at bottom left
      {if(impos == "BX")draw_image(a, x = 0.244, y = 0.25, hjust = 1, vjust = 0.9, 
                                  width = 0.25, height = 0.25)} +
      {if(grpos == "R")draw_image(b, x = 0.215, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.230, height = 0.07)} +
      {if(grpos == "R")draw_image(c, x = 0.287, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.105, height = 0.07)} +
      {if(grpos == "L")draw_image(b, x = 0.923, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.230, height = 0.07)} +
      {if(grpos == "L")draw_image(c, x = 0.995, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.105, height = 0.07)} +
      {if(grpos == "LX")draw_image(b, x = 0.923, y = 0.975, hjust = 1, vjust = 0.9, 
                                  width = 0.230, height = 0.07)} +
      {if(grpos == "LX")draw_image(c, x = 0.995, y = 0.975, hjust = 1, vjust = 0.9, 
                                  width = 0.105, height = 0.07)}
    
    
    ## include in the function component, probably
    
    
    if (namepos == "centre")
    {
      vq = viewport(width = wd, height = 0.04, x = 0.5,
                    y = 0.99, just = c("center","top"))
      
      vq2 = viewport(width = 0.1, height = 0.03, x = 0.5,
                     y = 0.94, just = c("center","top"))
    }
    
    if (namepos == "image")
    {
      vq = viewport(width = wd-0.03, height = 0.03, x = 0.117,
                    y = 0.31, just = c("center","top"))
      
      vq2 = viewport(width = 0.1, height = 0.03, x = 0.5,
                     y = 0.99, just = c("center","top"))
    }
      
    

    
    
    full = function() {
      print(vv)
      print(qj1, vp = vq)
      print(qj2, vp = vq2)
    }
    
    
    full()
  }
    
  dev.off(which = 2)
  
  if (n == 1)
  {
    nm = specs
    nm = paste(nm,"_",minlong,"_",minlat,"_",maxlong,"_",maxlat,".gif",sep = "")
  }
  
  if (n != 1)
  {
    nm1 = specs1
    nm2 = specs2
    nm = paste(nm1,"_",nm2,"_",minlong,"_",minlat,"_",maxlong,"_",maxlat,".gif",sep = "")
  }
  
  
  #animation = image_animate(img, fps = fps)
  image_write_gif(img, nm, delay = 1/fps)
  
  
  #ggsave("map_draft_2.png", width=24, height=14.5, units="cm", dpi=600)
}


##### two species without graphs

migrationmap2S = function(Species1, Species2, rawpath1, rawpath2, 
                          rawpathPhoto1, rawpathPhoto2,  res = 120, range = 30,
                          step = 10, fps = 2, col1 = "red", col2 = "blue",
                          pointsize, yaxis, world = F, 
                          minlong = -15, minlat = -33, maxlong = 180, maxlat = 70,
                          ggp,credit1,credit2,
                          impos,grpos,
                          credit1_color = "black",credit2_color = "black")
{

  data1 = readcleanrawdata(rawpath = rawpath1)
  data2 = readcleanrawdata(rawpath = rawpath2)
  data = rbind(data1,data2)
  data1 = data1 %>% select(COMMON.NAME)
  data2 = data2 %>% select(COMMON.NAME)  
  
  PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
  #PROJ = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
  
  projdat = project(cbind(data$LONGITUDE,data$LATITUDE), proj = PROJ)
  data = cbind(projdat, data)
  names(data)[1:2] = c("long","lat")
  
  data = data %>% select(long,lat,COMMON.NAME,day)
  
  
  min = project(cbind(minlong,minlat), proj = PROJ)
  max = project(cbind(maxlong,maxlat), proj = PROJ)
  
  windowsFonts("Gill Sans" = windowsFont("Gill Sans"))
  
  ##############
  
  spec1 = data1$COMMON.NAME[1]
  spec2 = data2$COMMON.NAME[1]
  specs = c(spec1,spec2)
  species = paste(specs[1],"         ",specs[2])
  
  if (sort(specs)[1] == specs[1])
  {
    cols = c(col2,col1)
  }
  if (sort(specs)[1] != specs[1])
  {
    cols = c(col1,col2)
  }
  wd1 = strwidth(spec1,family = "Gill Sans",units = 'figure')
  wd2 = strwidth(spec2,family = "Gill Sans",units = 'figure')
  wd1 = wd1 + 0.04
  wd2 = wd2 + 0.04
  
  
  mon = c(rep("January",31),rep("February",28),rep("March",31),rep("April",30),rep("May",31),rep("June",30),
          rep("July",31),rep("August",31),rep("September",30),rep("October",31),rep("November",30),
          rep("December",31))
  
  mlabs = c("J","F","M","A","M","J","J","A","S","O","N","D")
  
  img = image_graph(width = 1080, height = 810, res = res)
  #datalist = split(data, data$fort)
  
  l = list()
  nums = c(1:365,1:(range-1))
  x = c(seq(101,365,step),seq(1,100,step))
  
  ct = 0
  for (i in x)
  {
    ct = ct + 1
    l[[ct]] = nums[i:(i+range-1)]
    if(max(l[[ct]]) == 365 & min(l[[ct]]) == 1)
      l[[ct]][l[[ct]]<=365 & l[[ct]]>(365-range)] = l[[ct]][l[[ct]]<=365 & l[[ct]]>(365-range)] - 365
  }
  
  a1 = image_read(rawpathPhoto1)
  a1 = image_scale(a1, "300")
  a1 = image_border(a1, "#ffffff", "3x3")
  a1 = image_annotate(a1, credit1, font = 'Gill Sans', size = 24, color = credit1_color, location = "+8+4")
  
  a2 = image_read(rawpathPhoto2)
  a2 = image_scale(a2, "300")
  a2 = image_border(a2, "#ffffff", "3x3")
  a2 = image_annotate(a2, credit2, font = 'Gill Sans', size = 24, color = credit2_color, location = "+8+4")
  
  b = image_read("birdcountindia logo.png")
  b = image_scale(b, "300")
  b = image_background(b, "#ffffff", flatten = TRUE)
  #b = image_border(b, "black", "3x3")
  
  c = image_read("eBird India logo.png")
  c = image_scale(c, "300")
  c = image_background(c, "#ffffff", flatten = TRUE)
  #c = image_border(c, "black", "4x4")
  
  avg<-matrix(ncol = 2,nrow = 0)
  for (i in l){
    
    v1 = i
    if(min(v1) <= 0)
    {
      v1[v1<=0] = v1[v1<=0]+365
    }
    
    temp = data %>%
      filter(day %in% v1) %>%
      distinct(long,lat,COMMON.NAME)
    

    med = floor(median(i))
    if (med == 0)
      med = 365
    if (med < 0)
      med = med + 365
    
    #cols = "#c26023"
    
    if (isTRUE(world))
    {
      p = ggp +
        if(switchs)geom_point(data = temp, aes(x = long,y = lat, col = COMMON.NAME, alpha = 0.99, stroke = 0), size = pointsize) +
        #ggtitle(mon[med], size = 1) +
        theme(plot.title = element_text(hjust = 0.01, size = 10)) +
        scale_color_manual(breaks = sort(specs), values = cols) +
        theme(legend.position = "none")
      p1 = ggdraw(p)
    }
    else
    {
      p = ggp +
        geom_point(data = temp, aes(x = long,y = lat, col = COMMON.NAME, alpha = 0.99, stroke = 0), size = pointsize) +
        coord_cartesian(xlim = c(min[1],max[1]), ylim = c(min[2],max[2])) +
        theme(plot.title = element_text(hjust = 0.01, size = 10)) +
        scale_color_manual(breaks = sort(specs), values = cols) +
        #ggtitle(mon[med]) +
        theme(legend.position = "none")
      p1 = ggdraw(p)
    }
    
    xt = v1[floor(range/2)]

    
    qj = ggplot() +
      theme(text=element_text(family="Gill Sans"))+
      theme(panel.background = element_rect(fill = "#ffffff"),
            plot.margin = margin(1, 1, 1, 1, "mm"),
            plot.background = element_rect(
              fill = "#ffffff",
              colour = "#ffffff",
              size = 0.5),
            axis.line=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
    
    qk = ggplot() +
      theme(text=element_text(family="Gill Sans"))+
      theme(panel.background = element_rect(fill = "#ffffff"),
            plot.margin = margin(1, 1, 1, 1, "mm"),
            plot.background = element_rect(
              fill = "#ffffff",
              colour = "#ffffff",
              size = 0.3),
            axis.line=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
    
    qj1 = ggdraw(qj)+
      draw_label(spec2, 0.5, 0.59, size = 11, fontfamily="Gill Sans", fontface = 'bold', colour = col1)
    
    qj2 = ggdraw(qj) +
      draw_label(spec1, 0.5, 0.59, size = 11, fontfamily="Gill Sans", fontface = 'bold', colour = col2)
    
    qj3 = ggdraw(qk) +  
      draw_label(mon[med], 0.5, 0.59, size = 10, fontfamily="Gill Sans", fontface = 'bold', colour = "black")
    
    vv<-ggdraw(p1) + {if(impos == "R")draw_image(a1, x = 1.017, y = 0.955, hjust = 1, vjust = 0.9, 
                                                 width = 0.25, height = 0.25)} +
      {if(impos == "R")draw_image(a2, x = 0.234, y = 0.955, hjust = 1, vjust = 0.9, 
                                  width = 0.25, height = 0.25)} +
      {if(impos == "L")draw_image(a1, x = 0.234, y = 0.955, hjust = 1, vjust = 0.9, 
                                  width = 0.25, height = 0.25)} +
      {if(impos == "L")draw_image(a2, x = 1.017, y = 0.955, hjust = 1, vjust = 0.9, 
                                  width = 0.25, height = 0.25)}+
      {if(impos == "BL")draw_image(a2, x = 0.234, y = 0.6, hjust = 1, vjust = 0.9, 
                                   width = 0.25, height = 0.25)}+
      {if(impos == "BL")draw_image(a1, x = 0.234, y = 0.92, hjust = 1, vjust = 0.9, 
                                   width = 0.25, height = 0.25)}+
      ## images at bottom left
      {if(impos == "BX")draw_image(a1, x = 0.244, y = 0.55, hjust = 1, vjust = 0.9, 
                                   width = 0.25, height = 0.25)}+
      {if(impos == "BX")draw_image(a2, x = 0.244, y = 0.25, hjust = 1, vjust = 0.9, 
                                  width = 0.25, height = 0.25)} +
      
      ##########
      {if(grpos == "R")draw_image(b, x = 0.215, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.230, height = 0.07)} +
      {if(grpos == "R")draw_image(c, x = 0.287, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.105, height = 0.07)} +
      {if(grpos == "L")draw_image(b, x = 0.923, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.230, height = 0.07)} +
      {if(grpos == "L")draw_image(c, x = 0.995, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.105, height = 0.07)} +
      
      {if(grpos == "LX")draw_image(b, x = 0.923, y = 0.975, hjust = 1, vjust = 0.9, 
                                   width = 0.230, height = 0.07)} +
      {if(grpos == "LX")draw_image(c, x = 0.995, y = 0.975, hjust = 1, vjust = 0.9, 
                                   width = 0.105, height = 0.07)}
    
    ## include in the function component, probably
    
    vs = viewport(width = 0.1, height = 0.03, x = 0.5,
                  y = 0.99, just = c("center","top"))
    

    if(impos == "BL")
    {
      vq = viewport(width = wd1, height = 0.04, x = 0.117,
                    y = 0.99, just = c("center","top"))
    }
    else{
      vq = viewport(width = wd1, height = 0.04, x = 0.35,
                    y = 0.99, just = c("center","top"))
    }
    
    if(impos == "BL")
    {
      vr = viewport(width = wd2, height = 0.04, x = 0.117,
                    y = 0.67, just = c("center","top"))
    }
    else{vr = viewport(width = wd2, height = 0.04, x = 0.65,
                       y = 0.99, just = c("center","top"))
    }
    
    if (impos == "BX")
    {
      vr = viewport(width = wd1-0.03, height = 0.03, x = 0.117,
                    y = 0.61, just = c("center","top"))
      vq = viewport(width = wd2-0.03, height = 0.03, x = 0.117,
                    y = 0.31, just = c("center","top"))
      
    }
    
    
    full = function() {
      print(vv)
      print(qj1, vp = vq)
      print(qj2, vp = vr)
      print(qj3, vp = vs)
    }
    
    full()
    

  }
  
  dev.off(which = 2)
  
  
  nm1 = spec1
  nm2 = spec2
  nm = paste(nm1,"_",nm2,"_",minlong,"_",minlat,"_",maxlong,"_",maxlat,".gif",sep = "")
  
  
  #animation = image_animate(img, fps = fps)
  image_write_gif(img, nm, delay = 1/fps)
  
  #ggsave("map_draft_2.png", width=24, height=14.5, units="cm", dpi=600)
}


########################################################################################
############### Other colour schemes



migrationmapgreen = function(n=1, 
                             Species1,SciName, rawpath1, rawpath2=NA, 
                             rawpathPhoto,  res = 120, range = 30,
                        step = 10, fps = 2, col1 = "red", col2 = "blue", 
                        pointsize, yaxis, world = F, 
                        minlong = -15, minlat = -33, maxlong = 180, maxlat = 70,
                        ggp,dataall,migstatus,credit,
                        impos,grpos)
{
  require(tidyverse)
  require(rgdal)
  require(magick)
  require(gridExtra)
  require(grid)
  require(ggpubr)
  require(extrafont)
  require(ggformula)
  require(zoo)
  
  freq = create_freq(Species = Species1, data = dataall, migstatus = migstatus)
  
  freq1 = freq[[1]]
  #freq2 = freq[[2]]
  #freq3 = freq[[3]]
  freq4 = freq[[2]]
  freq1$checklists[freq1$checklists == 0] = 1
  #freq2$checklists[freq2$checklists == 0] = 1
  #freq3$checklists[freq3$checklists == 0] = 1
  freq4$checklists[freq4$checklists == 0] = 1
  freq1$perc = (freq1$detected/freq1$checklists)*100
  #freq2$perc = (freq2$detected/freq2$checklists)*100
  #freq3$perc = (freq3$detected/freq3$checklists)*100
  freq4$perc = (freq4$detected/freq4$checklists)*100
  mdays = c(15.5,45.0,74.5,105.0,135.5,166.0,196.5,227.5,258.0,288.5,319.0,349.5)
  #mweek = rollmean(seq(0,365,7),2)
  mfort = rollmean(seq(0,365,14),2)
  #freq2$day = mdays
  #freq3$day = c(mweek,mweek[1]+365)
  freq4$day = c(mfort,mfort[1]+365)
  
  spl1 = smooth.spline(c(freq1$day,(freq1$day+365),(freq1$day+730)),rep(freq1$perc,3),nknots=30)
  #spl2 = smooth.spline(c(freq2$day,(freq2$day+365),(freq2$day+730)),rep(freq2$perc,3),nknots=30)
  #spl3 = smooth.spline(c(freq3$day,(freq3$day+365),(freq3$day+730)),rep(freq3$perc,3),nknots=30)
  spl4 = smooth.spline(c(freq4$day,(freq4$day+365),(freq4$day+730)),rep(freq4$perc,3),nknots=30)
  
  spl1a = predict(spl1,366:730)
  spl1a = as.data.frame(spl1a)
  spl1a$y[spl1a$y<0] = 0
  
  #spl2a = predict(spl2,366:730)
  #spl2a = as.data.frame(spl2a)
  #spl2a$y[spl2a$y<0] = 0
  
  #spl3a = predict(spl3,366:730)
  #spl3a = as.data.frame(spl3a)
  #spl3a$y[spl3a$y<0] = 0
  
  spl4a = predict(spl4,366:730)
  spl4a = as.data.frame(spl4a)
  spl4a$y[spl4a$y<0] = 0
  
  #print(freq[[2]])
  #print(spl4a)
  
  spl = spl4a
  spl$x = 1:365
  
  mx = max(na.omit(spl$y))
  yaxis = c(0,(mx+0.02))
  ybreaks = seq(0,yaxis[2],1)
  
  if (n==1)
  {
    data = readcleanrawdata(rawpath = rawpath1)
  }
  
  if (n!=1)
  {
    data1 = readcleanrawdata(rawpath = rawpath1)
    data2 = readcleanrawdata(rawpath = rawpath2)
    data = rbind(data1,data2)
    data1 = data1 %>% select(COMMON.NAME)
    data2 = data2 %>% select(COMMON.NAME)
  }
  
  PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
  #PROJ = "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
  
  projdat = project(cbind(data$LONGITUDE,data$LATITUDE), proj = PROJ)
  data = cbind(projdat, data)
  names(data)[1:2] = c("long","lat")
  
  data = data %>% select(long,lat,COMMON.NAME,day)
  
  
  min = project(cbind(minlong,minlat), proj = PROJ)
  max = project(cbind(maxlong,maxlat), proj = PROJ)
  
  windowsFonts("Gill Sans" = windowsFont("Gill Sans"))
  
  ##############
  
  if (n==1)
  {
    species = data$COMMON.NAME[1]
    cols = col1
    specs = species
    wd = strwidth(species,family = "Gill Sans",units = 'figure')
    wd = wd + 0.04
  }
  
  if (n==2)
  {
    spec1 = data1$COMMON.NAME[1]
    spec2 = data2$COMMON.NAME[1]
    specs = c(spec1,spec2)
    species = paste(specs[1],"(blue)","    ",specs[2],"(red)")
    if (sort(specs)[1] == specs[1])
    {
      cols = c(col2,col1)
    }
    if (sort(specs)[1] != specs[1])
    {
      cols = c(col1,col2)
    }
    wd = strwidth(species,family = "Gill Sans",units = 'figure')
    wd = wd + 0.04
  }
  
  mon = c(rep("January",31),rep("February",28),rep("March",31),rep("April",30),rep("May",31),rep("June",30),
          rep("July",31),rep("August",31),rep("September",30),rep("October",31),rep("November",30),
          rep("December",31))
  
  mlabs = c("J","F","M","A","M","J","J","A","S","O","N","D")
  
  img = image_graph(width = 1080, height = 810, res = res)
  #datalist = split(data, data$fort)
  
  l = list()
  nums = c(1:365,1:(range-1))
  x = c(seq(101,365,step),seq(1,100,step))
  
  ct = 0
  for (i in x)
  {
    ct = ct + 1
    l[[ct]] = nums[i:(i+range-1)]
    if(max(l[[ct]]) == 365 & min(l[[ct]]) == 1)
      l[[ct]][l[[ct]]<=365 & l[[ct]]>(365-range)] = l[[ct]][l[[ct]]<=365 & l[[ct]]>(365-range)] - 365
  }
  
  a = image_read(rawpathPhoto)
  a = image_scale(a, "300")
  a = image_border(a, "black", "3x3")
  a = image_annotate(a, credit, font = 'Gill Sans', size = 24, location = "+8+4")
  
  b = image_read("birdcountindia logo.png")
  b = image_scale(b, "300")
  b = image_background(b, "#b6ccb6", flatten = TRUE)
  b = image_border(b, "black", "3x3")
  
  c = image_read("eBird India logo.png")
  c = image_scale(c, "300")
  c = image_background(c, "#b6ccb6", flatten = TRUE)
  c = image_border(c, "black", "4x4")
  
  avg<-matrix(ncol = 2,nrow = 0)
  for (i in l){
    
    v1 = i
    if(min(v1) <= 0)
    {
      v1[v1<=0] = v1[v1<=0]+365
    }
    
    temp = data %>%
      filter(day %in% v1) %>%
      distinct(long,lat,COMMON.NAME)
    
    temp1 = freq1 %>%
      filter(day %in% v1) 
    
    med = floor(median(i))
    if (med == 0)
      med = 365
    if (med < 0)
      med = med + 365
    
    #cols = "#c26023"
    
    if (isTRUE(world))
    {
      p = ggp +
        if(switchs)geom_point(data = temp, aes(x = long,y = lat, col = COMMON.NAME, alpha = 0.99, stroke = 0), size = 1.5) +
        ggtitle(mon[med], size = 1) +
        theme(plot.title = element_text(hjust = 0.01, size = 10)) +
        scale_color_manual(breaks = sort(specs), values = cols) +
        theme(legend.position = "none")
    }
    else
    {
      p = ggp +
        geom_point(data = temp, aes(x = long,y = lat, col = COMMON.NAME, alpha = 0.99, stroke = 0), size = pointsize) +
        coord_cartesian(xlim = c(min[1],max[1]), ylim = c(min[2],max[2])) +
        theme(plot.title = element_text(hjust = 0.01, size = 10)) +
        scale_color_manual(breaks = sort(specs), values = cols) +
        #ggtitle(mon[med]) +
        theme(legend.position = "none")
      p1 = ggdraw(p)
    }
    
    xt = v1[floor(range/2)]
    #ggg<-c(((sum(temp1$detected)/sum(temp1$checklists))*100),xt)
    #avg<-rbind(avg,ggg, deparse.level = 0)
    #colnames(avg)<-c("frequency","day")
    #avg<-as.data.frame(avg)
    #print(avg)
    
    
    qi<-ggplot(freq1,aes(y = perc,x =  day)) + 
      #geom_point(size = 1.5)+
      #geom_line()+
      geom_line(data = spl, aes(x=x,y=y), size=1)+
      geom_vline(xintercept = xt, size = 0.1)+
      scale_x_continuous(limits = c(1,365), breaks= mdays, labels=mlabs)+
      scale_y_continuous(limits = yaxis, breaks= ybreaks)+
      xlab("months")+ ggtitle(paste("frequency in India ","(max ",round(mx,1),"%)",sep = ""))+
      theme(text=element_text(family="Gill Sans"))+
      theme(axis.title.x = element_blank(), axis.text.x = element_text(colour = "black",size = 8),
            axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(),
            plot.title = element_text(size = 10, hjust = 0.5)) +
      theme(panel.background = element_rect(fill = "#008000"),
            plot.margin = margin(1, 1, 1, 1, "mm"),
            plot.background = element_rect(
              fill = "#008000",
              colour = "black",
              size = 0.75),
            axis.line=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
    
    qj = ggplot() +
      theme(text=element_text(family="Gill Sans"))+
      theme(panel.background = element_rect(fill = "#b6ccb6"),
            plot.margin = margin(1, 1, 1, 1, "mm"),
            plot.background = element_rect(
              fill = "#b6ccb6",
              colour = "black",
              size = 0.5),
            axis.line=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank())
    
    qj1 = ggdraw(qj) +
      draw_label(species, 0.5, 0.59, size = 13, fontfamily="Gill Sans", fontface = 'bold', colour = "black")
    
    vv<-ggdraw(p1) + {if(impos == "R")draw_image(a, x = 1.017, y = 0.955, hjust = 1, vjust = 0.9, 
                                                 width = 0.25, height = 0.25)} +
      {if(impos == "L")draw_image(a, x = 0.234, y = 0.955, hjust = 1, vjust = 0.9, 
                                  width = 0.25, height = 0.25)} +
      {if(grpos == "R")draw_image(b, x = 0.21, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.230, height = 0.07)} +
      {if(grpos == "R")draw_image(c, x = 0.282, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.105, height = 0.07)} +
      {if(grpos == "L")draw_image(b, x = 0.923, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.230, height = 0.07)} +
      {if(grpos == "L")draw_image(c, x = 0.995, y = 0.0826, hjust = 1, vjust = 0.9, 
                                  width = 0.105, height = 0.07)}
    
    
    ## include in the function component, probably
    
    if (grpos == "L")
    {
      vp = viewport(width = 0.3, height = 0.2, x = 0.316,
                    y = unit(6.22, "lines"), just = c("right","top"))
    }
    
    if (grpos == "R")
    {
      vp = viewport(width = 0.3, height = 0.2, x = 0.685,
                    y = unit(6.22, "lines"), just = c("left","top"))
    }
    
    vq = viewport(width = wd, height = 0.04, x = 0.5,
                  y = 0.99, just = c("center","top"))
    
    
    full = function() {
      print(vv)
      print(qi, vp = vp)
      print(qj1, vp = vq)
    }
    
    full()
    
    #text1<- paste(Species1)
    #text2<-paste(SciName)
    #tgrob1 <- text_grob(text1, family = "Gill Sans", face = "bold", color = "black", vjust = 2, size = 16)
    #tgrob2 <- text_grob(text2, family = "Gill Sans", face = "italic", color = "black", vjust = -1, size = 12)
    #tgrob3 <- text_grob(mon[med], family = "Gill Sans", face = "bold", color = "black", vjust = 2, size = 18 )
    
    #r<-grid.arrange(arrangeGrob(arrangeGrob(tgrob1,tgrob2, ncol=1, nrow=2),qi, tgrob3, nrow = 3, ncol = 1),
    #arrangeGrob(vv, ncol=1, nrow=1), heights=c(100,1), widths=c(1,2))
    #print(r)
  }
  
  dev.off()
  
  if (n == 1)
  {
    nm = specs
    nm = paste(nm,"_",minlong,"_",minlat,"_",maxlong,"_",maxlat,".gif",sep = "")
  }
  
  if (n != 1)
  {
    nm1 = specs1
    nm2 = specs2
    nm = paste(nm1,"_",nm2,"_",minlong,"_",minlat,"_",maxlong,"_",maxlat,".gif",sep = "")
  }
  
  
  #animation = image_animate(img, fps = fps)
  image_write_gif(img, nm, delay = 1/fps)
  
  #ggsave("map_draft_2.png", width=24, height=14.5, units="cm", dpi=600)
}
