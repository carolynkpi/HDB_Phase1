library(dplyr)
library(ggplot2)
library(grid)
library(gtable)

load('cleaned_data')

# Get names ####
renameVariable <- function(x) {
  words = strsplit(x, '_')[[1]]
  new_name = vector()
  for (i in words){
    s= toupper(substring(i, 1,1))
    new_word = paste0(s, substring(i,2,nchar(i)))
    if(i==words[1]){new_name = new_word}
    else {new_name = paste(new_name, new_word)}
  }
  return(new_name)
}
old_names = names(data)
display_names = sapply(old_names, renameVariable)
getDisplayName = function(old_name){
  return(display_names[old_name])
}
getOldName = function(display_name){
  if(display_name %in% display_names){
    return(names(display_names[display_names == display_name]))
  }else{
    return(display_name)
  }
}

# identify categorical and numerical variables ####
fac = vector()
num = vector()
for(i in names(data)){
  fac[i] = is.factor(data[[i]]) 
  num[i] = is.numeric(data[[i]])
}
fac_var = names(data)[fac]
num_var = names(data)[num]

#overwrite and rearrange variables
fac_var = c('year', 'month', 'region','town',
            'flat_type', 'flat_model', 'storey_range',
            'sqft_bin', 'age_bin', 
            'resale_price_bin', 'price_psf_bin')
num_var = c('resale_price', 'price_per_sqft',
            'floor_area_sqft', 'floor_area_sqm',
            'age')
all_var = c('year', 'month', 'region','town',
            'flat_type', 'flat_model', 'storey_range',
            'floor_area_sqm','floor_area_sqft', 'sqft_bin',
            'age','age_bin', 
            'resale_price','resale_price_bin',
            'price_per_sqft','price_psf_bin')

fac_var_d = unname(getDisplayName(fac_var))
num_var_d = unname(getDisplayName(num_var))
all_var_d = unname(getDisplayName(all_var))
remove(fac,num)

# pareto function ####
pareto = function(data, cat_var, xlab=NA, ylab = 'Frequency'){
  if (!cat_var %in% names(data)){return('Error: Variable not found in data frame')}
  if (!is.factor(data[[cat_var]])){return('Error: Variable is not factor type')}
  
  if (is.na(xlab)){xlab=cat_var}
  dt= as.data.frame(sort(table(data[cat_var], dnn=xlab), decreasing=TRUE), responseName = ylab)
  q = ggplot(dt,aes_string(xlab,ylab)) + geom_bar(stat='identity') 
  q = q + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  q = q + labs(x = getDisplayName(cat_var))
  print(q)
}

# ctable_bar function ####
ctable_bar = function(dt, rowlab='row',collab='col'){
  df = as.data.frame(dt)
  names(df) = c(rowlab, collab,'frequency')
  g = qplot(df[[rowlab]], df[['frequency']],fill = df[[collab]]) + 
    geom_bar(stat='identity',colour = 'black') + 
    labs(x=getDisplayName(rowlab), y = 'Frequency', fill = getDisplayName(collab)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(g)
}
# ctable_graph function ####
ctable_graph = function(dt,rowlab="row", collab="col"){
  x = 1:ncol(dt)
  y = 1:nrow(dt)
  centers <- expand.grid(y,x)
  par(mar = c(3,13,8,1))
  colfunc <- colorRampPalette(c("aliceblue","blue"))
  image(x, y, t(dt),
        col = colfunc(as.integer(max(dt)/10)+1),
        breaks = seq(0,as.integer(max(dt)/10)*10+10,10),
        xaxt = 'n', 
        yaxt = 'n', 
        xlab = '', 
        ylab = '',
        ylim = c(max(y) + 0.5, min(y) - 0.5)
  )
  text(centers[,2], centers[,1], c(dt), col= "black")
  mtext(attributes(dt)$dimnames[[2]], at=1:ncol(dt), padj = -1, las = 2)
  mtext(attributes(dt)$dimnames[[1]], at=1:nrow(dt), side = 2, las = 1, adj = 1.2)
  abline(h=y + 0.5)
  abline(v=x + 0.5)
  mtext(paste("Contingency table of",getDisplayName(rowlab),"vs", getDisplayName(collab) ,sep=" "),side=1,font=2)
  #title(main= paste("Contingency Table of",rowlab,"vs", collab ,sep=" "))
}





# unidist_num function ####
unidist_num = function(data, num_var, cat1, cat1vals){
  temp = as.data.frame(data
                       %>% filter_(.dots= paste0(cat1, '%in% cat1vals')))
  
  for(i in num_var){
    h = ggplotGrob(qplot(temp[[i]],main = getDisplayName(i), xlab=NULL, ylab = 'Frequency')+geom_vline(xintercept=mean(temp[[i]]),col='red') + scale_x_continuous(limits=c(min(temp[[i]]), max(temp[[i]]))))
    b = ggplotGrob(qplot('',temp[[i]], xlab = NULL, ylab =NULL, geom='boxplot')+coord_flip()+ scale_y_continuous(limits=c(min(temp[[i]]), max(temp[[i]]))))
    maxwidths <- grid::unit.pmax(h$widths[2:5], b$widths[2:5])
    h$widths[2:5] <- as.list(maxwidths)
    b$widths[2:5] <- as.list(maxwidths)
    g <- gtable_matrix(name = getDisplayName(i),
                       grobs = matrix(list(h, b), nrow = 2), 
                       widths = unit(6.5, "in"),
                       heights = unit(c(2.8, 0.7), "in"))
    grid.newpage()
    grid.draw(g)
  }
}

# unidist_cat function ####
unidist_cat = function(data, fac_var, cat1, cat1vals){
  temp = as.data.frame(data
                       %>% filter_(.dots= paste0(cat1, '%in% cat1vals')))
  
  for(i in fac_var){
    pareto(temp,i)
  }
}



# unidist_summary function ####
unidist_summary = function(data, var, cat1, cat1vals){
  temp = as.data.frame(data
                       %>% filter_(.dots= paste0(cat1, '%in% cat1vals')))
  s = summary(temp[[var]])
  if(is.numeric(temp[[var]])){s['Std. Dev.'] = sd(temp[[var]])}
  s
}

# bidist_cat_cat functions ####
bidist_cat_cat_table = function(data, cat1, cat2){
  ctable_graph(table(data[[cat1]],data[[cat2]]),cat1,cat2)
}
bidist_cat_cat_bar = function(data, cat1, cat2){
  ctable_bar(table(data[[cat1]],data[[cat2]]),cat1,cat2)
}


# bidist_cat_num function ####
bidist_cat_num = function(data, cat1, num1){
  qplot(data[[cat1]],data[[num1]],geom='boxplot',xlab=getDisplayName(cat1),ylab=getDisplayName(num1)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


# bidist_num_num function ####
bidist_num_num = function (data, num1, num2){
  qplot(data[[num1]],data[[num2]],xlab=getDisplayName(num1),ylab=getDisplayName(num2))
}



# mdist_2num_1cat function ####
multidist_2num_1cat = function(data, num1, num2, cat1){
  qplot(data[[num1]], data[[num2]], colour = data[[cat1]])+
    labs(x = getDisplayName(num1), y = getDisplayName(num2), colour = getDisplayName(cat1))
}
# mdist_1num_2cat function ####
multidist_1num_2cat = function(data, cat1, num1, cat2){
  qplot(data[[cat1]], data[[num1]], colour = data[[cat2]])+
    labs(x = getDisplayName(cat1),y = getDisplayName(num1), colour = getDisplayName(cat2)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
# dist function ####
dist = function(data, x, y, pan =NULL, pan_vals=NULL, col=NULL, col_vals=NULL){
  
  if(pan == 'Not Required'){
    pan = NULL
    pan_vals = NULL
  }
  if(col == 'Not Required'){
    col = NULL
    col_vals = NULL
  }
  
  temp = data
  if(!is.null(pan_vals)){temp = temp %>% filter_(.dots= paste0(pan, ' %in% pan_vals'))}
  if(!is.null(col_vals)){temp = temp %>% filter_(.dots= paste0(col, ' %in% col_vals'))}
  temp = as.data.frame(temp)
  
  g = ggplot(temp, aes_string(x,y)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  if(is.factor(data[[x]]) & is.factor(data[[y]])){
    if(!is.null(pan)){
      df = as.data.frame(table(temp[[x]],temp[[y]],temp[[pan]]))
      names(df) = c(x, y,pan,'frequency')
    }else{
      df = as.data.frame(table(temp[[x]],temp[[y]]))
      names(df) = c(x, y,'frequency')
    }
    
    g = ggplot(df, aes_string(x,'frequency')) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    g = g + geom_col(aes(fill = df[[y]]))
    g = g + labs(x = getDisplayName(x),y = 'Frequency', fill = getDisplayName(y)) 
  }
  
  if(is.factor(data[[x]]) & is.numeric(data[[y]])){
    if(!is.null(col)){
      g = g + geom_point(aes(col=temp[[col]]))
      g = g + labs(x = getDisplayName(x),y = getDisplayName(y), col = getDisplayName(col)) 
    }else{
      g = g + geom_boxplot()
      g = g + labs(x = getDisplayName(x),y = getDisplayName(y)) 
    }
  }
  
  if(is.numeric(data[[x]]) & is.numeric(data[[y]])){
    if(!is.null(col)){
      g = g + geom_point(aes(col=temp[[col]]))
      g = g + labs(x = getDisplayName(x), y = getDisplayName(y), col = getDisplayName(col))
    }
    else{
      g = g + geom_point()
      g = g + labs(x = getDisplayName(x), y = getDisplayName(y))
    }
  }
  
  if(!is.null(pan)){g = g + facet_wrap(as.formula(paste("~", pan)), nrow = 2, scales = 'free')}
  
  print(g)
}

# num_trend_gen function ####
num_trend_gen = function(data, num1, func){
  func2 = paste0(tolower(substring(func,1,1)), substring(func,2,nchar(func)))
  ggplot(data, aes_string(x='date', y=num1)) +
    stat_summary(fun.y = func2, 
                 geom='line') +
    labs(title =paste(func, 'of', getDisplayName(num1), 'Over Time', sep=' '), x = 'Date', y = paste(func, 'of', getDisplayName(num1)))
}

# num_trend_bycat function ####
num_trend_bycat = function(data, num1, cat1, cat1vals, func){
  if (func == "Mean"){f = function(x){mean(x)}}
  else {
    if (func == "Median"){f = function(x){median(x)}}
    else {return("Error: func needs to be Mean or Median in quotes and caps first letter")}
  }

  temp = as.data.frame(data 
                       %>% group_by_(.dots=c(cat1,'date'))
                       %>% summarise_(.dots= paste('summary = f(',num1,')',sep=''))
                       %>% filter_(.dots= paste0(cat1, '%in% cat1vals')))
  
  names(temp) = c(cat1, 'date','summary')
  g = ggplot(temp, aes(date, summary))
  g = g + geom_line(aes_string(color=cat1))
  g = g + labs(x = 'Date', y = paste(func, "of", getDisplayName(num1), sep = " "), color = getDisplayName(cat1) )
  print(g)
}



# agg_bi function ####
agg_bi = function(data, cat1, num1, cat1vals, func, desc = FALSE){
  if (func == "Mean"){f = function(x){mean(x)}}
  else {
    if (func == "Median"){f = function(x){median(x)}}
    else {return("Error: func needs to be Mean or Median in quotes and caps first letter")}
  }  
  
  temp = as.data.frame(data[c(cat1,num1)]
                       %>% group_by_(cat1)
                       %>% summarise_(.dots= paste('summary = f(',num1,')',sep=''))
                       %>% filter_(.dots= paste0(cat1, '%in% cat1vals')))
  
  names(temp) = c(cat1, 'summary')
  
  if(desc){
    g =ggplot(data = temp, aes_string(x=reorder(temp[[cat1]],temp[['summary']], function(x)-mean(x)), y='summary'))
  }else{
    g =ggplot(data = temp, aes_string(x=cat1, y='summary'))
  }
  g = g + 
    geom_bar(stat='identity') + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    labs(x= getDisplayName(cat1), y = paste(func,'of', getDisplayName(num1), sep= ' '))
  print(g)
}

# agg_tri function ####
agg_tri = function(data, cat1, num1, cat2, cat1vals, cat2vals, func, desc = FALSE){
  if (func == "Mean"){f = function(x){mean(x)}}
  else {
    if (func == "Median"){f = function(x){median(x)}}
    else {return("Error: func needs to be Mean or Median in quotes and caps first letter")}
  }  
  
  temp = as.data.frame(data %>% group_by_(.dots=c(cat1, cat2))
              %>% summarise_(.dots= paste('summary = f(',num1,')',sep=''))
              %>% filter_(.dots= paste0(cat2, '%in% cat2vals'))
              %>% filter_(.dots= paste0(cat1, '%in% cat1vals')))
  
  names(temp) = c(cat1, cat2,'summary')
  desc_sum <- paste0('desc(', 'summary)')
  temp2 = as.data.frame(temp %>% arrange_(.dots= c(cat2,desc_sum)) 
                             %>% mutate(.r = row_number()))

  if(desc){
    g =ggplot(data = temp2, aes(.r,summary))
    g = g + facet_wrap(as.formula(paste("~", cat2)), nrow = 2, scales = 'free')
    g = g + geom_col()
    g = g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    g = g + scale_x_continuous(breaks = temp2$.r, labels = temp2[[cat1]])
    g = g + labs(x = getDisplayName(cat1), y = paste(func, 'of', getDisplayName(num1), sep = ' '), facet = getDisplayName(cat2))
    g
    
  }else{
    g =ggplot(data = temp, aes_string(x=cat1, y='summary'))
    g = g + facet_wrap(as.formula(paste("~", cat2)), nrow = 2, scales = 'free')
    g = g + geom_col()
    g = g + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    g = g + labs(x = getDisplayName(cat1), y = paste(func, 'of', getDisplayName(num1), sep = ' '))
    g
  } 
}



