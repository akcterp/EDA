#' EDA Automation
#' @param values
#' @export
Graphs <- function(data,v=c(),location = "D://Praxis//Term 2//R")
{
  initial = getwd()
  if(!is.data.frame(data))
    stop("The given object is not a data frame")
  if(missing(v))
  {
    v = c(1:ncol(data)) # if the user doesn't specify the second argument which is the index of columns
    # for which he wants the graphs, then set the second argument to all the cols in the dataset
  }

  df = data.frame(data)
  nums = unlist(lapply(df, is.numeric))
  data_num = df[ , nums]

  for(i in 1:ncol(data))
  {
    if((is.numeric(data[,i]))&(i %in% v))
      # the condition checks if each column is numeric and if the column index is passed by the
      # user while calling the fuction Graphs
    {
      setwd(location)
      png(paste(names(data)[i], ".png", sep=""))

      op = par(mfrow=c(2,1),mar = c(9,3,3,1) + 0.1)
      details = summary(data[,i])
      summStr = paste(names(details), format(details, digits = 2), collapse = "; ")


      boxplot(data[,i], main = paste("Boxplot of", names(data)[i]),
              ylab = names(data)[i], col = "maroon", border = "grey5",
              horizontal = T)

      mtext(class(data[,i]),side = 2,col = "red")

      hist(data[,i], main = paste("Histogram of", names(data)[i]),
           xlab = names(data)[i], ylab = "No. of Houses", col = "lightgreen", border=F)

      title(sub = summStr, line = 4.5)
      par(op)
      # dir.create(location)
      dev.off()  #NOTE this step

    }

    if(!i(s.numeric(data[,i]))&(i %in% v))
      # checks if column is categorical and if column is selected by user
    {
      setwd(location)
      png(paste(names(data)[i], ".png", sep="")) #NOTE this step


      op = par(mfrow=c(2,1),mar = c(9,3,3,1) + 0.1)
      details = summary(data[,i])
      summStr = paste(names(details), format(details, digits = 2), collapse = "; ")

      counts = table(data[i]) # table of frequency of each level
      barplot(counts,main= paste("Distribution of ",names(data[i])),col = "red",horiz =FALSE,
              border = "black",xlab = names(data[i]),ylab = "Frequency")

      level = names(table(data[,i])) # gives levels of the categorical variable

      slices = c(counts) # tells the proportion of each level
      pct = round(slices/sum(slices)*100) # percentage of each level
      lbls = c(level) # creating labels for the pie chart
      lbls = paste(lbls, pct) # add percents to labels
      lbls = paste(lbls,"%",sep="") # add % to labels


      pie(slices, labels = lbls, main=paste("Pie Chart of ",names(data[i])),
          col=rainbow(length(lbls)),radius = 0.5)

      mtext(class(data[,i]),side = 2,col = "red")

      title(sub = summStr, line = 4.5)
      par(op)
      dev.off()  #NOTE this step
    }
  }

  plot_str(data)
  introduce(data)

  png(paste("Scatter", ".png", sep=""))
  plot(data_num)
  dev.off()  #NOTE this step

  png(paste("HeatMap", ".png", sep=""))
  M = cor(data_num) # get correlations
  library('corrplot') #package corrplot
  corrplot(M, method = "circle")
  dev.off()  #NOTE this step

  png(paste("BasicInfo", ".png", sep=""))
  plot_str(data)
  dev.off()  #NOTE this step

  png(paste("BasicInfoPlot", ".png", sep=""))
  plot_intro(data)
  dev.off()  #NOTE this step

  png(paste("HeatMap1", ".png", sep=""))
  plot_correlation(na.omit(data_num), maxcat = 5L)
  dev.off()  #NOTE this step

  png(paste("MissingPlot", ".png", sep=""))
  plot_missing(data)
  dev.off()  #NOTE this step


  setwd(initial)
}








