Y = read.csv('C:/Users/ABDUL WAHID/Google Drive/Iit Ism_/2019/new work/project/data5.csv')
X = Y[1:2]
pal <- c("Red", "Green")
p <- plot_ly(data = Y, x = Y[,1], y = Y[,2], color = Y[,8], colors = pal)
print(p)
#chart_link = api_create(p, filename="scatter-color-custom")
#chart_link

