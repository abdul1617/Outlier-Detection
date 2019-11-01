import pandas as pd
import math as m
import numpy as np
import xlsxwriter as writer
import heapq as hq
drop=[]
#df1 = pd.read_csv(open('Infection_Data.csv','rb'))
df1 = pd.read_excel(open('try.xlsx','rb'), sheetname='Sheet1',header=None)
for i in range(len(df1.axes[1])):
    if(type(df1.iat[0,i])== str):
        drop.append(i)
df1.drop(df1.columns[drop], axis=1, inplace=True)
#df1.drop(df1.columns[[0, 1]], axis=1, inplace=True)
#sp_mat =[[0] *(len(df1.axes[1]))]*len(df1.axes[0])
sp_mat= np.zeros((len(df1.axes[0]),len(df1.axes[1])),dtype=np.float)
c=[[0] *(len(df1.axes[1]))]*len(df1.axes[0])
k = int(round(m.sqrt(len(df1.axes[0]))))
neighbours=[]
for x in range(len(df1.columns)):
    d=df1.iloc[:,[x]]
    dlist = d.values.tolist()
    result = [item for sublist in dlist for item in sublist]
    result.sort()
    for y in range(len(dlist)):
        neighbours= hq.nsmallest(k,result,key=lambda z: abs(z - (dlist[y][0])))#Find K nearest neighbour
        neighbours.append(dlist[y][0])#Union between k nearest neighbour and element itself
        c[y][x]=sum(map(float,neighbours))/(k+1)
        ng=np.array(neighbours)
        ng=(ng-c[y][x])
        sp_mat[y][x]=np.sum(ng**2)/(k+1)

col=0
workbook = writer.Workbook('try_out2.xlsx')
worksheet = workbook.add_worksheet()
for row, data in enumerate(sp_mat):
    worksheet.write_row(row, col, data)
workbook.close()
print('Success')
