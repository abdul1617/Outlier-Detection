import pandas as pd
import Modified_knn as knn
import pprint as ppn
import math as m
import numpy as np
import xlsxwriter as writer
from  more_itertools import unique_everseen
import copy as cp

drop=[]
#df1 = pd.read_csv(open('Infection_Data.csv','rb'))
df1 = pd.read_excel(open('Internet_Usage_Data.xlsx','rb'), sheetname='Internet_Usage_Data',header=None)
for i in range(len(df1.axes[1])):
    if(type(df1.iat[0,i])== str):
        drop.append(i)
df1.drop(df1.columns[drop], axis=1, inplace=True)
#df1.drop(df1.columns[[0, 1]], axis=1, inplace=True)
#sp_mat =[[0] *(len(df1.axes[1]))]*len(df1.axes[0])
sp_mat= np.zeros((len(df1.axes[0]),len(df1.axes[1])),dtype=np.float)
c=[[0] *(len(df1.axes[1]))]*len(df1.axes[0])
l=len(df1.axes[0])
#k = round(m.sqrt(len(df1.axes[0])))
neighbours=[]
for x in range(len(df1.columns)):
    d=df1.iloc[:,[x]]
    dlist = d.values.tolist()
    result1 = []
    [ result1.extend(el) for el in dlist]
    result=list(unique_everseen(result1))
    result.sort()
    distances=[]
    if len(result)==1:
        distances.append(0)
    else:
        for t in range(len(result)-1):
            distances.append(abs(result[t+1]-result[t]))
        distances.append(abs(result[t+1]-result[t]))

    for z in range(len(dlist)):
        y=result.index(dlist[z][0])
        if len(result)==1:
            OD=0
        elif y==0 :
            B_OD=abs(result[y+1]-result[y])
            if len(distances[1:])==0:
                B_Dif=0
            else:
                B_Dif=np.mean(distances[1:])
            #B_Dif=abs(result[len(result)-1]-result[y])/(l-1)

            if np.isnan(B_Dif) or B_Dif==0:
                OD=0
            else:
                OD=B_OD/B_Dif
        elif y==len(result)-1:
            F_OD=abs(result[y]-result[y-1])
            #F_Dif=abs(result[y]-result[0])/(l-1)
            if len(distances[:l])==0:
                F_Dif=0
            else:
                F_Dif=np.mean(distances[:l])
            if np.isnan(F_Dif) or F_Dif==0:
                OD=0
            else:
                OD=F_OD/F_Dif
        else :
            B_OD=abs(result[y+1]-result[y])
            F_OD=abs(result[y]-result[y-1])
            #B_Dif=abs(result[len(result)-1]-result[y])/(l-y)
            if len(distances[y+1:])==0:
                B_Dif=0
            else:
                B_Dif=np.mean(distances[y+1:])
            #F_Dif=abs(result[y]-result[0])/y
            if len(distances[:y-1])==0:
                F_Dif=0
            else:
                F_Dif=np.mean(distances[:y-1])
            if np.isnan(B_Dif) or B_Dif==0:
                Br_OD=0
            else:
                Br_OD=B_OD/B_Dif
            if np.isnan(F_Dif) or F_Dif==0:
                Fr_OD=0
            else:
                Fr_OD=F_OD/F_Dif
            OD=Br_OD+Fr_OD

        sp_mat[z][x]=OD

# writer1 = pd.ExcelWriter('output.xlsx')
# df1.to_excel(writer1,'Sheet1')
# writer1.save()
col=0
workbook = writer.Workbook('Internet_Usage_Data_Sparse_Mat.xlsx')
worksheet = workbook.add_worksheet()
for row, data in enumerate(sp_mat):
    worksheet.write_row(row, col, data)
workbook.close()
print('Success')
#ppn.pprint(sp_mat)






