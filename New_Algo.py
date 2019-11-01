import pandas as pd
import pprint as ppn
import math as m
import numpy as np
import xlsxwriter as writer
import copy as cp
import heapq as hq
from scipy.spatial import distance as dist
import sys
from operator import itemgetter, attrgetter
import collections as col
import KNN_Implmnt as knn
import csv

drop=[]
#df1 = pd.read_csv(open('Infection_Data.csv','rb'))
df = pd.read_excel(open('ion_235.xlsx','rb'), sheet_name='Sheet1',header=None)
for i in range(len(df.axes[1])):
    if(type(df.iat[0,i])== str):
        drop.append(i)
df.drop(df.columns[drop], axis=1, inplace=True)
k=11
#k = int(round(m.sqrt(len(df.axes[0]))))
neighList=[]
for index, row in df.iterrows():
    d=df.iloc[index]
    b=np.array(d.values)
    neigh=knn.knn(df,k,b)
    neigh.remove(index)
    neighList.append(neigh)
Rev_neighList=[]
for index, row in df.iterrows():
    temp_lst=[]
    for x in range(len(neighList)):
        if index in neighList[x]:
            temp_lst.append(x)
    Rev_neighList.append(temp_lst)
h=0.1
d=len(df.axes[1])
den_xp=[]
for index, row in df.iterrows():
    neigh_set=set(neighList[index])
    rev_neigh_set=set(Rev_neighList[index])
    I_Set=neigh_set.union(rev_neigh_set)
    I_Set_Card=len(I_Set) #Cardinality of I_Set
    I_Set_Ext=I_Set
    I_Set_Ext.add(index)
    denom=(1/(I_Set_Card))
    #denom=(1/(I_Set_Card+1))
    const_deno=(1/((h**d)*(1.5*((m.pi*2)**(d/2)))))
    tmp_den_xp=0
    d1=df.iloc[index]
    j=np.array(d1.values)

    for y in I_Set_Ext:
        k_row=df.iloc[y]
        k=np.array(k_row.values)
        dist1 = np.linalg.norm(j-k)
        power=(-1)*(dist1**2)/(2*(h**2))
        tmp=denom*const_deno*m.exp(power)
        tmp_den_xp=tmp_den_xp+tmp

        print(tmp_den_xp)
    den_xp.append(tmp_den_xp)
    print(I_Set_Ext)
AF=[]
for index, row in df.iterrows():
    neigh_set=set(neighList[index])
    rev_neigh_set=set(Rev_neighList[index])
    I_Set=neigh_set.union(rev_neigh_set)
    I_Set_Card=len(I_Set) #Cardinality of I_Set
    den_xp_sum=0
    for z in I_Set:
        den_tmp=(den_xp[index]-den_xp[z])**2
        den_xp_sum+=den_tmp
    AF.append(den_xp_sum/I_Set_Card)
OD=[]
for index, row in df.iterrows():
    neigh_set=set(neighList[index])
    rev_neigh_set=set(Rev_neighList[index])
    I_Set=neigh_set.union(rev_neigh_set)
    I_Set_Card=len(I_Set) #Cardinality of I_Set
    AF_xp_sum=0
    for z in I_Set:
        AF_xp_sum+=AF[z]
    if AF_xp_sum==0:
        AF_xp_sum=1
    OD_tmp=AF[index]*I_Set_Card/AF_xp_sum
    OD.append(OD_tmp)
unDict =dict()

for p in range(len(OD)):
    unDict[p]=OD[p]
odr = col.OrderedDict(sorted(unDict.items(), key=itemgetter(1)))



w = csv.writer(open("ion_235_output.csv", "w"))
for key, val in odr.items():
    w.writerow([key, val])

print('Success')