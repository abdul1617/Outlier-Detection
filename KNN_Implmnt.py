import pandas as pd
import pprint as ppn
import math as m
import numpy as np
import xlsxwriter as writer
import copy as cp
import heapq as hq
from scipy.spatial import distance as dist
import sys

drop=[]
#df1 = pd.read_csv(open('Infection_Data.csv','rb'))
# df = pd.read_excel(open('DataTest.xlsx','rb'), sheet_name='Sheet1',header=None)
# for i in range(len(df.axes[1])):
#     if(type(df.iat[0,i])== str):
#         drop.append(i)
# df.drop(df.columns[drop], axis=1, inplace=True)
def knn(df,k,b):
    #k=4
    #d1=df.iloc[1]
    #b=np.array(d1.values)
    #df.drop(df.index[ind])
    distList=[]
    for index, row in df.iterrows():
        d=df.iloc[index]
        a=np.array(d.values)
        dist = np.linalg.norm(a-b)#It will calculate euclidean distance
        distList.append(dist)#It will store all euclidean distance in a list
    #print(distList)
    SortList=sorted(distList)
    neighbours=SortList[0:k]
    #print(neighbours)
    neighList=[]
    for x in neighbours:
        i=distList.index(x)
        neighList.append(i)
    return  neighList

print()








