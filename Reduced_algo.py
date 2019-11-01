import pandas as pd
import numpy as np
import xlsxwriter as writer

df1 = pd.read_excel(open('clean2.xlsx','rb'), sheetname='clean2', header=None)
#df1 = pd.read_csv(open('Infection_Data.csv','rb'))
df = pd.read_excel(open('density_mat_clean2_0.001.xlsx','rb'), sheetname='Sheet1', header=None)
drop=[]
for i in range(len(df1.axes[1])):
    if(type(df1.iat[0,i])== str):
        drop.append(i)
df1.drop(df1.columns[drop], axis=1, inplace=True)


n=len(df1.axes[1])
drop_rows=[]
for index, row in df.iterrows():
    if(df.iloc[index].sum()== n):
        drop_rows.append(index)
df1.drop(df1.index[drop_rows], inplace=True)
df.drop(df.index[drop_rows], inplace=True)
d=len(df1.axes[0])
drop_cols=[]
for column in df:
    if(df[column].sum()== d):
        drop_cols.append(column)
        #df1.drop(df1.loc[column],axis=1, inplace=True)
df1.drop(df1.columns[drop_cols], axis=1, inplace=True)



writer1 = pd.ExcelWriter('red_mat_clean2_0.001.xlsx')
df1.to_excel(writer1,'Sheet1')
writer1.save()



