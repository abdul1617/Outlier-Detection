import pandas as pd
import numpy as np
import xlsxwriter as writer

df = pd.read_excel(open('sparse_clean2_out.xlsx','rb'), sheetname='Sheet1', header=None)
sp_mat= np.zeros((len(df.axes[0]),len(df.axes[1])),dtype=np.float)
for index, row in df.iterrows():
    for i in range(len(row)):
        if df.iat[index,i] > 0.001:
            sp_mat[index][i]=1
        else :
            sp_mat[index][i]=0

col=0
workbook = writer.Workbook('density_mat_clean2_0.001.xlsx')
worksheet = workbook.add_worksheet()
for row, data in enumerate(sp_mat):
    worksheet.write_row(row, col, data)

workbook.close()

print(sp_mat)

