# -*- coding: utf-8 -*-
"""
Created on Mon Aug 29 18:31:23 2022

@author: Yurim
"""

import os
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

os.chdir(r'E:\2022\0_study\설문조사_통계상담')

data1 = pd.read_csv('설문조사.csv', encoding='cp949')
data1 = data1.T
data2 = pd.read_csv('설문조사 (1).csv', encoding='cp949')

data2.columns


# =============================================================================
# 빈도수 시각화
# =============================================================================
sum1 = data2[['1-1', '1-2', '1-3']].sum()

x = np.arange(3)
values = sum1.values
plt.bar(x, values)
plt.xticks(x, sum1.index)
plt.show()

