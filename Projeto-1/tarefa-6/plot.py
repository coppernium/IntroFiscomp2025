import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

df1 = pd.read_csv("fort.1", header=None,sep='   ')
df2 = pd.read_csv("fort.2", header=None,sep='   ')

plt.plot(df1[0],df1[1])
plt.plot(df2[0],df2[1])
plt.show()