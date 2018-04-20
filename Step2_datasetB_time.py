# Load Data Frame Test
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import datetime

DF= pd.read_pickle('DataFrameOutput_pull_comments_PC.pkl')
DFrest = pd.read_pickle('DataFrameOutput_pull_comments_rest.pkl')

DFtest = DF.append(DFrest, ignore_index=True)

DFtest.to_pickle('DataFrameOutput_pull_comments_comp.pkl')

DFtest = pd.read_pickle('DataFrameOutput_pull_comments_comp.pkl')


#%%
DT = []
for i in range(len(DFtest)):
    try:
        date_list = DFtest.loc[i]['event_time'].replace('-', ' ').replace(':', ' ').replace('T', ' ').strip('Z').split()
        date_list = [ int(x) for x in date_list]
    except:
        date_list = DFtest.loc[i]['event_time'].replace('/', ' ').replace(':', ' ').replace('T', ' ').strip('Z')[:-5].split()
        date_list = [ int(x) for x in date_list]
    dt = datetime.datetime(date_list[0], date_list[1], date_list[2], date_list[3], date_list[4], date_list[5])
    DT.append(pd.Timestamp(dt))
    
# http://stackoverflow.com/questions/13703720/converting-between-datetime-timestamp-and-datetime64?noredirect=1&lq=1
pd.DatetimeIndex([dt])[0]
np.datetime64(dt)

DFtest3 = DFtest
DFtest3['event_time'] = DT
DFcomplete = DFtest3
DFcomplete.to_pickle('DataFrameOutput_pull_ts_comments_comp.pkl')
