# Load Data Frame Test
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import datetime

DFtest = pd.read_pickle('DataFrameOutput_datasetB.pkl')


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
DFcomplete.to_pickle('DataFrameOutput_datasetB_time.pkl')
##%%
#DFtest2 = DFtest 
#DFtest2.index = DT
#
#
##%%
#DFtest2['index2'] = DFtest.index
#df_ts_index = DFtest2
#DFtest2.to_pickle('DataFrameOutput_ts_index.pkl')
#
##%%
#DFtest_read = pd.read_pickle('/Users/B.Bronk/Dropbox/Python/IPython/DataFrameOutput_ts_index.pkl')
#
##%%
##df.groupby(['decade', 'haircolor']).haircolor.count().plot(kind='bar')
##http://pandas.pydata.org/pandas-docs/stable/timeseries.html#resampling
#
#
#DFtest['event_type'].unique()
#DFtest3 = DFtest2.loc[DFtest2['event_type']== 'ForkEvent']
#
#test3 = DFtest3.groupby('repo_name').resample('W').count()
#
#test3_name = test3['repo_name']
#
#test3_name.unstack(level=0).plot()
#
#
#
#
##%%
#pd.melt(test3_name, id_vars='group', value_vars=['flag_A', 'flag_B', 'flag_C'])