# SPeicher als R data frame

from rpy2.robjects import pandas2ri
from rpy2.robjects import r
#import pandas.rpy.common as com
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import datetime

df_ts_index = pd.read_pickle('DataFrameOutput_datasetB_time.pkl')

#http://stackoverflow.com/questions/11586582/save-2d-numpy-array-to-r-file-format-using-rpy2

R_df = pandas2ri.py2ri(df_ts_index)
r.assign("GH_data_df_pull_comp", R_df)
r("save(GH_data_df_pull_comp, file='RObject_pull_comp_comments.gzip', compress=TRUE)")