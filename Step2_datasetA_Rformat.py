# SPeicher als R data frame
from rpy2.robjects import pandas2ri
from rpy2.robjects import r
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import datetime

df_ts_index = pd.read_pickle('DataFrameOutput_pull_ts_comments_comp.pkl')

#http://stackoverflow.com/questions/11586582/save-2d-numpy-array-to-r-file-format-using-rpy2

R_df = pandas2ri.py2ri(df_ts_index)
r.assign("GH_data_df_comp", R_df)
r("save(GH_data_df_comp, file='RObject_comp.gzip', compress=TRUE)")