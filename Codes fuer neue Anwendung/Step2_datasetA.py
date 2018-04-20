# Read data and plot
import json, urllib, time, os
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# Speicherort ggf anpassen
#folder_name = '/Users/B.Bronk/'
folder_name = '/Volumes/SANDISK/GitHubData/'


# Liste wie in Step 1
repo_list = ['davisking/dlib',
            'elki-project/elki',
            'encog/encog-java-core',
            'h2oai/h2o-3',
            'numpy/numpy',
            'numenta/nupic',
            'deeplearning4j/deeplearning4j',
            'apache/spark',
            'openai/gym',
            'Artelnics/OpenNN',
            'biolab/orange3',
            'scikit-learn/scikit-learn',
            'shogun-toolbox/shogun',
            'tensorflow/tensorflow',
            'Theano/Theano',
            'torch/torch7',
            'fchollet/keras',
            'apache/hadoop',
            'mlpack/mlpack',
            'BVLC/caffe',
            'NervanaSystems/neon',
            'apache/kafka-site',
            'apache/storm',
            'marmanis/yooreeka',
            'wch/r-source']

#Beginnt Maerz 2011, endet Jan 2017
months_for_year = {2011:(2,3,4,5,6,7,8,9,10,11),
             2012:(0,1,2,3,4,5,6,7,8,9,10,11),
             2013:(0,1,2,3,11),
             2014:(0,1,2,3,4,5,6,7,8,9,10,11),
             2015:(0,1,2,3,4,5,6,7,8,9,10,11),
             2016:(0,1,2,3,4,5,6,7,8,9,10,11),
             2017:(0,1)}

#Schaltjahre
february_length = {2011:28, 
                   2012:29, 
                   2013:28, 
                   2014:28, 
                   2015:28, 
                   2016:29, 
                   2017:28}

#andere Datenstruktur im Laufe der Jahre
repo_dict = {"2017":('repo','name', 0),
             "2016":('repo','name', 0),
             "2015":('repo','name', 0),
             "2014":('repository','url', 19), 
             "2013":('repository','url', 19),
             "2012a":('repository','url', 19),
             "2012b":('repo','name', 0),
             "2011":('repo','name', 0)}

start = time.time()

year_list = sorted(list(months_for_year.keys()))
year_list = year_list[0:-1] # ohne 2011 und 2017

print(year_list)


repo_len = len(repo_list)
event_count = np.zeros((len(year_list), repo_len))
event_number = 0

resultDF_init_dict = {'repo_name': pd.Series([]), 'int_repo_id': pd.Series([]), 'event_type': pd.Series([]), 'event_time': pd.Series([]), 'user_name': pd.Series([]) }
resultDF = pd.DataFrame(resultDF_init_dict)
resultDF = pd.DataFrame(resultDF, columns=['repo_name', 'int_repo_id', 'event_type', 'event_time', 'user_name'])
tempDF = pd.DataFrame(resultDF_init_dict)
tempDF = pd.DataFrame(tempDF, columns=['repo_name', 'int_repo_id', 'event_type', 'event_time', 'user_name'])

for year in year_list:
#for  year in [2017]:
    
    month_list = list(months_for_year[year])
    for month in month_list:
#    for month in [0]:
        # Anfang 2012 Änderung der Datenstruktur
        if year == 2012: 
            if month in [0,1,2]:
                year_string = "2012b"
            else:
                year_string = "2012a"
        else:
            year_string = str(year)

        file_name = 'github_data_{0:4}_{1:02d}.txt'.format(year, month+1)
        file_name = folder_name + file_name
        end = time.time()
        print(end-start)
        print(file_name)
        
        with open(file_name, 'rb') as f:
            file_content = f.read()
        
        file_content_decode = file_content.decode("utf-8")
                # http://stackoverflow.com/questions/13938183/python-json-string-to-list-of-dictionaries-getting-error-when-iterating

#%%
        json_string0 = file_content_decode.replace('\n','').replace('\r', '') #returns entfernen
        json_string = json_string0[:-2]+']' #finales komma entfernen
        jdata = json.loads(json_string) #wandelt json_string in list of dictionaries um
        month_number = 0
        for events in jdata:
            repo_name = events[repo_dict[year_string][0]][repo_dict[year_string][1]][repo_dict[year_string][2]:]
            repo = repo_list.index(repo_name)
            event_count[year_list.index(year)][repo] = event_count[year_list.index(year)][repo] + 1
            event_number = event_number+1
            month_number = month_number+1
            event_type = events['type']
            event_time = events['created_at']
            
            if repo_dict[year_string][2] == 0:
                user_name = events['actor']['login']
            elif repo_dict[year_string][2] == 19:
                user_name = events['actor']
                
            tempDF.loc[month_number] = [repo_name, int(repo), event_type, event_time, user_name]
        resultDF = resultDF.append(tempDF, ignore_index=True) #http://pandas.pydata.org/pandas-docs/stable/generated/pandas.DataFrame.append.html
        tempDF = pd.DataFrame(resultDF_init_dict)
        tempDF = pd.DataFrame(tempDF, columns=['repo_name', 'int_repo_id', 'event_type', 'event_time', 'user_name'])

resultDF.to_pickle('DataFrameOutput_datasetA.pkl') # Speichern des Data Frames http://pandas.pydata.org/pandas-docs/stable/io.html#io-pickle
