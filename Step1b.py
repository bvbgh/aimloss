# Test output
# python 3, load libraries
# speicherproblem gelöst ja
# iteration über ganzes Jahr
# irgendwann in 2014 wurde repository erst in repo abgekürzt
# anfang 2011-2012 wieder repo statt repository
import json, gzip, urllib, time, os


tempDelete = 0
temp_folder = 'C:\\Users\\<USERNAME>\\AppData\\Local\\Temp' #Windows

# Speicherort ggf anpassen
#folder_name = '/Users/B.Bronk/'
folder_name = ''

#tmp_data_before = set(os.listdir('C:\\Users\\Benedikt\\AppData\\Local\\Temp'))
repo_list = ['davisking/dlib',
            'elki-project/elki',
            'encog/encog-java-core',
            'h2oai/h2o-3',
            'apache/hadoop',
            'mlpack/mlpack',
            'waikato/moa',
            'numpy/numpy',
            'numenta/nupic',
            'deeplearning4j/deeplearning4j',
            'apache/spark',
            'openai/gym',
            'Artelnics/OpenNN',
            'biolab/orange3',
            'wch/r-source',
            'scikit-learn/scikit-learn',
            'shogun-toolbox/shogun',
            'tensorflow/tensorflow',
            'Theano/Theano',
            'torch/torch7',
            'fchollet/keras',
            'marmanis/yooreeka']

repo_list = ['apache/hadoop',
            'mlpack/mlpack',
            'BVLC/caffe',
            'NervanaSystems/neon',
            'apache/kafka-site',
            'apache/storm']

#Beginnt Maerz 2011, endet Jan 2017
months_for_year = {2011:(2,3,4,5,6,7,8,9,10,11),
             2012:(0,1,2,3,4,5,6,7,8,9,10,11),
#             2013:(0,1,2,3,4,5,6,7,8,9,10,11),
#             2013:(0,1,2,3,11),
             2013:(4,5,6,7,8,9,10),
             2014:(0,1,2,3,4,5,6,7,8,9,10,11),
             2015:(0,1,2,3,4,5,6,7,8,9,10,11),
             2016:(0,1,2,3,4,5,6,7,8,9,10,11),
             2017:(0)}

#months_for_year = {2017:(1,2)}

#Schaltjahre
february_length = {2011:28, 
                   2012:29, 
                   2013:28, 
                   2014:28, 
                   2015:28, 
                   2016:29, 
                   2017:28}

#andere Datenstruktur im Laufe der Jahre
repo_name = {"2017":('repo','name', 0),
             "2016":('repo','name', 0),
             "2015":('repo','name', 0),
             "2014":('repository','url', 19), 
             "2013":('repository','url', 19),
             "2012a":('repository','url', 19),
             "2012b":('repo','name', 0),
             "2011":('repo','name', 0)}

start = time.time()

year_list = list(months_for_year.keys())
year_list = [2013,2018]
for year in year_list:
    month_list = list(months_for_year[year])
    for month in month_list:
        
        # Anfang 2012 Änderung der Datenstruktur
        if year == 2012: 
            if month in [0,1,2]:
                year_string = "2012b"
            else:
                year_string = "2012a"
        else:
            year_string = str(year)
        
        # Dateistruktur fuer USB Stick
        file_name = 'github_data_{0:4}_{1:02d}_rest.txt'.format(year, month+1)
        
        file_name = folder_name + file_name
        
        
        temp_folder = 'C:\\Users\\Benedikt\\AppData\\Local\\Temp'
        month_length = [31,february_length[year],31,30,31,30,31,31,30,31,30,31]
             
        with open(file_name, 'w') as outfile:
            outfile.write('[')
        #%%
        for day in range(month_length[month]):
            
            j = 0
            hour_range = range(24)
            
            if year == 2016 and month == 9 and day == 20:
                hour_range = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,19,20,21,22,23] #Probleme, weil Komma fehlte
            for hour in hour_range:
                # http://data.githubarchive.org/2015-01-{01..30}-{0..23}.json.gz
                if tempDelete == 1:
                    tmp_data_before = set(os.listdir(temp_folder))
                url = "http://data.githubarchive.org/{0:4}-{1:02d}-{2:02d}-{3:d}.json.gz".format(year, month+1, day+1, hour) 
                # formatierter String funktioniert
                local_filename, headers = urllib.request.urlretrieve(url)
                with gzip.open(local_filename, 'rb') as f: #https://docs.python.org/2/library/gzip.html
                    file_content = f.read()
                if tempDelete == 1:
                    tmp_data_after = set(os.listdir(temp_folder))
                    new_temp_file = list(tmp_data_after.difference(tmp_data_before))[0] #http://stackoverflow.com/questions/59825/how-to-retrieve-an-element-from-a-set-without-removing-it
                    try:
#                    Auf meinen Windowsrechnern haben die gzip-Zugriffe grosse
#                    temporaere Daten hinterlassen, die den Rechner mit der Zeit
#                    verstopften. Deshalb werden die neuen temp. Dateien wieder
#                    geloescht. Noch nicht perfekt, aber funktioniert und ist
#                    einigermassen robust.
                        os.remove(temp_folder+'\\{}'.format(new_temp_file)) #https://docs.python.org/2/library/os.html
                    except:
                        print('Tempfile exception')
                        pass
                
                #kommt als bytes format raus, umwandeln:
                file_content_decode = file_content.decode("utf-8")
                # http://stackoverflow.com/questions/13938183/python-json-string-to-list-of-dictionaries-getting-error-when-iterating
                json_string = '['+file_content_decode[0:-1].replace('\n',' , ')+']'

                jdata = json.loads(json_string) #wandelt json_string in list of dictionaries um
                i = 0
                for events in jdata:
                    try:         
                        if events[repo_name[year_string][0]][repo_name[year_string][1]][repo_name[year_string][2]:] in repo_list:
                            with open(file_name, 'a') as outfile:
                                json.dump(events, outfile) #http://stackoverflow.com/questions/12309269/how-do-i-write-json-data-to-a-file-in-python
                                outfile.write(',\n')
                                i = i + 1
                                j = j + 1
                    except:
                        pass
                
                print(i)
                
            end = time.time()
            print(j, day, end-start)
            
        with open(file_name, 'a') as outfile:
            outfile.write(']')
        end = time.time()
        print(end-start)
#%%