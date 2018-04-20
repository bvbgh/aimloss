# GitHub Analysis Workflow

#%% Step 1: Data extraction form GitHub
# Vor Ausfuehrung Code in Step1_AccessGithubRepo.py anpassen
# Daten werden gemaess der Liste in Step1_AccessGithubRepo.py von GitHub 
# extrahiert. Ausfuehrung kann lange dauern 
# ~20 Repositories, Zeitraum 5 Jahre haben bei mir ca. 1-2 Tage gedauert.
# Zur Abschaetzung der Dauer stueckweise ausfuehren.


exec(open("./Step1_AccessGithubRepo.py", encoding='utf-8').read())

#%% Step 2: Daten reformatieren in sinnvolles Format (data frame)
# Es werden zwei verschiedene Datensaetze generiert (Dataset A & B)
# Auch hier gilt wieder, dass die Codes angepasst werden muessen.
# Die Groessenordnung fuer Step 2 ist Stunden

# Fuer dataset A
exec(open("./Step2_datasetA.py", encoding='utf-8').read())
# Fuer dataset B
exec(open("./Step2_datasetB.py").read())



# Nun muessen die Zeitpunkt-Formate noch umgewandelt werden
# keine Aenderung noetig

# Fuer dataset A
exec(open("./Step2_datasetA_time.py").read())
# Fuer dataset B
exec(open("./Step2_datasetB_time.py").read())



# Da die weitere Analyse in R stattfindet, muessen die Daten in R data frames 
# umgewandelt werden. Dafuer wird rpy2 benoetigt
# Mac Installation: 
# https://rpy2.readthedocs.io/en/version_2.8.x/overview.html#install-from-source
# Ich musste nach rpy2 Installation noch folgendes ausfuehren
# ggf mit '.Library' in der R Console den richtigen Pfad finden
import os
os.environ['R_HOME'] = '/Library/Frameworks/R.framework/Resources'

# Fuer dataset A
exec(open("./Step2_datasetA_Rformat.py").read())
# Fuer dataset B
exec(open("./Step2_datasetB_Rformat.py").read())


#%% Step 3: Weitere Analyse in R, die auf Fragestellung angepasst werden muss.