Analyse Workflow fuer die Masterarbeit
****Open Source and Artificial Intelligence****
im Fach TUM NAWI
von Benedikt von Bronk
Sommersemester 2017
Technische Universitaet Muenchen



Im Ordner "Codes fuer neue Anwendung" sind Python Skripte fuer Schritte 1 & 2 des Data Analysis Workflow zu finden.
Die Datei 'GitHubAnalysisWorkflow.py' fuehrt durch den Prozess. Sie kann auch mit einem Texteditor geoeffnet werden.
Ich habe versucht den Code so zu verallgemeinern, dass eine neue Anwendung mit ein paar Aenderungen einfach moeglich ist.
Schritt 3, die Analyse in R, muss dann auf die Fragestellung angepasst werden, deshalb sind diese R Skripte nicht enthalten.

Im Ordner "tatsaechlicher Workflow" sind die Skripte, die ich fuer die Analyse benutzt habe, und die von GitHub extrahierten Rohdaten (im Ordner „Daten aus Step 1“ (im Dropbox Ordner nicht vorhanden)). 
Da ich die Daten in zwei Etappen von GitHub extrahiert habe, sind die Zwischenschritte auch teilweise separat durchgefuehrt worden. Das ist an den kleinen a und b zu erkennen. Ansonsten ist die Abfolge, wie in 'GitHubAnalysisWorkflow.py' beschrieben.
Darueberhinaus enthaelt der Ordner auch die weiteren Analyse-Schritte, die ich in R vorgenommen habe.
kurze Uebersicht:
Step3_1.R: Alte Auswertung. Wird nur benoetigt, um DFusers und DFlong Dataframes zu erzeugen
Step3_2.R: Auswertung, die in Masterarbeit eingegangen ist.
Step3_3_network.R: Netzwerk-Auswertung, die in Masterarbeit eingegangen ist.

benutzte Software (Open Source)
- Python 3.7 aus der Anaconda 3 distribution (https://docs.anaconda.com/anaconda/install/)
- R (Version 3.3.2) und R Studio (Version 1.0.153)
- die in den einzelnen Scripts aufgefuehrten Libraries dazu

Schritt 1 wurde mit einem Windows PC durchgefuehrt (Windows 10)
Schritte 2 und 3 mit einem MacBook Pro (OS Version macOS Sierra Version 10.12.6)

