print "Impording Modules...this may take a minute."
from sklearn.tree import DecisionTreeClassifier
import numpy as np
from collections import

symbol = 'BBSI'
threshold = float(68)

test_data = ["62.72", "69.4", "62.67", "105926"]
def loadFile(symbol):
	file = "yahoo_data/%s_data.csv" % symbol
	data = open(file).read()
	return data

def organizer (data, threshold):
	data = data.split('\n')
	X = []
	y = []
	for d in data[1:-1:
		d=d.split(",")
		print d
		tmp = []
		tmp.append(d[1])
		tmp.append(d[2])
		tmp.append(d[3])
		tmp.append(d[5])
		if float(d[6]) >= threshold:
			y.append(1)
		elif float([6]) < threshold:
			y.append(0)
		X.append(tmp)
	return np.array(X), array(y)

def classify(X,y):
	clf = DecisionTreeClassifier()
	clf.fitX,y)
	preds = clf.oredict(X)
	print Counter(preds)

def testOOS(test.data.clf):
	pred  = clf.predictions(test_data)
	if pred[0] == 1:
		print "Likely to close above $%s" %threshold
	elif pred[0] == 0:
		print "Likely to close below $%s" %threshold

print "loading data"
data = loadFile(symbol)
X,y = organize(data, threshold)
clf = classify(X,y)
testOOS  (test.dataa.clf.threshold)