import math
import operator
import sys

def loadDataset(trainingSet, testSet,k):
        dataset = []
        k1=round(k)
	for x in range(len(trainingSet)):
			if(trainingSet[x]==testSet[0]):
					for y in range(k1):
						if(not x - y <= 0):
							dataset.append(trainingSet[x-y])
					for z in range(k1):
						if not x + z >(len(trainingSet)-1):
							dataset.append(trainingSet[x+z])
                        return dataset


def euclideanDistance(instance1, instance2):
	distance = 0
	distance += pow((float(instance1) - float(instance2)), 2)
	return math.sqrt(distance)

def getNeighbors(trainingSet, testInstance,k):
	distances = []
	for x in range(len(trainingSet)):
		dist = euclideanDistance(testInstance, trainingSet[x])
		distances.append((trainingSet[x], dist))
	distances.sort(key=operator.itemgetter(1))
	neighbors = []
	try:
		for a in range(k):
			neighbors.append(distances[a][0])
	except:
		print("Oops!",sys.exc_info()[0],"occured.")
		print(trainingSet[:])
		print(testInstance)
	return neighbors


def KNN(attr,test,k):

	testSet=test
	trainingSet = attr

	dataset=loadDataset(trainingSet,testSet,k)

	result = getNeighbors(dataset, testSet[0],k)

	return (result)


