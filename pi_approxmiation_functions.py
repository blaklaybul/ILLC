import math
import random

def napprox(n): 
	r = range(n)
	for i in r:
		r[i] = (2-random.random()*4,2-random.random()*4)

def is_inside(x,y):
	if(dist(x,y) <1):
		return True
	else:
		return False

def dist(x,y):
	return math.sqrt(x**2+y**2)