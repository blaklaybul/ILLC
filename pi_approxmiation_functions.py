import math
import random
import decimal

def napprox(n): 
	r = range(n)
	for i in r:
		r[i] = (2-random.random()*4,2-random.random()*4)
	return r

def is_inside(x,y):
	if(dist(x,y) < 1.0):
		return True
	else:
		return False

def dist(x,y):
	return math.sqrt(x**2+y**2)

def pi_approx(n):
	app = napprox(n)
	counter = 0
	for (x,y) in app:
		if(is_inside((x,y)[0],(x,y)[1]) == True):
			counter += 1.0
	return 16.0 * (counter/n)