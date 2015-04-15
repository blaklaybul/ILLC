import string
import random
import bisect
import pandas as pd

def process_book(book,skip_head, pref = 2):
	hist_of_words = {}
	hist_of_pairs = {}
	
	fp = file(book)

	if skip_head:
		skip_gut_head(fp)

	for line in fp:
		if line.startswith("End of Project Gutenberg"):
			break
		process_line(line, hist_of_words)
		process_pref(line, hist_of_pairs, pref)


	dfwords = pd.DataFrame(hist_of_words.items(), columns = ["word", "count"])
	dfpairs = pd.DataFrame(hist_of_pairs.items(), columns = ["group", "count"])

	for i in range(pref):
		dfpairs["word" + str(i)] = [x[i] for x in dfpairs["group"]]

	dfpairs.drop("group", axis = 1, inplace = True)

	return (dfwords, dfpairs)


'''i need to make this be able to handle triples, quadruples... n-tuples. for now we just
consider pairs for prefixes and suffixes'''

def process_pref(line,hist, num):
	line = line.replace("-"," ")

	for idx, word in enumerate(line.split()):
		if idx < len(line.split())-(num-1):
			pair = (word.strip(string.whitespace), line.split()[idx+1].strip(string.whitespace))
			hist[pair] = hist.get(pair,0) + 1


def skip_gut_head(book):
	for line in book:
		if line.startswith("*** START OF THIS PROJECT GUTENBERG EBOOK"):
			break

def process_line(line, hist):
	line = line.replace("-", " ")

	for word in line.split():
		word = word.strip(string.punctuation + string.whitespace)
		word = word.lower()

		hist[word] = hist.get(word,0) + 1

def most_common(hist):
	print hist.sort(["count"], ascending = False).head(20)

	

def print_common(hist, num = 10):
	t = most_common(hist)
	print "most common words are: "
	for freq, word in t[:num]:
		print word, "\t", freq

		definn


def total_words(hist):
	return hist["count"].sum()

def different_words(hist):
	return len(hist)

def weighted_choice(hist):
	''' update cumsum every time and take random choice using bisect search
	'''
	print hist.head()
	hist["cum"] = hist["count"].cumsum()
	x = random.random() * hist["count"].max()
	i = bisect.bisect(hist["cum"], x)
	return hist[["word0", "word1"]][hist["cum"]==i][-1:] ##wtf is toging on here

def random_text(hist, num = 5):
	''' i need to figure out how to get all pairs that begin with end word
	of previous pair
	'''
	t=[]
	t.append(' '.join(weighted_choice(hist)) + " ")
	new_hist = hist[hist["word1"] == t[-1].split()[-1]] #get the end!
	for i in range(num-1):
		next = t[-1].split()[-1]
		#print next
		#new_hist = {(k:v) for k,v in hist.iteritems() if k[-1] == next}
		t.append(' '.join(weighted_choice(new_hist)) + " ")

		#t.append(hist.items()[random.randint(0,len(hist)-1)][0] + " ")

	return t

if __name__ == '__main__':
	print("working with time machine")
	hist = process_book("timemachine.txt", skip_head =True,pref = 2)
	
	print "Total words: ", total_words(hist[0])
	print "diff words: ", different_words(hist[0])
	print "diff pairs ", different_words(hist[1])

	print "The Most Common Words are: \n", most_common(hist[0])
	print "The Most Common Pairs are: \n", most_common(hist[1])

	print ''.join(random_text(hist[1],4))
	# print ''.join(random_text(hist[1],4))
	# print ''.join(random_text(hist[1],4))