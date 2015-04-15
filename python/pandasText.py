import string
import pandas as pd

def process_book(book,skip_head):
	hist_of_words = pd.DataFrame(columns = ['word'])
	hist_of_pairs = pd.DataFrame(columns = ["word1", "word2"])
	
	fp = file(book)

	if skip_head:
		skip_gut_head(fp)

	for line in fp:
		if line.startswith("End of Project Gutenberg"):
			break
		process_line(line,hist_of_words)
		process_pref(line, hist_of_pairs)
	
	hist_of_words['count'] = 1
	hist_of_words = hist_of_words.groupby(list(hist_of_words.columns[0]), as_index = False).sum()

	hist_of_pairs['count'] = 1
	hist_of_pairs = hist_of_pairs.groupby(list(hist_of_pairs.columns[0:-1]), as_index=False).sum()
	return (hist_of_words, hist_of_pairs)


'''i need to make this be able to handle triples, quadruples... n-tuples. for now we just
consider pairs for prefixes and suffixes'''

def process_pref(line,hist, num =2):
	line = line.replace("-"," ")
	for idx, word in enumerate(line.split()):
		if idx < len(line.split())-(num-1):
			pair = pd.Series([word.strip(string.whitespace), line.split()[idx+1].strip(string.whitespace)])
			hist.loc[len(hist)] = [pair[0], pair[1]]


def skip_gut_head(book):
	for line in book:
		if line.startswith("*** START OF THIS PROJECT GUTENBERG EBOOK"):
			break

def process_line(line, hist):
	print len(hist)
	line = line.replace("-", " ")
	for word in line.split():
		word = word.strip(string.punctuation + string.whitespace)
		word = pd.Series([word.lower()])
		hist.loc[len(hist)] = word[0]

def most_common(hist):
	t = []
	for key, value in hist.items():
		t.append((value,key))

	t.sort()
	t.reverse()
	return t
	

def print_common(hist, num = 10):
	t = most_common(hist)
	print "most common words are: "
	for freq, word in t[:num]:
		print word, "\t", freq


def total_words(hist):
	return sum(hist.values())

def different_words(hist):
	return len(hist)

def weighted_choice(hist):
	r = random.uniform(0, sum(hist.values()))
	s = 0.0
	for k,w in hist.iteritems():
		s+=w
		if r < s:
			return k
	return k

def random_text(hist, num = 5):
	''' i need to figure out how to get all pairs that begin with end word
	of previous pair
	'''
	t=[]
	t.append(' '.join(weighted_choice(hist)) + " ")
	new_hist = hist
	for i in range(num-1):
		next = t[-1].split()[-1]
		#print next
		#new_hist = {(k:v) for k,v in hist.iteritems() if k[-1] == next}
		t.append(' '.join(weighted_choice(new_hist)) + " ")

		#t.append(hist.items()[random.randint(0,len(hist)-1)][0] + " ")

	return t

if __name__ == '__main__':
	print("working with time machine")
	hist = process_book("timemachine.txt", skip_head =True)
	
	# print "Total words: ", total_words(hist[0])
	# print "diff words: ", different_words(hist[0])
	# print "diff pairs ", different_words(hist[1])

	# w = most_common(hist[0])
	# p = most_common(hist[1])

	# print "the most common words are: "
	# for freq, word in w[0:20]:
	# 	print word, "\t", freq

	# print "the most common pairs are: "
	# for freq, word in p[0:20]:
	# 	print word, "\t", freq

	# print ''.join(random_text(hist[1],4))
	# print ''.join(random_text(hist[1],4))
	# print ''.join(random_text(hist[1],4))