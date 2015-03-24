import string
import random

def process_book(book,skip_head):
	hist_of_words = {}
	fp = file(book)

	if skip_head:
		skip_gut_head(fp)

	for line in fp:
		process_line(line,hist_of_words)

	return hist_of_words

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

def sub(d1,d2):
	res = {}
	for key in d1:
		if key not in d2:
			res[key]= None
	return res

def total_words(hist):
	return sum(hist.values())

def different_words(hist):
	return len(hist)

def random_word(hist):
	t = []
	for word, freq in hist.items():
		t.extend([word]*freq)

	return random.choice(t)

if __name__ == '__main__':
	print("working with time machine")
	hist = process_book("timemachine.txt", skip_head =True)
	print "Total words: ", total_words(hist)
	print "diff words: ", different_words(hist)

	t = most_common(hist)
	
	print "the most common words are: "
	for freq, word in t[0:20]:
		print word, "\t", freq
