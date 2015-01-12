def first(word):
    return word[0]

def last(word):
    return word[-1]

def middle(word):
    return word[1:-1]

def is_palindrome(word):       
	if (len(word)==1):
	    return "yep"
	elif(len(word) == 2):
	    if(word[0] == word[-1]):
	        return "palin!"
	    else:
	        return "no way!"
	elif(len(word) > 2):
		newword	= middle(word)
		while(len(newword) > 1):
			if(first(word) == last(word)):
				is_palindrome(newword)
			else:
				return "not a palindrome!"
