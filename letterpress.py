dictionary = open('/usr/share/dict/words', 'r').read().splitlines()

# a word is a subset of a list if every 
# single character of the word intersect
# with characters in the list

# don't ask me why I am not returning right away 
# by checking if length equals zero
def word_subset(word, possibleLetters):
  if not (list(set(word) - set(possibleLetters))):
    return True
  else: 
    return False

answers = []

for word in dictionary: 
  if word_subset(word, "abcdefghijklmnopqrstuvwxy"):
    answers.append(word)

answers.sort(key=len, reverse=True)
for word in answers[:10]:
  print word