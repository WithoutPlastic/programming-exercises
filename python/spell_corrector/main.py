#/usr/bin/python2

"""
Original link: www.norvig.com/spell-correct.html
License: MIT

Python requirement: 2.5+

Introduction:
A simple toy spell corrector about the core concepts of statistical language
processing problems.

Make it runnable, you need:
- big text file which well descript the language model

Improvements needed:
- Better langauge model with standard input text
- Langauge model based on components of words, e.g syllables or suffixes based
- More optimization on weight of correct distance
- More diversity on deletes, transposes, replaces, inserts
- When met same correct weight, minor weight count should be developed
- Take the keyboard or physical input device input consideration
- Take culture into consideration
- A better word split method to process words like don't, H&M etc
- Support a limited set of distance three, for the sake of performance
- Correct word based on itself is hard, we need take a look at words around.
- The statistical markov chain is really important for context based correction
- Solve the confusion of American and British english
- Improve performance, via implement it as compiled language, specialized data
  structure

Further reading:
- http://www.dcs.bbk.ac.uk/~roger/spellchecking.html
- http://www.cs.colorado.edu/~martin/slp.html
- http://aspell.net/

Resources:
- http://googleresearch.blogspot.hk/2006/08/all-our-n-gram-are-belong-to-you.html
"""


import re, collections

#-------------------- Generate Language Model Begin ---------------------------
def words(text):
    return re.findall('[a-z]+', text.lower())

def train(word_names):
    model = collections.defaultdict(lambda : 1)
    for n in word_names:
        model[n] += 1
    return model
#-------------------- Generate Language Model End -----------------------------

#------------------------- Spell Corrector Begin ------------------------------
NWORDS = train(words(file('big.txt').read()))
alphabet = 'abcdefghijklmnopqrstuvwxyz'

def edits1(word):
   splits     = [(word[:i], word[i:]) for i in range(len(word) + 1)]
   deletes    = [a + b[1:] for a, b in splits if b]
   transposes = [a + b[1] + b[0] + b[2:] for a, b in splits if len(b)>1]
   replaces   = [a + c + b[1:] for a, b in splits for c in alphabet if b]
   inserts    = [a + c + b     for a, b in splits for c in alphabet]
   return set(deletes + transposes + replaces + inserts)

def known_edits2(word):
    return set(e2 for e1 in edits1(word) for e2 in edits1(e1) if e2 in NWORDS)

def known(words):
    return set(w for w in words if w in NWORDS)

def correct(word):
    candidates = known([word]) or known(edits1(word)) or known_edits2(word) or [word]
    return max(candidates, key=NWORDS.get)
#------------------------- Spell Corrector End --------------------------------

#----------------------------- Tests Begin ------------------------------------

def spelltest(tests, bias=0, verbose=False):
    import time
    n, bad, unknown, start = 0, 0, 0, time.clock()
    if bias:
        for target in tests: NWORDS[target] += bias
    for target, wrongs in tests.items():
        for wrong in wrongs.split():
            n += 1
            w = correct(wrong)
            if w!=target:
                bad += 1
                unknown += (target not in NWORDS)
                if verbose:
                    print 'correct(%r) => %r (%d); expected %r (%d)' % (
                        wrong, w, NWORDS[w], target, NWORDS[target])
    return dict(bad=bad, n=n, bias=bias, pct=int(100. - 100.*bad/n), 
                unknown=unknown, secs=int(time.clock()-start) )
