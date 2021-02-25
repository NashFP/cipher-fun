# Cipher tools in Lisp

Rather than making command-line tools, I just make functions that I can interact with in the Lisp REPL. I am using SBCL, usually from Emacs. Typically I will edit a Lisp file and then use c-c c-k to load the whole file again in SBCL.

## Some math utilities for analysis
There are a couple of math routines that can greatly assist in analyzing an unknown ciphertext, and they start with counting the letters. I made two letter counting functions - `count-letters` and `count-letters-pct`. The `count-letters` function gives you a raw count of the letters, `count-letters-pct` gives you the counts as a percentage of the overall letters. This can be compared with the English-Frequencies array that is also defined in the analysis.lisp file.

The first question you might ask with an unknown ciphertext is, have the letters been changed? Some ciphers are transposition ciphers, where the letters are just rearranged, some are substitutions where the letters keep their original position, but are changed to other letters. There can also be ciphers that are a combination of these two.

So, how do you know if the letters have been changed? Use `count-letters-pct` to get the counts as percentages, and then compare this with the English-Frequences array using the `correlate` function.

### Correlation
The `correlate` function in analysis.lisp computes a correlation coefficient. It may vary slightly from other ways of defining a correlation coefficient, but the formula here is:

Let c1 and c2 be the arrays of values you want to compare.  
Let sx = the sum of all the items in c1  
Let sx2 = the sum of the squares of all the items in c1  
Let sy = the sum of all the items in c2  
Let sy2 = the sum of all the items in c2  
Let sxy = the sum of the products of corresponding items in c1 and c2 (i.e. c1[0] * c2[0] + c1[1] * c2[1] + ...)  

Let sxy_adj = sxy - (sx * sy) / number-of-items  
Let sx2_adj = sx2 - (sx * sx) / number-of-items  
Let sy2_adj = sy2 - (sy * sy) / number-of-items  

The correlation coefficient is then sxy_adj / sqrt (sx2_adj * sy2_adj)

A good rule of thumb for me has been that a value above 0.6 tends to indicate a good correlation, but often I just search for the maximum value.

### Index of Coincidence
The index of coincidence is sometimes used to compare two texts, but can also be used to determine if a text contains a single alphabet or multiples (a polyalphabetic cipher). The way it is computed in the `compute-ic` function in analysis.lisp is this:  

Let c = the counts of the letters in the ciphertext  
Let fsum = the sum of the squares of the counts (I am summing square - 1 of the counts, I don't think it matters).  
The Index of coincidence is fsum / square(number-of-letters). This value is usually in the 0.06 - 0.08 range for English text. It is lower when there are multiple keys.

### Cracking a Vigenere Cipher
Now that we have a couple of mathematical tools, this is how to attack a Vigenere cipher. A Vigenere cipher is just using several Caesar ciphers. A Caesar cipher is just a shift in the alphabet. That is, you might shift everything over by 3 letters, so A becomes D, B becomes E, C->F, D->G ... X->A, Y->B, Z->C. So let's first start with how to crack a Caesar cipher.

#### Cracking a Caesar Cipher
This is straightforward, but the caveat is that if the ciphertext is too short, this method may not work. There are only 26 possible ways to encrypt something with a Caesar cipher. All you do is try to each shift, and then use the Correlation Coefficient to compare the result with the English_Frequences count. The shift that yields the highest CC should be the correct one.

#### Determining the Key Length in Vigenere
Before you can try to crack each Caesar cipher in a Vigenere cipher, you first need to know how long the key is. Suppose my key was "NASHFP". It is easiest to reduce letters to numbers in the range 0-25, so this would be 13,0,18,7,5,15. So, I take the first number in the key, 13, and add it to the first number in the text I am encrypting (I have converted the text to the same 0-25 range), modulo 26. Next, I take the second number in the key, 0, and add it to the second value in the text, and proceed on. After the 6th number, I go back to the beginning of the key again, so it's as if I laid out NASHFPNASHFPNASHFPNASHFPNASHFP over and over on top of the text and just added the corresponding letters together.

To determine the key length, try possible key lengths up to some small, reasonable limit, say 12. I split the text into lists of numbers where each number was encrypted by the same key value. That is, if I am trying a key length of 6, like I did with NASHFP, the first list would be all the letters encrypted with N, the second list is all the letters encrypted with A, and so on. I don't know what they were encrypted with yet, of course.

Now, once I have split the list with the key length I am trying, I compute the index of coincidence for each list, and then average those together. The thinking goes like this: when you encrypt with one alphabet, there are certain letters that will occur more frequently (those corresponding to ETAIONSHR in the plaintext). When you encrypt with two alphabets, there are now two different letters that represent the original E, and the original T, etc., so the counts are spread out more, and the Index of Coincidence has lower values. Say the original key length was 5, and I am currently trying a possible key length of 4. In my first list, I do have the first letter of the text, but then I have the 5th letter as well, and the 9th, and the 13th, but if the original key length was 5, the 5th letter was probably encrypted with a different key, so the letter counts are going to be spread out. Now, when I try a key length of 5, my first list has the 1st, 6th, 11th, 16th, etc. letters which were all encrypted with the same key, so the Index of Coincidence should be higher, and roughly the same as for English.

#### Putting the Techniques Together
To crack a Vigenere cipher, I determine the key length, and then at each key position, I try all 26 possible key values, cracking each position as if it were a Caesar cipher. When I am done, I should have the original key and I can use that to decrypt the value.

Here is my Lisp function to crack a Vigenere cipher:
```lisp
(defun crack-vigenere (ciphertext)
  (let* ((key-length (determine-key-length ciphertext 10))
	 (splits (poly-split ciphertext key-length))
	 (key (mapcar #'determine-key splits)))
    (decrypt-vigenere key ciphertext)))
```
After using `determine-key-length` to guess the key length, I use poly-split to split the text into separate lists such that each item in the list was encrypted by the same key value. Next, for each split, I use `determine-key` to guess the key. It just cycles through all 26 letters and uses the Correlation Coefficient to see which result most closely matches English frequency counts. Once I have that, I just use that key to decrypt the ciphertext and hopefully I have the right answer. Here is a sample session with SBCL (minus some warnings about using SETF):

```
CL-USER> (load "analysis.lisp")
T
CL-USER> (load "vigenere.lisp")
T
CL-USER> (setf plaintext "One can even conjecture that Lisp owes its survival specifically to the fact that its programs are lists, which everyone, including me, has regarded as a disadvantage")
CL-USER> (setf ciphertext (encrypt-vigenere "lisp" plaintext))
(25 21 22 17 11 21 22 10 15 21 20 3 24 17 22 17 4 2 9 19 4 15 18 8 22 16 10 4
 25 4 22 7 19 1 10 7 5 25 13 23 6 8 3 7 0 12 20 23 16 16 20 15 22 19 16 8 25 1
 25 19 16 8 20 8 4 15 18 8 19 1 10 4 2 22 24 6 11 20 10 15 2 12 3 23 3 1 10 11
 18 16 20 22 15 3 22 6 9 22 5 19 19 21 20 0 5 11 0 2 17 20 22 22 11 0 9 19 17 8
 9 18 15 11 18 7 11 11 0 7 11 11 13 15 24 1 18 21 15)
CL-USER> (setf cracked (crack-vigenere ciphertext))
(14 13 4 2 0 13 4 21 4 13 2 14 13 9 4 2 19 20 17 4 19 7 0 19 11 8 18 15 14 22 4
 18 8 19 18 18 20 17 21 8 21 0 11 18 15 4 2 8 5 8 2 0 11 11 24 19 14 19 7 4 5 0
 2 19 19 7 0 19 8 19 18 15 17 14 6 17 0 12 18 0 17 4 11 8 18 19 18 22 7 8 2 7 4
 21 4 17 24 14 13 4 8 13 2 11 20 3 8 13 6 12 4 7 0 18 17 4 6 0 17 3 4 3 0 18 0
 3 8 18 0 3 21 0 13 19 0 6 4)
CL-USER> (string-from-26 cracked)
"ONECANEVENCONJECTURETHATLISPOWESITSSURVIVALSPECIFICALLYTOTHEFACTTHATITSPROGRAMSARELISTSWHICHEVERYONEINCLUDINGMEHASREGARDEDASADISADVANTAGE"
```

## Shotgun-Hillclimbing
One way to solve a simple substitution cipher is by shotgun-hillclimbing. This method works whether or not you have word breaks. The basic idea is simple: generate a random key (that's the shotgun part), and then try swapping values in the key to see if that improves the decryption. We try all possible swaps and see which one has the best improvement, make that swap permanent, and then try again to see what else we can swap. When there are no swaps that can make an improvement, we give up,
and go back to the shotgun by generating another random key.

One of the difficulties here is how you measure improvement. One way might be to compare the resulting text to the English letter frequencies, but that doesn't actually work well in practice. The solution we are using here is to look for actual words. The basic idea is to scan through the decrypted text looking for the longest word possible at a position, and then do the same scan for everything that comes after it. We sum the squares of the longest words found.
The reason for summing the squares instead of just the lengths of the
longest words is that just using the lengths tends to reward short
words. The dictionary has a lot of 2 and 3 letter words and the
algorithm could fill up most of the space with short words but is
meaningless overall. By squaring, we reward it for finding longer words.

To determine when to quit, we just compute how many letters are part of
words in the decrypted text (i.e. summing the longest words without
squaring them). In the example below, we pass 0.9 as the
target percentage, so 90% of the letters should be part of words. When
it hits the target percentage, shotgun-hillclimb returns the key that
hit the target.

For example:
```
CL-USER> (shotgun-hillclimb ciphertext enable1 0.90)
Score 950 with key OGMTYCFNQJPEUSVKAXHBRZWILD: ARDFRDHCLEWNDNKCHICAINDEFUCOUPSSRHOAPHOTPONRHFUNDNHELRINDFUNPITDNPMENUMCUEUPHREDKCUNCKKTFRNDDCSNJRHICMACKPACFERSTSRHEWNDFPKNCMFUCOUPSSRHOAPHOTPONDORZNHEWPEDEPERKMURKERCHIRDKCTUPONDFTUNABHCEPERCHPAKWPHONDUNKTUDRZNTDNCMKCHIRERCHPANGFUNDDRCHDUNFUNDNHEPERCHCMDBSYCARKRHMCUSPERCHNGENUHPAABYBARDEDPHIRHENUHPAABYBARDEDEUTKETUNPHIUNFUNDNHEPERCHCMFUCOUPSRHEWNDPSNLPBLRAAFUCYPYABWPZNPZNUBACHOARMN
Score 1093 with key MBAPKFRHXDZNOUSGJCTYVEQIWL: MISRISLAVTYOSODALWAMWOSTRGAUGEHHILUMELUBEUOILRGOSOLTVIWOSRGOEWBSOENTOGNAGTGELITSDAGOADDBRIOSSAHOQILWANMADEMARTIHBHILTYOSREDOANRGAUGEHHILUMELUBEUOSUIJOLTYETSTETIDNGIDTIALWISDABGEUOSRBGOMPLATETIALEMDYELUOSGODBGSIJOBSOANDALWITIALEMOFRGOSSIALSGORGOSOLTETIALANSPHZAMIDILNAGHETIALOFTOGLEMMPZPMISTSELWILTOGLEMMPZPMISTSTGBDTBGOELWGORGOSOLTETIALANRGAUGEHILTYOSEHOVEPVIMMRGAZEZMPYEJOEJOGPMALUMINO
Score 1114 with key COVRSELBKJDHQPMWATNYXFZIUG: BURAURSOFTPERENOSMOBMERTADOCDILLUSCBISCHICEUSADERESTFUMERADEIMHREIYTEDYODTDISUTRNODEONNHAUERROLEJUSMOYBONIBOATULHLUSTPERAINEOYADOCDILLUSCBISCHICERCUKESTPITRTITUNYDUNTUOSMURNOHDICERAHDEBZSOTITUOSIBNPISCERDENHDRUKEHREOYNOSMUTUOSIBEVADERRUOSRDEADERESTITUOSOYRZLGOBUNUSYODLITUOSEVTEDSIBBZGZBURTRISMUSTEDSIBBZGZBURTRTDHNTHDEISMDEADERESTITUOSOYADOCDILUSTPERILEFIZFUBBADOGIGBZPIKEIKEDZBOSCBUYE
Score 1138 with key SCLNXETVMAFQOUBHIPYRKJWDGZ: MEGBEGDIFSWAGARIDLIMLAGSBTIHTUPPEDHMUDHOUHAEDBTAGADSFELAGBTAULOGAUNSATNITSTUDESGRITAIRROBEAGGIPAVEDLINMIRUMIBSEPOPEDSWAGBURAINBTIHTUPPEDHMUDHOUHAGHEXADSWUSGSUSERNTERSEIDLEGRIOTUHAGBOTAMYDISUSEIDUMRWUDHAGTAROTGEXAOGAINRIDLESEIDUMAKBTAGGEIDGTABTAGADSUSEIDINGYPCIMEREDNITPUSEIDAKSATDUMMYCYMEGSGUDLEDSATDUMMYCYMEGSGSTORSOTAUDLTABTAGADSUSEIDINBTIHTUPEDSWAGUPAFUYFEMMBTICUCMYWUXAUXATYMIDHMENA
Score 2890 with key KLPQSUVWXZJOHNMCARTYBDEFGI: LISPISNOWTHESECONDOLDESTPROGRAMMINGLANGUAGEINPRESENTWIDESPREADUSEAFTERFORTRANITSCOREOCCUPIESSOMEKINDOFLOCALOPTIMUMINTHESPACEOFPROGRAMMINGLANGUAGESGIVENTHATSTATICFRICTIONDISCOURAGESPURELYNOTATIONALCHANGESRECURSIVEUSEOFCONDITIONALEXPRESSIONSREPRESENTATIONOFSYMBOLICINFORMATIONEXTERNALLYBYLISTSANDINTERNALLYBYLISTSTRUCTUREANDREPRESENTATIONOFPROGRAMINTHESAMEWAYWILLPROBABLYHAVEAVERYLONGLIFE
#(16 20 15 21 22 23 24 12 25 10 0 1 14 13 11 2 3 17 4 18 5 6 7 8 19 9)
CL-USER> 
```

To try the shotgun-hillclimbing, you can do this:
```
(load "shotgun-hillclimb.lisp")
(load "scoring.lisp")
(load "quotes.lisp")
(setf enable1 (load-dawg "enable1.daw"))
(setf plaintext (string-to-26 mccarthy-oldest))
(setf enc-key (make-key "johnmccarthy" 10))
(setf ciphertext (do-substitution plaintext enc-key))
(shotgun-hillclimb ciphertext enable1 #'score-segment 0.9)
```
If you are running this via SLIME in Emacs, hit c-c c-c to interrupt it
if you get impatient. The last parameter to shotgun-hillclimb is the
percentage of the decrypted text that should be part of a word. You
probably don't want to try for 100% because the text might contain
words that aren't in the dictionary. Once it hits the target percentage
it returns the key it found.

The `#'score-segment` passes a scoring function because I am
experimenting with an alternative scoring where instead of looking for
the longest word, I find all lengths of words and try scoring with each,
which seems like it should be more accurate, but doesn't make a
noticeable difference in speed or accuracy so far.

## Using the DAWG Dictionary File
The enable1.daw file is a Directed Acyclic Word Graph encoded into a
series of 32-bit numbers. You can think of each number as a node
in the graph. At the top of the graph is a list of the nodes for the
first letter in a word. These nodes are stored in the last 26 numbers
in the file. While each node has a pointer to its children, consecutive
nodes can also form a list.

The format of a node is:


Child Node Offset |  Is-End-Of-List  |  Is-End-Of-Word  |  Letter
---|---|---|---
22 bits | 1 bit | 1 bit |  8 bits

The Child Node Offset is the offset of the first child node. If you
think of these numbers as an array, this is the array index of the
first child. The children of a node represent the letters that can
come after that node.

The Is-End-Of-List flag indicates that this is the last node in the
list of consecutive nodes. That is, while Child Node Offset points to
the first child of a node, all the nodes immediately following the
child node are also children of the current node, until you hit one
whose Is-End-Of-List flag is 1, and then that is the last child.

The Is-End-Of-Word flag indicates that the path you have traversed so
far forms a valid word. For example, if you are trying to see if "foods"
is a word, when you get to the node with "d", its Is-End-Of-Word flag
will be 1 because "food" is a valid word, but you still need to traverse
its children looking for one whose letter is "s", and that one should
also have Is-End-Of-Word = 1.

Letter is the letter that the node represents. In the Enable1.daw file,
the letters are lowercase ASCII values.

### Looking Up A Word
To look up a word in the DAWG, first go to the top of the graph, which
is the 26th number from the end. The last 26 nodes in the file have the
letters a-z consecutively. Let's suppose you are looking for "car", and
also supposed that the enable1.daw file has 122197 entries. The first
node is then 122197-26, or 122171.

The first node you look at has the letter 'a', so go to the next node
(the one immediately after it in memory or in the file, which would
be 122172 in an array), and that node has 'b', but the one after that
, number 122173, has 'c'. So now we look at the child index of the 'c'
node and find that it is 17812, so we look at node 17812. The very
first node in the list is the 'a', so now we look at its child index,
which is 17825. So, the children of 'a' that start at 17825 represent
all the letters that can immediately follow "ca" inthe dictionary.
The node at 17825 has the letter 'b' (and incidentally
has is-end-of-word=1 since "cab" is a valid word), followed by 'c' at
17826, 'd' at 17827. In fact, we have to keep going until we get to
17840 to find 'r', and of course the is-end-of-word bit is 1 because
"car" is a valid word. If you were to continue to the child node of 'r'
from this point, you would see all the letters that can immediately
follow "car" in the dictionary. Also, if you continue past node 17840
to 17841 and beyond, you'll see that 's', 't', 'u', 'v', 'w', 'y',
and 'z' can also follow "ca", that "cat", "caw", and "cay" are valid
words, and in case you are wondering about "caz", it is there for the
words "cazique" and "caziques". You should also see that the 'z' node
at 17847 has is-end-of-list=1 since it is the end of the list of
nodes that can follow "ca".

This type of dictionary storage has been used by numerous Scrabble
programs, and it makes sense in that you often want to know what letters
can follow a word. (FYI, Steven Gordon proposed a structure called a
GADDAG in 1994 that let you do fast lookups on what letters can precede
a word - [https://ericsink.com/downloads/faster-scrabble-gordon.pdf]).

For cipher-cracking purposes, we want to find either the longest
possible word, or all the words possible from a given starting point.
In this case, we just keep following the graph matching the letters
we have, and every time we hit an "is-end-of-word" we note as
either the new longest word or just another possible word length.
If we hit an is-end-of-list without matching, we
stop searching because that means the current letter cannot follow
the ones before it in the dictionary.

