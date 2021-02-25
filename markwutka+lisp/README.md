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
One way to solve a simple substitution cipher is by shotgun-hillclimbing. This method works whether or not you have word breaks. The basic idea is simple: generate a random key (that's the shotgun part), and then try swapping values in the key to see if that improves the decryption. We try all possible swaps and see which one has the best improvement, make that swap permanent, and then try again to see what else we can swap. When there are no swaps that can make an improvement, we give up.

One of the difficulties here is how you measure improvement. One way might be to compare the resulting text to the English letter frequencies, but that doesn't actually work well in practice. The solution we are using here is to look for actual words. The basic idea is to scan through the decrypted text looking for the longest word possible at a position, and then do the same scan for everything that comes after it. We sum the squares of the longest words found.

Unfortunately, we still don't actually know when we are done, so right now the shotgun-hillclimbing function runs forever, printing out how well it has done so far, and you have to interrupt it.

For example:
```
CL-USER> (shotgun-hillclimb ciphertext enable1)
Score 913 with key SNVBXCMORIDUGPYHZTKQLJEAWF: HERFERBGWOYARANGBTGHTAROFIGCISPPEBCHSBCDSCAEBFIARABOWETARFIASTDRASLOAILGIOISBEORNGIAGNNDFEARRGPAVEBTGLHGNSHGFOEPDPEBOYARFSNAGLFIGCISPPEBCHSBCDSCARCEKABOYSOROSOENLIENOEGBTERNGDISCARFDIAHMBGOSOEGBSHNYSBCARIANDIREKADRAGLNGBTEOEGBSHAZFIARREGBRIAFIARABOSOEGBGLRMPUGHENEBLGIPSOEGBAZOAIBSHHMUMHERORSBTEBOAIBSHHMUMHEROROIDNODIASBTIAFIARABOSOEGBGLFIGCISPEBOYARSPAWSMWEHHFIGUSUHMYSKASKAIMHGBCHELA
Score 967 with key SKZUMPXVCIFLDRYEAOTHGQBJNW: RGSIGSYEPOZASAFEYVERVASOINEHNBTTGYHRBYHWBHAGYINASAYOPGVASINABVWSABDOANDENONBYGOSFENAEFFWIGASSETAXGYVEDREFBREIOGTWTGYOZASIBFAEDINEHNBTTGYHRBYHWBHASHGMAYOZBOSOBOGFDNGFOGEYVGSFEWNBHASIWNARUYEOBOGEYBRFZBYHASNAFWNSGMAWSAEDFEYVGOGEYBRAKINASSGEYSNAINASAYOBOGEYEDSUTLERGFGYDENTBOGEYAKOANYBRRULURGSOSBYVGYOANYBRRULURGSOSONWFOWNABYVNAINASAYOBOGEYEDINEHNBTGYOZASBTAPBUPGRRINELBLRUZBMABMANUREYHRGDA
Score 1140 with key XGCUSEDRKJWHOPMVZNYQBFTALI: MAWCAWROFSKEWENORTOMTEWSCHOPHILLARPMIRPUIPEARCHEWERSFATEWCHEITUWEIDSEHDOHSHIRASWNOHEONNUCAEWWOLEJARTODMONIMOCSALULARSKEWCINEODCHOPHILLARPMIRPUIPEWPAGERSKISWSISANDHANSAORTAWNOUHIPEWCUHEMBROSISAORIMNKIRPEWHENUHWAGEUWEODNORTASAORIMEVCHEWWAORWHECHEWERSISAORODWBLYOMANARDOHLISAOREVSEHRIMMBYBMAWSWIRTARSEHRIMMBYBMAWSWSHUNSUHEIRTHECHEWERSISAORODCHOPHILARSKEWILEFIBFAMMCHOYIYMBKIGEIGEHBMORPMADE
Score 1152 with key MPFRXBVQSLAHWCKOIYTNGJUDEZ: PESNESTAYRMISIBATHAPHISRNDAGDOLLETGPOTGFOGIETNDISITRYEHISNDIOHFSIOWRIDWADRDOTERSBADIABBFNEISSALIVETHAWPABOPANRELFLETRMISNOBIAWNDAGDOLLETGPOTGFOGISGEXITRMORSROREBWDEBREATHESBAFDOGISNFDIPUTAROREATOPBMOTGISDIBFDSEXIFSIAWBATHEREATOPICNDISSEATSDINDISITROREATAWSULJAPEBETWADLOREATICRIDTOPPUJUPESRSOTHETRIDTOPPUJUPESRSRDFBRFDIOTHDINDISITROREATAWNDAGDOLETRMISOLIYOUYEPPNDAJOJPUMOXIOXIDUPATGPEWI
Score 2890 with key KLPQSUVWXIJOHNMCARTYBDEFGZ: LISPISNOWTHESECONDOLDESTPROGRAMMINGLANGUAGEINPRESENTWIDESPREADUSEAFTERFORTRANITSCOREOCCUPIESSOMEKINDOFLOCALOPTIMUMINTHESPACEOFPROGRAMMINGLANGUAGESGIVENTHATSTATICFRICTIONDISCOURAGESPURELYNOTATIONALCHANGESRECURSIVEUSEOFCONDITIONALEXPRESSIONSREPRESENTATIONOFSYMBOLICINFORMATIONEXTERNALLYBYLISTSANDINTERNALLYBYLISTSTRUCTUREANDREPRESENTATIONOFPROGRAMINTHESAMEWAYWILLPROBABLYHAVEAVERYLONGLIFE
; Evaluation aborted on NIL.
CL-USER> 
```

To try the shotgun-hillclimbing, you can do this:
```
(load "analysis.lisp")
(load "dawg.lisp")
(load "scoring.lisp")
(load "shotgun-hillclimb.lisp")
(load "quotes.lisp")
(setf enable1 (load-dawg "enable1.daw"))
(setf plaintext (string-to-26 mccarthy-oldest))
(setf enc-key (make-key "johnmccarthy" 10))
(setf ciphertext (do-substitution plaintext enc-key))
(shotgun-hillclimb ciphertext enable1)
```
If you are running this via SLIME in Emacs, hit c-c c-c to interrupt it. At this point. I'm not sure why, but sometimes it seems to just sit for a long time, and then if I interrupt it and start again, it might find the solution quickly. I don't know why
