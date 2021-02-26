# Resources for Cryptography / Cryptanalysis

## enable1.txt
The `enable1.txt` file is a freely available list of words used by a
number of word games. It can be handy for trying to decrypt texts
where you have word breaks and want to see what words would fit.

## enable1.daw
The `enable1.daw` file is a Directed Acyclic Word Graph encoded into a
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

Letter is the letter that the node represents. In the `enable1.daw` file,
the letters are lowercase ASCII values.

### Looking Up A Word
To look up a word in the DAWG, first go to the top of the graph, which
is the 26th number from the end. The last 26 nodes in the file have the
letters a-z consecutively. Let's suppose you are looking for "car", and
also supposed that the `enable1.daw` file has 122197 entries. The first
node is then 122197-26, or 122171.

The first node you look at has the letter 'a', so go to the next node
(the one immediately after it in memory or in the file, which would
be 122172 in an array), and that node has 'b', but the one after that
, number 122173, has 'c'. So now we look at the child index of the 'c'
node and find that it is 17812, so we look at node 17812. The very
first node in the list is the 'a', so now we look at its child index,
which is 17825. So, the children of 'a' that start at 17825 represent
all the letters that can immediately follow "ca" in the dictionary.
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
GADDAG in 1994 that also lets you do fast lookups on what letters can precede
a word - [https://ericsink.com/downloads/faster-scrabble-gordon.pdf]).

For cipher-cracking purposes, we want to find either the longest
possible word, or all the words possible from a given starting point.
In this case, we just keep following the graph matching the letters
we have, and every time we hit an "is-end-of-word" we note as
either the new longest word or just another possible word length.
If we hit an is-end-of-list without matching, we
stop searching because that means the current letter cannot follow
the current word so far in the dictionary.
