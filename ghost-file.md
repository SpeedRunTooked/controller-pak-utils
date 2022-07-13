Introduction and Background
===========================

Motivation
----------

Recently, my friends have gotten into playing Mario Kart 64 (for the Nintendo 64).
They compete by running Time Trials and comparing times, no glitches or use of tools.
We have already written software that can automatically detect when new personal records are set and have them uploaded to a server to compare.
An additional feature that would be useful is the ability to share 'ghost' data.
After completing a run, the game allows you to save a 'ghost' of that run (if certain conditions are met).
This ghost can then be projected the next time you do a Time Trial on that track, so that you can more easily see the difference between your current run and the 'ghost run'.

Background
----------

On the N64 itself, the ghost data is saved on the optional 'Controller Pak', which was a memory device that would be inserted into your controller.
This provided more memory than the EEPROM that was in the game cartridge itself (which was used for track records, but not ghost data).
When using an emulator, the emulator will write out an `mpk` file which corresponds to the data that would have been written to the Controller Pak itself.

A filesystem was necessary since the controller paks could be used for multiple games (though in the particular case of Mario Kart 64, saving ghost data would use the vast majority of the storage available (121 of 123 pages are used by a ghost file).
This filesystem is well documented and writing the code to inspect and navigate the filesystem was straightforward.

Ghost File Format
-----------------

We're going to call a Mario Kart 64 save of Ghost data a 'Ghost File', even though that is not the terminology used by Nintendo (they called things 'Notes' and it's not clear what the Mario Kart 64 devs called the specific format for a MK64 Note).

A Ghost File has two parts:

* A Header (what this document is mostly about)
* The ghost 'stream', which is the data that allows the game to replay your run as the ghost.

Folks have already reverse engineered the ghost _stream_ data, but not the header.
Let's talk a bit about what is in each.
We'll start with the stream, since that's better understood.

### The Stream

The ghost stream itself is a set of records that is run-length encoded.
Each record has the following information:

* The state of buttons
  - A, the accelerator
  - B, the brake
  - Z, triggering an item
  - R, hopping (so that you can drift/powerslide)
* The state of the analog stick's Y-axis
* The state of the analog stick's X-axis

The game keeps track of this information for each frame, but in order to avoid wasting space the state of the buttons are single bits within a byte and the records are run-length encoded.
So the data as written follows the following format (each box is a byte):

```
+------------+----------+-------+-------+
|button-state|#-of-frame|stick-y|stick-x|
+------------+----------+-------+-------+
```

This data is then _further compressed_ using Nintendo's MIO0 compression scheme, which is decently documented.
A consequence of this is that you will not see bytes that follow that format in the `.mpk` file itself, you must find the appropriate place where the ghost stream you care about is and then decompress the data.
My implementation of the decompression is in [src/Codec/MIO0.hs](src/Codec/MIO0.hs).

The ghost stream data is _only this sequence of records_.
In other words, it contains no information about either of the following:

* The character that the player used
* The track where the run was recorded

But we _know_ that this information exists somewhere in the Ghost File because the game tells you which tracks have ghosts recorded for them and when you play a track with a ghost, it renders the same character that was used when the recorded run was saved.

So where is this data?
It must be in the header.


The Header
==========

I have written a tool (`cpak`) to extract files out of the controller pak files themselves, as well as to perform decoding of the `MIO0` stream and query/dump specific pages of the controller pak. The functionality of `cpak` is not specific to MK64.

I have written a much smaller tool [`haunting`](https://github.com/SpeedRunTooked/haunting), which uses `cpak` as a library to get the uncompressed ghost data and then converts it into a `.csv` for easy processing.

Quick note: When we talk about 'pages' we are talking about pages on the controller pak.
These are completely unrelated to pages on my local filesystem.

Things we know:
---------------

### Basic Facts

* The actual Ghost data is a MIO0-encoded stream which starts 256 bytes after the start of the save file
* You can only save 2 ghost streams in a save file
* The stream data _does not_ contain information about the track and/or the character used.
* There is at least 256 bytes of header for the save data, but it's possible that there's also a head in between ghost streams
* Course time records are two bytes
* Character values are 1 byte

### The two slots

It seems that the slots always start at the same offsets, regardless of length.
`cpak -m <N>` gives the byte offsets of any `MIO0` signatures.
Running it on a variety of controller pak files with many different ghost files gives me:


```
[jmct@flip controller-pak]$ for f in `ls pak-files/*.mpk`; do ./cpak -m 0 -f $f; done
Note #0 had MIO0 signatures at bytes: [256]
Note #0 had MIO0 signatures at bytes: [256,15616]
Note #0 had MIO0 signatures at bytes: [256,15616]
Note #0 had MIO0 signatures at bytes: [256,15616]
Note #0 had MIO0 signatures at bytes: [256,15616]
Note #0 had MIO0 signatures at bytes: [256]
Note #0 had MIO0 signatures at bytes: [256]
Note #0 had MIO0 signatures at bytes: [256]
Note #0 had MIO0 signatures at bytes: [256,15616]
Note #0 had MIO0 signatures at bytes: [256,15616]
```

The decompressed ghost streams are not all the same length, however.
You can easily see this by comparing the number of lines of a few ghost stream `csv` files:

```
[jmct@flip haunting]$ for f in `ls *.csv`; do wc -l $f; done
9167 tlrw-only-left.csv
5363 tlrw-shorter1.csv
6667 tlrw-shorter.csv
```

So the there's either junk in the bytes between the first slot and the start of the second or some sort of padding.

### Track Values

0000  Mario Raceway
0001  Choco Mountain
0002  Bowser's Castle
0003  Banshee Boardwalk
0004  Yoshi Valley
0005  Frappe Snowland
0006  Koopa Troopa Beach
0007  Royal Raceway
0008  Luigi Raceway
0009  Moo Moo Farm
000A  Toad's Turnpike
000B  Kalimari Desert
000C  Sherbet Land
000D  Rainbow Road
000E  Wario Stadium
000F  Block Fort
0010  Skyscraper
0011  Double Deck
0012  DK's Jungle Parkway
0013  Big Donut

### Character Values

0 	MARIO
1 	LUIGI
2 	YOSHI
3 	TOAD
4 	D.K.
5 	WARIO
6 	PEACH
7 	BOWSER 

An Example:
-----------

A save file that has only one ghost, Toad on Luigi's raceway, in slot one, starts off like this:

```
00000000: 0000 32c1 0100 031e 1838 103e 585c 982b  ..2......8.>X\.+
00000010: d432 14b7 3a8e 3026 56cd a429 6e6b 5851  .2..:.0&V..)nkXQ
00000020: 4491 dc8c b231 8080 8ace 14ad 8a4b 90de  D....1.......K..
00000030: 8aa0 c8a2 1294 8080 8080 60e1 fabf c04d  ..........`....M
00000040: 521f 1000 0000 0000 0000 0000 0000 0000  R...............
00000050: 0000 0000 0000 0000 0000 0000 0000 0000  ................
00000060: 0000 0000 0000 0000 0000 0000 0000 0000  ................
00000070: 0000 0000 0000 0000 0000 0000 0000 006c  ...............l
00000080: 0000 309c 0000 0000 0102 0304 0506 0708  ..0.............
00000090: 090a 0b0c 0d0e 0f10 1112 1314 1516 1718  ................
000000a0: 191a 1b1c 1d1e 1f20 2122 2324 2526 2728  ....... !"#$%&'(
000000b0: 292a 2b2c 2d2e 2f30 3132 3334 3536 3738  )*+,-./012345678
000000c0: 393a 3b00 0000 0000 0000 0000 0000 0000  9:;.............
000000d0: 0000 0000 0000 0000 0000 0000 0000 0000  ................
000000e0: 0000 0000 0000 0000 0000 0000 0000 0000  ................
000000f0: 0000 0000 0000 0000 0000 0000 0000 000f  ................
00000100: 4d49 4f30 0000 1814 0000 0168 0000 097c  MIO0.......h...|
```

This has the full first page and the first 16 bytes of Page 2.
You can see the `MIO0` signature at the beginning of the second page.

Here's Peach on the same track and in the same slot:

```
00000000: 0000 3203 0100 06e0 06c5 d8d7 c00b b8c6  ..2.............
00000010: 76cb 50aa 1ec4 6077 38cd a429 6e6b 5851  v.P...`w8..)nkXQ
00000020: 4491 dc8c b231 8080 8ace 14ad 8a4b 90de  D....1.......K..
00000030: 8aa0 c8a2 1294 8080 8080 60e1 fabf c04d  ..........`....M
00000040: 521f 1000 0000 0000 0000 0000 0000 0000  R...............
00000050: 0000 0000 0000 0000 0000 0000 0000 0000  ................
00000060: 0000 0000 0000 0000 0000 0000 0000 0000  ................
00000070: 0000 0000 0000 0000 0000 0000 0000 0008  ................
00000080: 0000 309c 0000 0000 0102 0304 0506 0708  ..0.............
00000090: 090a 0b0c 0d0e 0f10 1112 1314 1516 1718  ................
000000a0: 191a 1b1c 1d1e 1f20 2122 2324 2526 2728  ....... !"#$%&'(
000000b0: 292a 2b2c 2d2e 2f30 3132 3334 3536 3738  )*+,-./012345678
000000c0: 393a 3b00 0000 0000 0000 0000 0000 0000  9:;.............
000000d0: 0000 0000 0000 0000 0000 0000 0000 0000  ................
000000e0: 0000 0000 0000 0000 0000 0000 0000 0000  ................
000000f0: 0000 0000 0000 0000 0000 0000 0000 000f  ................
```

We know that the character value is only one byte, and that they range from 0 - 7.
So we're looking for a one byte value that is 03 in the first and 06 in the second.
Let's zoom in the on the first line and see if we spot anything:

```
00000000: 0000 32c1 0100 031e 1838 103e 585c 982b  ..2......8.>X\.+
00000000: 0000 3203 0100 06e0 06c5 d8d7 c00b b8c6  ..2.............
```

Luckily it's not too far from the beginning: the 7th byte!
If we try to test this, by changing the 03 to a 06, the game no longer recognizes the file.

However, this is the _only_ place on this page where we have a single byte where we have 03 in the Toad file and 06 in the Peach file.
As such, I am not ready to abandon the idea that this is indeed the byte that stores the character value.
It's likely that there's a checksum value somewhere.
Nintendo uses checksums often (I know because I had to write code to calculate checksums for reading from the memory paks!).

Using our best pattern matcher
==============================

I wanted to get a better sense of the 'shape' of the header, even if I can't yet figure out what the fields are for.

I wrote the following shell script, which prints the hexdump of a file, waits for me to press a key and then prints the hexdump of another file.
It will loop forever switching between all the files that I provide it in sequence.

```{sh}
while true;
  do for f in "$@"; do
    clear
    echo "xxd $f"
    xxd $f
    read -n 1;
  done;
done
```

Running this script as `sh switch.sh file1 file2` allows me to switch back and forth at whatever rate I feel like.
Holding down return makes the different parts of the hexdump blur, which is nice.
Doing this, I immediately saw some patterns that were helpful.
Below I show a diff between the two files with the following key:

* `##` is where they differ in a way that I don't understand
* `CC` is where the Toad save has `03` and the Peach save has `06`
* `..` is where they are the same
* Actual values is where they are the same but for some reason interesting to me

They are identical from byte 128 onward, so I just ignore that:

```
00000000: 0000 32## 0100 CC## #### #### #### ####
00000010: #### #### #### #### ##.. .... .... ....
00000020: .... .... .... .... .... .... .... ....
00000030: .... .... .... .... .... .... .... ....
00000040: .... .... 0000 0000 0000 0000 0000 0000
00000050: 0000 0000 0000 0000 0000 0000 0000 0000
00000060: 0000 0000 0000 0000 0000 0000 0000 0000
00000070: 0000 0000 0000 0000 0000 0000 0000 00##
```

When saving ghost data in the _second_ slot, things are very different, but it seems like the second slot takes up the second 128 bytes.
If we diff the first 128 bytes of the Toad save in slot 1 and the second 128 bytes of the Toad save in slot 2 we get the following (remember that the offsets on the left are only different because we're looking at different part of the file):

```
000000##: 0000 3### 0100 03## #### #### #### ###b
000000##: #### #### #### #### #### #### #### ####
000000##: #### #### #### 8080 #### #### #### ####
000000##: #### #### #### 8080 8080 #### #### ####
000000##: #### #### 0000 0000 0000 0000 0000 0000
000000##: 0000 0000 0000 0000 0000 0000 0000 0000
000000##: 0000 0000 0000 0000 0000 0000 0000 0000
000000##: 0000 0000 0000 0000 0000 0000 0000 00##
```

Now suspecting that the two slots take the first 128 bytes and the second 128 bytes, respectively, we can make more sense of the slot that _isn't_ used.

If we diff the non-used slot on both we get the following:


```
000000##: 0000 30## 0000 0000 0102 0304 0506 0708
000000##: 090a 0b0c 0d0e 0f10 1112 1314 1516 1718
000000##: 191a 1b1c 1d1e 1f20 2122 2324 2526 2728
000000##: 292a 2b2c 2d2e 2f30 3132 3334 3536 3738
000000##: 393a 3b00 0000 0000 0000 0000 0000 0000
000000##: 0000 0000 0000 0000 0000 0000 0000 0000
000000##: 0000 0000 0000 0000 0000 0000 0000 0000
000000##: 0000 0000 0000 0000 0000 0000 0000 00##
```

That's right, only two bytes are different between them!

Taking Stock
============

Here's what I think we've discovered to far:


```
  A    B    C    D    E
+----+----+----+----+----+----+----+----+
|0000|????|0X00|0X??|????|????|????|????|...
+----+----+----+----+----+----+----+----+
```

* A: always null
* B: unknown
* C: first byte is likely "is this slot used", second byte often 0, sometimes something else...
* D: first byte is very likely the character ID
* Rest: bytes from here until 0x42 are unknown
* Rest: when slot is not used, 'Rest' is just ascending values starting from 0

Slightly More Structure
=======================

When looking at two save files, I the `nkXQ` pattern that was present in the first two examples I showed.
However, there was a bit more information that just wasn't present in those examples.

Let's take a look at the relevant bytes from our first example:

```
00000000: 0000 32c1 0100 031e 1838 103e 585c 982b  ..2......8.>X\.+
00000010: d432 14b7 3a8e 3026 56cd a429 6e6b 5851  .2..:.0&V..)nkXQ
00000020: 4491 dc8c b231 8080 8ace 14ad 8a4b 90de  D....1.......K..
00000030: 8aa0 c8a2 1294 8080 8080 60e1 fabf c04d  ..........`....M
00000040: 521f 1000 0000 0000 0000 0000 0000 0000  R...............
```

Our examples had everything between the 8th byte and the 66th byte the same, but then I saw this other save file:

```
00000080: 0000 301c 0100 0284 4afd 7471 c48c b030  ..0.....J.tq...0
00000090: 4a1f 90d8 6ade c0d4 40f1 7c8b 6e6b 5851  J...j...@.|.nkXQ
000000a0: 4491 dc8c b231 8080 8ace 14ad 8a4b 90de  D....1.......K..
000000b0: 8aa0 c8a2 1294 8080 8080 60e1 fabf c04d  ..........`....M
000000c0: 521f 1000 0000 0000 0000 0000 0000 0000  R...............
```

Now the 21 bytes between the character code and byte 0x9b are different.

Now before when we looked at a save of Luigi's raceway on the second slot, it didn't share any common bytes, but it's always possible that it was a mislabel.
Either way this doesn't share _all_ the same bytes, and maybe there's some important context in that.

(The next day...)

I was able to download a few more MPKs, the following two are the first slots from two saves on Frappe:


```
00000000: 0000 353b 0105 0336 9653 48fc 02ba c04a  ..5;...6.SH....J
00000010: d607 b0b3 b25a e0ad 0480 8080 8080 8080  .....Z..........
00000020: 8080 8080 8080 8080 8080 8080 8080 8080  ................
00000030: 8080 8080 8080 8080 8080 8080 8080 8080  ................
00000040: 8080 8000 0000 0000 0000 0000 0000 0000  ................
00000050: 0000 0000 0000 0000 0000 0000 0000 0000  ................
00000060: 0000 0000 0000 0000 0000 0000 0000 0000  ................
00000070: 0000 0000 0000 0000 0000 0000 0000 0022  ..............."
```

and

```
00000000: 0000 340b 0105 0318 4233 288a a8a0 404c  ..4.....B3(...@L
00000010: dedd 4c52 3ca1 b0ad 0480 8080 8080 8080  ..LR<...........
00000020: 8080 8080 8080 8080 8080 8080 8080 8080  ................
00000030: 8080 8080 8080 8080 8080 8080 8080 8080  ................
00000040: 8080 8000 0000 0000 0000 0000 0000 0000  ................
00000050: 0000 0000 0000 0000 0000 0000 0000 0000  ................
00000060: 0000 0000 0000 0000 0000 0000 0000 0000  ................
00000070: 0000 0000 0000 0000 0000 0000 0000 0095  ................
```

This is also the same between 0x17 and 0x42 (most of those bytes being 0x80, which I think is some sort of padding).

So I think we can further refine our view of the header, though it's possible that what we're identifying as a pattern is a coincidence.

Taking Stock: Part II
=====================

Here's what I think we've discovered to far:


```
  A    B    C   D,   E (16 bytes)
+----+----+----+--+----------------+----+
|0000|????|0X00|0X|????????????????|????|...
+----+----+----+--+----------------+----+
```

* A: always null
* B: unknown, but always starts with 0x3?, perhaps this is a 16-bit pointer?
* C: first byte is likely "is this slot used", second byte often 0, sometimes something else...
* D: first byte is very likely the character ID
* E: 16 bytes of unknown purpose
* Rest: bytes from here until 0x42 are unknown
* Rest: when slot is not used, 'E' and 'Rest' is just ascending values starting from 0

`ad'?
-----

Every single example has the byte `ad` in it, perhaps this separates different sections of the file?
Probably not.

Conclusions
===========

Very few... so far.

Appendix A:
===========

Finding Checksums
=================

EDITOR'S NOTE!!!!
-----------------

This was almost certainly a waste of time.
My assumption that a checksum is involved hasn't panned out.
It's possible that there's still a checksum involved somehow, but the fact that the empty slots aren't _byte for byte_ identical makes me want to put this aside for now.

Back to Content
---------------

I wrote some code, using my `cpak` library that gives me a list of potential checksum locations.
First some boilerplate.

Load the controller pak for the toad and peach saves, respectively:

```{haskell}
Right toad <- parsePak "pak-files/tlrw.mpk" 
Right peach <- parsePak "pak-files/plrw.mpk" 
```

Process the paks so that we can navigate them easily:

```{haskell}
Right (Pak _ itab _ (RP toadmap))  = processPak toad
Right (Pak _ itab _ (RP peachmap)) = processPak peach
```

Load all of the pages for each of the save files:

```{haskell}
let Just toadbytes  = getPages (fileSeq (iNodeFiles itab !! 0)) (RP toadmap)
let Just peachbytes = getPages (fileSeq (iNodeFiles itab !! 0)) (RP peachmap)
```

Now let's start digging.

The API for getting checksum candidates looks like the following:


```{haskell}
checksumCandidates :: Bool -> Int -> ByteString -> [(Int, Int)]
```

The `Bool` is whether you want to consider checksum values of `0`.
The `Int` is how many checksums you think would be on a single page (I'll explain why I added that).
The `ByteString` is the series of bytes you'd like to investigate.
The return value, of type `[(Int, Int)]` is a list of page/offset pairs.

There are 256 bytes per page, and let's say we want to consider 0-valued checksums (it's just as likely as any other value!) and allow for 256 checksums on a page (why would Nintendo do the latter? Not for me to judge.):

With that configuration we get 11281 candidates for both Toad and Peach. However, _a lot_ of these candidates end up being on the same page.
If we even restrict it to only allowing 255 checksums on a page, the numbers comes way down: 273 candidates.

Let's do some obvious filtering. 

First, if it's really a checksum then it'll be a checksum candidate in _both_ save files.
When I still allow for entire (pages - 1byte) to be checksums, that only brings it down to 251 possible candidates.

I wrote the following to see how the number of candidates-per-page affected the count:

```{haskell}
[(i, length (getBoth t p))
| i <- [256,255..2]
, let t = checksumCandidates True i toadbytes
, let p = checksumCandidates True i peachbytes
]
```

(This is a slight lie, I actually wrote it to decrement in large intervals and then did a binary search to find where the big steps happened, but this would get you the same thing, just with more CPU cycles)

What I found was that at 225 checksum candidates per page, the count goes down from 251 to 26.
I don't know about you, but I feel very comfortable with the idea that Nintendo would use more than 225 bytes of a 256-byte page for checksums!

