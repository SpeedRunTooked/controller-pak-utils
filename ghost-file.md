Ghost File Format
=================

Folks have already reverse engineered the ghost _stream_ data. But not the save file itself.

My friends have a use for this data, so I'm trying to reverse it.

Terminology:
------------

When I talk about a 'file', I am talking about the file that Mariokart64 stores on the controller pak.
I have written `cpak` as a tool to extract files out of the controller pak files themselves.

When we talk about 'pages' we are talking about pages on the controller pak.
These are completely unrelated to pages on my local filesystem.


Things we know:
---------------

* The actual GHOST data is a MIO0-encoded stream which starts 256 bytes after the start of the save file
* You can only save 2 ghost streams in a save file
* The stream data _does not_ contain information about the track and/or the character used.
* There is at least 256 bytes of header for the save data, but it's possible that there's also a head in between ghost streams
* Course time records are two bytes
* Character values are 1 byte

Track Values
------------

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

Character Values
----------------

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
* C: first byte is likely "is this slot used", second byte 0
* D: first byte is very likely the character ID
* Rest: bytes from here until 0x42 are unknown
* Rest: when slot is not used, 'Rest' is just ascending values starting from 0

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

