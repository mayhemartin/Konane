This is my version of the game of Konane.
Some things to note:
	- ELE = Black
	- KEA = White
	- Papamu = board
	- Kanaka = peice
	- KEAELE is set as the enumerable type because the peices (in Hawaian) are refered to as
		- ele ele (black)
		- ke oke o or kea (white)
	- puka = impressions in the papamu (spaces)
	- kulanakila = victor
	- All terms were pulled from http://www.k12.hi.us/~gkaapuni/konane.htm

Code issues:
	- When playing on a 12x or higher board, the rows and/or columns may be skewed by a space on columns higher
	  than 9
	- I had issues testing the game through Visual Studio's F# Interactive window, some printf statements were not
	  being displayed; However I had no issues when running Fsi.exe independently through command prompt.

Program plusses:
	- I tried to make the board dynamic, so for testing purposes, if you don't want to sit through an 8 x 8 game
	  feel free to run 4 x 4, or if you want to see how it handles larger memory mandates, crank the size up.
	- I've tested with a 4x and 6x board to the end of the game, but no higher.
