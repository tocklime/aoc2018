# Advent of code 2018 Solutions

These are my solutions to the [2018 advent of code](https://www.adventofcode.com/2018). They're written in Haskell. Some of them I stopped as soon as I got the right answer. Some of them I tidied up a bit afterwards.

On day 8, I didn't start at 5am, but I did time myself. I solved part A after 14:17 from start, and part B after 18:09 from start. If I had started at 5am, I would not have placed in the top 100. Place #100 solved both parts in 12:10. I think I could have placed if I hadn't missed a call to P.skipSpace in my parser. 

On day 9, I also didn't start at 5am, but I timed myself again. I solved both parts in 28:16 (the first part was solved just a few seconds before that - once I'd solved the first part in the way I did, the second part was solved quickly enough). This time would have placed me at #94.


I timed myself again on day 10. First problem in 46:23, both problems in 47:29. Position #100 had it in 16:49. Not super happy with that. It took me a long time to realise I could detect when the correct message was around by smallest total area. At least, I thought I could, and it turned out I could. I spent a long time mucking around with the parser, and I had another missing skipSpace. I should look into tokenizing before parsing. Possibly. Also, my solution doesn't run quickly -- about 8 seconds. That could be improved.

Day 11, part 1: 13:24, part 2: 1:01:02. My solution for part one was quick to write but not fast enough for part 2. For part 2 I had to implement (and remember how to use) a prefix sum grid.