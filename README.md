# AoC20

My solutions to the Advent of Code 2020, for now done in Haskell (which means I skipped a bunch of days where just reading the input seemed like a huge pita).

The Haskell solutions I've done so far will not take the raw input from AoC because, honestly, it was giving me too much of a headache trying to figure out how to read a list of numbers from the stdin. Instead the input needs to be reformatted such that it can be read in a line and parsed with `read input :: [String/Int]` (The type depending on the question).

### Reformat using vim

To reformat using vim, as I did, you can load up the input in vim and do the following:

1. `:%s/$/,` for adding a comma after each line
2. `VGJ` for visually selecting all lines and collapsing them into a single one
3. `A<backspace>]` and `I[` to surround the entire thing in [] (the `<backspace>` is there because in step 1 we add a comma to the last line that we want to remove)

If the input is not a list of numbers but rather a list of strings, substitute step one with `:%s/^\(.*\)$/"\1",` for also surrounding each line in quotes.

If the puzzle has a fixed number of lines as an input (such as day 13), it can be passed raw.
