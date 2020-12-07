# problem

https://adventofcode.com/2020/day/5

> What is the highest seat ID on a boarding pass?

This problem is a masked binary conversion and arithmetic problem.

I shall:

- get input lines. for each,
- Boarding_pass.of_string: parse the pass into two sets of things, row chars & col chars
  - ...but not really. start with 0 then...
    - depending on the char, bit shift the associated char for the binary pos into the number
    - multiply the row by 8, then add the column
