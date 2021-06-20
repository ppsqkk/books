In the program in Exercise 2.42, queen-cols is directly called only
once. In the program in Exercise 2.43, however, queen-cols is called
for each element in (enumerate-interval 1 board-size); in other words,
it is directly called board-size times. If we estimate based on the
number of evaluations of (queen-cols 0), we find that the program
in Exercise 2.43 runs board-size^board-size times more slowly.
