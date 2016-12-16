module D8Input where

data Command =
    R Int Int
  | RR Int Int
  | RC Int Int

input =
  [ R 1 1
  , RR 0 2
  , R 1 1
  , RR 0 5
  , R 1 1
  , RR 0 3
  , R 1 1
  , RR 0 3
  , R 2 1
  , RR 0 5
  , R 1 1
  , RR 0 5
  , R 4 1
  , RR 0 2
  , R 1 1
  , RR 0 2
  , R 1 1
  , RR 0 5
  , R 4 1
  , RR 0 3
  , R 2 1
  , RR 0 5
  , R 4 1
  , RR 0 2
  , R 1 2
  , RR 1 6
  , RR 0 2
  , R 1 2
  , RC 32 1
  , RC 23 1
  , RC 13 1
  , RR 0 6
  , RC 0 1
  , R 5 1
  , RR 0 2
  , RC 30 1
  , RR 1 20
  , RR 0 18
  , RC 13 1
  , RC 10 1
  , RC 7 1
  , RC 2 1
  , RC 0 1
  , R 17 1
  , RC 16 3
  , RR 3 7
  , RR 0 5
  , RC 2 1
  , RC 0 1
  , R 4 1
  , RC 28 1
  , RR 1 24
  , RR 0 21
  , RC 19 1
  , RC 17 1
  , RC 16 1
  , RC 14 1
  , RC 12 2
  , RC 11 1
  , RC 9 1
  , RC 8 1
  , RC 7 1
  , RC 6 1
  , RC 4 1
  , RC 2 1
  , RC 0 1
  , R 20 1
  , RC 47 1
  , RC 40 2
  , RC 35 2
  , RC 30 2
  , RC 10 3
  , RC 5 3
  , RR 4 20
  , RR 3 10
  , RR 2 20
  , RR 1 16
  , RR 0 9
  , RC 7 2
  , RC 5 2
  , RC 3 2
  , RC 0 2
  , R 9 2
  , RC 22 2
  , RR 3 40
  , RR 1 20
  , RR 0 20
  , RC 18 1
  , RC 17 2
  , RC 16 1
  , RC 15 2
  , RC 13 1
  , RC 12 1
  , RC 11 1
  , RC 10 1
  , RC 8 3
  , RC 7 1
  , RC 6 1
  , RC 5 1
  , RC 3 1
  , RC 2 1
  , RC 1 1
  , RC 0 1
  , R 19 1
  , RC 44 2
  , RC 40 3
  , RC 29 1
  , RC 27 2
  , RC 25 5
  , RC 24 2
  , RC 22 2
  , RC 20 5
  , RC 14 3
  , RC 12 2
  , RC 10 4
  , RC 9 3
  , RC 7 3
  , RC 3 5
  , RC 2 2
  , RR 5 10
  , RR 4 8
  , RR 3 8
  , RR 2 48
  , RR 1 47
  , RR 0 40
  , RC 47 5
  , RC 46 5
  , RC 45 4
  , RC 43 2
  , RC 42 3
  , RC 41 2
  , RC 38 5
  , RC 37 5
  , RC 36 5
  , RC 33 1
  , RC 28 1
  , RC 27 5
  , RC 26 5
  , RC 25 1
  , RC 23 5
  , RC 22 1
  , RC 21 2
  , RC 18 1
  , RC 17 3
  , RC 12 2
  , RC 11 2
  , RC 7 5
  , RC 6 5
  , RC 5 4
  , RC 3 5
  , RC 2 5
  , RC 1 3
  , RC 0 4
  ]

example =
  [R 3 2, RC 1 1, RR 0 4, RC 1 1]
