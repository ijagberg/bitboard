# bitboard64

An implementation of a 64-bit [bitboard](https://en.wikipedia.org/wiki/Bitboard), useful for chess move generation.

## Bitboard

The main point of this library is the `Bitboard` struct, which uses the bits in a `u64` to represent the squares on a chess board.

```rust
use bitboard64::prelude::*;

let bb = INITIAL_STATE;
println!("{bb}");
// prints:
//   ABCDEFGH
// 8 11111111
// 7 11111111
// 6 00000000
// 5 00000000
// 4 00000000
// 3 00000000
// 2 11111111
// 1 11111111
```

---

You can construct `Bitboards` with specified squares set to 1:

```rust
let bb_with_two_ones = Bitboard::with_ones([A2, C7]);
println!("{bb_with_two_ones}");
// prints:
//   ABCDEFGH
// 8 00000000
// 7 00100000
// 6 00000000
// 5 00000000
// 4 00000000
// 3 00000000
// 2 10000000
// 1 00000000
```

---

You can apply masks to include/clear ranks and files from the `Bitboard`:

```rust
let initial_state_but_only_file_c = INITIAL_STATE.include_file(File::C);
println!("{initial_state_but_only_file_c}");
// prints:
//   ABCDEFGH
// 8 00100000
// 7 00100000
// 6 00000000
// 5 00000000
// 4 00000000
// 3 00000000
// 2 00100000
// 1 00100000
```

---

You can use bitwise operators:

```rust
let initial_pawns = INITIAL_STATE & (RANK_2_MASK | RANK_7_MASK);
println!("{initial_pawns}");
// prints:
//   ABCDEFGH
// 8 00000000
// 7 11111111
// 6 00000000
// 5 00000000
// 4 00000000
// 3 00000000
// 2 11111111
// 1 00000000
```

---

You can generate rays:

```rust
let up_right_from_b5 = Bitboard::up_right_ray(B5);
println!("{up_right_from_b5}");
// prints:
//   ABCDEFGH
// 8 00001000
// 7 00010000
// 6 00100000
// 5 01000000
// 4 00000000
// 3 00000000
// 2 00000000
// 1 00000000
```

You can calculate target squares for pieces:

```rust
let white_pieces = Bitboard::with_one(D6);
let black_pieces = Bitboard::with_one(D2);
// A black bishop on B4 can move to any empty square on its diagonals.
// There is a black piece on D2, which blocks the black bishop.
// There is a white piece on D6, which the black bishop can take, but not move through.
assert_eq!(Bitboard::black_bishop_targets(B4, white_pieces, black_pieces), Bitboard::with_ones([A3, A5, C5, D6, C3]));
//   ABCDEFGH      ABCDEFGH
// 8 00000000    8 00000000
// 7 00000000    7 00000000
// 6 000w0000    6 00010000
// 5 00000000 -> 5 10100000
// 4 0x000000    4 0x000000
// 3 00000000    3 10100000
// 2 000b0000    2 00000000
// 1 00000000    1 00000000
```

And a lot more!
