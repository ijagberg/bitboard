#![allow(unused)]

pub use crate::consts::*;
use chesspos::prelude::*;
use std::{
    fmt::{Debug, Display},
    ops::{
        Add, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not, Shl, Shr, Sub,
    },
};

pub mod consts;
pub mod prelude;

/// A 64-bit Bitboard.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Bitboard(u64);

impl Bitboard {
    const fn construct(v: u64) -> Self {
        Self(v)
    }

    /// Create a new `Bitboard`, containing the value `v`.
    pub const fn new(v: u64) -> Self {
        Self::construct(v)
    }

    /// Create a full `Bitboard`.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::Bitboard;
    /// let full = Bitboard::full();
    /// assert_eq!(full, Bitboard::new(0b1111111111111111111111111111111111111111111111111111111111111111));
    /// ```
    pub const fn full() -> Self {
        Self::construct(!0)
    }

    /// Create an empty `Bitboard`.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::Bitboard;
    /// let empty = Bitboard::empty();
    /// assert_eq!(empty, Bitboard::new(0b0000000000000000000000000000000000000000000000000000000000000000));
    /// ```
    pub const fn empty() -> Self {
        Self::construct(0)
    }

    /// Create a `Bitboard` with `pos` set to 1.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::with_one(A7);
    /// assert_eq!(bb, Bitboard::new(0b1000000000000000000000000000000000000000000000000));
    /// //   ABCDEFGH
    /// // 8 00000000
    /// // 7 10000000
    /// // 6 00000000
    /// // 5 00000000
    /// // 4 00000000
    /// // 3 00000000
    /// // 2 00000000
    /// // 1 00000000
    /// ```
    pub fn with_one(pos: Position) -> Self {
        Self::construct(1 << bitboard_index_of(pos))
    }

    /// Create a `Bitboard` with all positions in `positions` set to 1.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::with_ones([A1, B2, C3, D4, E5, F6, G7, H8]);
    /// assert_eq!(bb, Bitboard::new(0b1000000001000000001000000001000000001000000001000000001000000001));
    /// //   ABCDEFGH
    /// // 8 00000001
    /// // 7 00000010
    /// // 6 00000100
    /// // 5 00001000
    /// // 4 00010000
    /// // 3 00100000
    /// // 2 01000000
    /// // 1 10000000
    /// ```
    pub fn with_ones(positions: impl IntoIterator<Item = Position>) -> Self {
        let mut s = Self(0);
        for pos in positions {
            s |= Self::with_one(pos);
        }

        s
    }

    /// Returns `true` if the bit at `pos` is 1, `false` otherwise.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::with_one(F2);
    /// assert!(bb.bit_at(F2));
    /// assert!(!bb.bit_at(F3));
    /// ```
    pub fn bit_at(&self, pos: Position) -> bool {
        let idx = bitboard_index_of(pos);
        ((1 << idx) & self.0) > 0
    }

    /// Apply a mask to `self`, only including the bit at `pos`.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::with_ones([F2, F3]);
    /// let masked = bb.include_position(F3);
    /// assert_eq!(masked, Bitboard::with_one(F3)); // the bit at F2 is now 0
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00000000    6 00000000
    /// // 5 00000000 -> 5 00000000
    /// // 4 00000000    4 00000000
    /// // 3 00000100    3 00000100
    /// // 2 00000100    2 00000000
    /// // 1 00000000    1 00000000
    /// ```
    pub fn include_position(self, pos: Position) -> Self {
        self & Self::with_one(pos)
    }

    /// Apply a mask to `self`, only including the bits at the given positions.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::with_ones([F2, F3, F4]);
    /// let masked = bb.include_positions([F3, F4, F5]);
    /// assert_eq!(masked, Bitboard::with_ones([F3, F4])); // neither F2 nor F5 are in the resulting bitboard
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00000000    6 00000000
    /// // 5 00000000 -> 5 00000000
    /// // 4 00000100    4 00000100
    /// // 3 00000100    3 00000100
    /// // 2 00000100    2 00000000
    /// // 1 00000000    1 00000000
    /// ```
    pub fn include_positions(self, positions: impl IntoIterator<Item = Position>) -> Self {
        let mut include = Self::empty();
        for pos in positions {
            include |= Self::with_one(pos);
        }

        self & include
    }

    /// Apply a mask to `self`, clearing the bit at position `pos`.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::with_ones([F4, F5]);
    /// let cleared = bb.clear_position(F4);
    /// assert_eq!(cleared, Bitboard::with_one(F5)); // F4 has been cleared
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00000000    6 00000000
    /// // 5 00000100 -> 5 00000100
    /// // 4 00000100    4 00000000
    /// // 3 00000000    3 00000000
    /// // 2 00000000    2 00000000
    /// // 1 00000000    1 00000000
    /// ```
    pub fn clear_position(self, pos: Position) -> Self {
        self & !Self::with_one(pos)
    }

    /// Apply a mask to `self`, clearing the bit at each position in `positions`.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::with_ones([F2, F3, F4]);
    /// let cleared = bb.clear_positions([F2, F3]);
    /// assert_eq!(cleared, Bitboard::with_one(F4)); // only F4 remains after clearing F2 and F3
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00000000    6 00000000
    /// // 5 00000000 -> 5 00000000
    /// // 4 00000100    4 00000100
    /// // 3 00000100    3 00000000
    /// // 2 00000100    2 00000000
    /// // 1 00000000    1 00000000
    /// ```
    pub fn clear_positions(self, positions: impl IntoIterator<Item = Position>) -> Self {
        let mut clear = Self::full();
        for pos in positions {
            clear &= !Self::with_one(pos);
        }

        self & clear
    }

    fn include_file_bitboard(file: File) -> Self {
        match file {
            File::A => FILE_A_MASK,
            File::B => FILE_B_MASK,
            File::C => FILE_C_MASK,
            File::D => FILE_D_MASK,
            File::E => FILE_E_MASK,
            File::F => FILE_F_MASK,
            File::G => FILE_G_MASK,
            File::H => FILE_H_MASK,
        }
    }

    /// Apply a mask to `self`, only including the bits at positions in the given file.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::with_ones([A1, B1, C1]);
    /// let masked = bb.include_file(File::A);
    /// assert_eq!(masked, Bitboard::with_one(A1)); // B1 and C1 are not in file A
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00000000    6 00000000
    /// // 5 00000000 -> 5 00000000
    /// // 4 00000000    4 00000000
    /// // 3 00000000    3 00000000
    /// // 2 00000000    2 00000000
    /// // 1 11100000    1 10000000
    /// ```
    pub fn include_file(self, file: File) -> Self {
        self & Self::include_file_bitboard(file)
    }

    /// Apply a mask to `self`, only including the bits at positions in the given files.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::with_ones([A1, B1, C1]);
    /// let masked = bb.include_files([File::A, File::B]);
    /// assert_eq!(masked, Bitboard::with_ones([A1, B1])); // C1 is not in file A or B
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00000000    6 00000000
    /// // 5 00000000 -> 5 00000000
    /// // 4 00000000    4 00000000
    /// // 3 00000000    3 00000000
    /// // 2 00000000    2 00000000
    /// // 1 11100000    1 11000000
    /// ```
    pub fn include_files(self, files: impl IntoIterator<Item = File>) -> Self {
        let mut mask = Bitboard::empty();
        for file in files {
            mask |= Self::include_file_bitboard(file)
        }

        self & mask
    }

    fn clear_file_bitboard(file: File) -> Self {
        match file {
            File::A => FILE_A_CLEAR,
            File::B => FILE_B_CLEAR,
            File::C => FILE_C_CLEAR,
            File::D => FILE_D_CLEAR,
            File::E => FILE_E_CLEAR,
            File::F => FILE_F_CLEAR,
            File::G => FILE_G_CLEAR,
            File::H => FILE_H_CLEAR,
        }
    }

    /// Apply a mask to `self`, setting the bits at positions in the given file to 0.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::with_ones([C1, C2, B2]);
    /// let cleared = bb.clear_file(File::C);
    /// assert_eq!(cleared, Bitboard::with_one(B2)); // C1 and C2 have been cleared
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00000000    6 00000000
    /// // 5 00000000 -> 5 00000000
    /// // 4 00000000    4 00000000
    /// // 3 00000000    3 00000000
    /// // 2 01100000    2 01000000
    /// // 1 00100000    1 00000000
    /// ```
    pub fn clear_file(self, file: File) -> Self {
        self & Self::clear_file_bitboard(file)
    }

    /// Apply a mask to `self`, setting the bits at positions in the given files to 0.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::with_ones([C1, C2, B2, D1]);
    /// let cleared = bb.clear_files([File::C, File::D]);
    /// assert_eq!(cleared, Bitboard::with_one(B2)); // C1, C2 and D1 have been cleared
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00000000    6 00000000
    /// // 5 00000000 -> 5 00000000
    /// // 4 00000000    4 00000000
    /// // 3 00000000    3 00000000
    /// // 2 01100000    2 01000000
    /// // 1 00110000    1 00000000
    /// ```
    pub fn clear_files(self, files: impl IntoIterator<Item = File>) -> Self {
        let mut clear = Bitboard::full();
        for file in files {
            clear &= Self::clear_file_bitboard(file)
        }

        self & clear
    }

    fn include_rank_bitboard(rank: Rank) -> Self {
        match rank {
            Rank::One => RANK_1_MASK,
            Rank::Two => RANK_2_MASK,
            Rank::Three => RANK_3_MASK,
            Rank::Four => RANK_4_MASK,
            Rank::Five => RANK_5_MASK,
            Rank::Six => RANK_6_MASK,
            Rank::Seven => RANK_7_MASK,
            Rank::Eight => RANK_8_MASK,
        }
    }

    /// Apply a mask to `self`, only including bits at positions in the given rank.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::with_ones([A1, A2, G2]);
    /// let rank_two = bb.include_rank(Rank::Two);
    /// assert_eq!(rank_two, Bitboard::with_ones([A2, G2]));
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00000000    6 00000000
    /// // 5 00000000 -> 5 00000000
    /// // 4 00000000    4 00000000
    /// // 3 00000000    3 00000000
    /// // 2 10000010    2 10000010
    /// // 1 10000000    1 00000000
    /// ```
    pub fn include_rank(self, rank: Rank) -> Self {
        self & Self::include_rank_bitboard(rank)
    }

    /// Apply a mask to `self`, only including bits at positions in the given ranks.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::with_ones([A1, A2, B2, F3]);
    /// let rank_two_and_three = bb.include_ranks([Rank::Two, Rank::Three]);
    /// assert_eq!(rank_two_and_three, Bitboard::with_ones([A2, B2, F3])); // A1 is not in rank two or three
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00000000    6 00000000
    /// // 5 00000000 -> 5 00000000
    /// // 4 00000000    4 00000000
    /// // 3 00000100    3 00000100
    /// // 2 11000000    2 11000000
    /// // 1 10000000    1 00000000
    /// ```
    pub fn include_ranks(self, ranks: impl IntoIterator<Item = Rank>) -> Self {
        let mut mask = Bitboard::empty();
        for rank in ranks {
            mask |= Self::include_rank_bitboard(rank)
        }

        self & mask
    }

    fn clear_rank_bitboard(rank: Rank) -> Self {
        match rank {
            Rank::One => RANK_1_CLEAR,
            Rank::Two => RANK_2_CLEAR,
            Rank::Three => RANK_3_CLEAR,
            Rank::Four => RANK_4_CLEAR,
            Rank::Five => RANK_5_CLEAR,
            Rank::Six => RANK_6_CLEAR,
            Rank::Seven => RANK_7_CLEAR,
            Rank::Eight => RANK_8_CLEAR,
        }
    }

    /// Apply a mask to `self`, setting the bits at positions in the given rank to 0.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::with_ones([C1, C2, B2]);
    /// let without_rank_1 = bb.clear_rank(Rank::One);
    /// assert_eq!(without_rank_1, Bitboard::with_ones([C2, B2])); // C1 has been cleared
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00000000    6 00000000
    /// // 5 00000000 -> 5 00000000
    /// // 4 00000000    4 00000000
    /// // 3 00000000    3 00000000
    /// // 2 01100000    2 01100000
    /// // 1 00100000    1 00000000
    /// ```
    pub fn clear_rank(self, rank: Rank) -> Self {
        self & Self::clear_rank_bitboard(rank)
    }

    /// Apply a mask to `self`, setting the bits at positions in the given ranks to 0.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::with_ones([C1, C2, B2, D3]);
    /// let cleared = bb.clear_ranks([Rank::Two, Rank::Three]);
    /// assert_eq!(cleared, Bitboard::with_one(C1)); // C2, B2, D3 have been cleared
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00000000    6 00000000
    /// // 5 00000000 -> 5 00000000
    /// // 4 00000000    4 00000000
    /// // 3 00010000    3 00000000
    /// // 2 01100000    2 00000000
    /// // 1 00100000    1 00100000
    /// ```
    pub fn clear_ranks(self, ranks: impl IntoIterator<Item = Rank>) -> Self {
        let mut clear = Bitboard::full();
        for rank in ranks {
            clear &= Self::clear_rank_bitboard(rank);
        }

        self & clear
    }

    /// Create a `Bitboard`, setting each bit at positions in a ray starting at `from` and going straight up to 1.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::up_ray(C5);
    /// assert_eq!(bb, Bitboard::with_ones([C5, C6, C7, C8]));
    /// //   ABCDEFGH
    /// // 8 00100000
    /// // 7 00100000
    /// // 6 00100000
    /// // 5 00100000
    /// // 4 00000000
    /// // 3 00000000
    /// // 2 00000000
    /// // 1 00000000
    /// ```
    pub fn up_ray(from: Position) -> Self {
        Self::full()
            .include_ranks(from.rank().walk_up())
            .include_file(from.file())
    }

    /// Create a `Bitboard`, setting each bit at positions in a ray starting at `from` and going diagonally up and to the right to 1.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::up_right_ray(C5);
    /// assert_eq!(bb, Bitboard::with_ones([C5, D6, E7, F8]));
    /// //   ABCDEFGH
    /// // 8 00000100
    /// // 7 00001000
    /// // 6 00010000
    /// // 5 00100000
    /// // 4 00000000
    /// // 3 00000000
    /// // 2 00000000
    /// // 1 00000000
    /// ```
    pub fn up_right_ray(from: Position) -> Self {
        Self::full().include_positions(
            from.file()
                .walk_right()
                .zip(from.rank().walk_up())
                .map(|(file, rank)| Position::new(file, rank)),
        )
    }

    /// Create a `Bitboard`, setting each bit at positions in a ray starting at `from` and going straight to the right to 1.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::right_ray(C5);
    /// assert_eq!(bb, Bitboard::with_ones([C5, D5, E5, F5, G5, H5]));
    /// //   ABCDEFGH
    /// // 8 00000000
    /// // 7 00000000
    /// // 6 00000000
    /// // 5 00111111
    /// // 4 00000000
    /// // 3 00000000
    /// // 2 00000000
    /// // 1 00000000
    /// ```
    pub fn right_ray(from: Position) -> Self {
        Self::full()
            .include_files(from.file().walk_right())
            .include_rank(from.rank())
    }

    /// Create a `Bitboard`, setting each bit at positions in a ray starting at `from` and going diagonally down and to the right to 1.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::down_right_ray(C5);
    /// assert_eq!(bb, Bitboard::with_ones([C5, D4, E3, F2, G1]));
    /// //   ABCDEFGH
    /// // 8 00000000
    /// // 7 00000000
    /// // 6 00000000
    /// // 5 00100000
    /// // 4 00010000
    /// // 3 00001000
    /// // 2 00000100
    /// // 1 00000010
    /// ```
    pub fn down_right_ray(from: Position) -> Self {
        Self::full().include_positions(
            from.file()
                .walk_right()
                .zip(from.rank().walk_down())
                .map(|(file, rank)| Position::new(file, rank)),
        )
    }

    /// Create a `Bitboard`, setting each bit at positions in a ray starting at `from` and going straight down to 1.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::down_ray(C5);
    /// assert_eq!(bb, Bitboard::with_ones([C5, C4, C3, C2, C1]));
    /// //   ABCDEFGH
    /// // 8 00000000
    /// // 7 00000000
    /// // 6 00000000
    /// // 5 00100000
    /// // 4 00100000
    /// // 3 00100000
    /// // 2 00100000
    /// // 1 00100000
    /// ```
    pub fn down_ray(from: Position) -> Self {
        Self::full()
            .include_ranks(from.rank().walk_down())
            .include_file(from.file())
    }

    /// Create a `Bitboard`, setting each bit at positions in a ray starting at `from` and going diagonally down and to the left to 1.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::down_left_ray(C5);
    /// assert_eq!(bb, Bitboard::with_ones([C5, B4, A3]));
    /// //   ABCDEFGH
    /// // 8 00000000
    /// // 7 00000000
    /// // 6 00000000
    /// // 5 00100000
    /// // 4 01000000
    /// // 3 10000000
    /// // 2 00000000
    /// // 1 00000000
    /// ```
    pub fn down_left_ray(from: Position) -> Self {
        Self::full().include_positions(
            from.file()
                .walk_left()
                .zip(from.rank().walk_down())
                .map(|(file, rank)| Position::new(file, rank)),
        )
    }

    /// Create a `Bitboard`, setting each bit at positions in a ray starting at `from` and going straight left to 1.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::left_ray(C5);
    /// assert_eq!(bb, Bitboard::with_ones([C5, B5, A5]));
    /// //   ABCDEFGH
    /// // 8 00000000
    /// // 7 00000000
    /// // 6 00000000
    /// // 5 11100000
    /// // 4 00000000
    /// // 3 00000000
    /// // 2 00000000
    /// // 1 00000000
    /// ```
    pub fn left_ray(from: Position) -> Self {
        Self::full()
            .include_files(from.file().walk_left())
            .include_rank(from.rank())
    }

    /// Create a `Bitboard`, setting each bit at positions in a ray starting at `from` and going diagonally up and to the left to 1.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let bb = Bitboard::up_left_ray(C5);
    /// assert_eq!(bb, Bitboard::with_ones([C5, B6, A7]));
    /// //   ABCDEFGH
    /// // 8 00000000
    /// // 7 10000000
    /// // 6 01000000
    /// // 5 00100000
    /// // 4 00000000
    /// // 3 00000000
    /// // 2 00000000
    /// // 1 00000000
    /// ```
    pub fn up_left_ray(from: Position) -> Self {
        Self::full().include_positions(
            from.file()
                .walk_left()
                .zip(from.rank().walk_up())
                .map(|(file, rank)| Position::new(file, rank)),
        )
    }

    /// Returns a `Bitboard` describing valid target squares for a white pawn on `pos`.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let white = Bitboard::with_ones([]); // white has no pieces on the board
    /// let black = Bitboard::with_ones([C5]); // black has a piece at C5
    /// // a white pawn at D4 can either move to C5 (taking blacks piece), or move to D5
    /// assert_eq!(Bitboard::white_pawn_targets(D4, white, black), Bitboard::with_ones([C5, D5]));
    /// ```
    pub fn white_pawn_targets(pos: Position, white_occupancy: Self, black_occupancy: Self) -> Self {
        let full_occupancy = black_occupancy | white_occupancy;
        let pos_bb = Self::with_one(pos);
        let mut targets = (pos_bb << 8) & !full_occupancy;
        if targets != 0 && pos.rank() == Rank::Two {
            targets |= (pos_bb << 16) & !full_occupancy;
        }

        let up_left = (pos_bb << 7) & black_occupancy;
        let up_right = (pos_bb << 9) & black_occupancy;
        targets | up_left | up_right
    }

    /// Returns a `Bitboard` describing valid target squares for a black pawn on `pos`.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let white = Bitboard::with_ones([C3]); // White has a piece at C3
    /// let black = Bitboard::with_ones([D3]); // Black has a piece at D3
    /// // A black pawn at D4 can move to C3 (taking whites piece). Note that it can't move to D3, since black already has a piece there.
    /// assert_eq!(Bitboard::black_pawn_targets(D4, white, black), Bitboard::with_ones([C3]));
    /// ```
    pub fn black_pawn_targets(pos: Position, white_occupancy: Self, black_occupancy: Self) -> Self {
        let full_occupancy = white_occupancy | black_occupancy;
        let pos_bb = Self::with_one(pos);
        let mut targets = (pos_bb >> 8) & !full_occupancy;
        if targets != 0 && pos.rank() == Rank::Seven {
            targets |= (pos_bb >> 16) & !full_occupancy;
        }

        let down_left = (pos_bb >> 9) & white_occupancy;
        let down_right = (pos_bb >> 7) & white_occupancy;
        targets | down_left | down_right
    }

    /// Returns a `Bitboard` describing valid target squares for a white king on `pos`.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let white_pieces = Bitboard::with_ones([B4]);
    /// // A white king can move to any square one Manhattan distance step away.
    /// // There is a white piece on B4, which blocks the white king.
    /// assert_eq!(Bitboard::white_king_targets(C4, white_pieces), Bitboard::with_ones([C5, D5, D4, D3, C3, B3, B5]));
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00000000    6 00000000
    /// // 5 00000000 -> 5 01110000
    /// // 4 01x00000    4 00010000
    /// // 3 00000000    3 01110000
    /// // 2 00000000    2 00000000
    /// // 1 00000000    1 00000000
    /// ```
    pub fn white_king_targets(from: Position, white_occupancy: Self) -> Self {
        Self::king_targets(from, white_occupancy)
    }

    /// Returns a `Bitboard` describing valid target squares for a black king on `pos`.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let black_pieces = Bitboard::with_ones([B4]);
    /// // A black king can move to any square one Manhattan distance step away.
    /// // There is a black piece on B4, which blocks the black king.
    /// assert_eq!(Bitboard::black_king_targets(C4, black_pieces), Bitboard::with_ones([C5, D5, D4, D3, C3, B3, B5]));
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00000000    6 00000000
    /// // 5 00000000 -> 5 01110000
    /// // 4 01x00000    4 00010000
    /// // 3 00000000    3 01110000
    /// // 2 00000000    2 00000000
    /// // 1 00000000    1 00000000
    /// ```
    pub fn black_king_targets(from: Position, black_occupancy: Self) -> Self {
        Self::king_targets(from, black_occupancy)
    }

    fn king_targets(from: Position, self_occupancy: Self) -> Self {
        let king_bb = Bitboard::with_one(from);
        let up = king_bb << 8;
        let up_right = (king_bb << 9).clear_file(File::A);
        let right = (king_bb << 1).clear_file(File::A);
        let down_right = (king_bb >> 7).clear_file(File::A);
        let down = king_bb >> 8;
        let down_left = (king_bb >> 9).clear_file(File::H);
        let left = king_bb >> 1;
        let up_left = (king_bb << 7).clear_file(File::H);

        (up | up_right | right | down_right | down | down_left | left | up_left) & !self_occupancy
    }

    /// Returns a `Bitboard` describing valid target squares for a knight on `pos`.
    ///
    /// ## Notes
    /// `self_occupancy` should be a `Bitboard` representing the pieces with the same color as the given knight.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let white_pieces = Bitboard::with_ones([A6, C6]);
    /// // a white knight on B4 can't move to A6 or C6 if there are white pieces there
    /// assert_eq!(Bitboard::knight_targets(B4, white_pieces), Bitboard::with_ones([D5, D3, C2, A2]));
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00000000    6 10100000
    /// // 5 00000000 -> 5 00010000
    /// // 4 01000000    4 00000000
    /// // 3 00000000    3 00010000
    /// // 2 00000000    2 10100000
    /// // 1 00000000    1 00000000
    /// ```
    pub fn knight_targets(pos: Position, self_occupancy: Self) -> Self {
        let knight_bb = Bitboard::with_one(pos);
        let up_2_right_1 = (knight_bb << 17).clear_file(File::A);
        let right_2_up_1 = (knight_bb << 10).clear_files([File::A, File::B]);
        let right_2_down_1 = (knight_bb >> 6).clear_files([File::A, File::B]);
        let down_2_right_1 = (knight_bb >> 15).clear_file(File::A);
        let down_2_left_1 = (knight_bb >> 17).clear_file(File::H);
        let left_2_down_1 = (knight_bb >> 10).clear_files([File::G, File::H]);
        let left_2_up_1 = (knight_bb << 6).clear_files([File::G, File::H]);
        let up_2_left_1 = (knight_bb << 15).clear_file(File::H);

        (up_2_right_1
            | right_2_up_1
            | right_2_down_1
            | down_2_right_1
            | down_2_left_1
            | left_2_down_1
            | left_2_up_1
            | up_2_left_1)
            & !self_occupancy
    }

    /// Returns a `Bitboard` describing valid target squares for a white bishop on `pos`.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let white_pieces = Bitboard::with_one(E1);
    /// let black_pieces = Bitboard::with_one(D6);
    /// // a white bishop on B4 can move to any empty square on its diagonals,
    /// // there is a white piece on E1, which blocks the white bishop
    /// // there is a black piece on D6, which the white bishop can take, but not move through
    /// assert_eq!(Bitboard::white_bishop_targets(B4, white_pieces, black_pieces), Bitboard::with_ones([A3, A5, C5, D6, C3, D2]));
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00010000    6 00010000
    /// // 5 00000000 -> 5 10100000
    /// // 4 0x000000    4 0x000000
    /// // 3 00000000    3 10100000
    /// // 2 00000000    2 00010000
    /// // 1 00000000    1 00000000
    /// ```
    pub fn white_bishop_targets(
        from: Position,
        white_occupancy: Self,
        black_occupancy: Self,
    ) -> Self {
        Self::bishop_targets(from, white_occupancy, black_occupancy)
    }

    /// Returns a `Bitboard` describing valid target squares for a black bishop on `pos`.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let white_pieces = Bitboard::with_one(D6);
    /// let black_pieces = Bitboard::with_one(E1);
    /// // A black bishop on B4 can move to any empty square on its diagonals.
    /// // There is a black piece on E1, which blocks the black bishop.
    /// // There is a white piece on D6, which the black bishop can take, but not move through.
    /// assert_eq!(Bitboard::black_bishop_targets(B4, white_pieces, black_pieces), Bitboard::with_ones([A3, A5, C5, D6, C3, D2]));
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00010000    6 00010000
    /// // 5 00000000 -> 5 10100000
    /// // 4 0x000000    4 0x000000
    /// // 3 00000000    3 10100000
    /// // 2 00000000    2 00010000
    /// // 1 00000000    1 00000000
    /// ```
    pub fn black_bishop_targets(
        from: Position,
        white_occupancy: Self,
        black_occupancy: Self,
    ) -> Self {
        Self::bishop_targets(from, black_occupancy, white_occupancy)
    }

    fn bishop_targets(from: Position, self_occupancy: Self, other_occupancy: Self) -> Self {
        let full_occupancy = self_occupancy | other_occupancy;
        (Self::line_targets(from, full_occupancy, LineAttack::Slash)
            | Self::line_targets(from, full_occupancy, LineAttack::Backslash))
        .clear_position(from)
            & !self_occupancy
    }

    /// Returns a `Bitboard` describing valid target squares for a white rook on `pos`.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let white_pieces = Bitboard::with_one(G4);
    /// let black_pieces = Bitboard::with_one(D6);
    /// // A white rook can move to any empty squares on its cardinals.
    /// // There is a white piece on G4, which blocks the white rook.
    /// // There is a black piece on D6, which the white rook can take, but not move through.
    /// assert_eq!(Bitboard::white_rook_targets(D4, white_pieces, black_pieces), Bitboard::with_ones([A4, B4, C4, D5, D6, D3, D2, D1, E4, F4]));
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00010000    6 00010000
    /// // 5 00000000 -> 5 00010000
    /// // 4 000x0000    4 11101100
    /// // 3 00000000    3 00010000
    /// // 2 00000000    2 00010000
    /// // 1 00000000    1 00010000
    /// ```
    pub fn white_rook_targets(
        from: Position,
        white_occupancy: Self,
        black_occupancy: Self,
    ) -> Self {
        Self::rook_targets(from, white_occupancy, black_occupancy)
    }

    /// Returns a `Bitboard` describing valid target squares for a black rook on `pos`.
    ///
    /// ## Example
    /// ```rust
    /// # use bitboard::prelude::*;
    /// let white_pieces = Bitboard::with_one(D6);
    /// let black_pieces = Bitboard::with_one(G4);
    /// // A black rook can move to any empty squares on its cardinals.
    /// // There is a white piece on D6, which the black rook can take, but not move through.
    /// // There is a black piece on G4, which blocks the black rook.
    /// assert_eq!(Bitboard::black_rook_targets(D4, white_pieces, black_pieces), Bitboard::with_ones([A4, B4, C4, D5, D6, D3, D2, D1, E4, F4]));
    /// //   ABCDEFGH      ABCDEFGH
    /// // 8 00000000    8 00000000
    /// // 7 00000000    7 00000000
    /// // 6 00010000    6 00010000
    /// // 5 00000000 -> 5 00010000
    /// // 4 000x0000    4 11101100
    /// // 3 00000000    3 00010000
    /// // 2 00000000    2 00010000
    /// // 1 00000000    1 00010000
    /// ```
    pub fn black_rook_targets(
        from: Position,
        white_occupancy: Self,
        black_occupancy: Self,
    ) -> Self {
        Self::rook_targets(from, black_occupancy, white_occupancy)
    }

    fn rook_targets(from: Position, self_occupancy: Self, other_occupancy: Self) -> Self {
        let full_occupancy = self_occupancy | other_occupancy;
        (Self::line_targets(from, full_occupancy, LineAttack::File)
            | Self::line_targets(from, full_occupancy, LineAttack::Rank))
        .clear_position(from)
            & !self_occupancy
    }

    pub fn white_queen_targets(
        from: Position,
        white_occupancy: Self,
        black_occupancy: Self,
    ) -> Self {
        Self::queen_targets(from, white_occupancy, black_occupancy)
    }

    pub fn black_queen_targets(
        from: Position,
        white_occupancy: Self,
        black_occupancy: Self,
    ) -> Self {
        Self::queen_targets(from, black_occupancy, white_occupancy)
    }

    fn queen_targets(from: Position, self_occupancy: Self, other_occupancy: Self) -> Self {
        (Self::bishop_targets(from, self_occupancy, other_occupancy)
            | Self::rook_targets(from, self_occupancy, other_occupancy))
        .clear_position(from)
    }

    pub fn first_position(self) -> Option<Position> {
        let zeros = self.0.trailing_zeros();
        if zeros == 64 {
            None
        } else {
            Some(INCREASING_A1_B1[zeros as usize])
        }
    }

    pub fn positions(self) -> Vec<Position> {
        let mut positions = Vec::with_capacity(64);

        for shift in 0..64 {
            let bit_at = self & (1 << shift);
            if bit_at != 0 {
                let file = File::try_from(shift % 8).unwrap();
                let rank = Rank::try_from(shift / 8).unwrap();
                positions.push(Position::new(file, rank));
            }
        }
        positions
    }

    fn line_targets(from: Position, occupancy: Self, line: LineAttack) -> Self {
        let from_bb = Bitboard::with_one(from);
        let occupancy_without_self = occupancy & !Bitboard::with_one(from);
        match line {
            LineAttack::Rank => {
                let left_ray = Self::left_ray(from);
                let negative = left_ray & occupancy_without_self;
                let negative_ms1b = negative.ms1b();
                let left = if negative_ms1b == 0 {
                    left_ray.ls1b()
                } else {
                    negative_ms1b
                };

                let right_ray = Self::right_ray(from);
                let positive = right_ray & occupancy_without_self;
                let positive_ls1b = positive.ls1b();
                let right = if positive_ls1b == 0 {
                    right_ray.ms1b()
                } else {
                    positive_ls1b
                };

                let ends = left | right;

                let bb = ((Self::full() >> ends.data().leading_zeros())
                    & (Self::full() << ends.data().trailing_zeros()));
                if bb == 0 {
                    Self::full().include_rank(from.rank())
                } else {
                    bb.include_rank(from.rank())
                }
            }
            LineAttack::File => {
                let down_ray = Self::down_ray(from);
                let negative = down_ray & occupancy_without_self;
                let negative_ms1b = negative.ms1b();
                let left = if negative_ms1b == 0 {
                    down_ray.ls1b()
                } else {
                    negative_ms1b
                };

                let up_ray = Self::up_ray(from);
                let positive = up_ray & occupancy_without_self;
                let positive_ls1b = positive.ls1b();
                let right = if positive_ls1b == 0 {
                    up_ray.ms1b()
                } else {
                    positive_ls1b
                };

                let ends = left | right;

                let bb = ((Self::full() >> ends.data().leading_zeros())
                    & (Self::full() << ends.data().trailing_zeros()));
                if bb == 0 {
                    Self::full().include_file(from.file())
                } else {
                    bb.include_file(from.file())
                }
            }
            LineAttack::Slash => {
                let up_right_ray = Self::up_right_ray(from);
                let positive = up_right_ray & occupancy_without_self;
                let positive_ls1b = positive.ls1b();
                let left = if positive_ls1b == 0 {
                    up_right_ray.ms1b()
                } else {
                    positive_ls1b
                };

                let down_left_ray = Self::down_left_ray(from);
                let negative = down_left_ray & occupancy_without_self;
                let negative_ms1b = negative.ms1b();
                let right = if negative_ms1b == 0 {
                    down_left_ray.ls1b()
                } else {
                    negative_ms1b
                };

                let ends = left | right;

                let leading_zeros =
                    std::cmp::min(ends.data().leading_zeros(), from_bb.data().leading_zeros());
                let trailing_zeros = std::cmp::min(
                    ends.data().trailing_zeros(),
                    from_bb.data().trailing_zeros(),
                );
                let bb = ((Self::full() >> leading_zeros) & (Self::full() << trailing_zeros));
                if bb == 0 {
                    Self::full() & (down_left_ray | up_right_ray)
                } else {
                    bb & (down_left_ray | up_right_ray)
                }
            }
            LineAttack::Backslash => {
                let down_right_ray = Self::down_right_ray(from);
                let negative = down_right_ray & occupancy_without_self;
                let negative_ms1b = negative.ms1b();
                let left = if negative_ms1b == 0 {
                    down_right_ray.ls1b()
                } else {
                    negative_ms1b
                };

                let up_left_ray = Self::up_left_ray(from);
                let positive = up_left_ray & occupancy_without_self;
                let positive_ls1b = positive.ls1b();
                let right = if positive_ls1b == 0 {
                    up_left_ray.ms1b()
                } else {
                    positive_ls1b
                };

                let ends = left | right;

                let leading_zeros =
                    std::cmp::min(ends.data().leading_zeros(), from_bb.data().leading_zeros());
                let trailing_zeros = std::cmp::min(
                    ends.data().trailing_zeros(),
                    from_bb.data().trailing_zeros(),
                );
                let bb = ((Self::full() >> leading_zeros) & (Self::full() << trailing_zeros));
                if bb == 0 {
                    Self::full() & (down_right_ray | up_left_ray)
                } else {
                    bb & (down_right_ray | up_left_ray)
                }
            }
        }
    }

    fn data(&self) -> u64 {
        self.0
    }

    fn ls1b(&self) -> Self {
        let shifts = self.data().trailing_zeros();
        if shifts == 64 {
            Self::empty()
        } else {
            Self::construct(1_u64 << shifts)
        }
    }

    fn ms1b(&self) -> Self {
        let shifts = self.data().leading_zeros();
        if shifts == 64 {
            Self::empty()
        } else {
            Self::construct(1 << (63 - shifts))
        }
    }
}

impl Not for Bitboard {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self::construct(!self.data())
    }
}

impl BitOr for Bitboard {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self::construct(self.0.bitor(rhs.0))
    }
}

impl BitOr<u64> for Bitboard {
    type Output = Self;

    fn bitor(self, rhs: u64) -> Self::Output {
        Self::construct(self.data().bitor(rhs))
    }
}

impl BitOr<Position> for Bitboard {
    type Output = Self;

    fn bitor(self, rhs: Position) -> Self::Output {
        self.bitor(Self::with_one(rhs))
    }
}

impl BitOrAssign for Bitboard {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = self.bitor(rhs);
    }
}

impl BitOrAssign<u64> for Bitboard {
    fn bitor_assign(&mut self, rhs: u64) {
        *self = self.bitor(rhs)
    }
}

impl BitOrAssign<Position> for Bitboard {
    fn bitor_assign(&mut self, rhs: Position) {
        *self = self.bitor(rhs)
    }
}

impl BitAnd for Bitboard {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self::construct(self.0.bitand(rhs.0))
    }
}

impl BitAnd<u64> for Bitboard {
    type Output = Self;

    fn bitand(self, rhs: u64) -> Self::Output {
        Self::construct(self.data().bitand(rhs))
    }
}

impl BitAnd<Position> for Bitboard {
    type Output = Self;

    fn bitand(self, rhs: Position) -> Self::Output {
        self.bitand(Self::with_one(rhs))
    }
}

impl BitAndAssign for Bitboard {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = self.bitand(rhs)
    }
}

impl BitAndAssign<u64> for Bitboard {
    fn bitand_assign(&mut self, rhs: u64) {
        *self = self.bitand(rhs);
    }
}

impl BitAndAssign<Position> for Bitboard {
    fn bitand_assign(&mut self, rhs: Position) {
        *self = self.bitand(rhs)
    }
}

impl BitXor for Bitboard {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Self::construct(self.0.bitxor(rhs.0))
    }
}

impl BitXor<u64> for Bitboard {
    type Output = Self;

    fn bitxor(self, rhs: u64) -> Self::Output {
        Self::construct(self.data().bitxor(rhs))
    }
}

impl BitXor<Position> for Bitboard {
    type Output = Self;

    fn bitxor(self, rhs: Position) -> Self::Output {
        self.bitxor(Self::with_one(rhs))
    }
}

impl BitXorAssign for Bitboard {
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = self.bitxor(rhs)
    }
}

impl BitXorAssign<u64> for Bitboard {
    fn bitxor_assign(&mut self, rhs: u64) {
        *self = self.bitxor(rhs)
    }
}

impl BitXorAssign<Position> for Bitboard {
    fn bitxor_assign(&mut self, rhs: Position) {
        *self = self.bitxor(rhs)
    }
}

impl Add for Bitboard {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::construct(self.data() + rhs.data())
    }
}

impl Sub for Bitboard {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::construct(self.data() - rhs.data())
    }
}

impl PartialEq<u64> for Bitboard {
    fn eq(&self, other: &u64) -> bool {
        self.0 == *other
    }
}

impl PartialOrd<u64> for Bitboard {
    fn partial_cmp(&self, other: &u64) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(other)
    }
}

impl Debug for Bitboard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

impl Display for Bitboard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let chars: Vec<char> = format!("{:064b}", self.data()).chars().collect();

        let mut output = vec![String::from("  ABCDEFGH")];
        for (idx, chunk) in chars.chunks(8).enumerate() {
            let mut line = String::with_capacity(10);
            line.push_str(&format!("{} ", 8 - idx));
            line.push_str(&chunk.iter().rev().collect::<String>());
            output.push(line);
        }

        write!(f, "bitboard:\n{}", output.join("\n"))
    }
}

impl Shl<u32> for Bitboard {
    type Output = Self;

    fn shl(self, rhs: u32) -> Self {
        Self(self.data().checked_shl(rhs).unwrap_or(0))
    }
}

impl Shr<u32> for Bitboard {
    type Output = Self;

    fn shr(self, rhs: u32) -> Self {
        Self(self.data().checked_shr(rhs).unwrap_or(0))
    }
}

enum LineAttack {
    /// A vertical line
    /// ```ignore
    /// // |
    /// // |
    /// // |
    /// ```
    Rank,
    /// A horizontal line
    /// ```ignore
    /// // ---
    /// ```
    File,
    /// A diagonal line going up-right
    /// ```ignore
    /// //   /
    /// //  /
    /// // /
    /// ```
    Slash,
    /// A diagonal line going down-right
    /// ```ignore
    /// // \
    /// //  \
    /// //   \
    /// ```
    Backslash,
}

pub(crate) fn bitboard_index_of(pos: Position) -> usize {
    // A1 is index 0, H8 is index 63
    usize::from(8 * u8::from(pos.rank()) + u8::from(pos.file()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shl_test() {
        let shifted = INITIAL_STATE << 16;
        assert_eq!(
            shifted,
            Bitboard::construct(0b0000000000000000000000000000000011111111111111110000000000000000)
        );
    }

    #[test]
    fn shr_test() {
        let shifted = INITIAL_STATE >> 16;
        assert_eq!(
            shifted,
            Bitboard::construct(0b0000000000000000111111111111111100000000000000000000000000000000)
        );
    }

    #[test]
    fn bitor_test() {
        let a4 = Bitboard::with_one(A4);
        let b3 = Bitboard::with_one(B3);
        let a4_or_b3 = a4 | b3;

        assert_eq!(a4_or_b3, Bitboard::with_ones([A4, B3]));

        let mut empty = Bitboard::empty();
        empty |= a4;
        assert_eq!(empty, Bitboard::with_one(A4));
    }

    #[test]
    fn bitand_test() {
        let a4 = Bitboard::with_one(A4);
        let a4_and_g6 = Bitboard::with_ones([A4, G6]);
        let only_a4 = a4 & a4_and_g6;

        assert_eq!(only_a4, a4);

        let mut full = Bitboard::full();
        full &= a4;
        assert_eq!(full, Bitboard::with_one(A4));
    }

    #[test]
    fn bitxor_test() {
        let a4_and_b3 = Bitboard::with_ones([A4, B3]);
        let a4 = Bitboard::with_one(A4);
        let only_b3 = a4 ^ a4_and_b3;

        assert_eq!(only_b3, Bitboard::with_one(B3));
    }

    #[test]
    fn mask_file_test() {
        let file_b_only = INITIAL_STATE.include_file(File::B);
        assert_eq!(
            file_b_only,
            Bitboard::construct(0b0000001000000010000000000000000000000000000000000000001000000010)
        );
    }

    #[test]
    fn include_files_test() {
        let file_b_and_e = INITIAL_STATE.include_files([File::B, File::E]);
        assert_eq!(
            file_b_and_e,
            Bitboard::construct(0b0001001000010010000000000000000000000000000000000001001000010010)
        );

        assert_eq!(Bitboard::empty(), Bitboard::full().include_files([]));

        assert_eq!(Bitboard::full(), Bitboard::full().include_files(ALL_FILES));
    }

    #[test]
    fn clear_file_test() {
        let no_file_b = INITIAL_STATE.clear_file(File::B);
        assert_eq!(
            no_file_b,
            Bitboard::construct(0b1111110111111101000000000000000000000000000000001111110111111101)
        );
    }

    #[test]
    fn clear_files_test() {
        let no_file_b_or_e = INITIAL_STATE.clear_files([File::B, File::E]);
        assert_eq!(
            no_file_b_or_e,
            Bitboard::construct(0b1110110111101101000000000000000000000000000000001110110111101101)
        );
    }

    #[test]
    fn mask_rank_test() {
        let rank_2_only = INITIAL_STATE.include_rank(Rank::Two);
        assert_eq!(
            rank_2_only,
            Bitboard::construct(0b0000000000000000000000000000000000000000000000001111111100000000)
        );
    }

    #[test]
    fn mask_ranks_test() {
        let rank_2_and_8 = INITIAL_STATE.include_ranks([Rank::Two, Rank::Eight]);
        assert_eq!(
            rank_2_and_8,
            Bitboard::construct(0b1111111100000000000000000000000000000000000000001111111100000000)
        );

        assert_eq!(Bitboard::empty(), Bitboard::full().include_ranks([]));

        assert_eq!(Bitboard::full(), Bitboard::full().include_ranks(ALL_RANKS));
    }

    #[test]
    fn clear_rank_test() {
        let no_rank_2 = INITIAL_STATE.clear_rank(Rank::Two);
        assert_eq!(
            no_rank_2,
            Bitboard::construct(0b1111111111111111000000000000000000000000000000000000000011111111)
        );
    }

    #[test]
    fn clear_ranks_test() {
        let no_rank_2_or_7 = INITIAL_STATE.clear_ranks([Rank::Two, Rank::Seven]);
        assert_eq!(
            no_rank_2_or_7,
            Bitboard::construct(0b1111111100000000000000000000000000000000000000000000000011111111)
        );

        assert_eq!(Bitboard::empty(), Bitboard::full().clear_files(ALL_FILES));
    }

    #[test]
    fn king_targets_test() {
        let targets_of_e2 = Bitboard::king_targets(E2, Bitboard::empty());
        assert_eq!(
            targets_of_e2,
            Bitboard::full().include_positions([E3, F3, F2, F1, E1, D1, D2, D3])
        );

        let targets_of_g8 = Bitboard::king_targets(G8, Bitboard::empty());
        assert_eq!(
            targets_of_g8,
            Bitboard::full().include_positions([H8, H7, G7, F7, F8])
        );
    }

    #[test]
    fn knight_targets_test() {
        let targets_of_e5 = Bitboard::knight_targets(E5, Bitboard::empty());
        assert_eq!(
            targets_of_e5,
            Bitboard::full().include_positions([F7, G6, G4, F3, D3, C4, C6, D7])
        );

        let targets_of_a1 = Bitboard::knight_targets(A1, Bitboard::empty());
        assert_eq!(targets_of_a1, Bitboard::full().include_positions([B3, C2]));
    }

    #[test]
    fn up_ray_test() {
        assert_eq!(Bitboard::up_ray(C5), Bitboard::with_ones([C5, C6, C7, C8]));
    }

    #[test]
    fn up_right_ray_test() {
        assert_eq!(
            Bitboard::up_right_ray(E4),
            Bitboard::with_ones([E4, F5, G6, H7])
        );

        assert_eq!(Bitboard::up_right_ray(H7), Bitboard::with_ones([H7]));
        assert_eq!(
            Bitboard::up_right_ray(A4),
            Bitboard::with_ones([A4, B5, C6, D7, E8])
        );
    }

    #[test]
    fn right_ray_test() {
        assert_eq!(
            Bitboard::right_ray(A7),
            Bitboard::with_ones([A7, B7, C7, D7, E7, F7, G7, H7])
        );
    }

    #[test]
    fn down_right_ray_test() {
        assert_eq!(
            Bitboard::down_right_ray(C6),
            Bitboard::with_ones([C6, D5, E4, F3, G2, H1])
        );
    }

    #[test]
    fn down_ray_test() {
        assert_eq!(
            Bitboard::down_ray(G5),
            Bitboard::with_ones([G5, G4, G3, G2, G1])
        );
    }

    #[test]
    fn down_left_ray_test() {
        assert_eq!(
            Bitboard::down_left_ray(C7),
            Bitboard::with_ones([C7, B6, A5])
        );
        assert_eq!(Bitboard::down_left_ray(A4), Bitboard::with_ones([A4]));
    }

    #[test]
    fn left_ray_test() {
        assert_eq!(Bitboard::left_ray(A7), Bitboard::with_ones([A7]));

        assert_eq!(
            Bitboard::left_ray(E4),
            Bitboard::with_ones([E4, D4, C4, B4, A4])
        );
    }

    #[test]
    fn up_left_ray_test() {
        assert_eq!(
            Bitboard::up_left_ray(H1),
            Bitboard::with_ones([H1, G2, F3, E4, D5, C6, B7, A8])
        );
    }

    #[test]
    fn ls1b_test() {
        assert_eq!(
            Bitboard::with_ones([C3, D3, E3]).ls1b(),
            Bitboard::with_one(C3)
        );
    }

    #[test]
    fn ms1b_test() {
        assert_eq!(
            Bitboard::with_ones([C3, D3, E3]).ms1b(),
            Bitboard::with_one(E3)
        );
    }

    #[test]
    fn white_pawn_targets_test() {
        assert_eq!(
            Bitboard::white_pawn_targets(
                E2,
                Bitboard::full().include_ranks([Rank::Seven, Rank::Eight]),
                Bitboard::full().include_ranks([Rank::One, Rank::Two])
            ),
            Bitboard::with_ones([E3, E4])
        );

        assert_eq!(
            Bitboard::white_pawn_targets(
                E6,
                Bitboard::empty(),
                Bitboard::full().include_ranks([Rank::Seven, Rank::Eight]),
            ),
            Bitboard::with_ones([D7, F7])
        );
    }

    #[test]
    fn black_pawn_targets_test() {
        assert_eq!(
            Bitboard::black_pawn_targets(
                A7,
                INITIAL_STATE.include_ranks([Rank::One, Rank::Two]),
                INITIAL_STATE.include_ranks([Rank::Seven, Rank::Eight])
            ),
            Bitboard::with_ones([A6, A5])
        );

        assert_eq!(
            Bitboard::black_pawn_targets(
                H6,
                INITIAL_STATE.include_ranks([Rank::One, Rank::Two]),
                Bitboard::empty()
            ),
            Bitboard::with_ones([H5])
        );

        assert_eq!(
            Bitboard::black_pawn_targets(
                H3,
                INITIAL_STATE.include_ranks([Rank::One, Rank::Two]),
                Bitboard::empty()
            ),
            Bitboard::with_ones([G2])
        );
    }

    #[test]
    fn bishop_targets_test() {
        assert_eq!(
            Bitboard::white_bishop_targets(E4, INITIAL_WHITE_OCCUPANCY, INITIAL_BLACK_OCCUPANCY),
            Bitboard::with_ones([B7, H7, C6, G6, D5, F5, D3, F3,])
        );

        assert_eq!(
            Bitboard::black_bishop_targets(F4, INITIAL_WHITE_OCCUPANCY, INITIAL_BLACK_OCCUPANCY),
            Bitboard::with_ones([D2, H2, E3, G3, E5, G5, D6, H6,])
        );

        let pawn_moved = Bitboard::with_ones([
            A1, B1, C1, D1, E1, F1, G1, H1, A2, B2, C2, D2, E3, F2, G2, H2,
        ]);
        assert_eq!(
            Bitboard::white_bishop_targets(D1, pawn_moved, Bitboard::empty()),
            Bitboard::with_ones([E2, F3, G4, H5])
        );

        let pawn_moved = Bitboard::with_ones([
            A1, B1, C1, D1, E1, F1, G1, H1, A2, B2, C2, D2, E3, F2, G2, H2,
        ]);
        assert_eq!(
            Bitboard::black_bishop_targets(F1, pawn_moved, Bitboard::empty()),
            Bitboard::with_ones([G2, E2, D3, C4, B5, A6])
        );
    }

    #[test]
    fn rook_targets_test() {
        assert_eq!(
            Bitboard::white_rook_targets(H1, Bitboard::with_ones([H3]), Bitboard::with_ones([F1])),
            Bitboard::with_ones([F1, G1, H2])
        );
    }

    #[test]
    fn queen_targets_test() {
        assert_eq!(
            Bitboard::white_queen_targets(E4, INITIAL_WHITE_OCCUPANCY, INITIAL_BLACK_OCCUPANCY),
            Bitboard::with_ones([
                D3, E3, F3, A4, B4, C4, D4, F4, G4, H4, D5, E5, F5, C6, E6, G6, B7, E7, H7
            ])
        );
        assert_eq!(
            Bitboard::white_queen_targets(A4, INITIAL_WHITE_OCCUPANCY, INITIAL_BLACK_OCCUPANCY),
            Bitboard::with_ones([A7, D7, A6, C6, A5, B5, B4, C4, D4, E4, F4, G4, H4, A3, B3,])
        );

        let pawn_moved = Bitboard::with_ones([
            A1, B1, C1, D1, E1, F1, G1, H1, A2, B2, C2, D2, E3, F2, G2, H2,
        ]);
        assert_eq!(
            Bitboard::white_queen_targets(F1, pawn_moved, Bitboard::empty()),
            Bitboard::with_ones([A6, B5, C4, D3, E2])
        );
    }

    #[test]
    fn positions_test() {
        assert_eq!(
            INITIAL_STATE.positions(),
            vec![
                A1, B1, C1, D1, E1, F1, G1, H1, A2, B2, C2, D2, E2, F2, G2, H2, A7, B7, C7, D7, E7,
                F7, G7, H7, A8, B8, C8, D8, E8, F8, G8, H8
            ]
        );
    }

    #[test]
    fn first_position_test() {
        assert_eq!(INITIAL_STATE.first_position().unwrap(), A1);
        assert_eq!(Bitboard::with_one(A1).first_position().unwrap(), A1);
        assert_eq!(Bitboard::with_one(E4).first_position().unwrap(), E4);
        assert_eq!(Bitboard::with_one(E1).first_position().unwrap(), E1);
    }
}
