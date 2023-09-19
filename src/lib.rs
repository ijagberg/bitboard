#![allow(unused)]

pub use crate::consts::*;
use crate::position::consts::*;
use crate::position::*;
use std::{
    fmt::{Debug, Display},
    ops::{
        Add, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not, Shl, Shr, Sub,
    },
};

pub mod consts;
pub mod position;
pub mod prelude;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Bitboard(u64);

impl Bitboard {
    const fn construct(v: u64) -> Self {
        Self(v)
    }

    pub fn new(v: u64) -> Self {
        Self::construct(v)
    }

    pub fn full() -> Self {
        !Self::empty()
    }

    pub fn empty() -> Self {
        Self::construct(0)
    }

    pub fn with_one(pos: Position) -> Self {
        Self::construct(1 << bitboard_index_of(pos))
    }

    pub fn with_ones(positions: impl IntoIterator<Item = Position>) -> Self {
        let mut s = Self(0);
        for pos in positions {
            s |= Self::with_one(pos);
        }

        s
    }

    pub fn bit_at(&self, pos: Position) -> bool {
        let idx = bitboard_index_of(pos);
        ((1 << idx) & self.0) > 0
    }

    pub fn mask_position(self, pos: Position) -> Self {
        self & Self::with_one(pos)
    }

    pub fn mask_positions(self, positions: impl IntoIterator<Item = Position>) -> Self {
        let mut include = Self::empty();
        for pos in positions {
            include |= Self::with_one(pos);
        }

        self & include
    }

    pub fn clear_position(self, pos: Position) -> Self {
        self & !Self::with_one(pos)
    }

    pub fn clear_positions(self, positions: impl IntoIterator<Item = Position>) -> Self {
        let mut clear = Self::full();
        for pos in positions {
            clear &= !Self::with_one(pos);
        }

        self & clear
    }

    fn mask_file_bitboard(file: File) -> Self {
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

    pub fn mask_file(self, file: File) -> Self {
        self & Self::mask_file_bitboard(file)
    }

    pub fn mask_files(self, files: impl IntoIterator<Item = File>) -> Self {
        let mut mask = Bitboard::empty();
        for file in files {
            mask |= Self::mask_file_bitboard(file)
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

    pub fn clear_file(self, file: File) -> Self {
        self & Self::clear_file_bitboard(file)
    }

    pub fn clear_files(self, files: impl IntoIterator<Item = File>) -> Self {
        let mut clear = Bitboard::full();
        for file in files {
            clear &= Self::clear_file_bitboard(file)
        }

        self & clear
    }

    fn mask_rank_bitboard(rank: Rank) -> Self {
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

    pub fn mask_rank(self, rank: Rank) -> Self {
        self & Self::mask_rank_bitboard(rank)
    }

    pub fn mask_ranks(self, ranks: impl IntoIterator<Item = Rank>) -> Self {
        let mut mask = Bitboard::empty();
        for rank in ranks {
            mask |= Self::mask_rank_bitboard(rank)
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

    pub fn clear_rank(self, rank: Rank) -> Self {
        self & Self::clear_rank_bitboard(rank)
    }

    pub fn clear_ranks(self, ranks: impl IntoIterator<Item = Rank>) -> Self {
        let mut clear = Bitboard::full();
        for rank in ranks {
            clear &= Self::clear_rank_bitboard(rank);
        }

        self & clear
    }

    pub fn up_ray(from: Position) -> Self {
        Self::full()
            .mask_ranks(from.rank().up_all())
            .mask_file(from.file())
    }

    pub fn up_right_ray(from: Position) -> Self {
        Self::full().mask_positions(
            from.file()
                .right_all()
                .zip(from.rank().up_all())
                .map(|(file, rank)| Position::new(file, rank)),
        )
    }

    pub fn right_ray(from: Position) -> Self {
        Self::full()
            .mask_files(from.file().right_all())
            .mask_rank(from.rank())
    }

    pub fn down_right_ray(from: Position) -> Self {
        Self::full().mask_positions(
            from.file()
                .right_all()
                .zip(from.rank().down_all())
                .map(|(file, rank)| Position::new(file, rank)),
        )
    }

    pub fn down_ray(from: Position) -> Self {
        Self::full()
            .mask_ranks(from.rank().down_all())
            .mask_file(from.file())
    }

    pub fn down_left_ray(from: Position) -> Self {
        Self::full().mask_positions(
            from.file()
                .left_all()
                .zip(from.rank().down_all())
                .map(|(file, rank)| Position::new(file, rank)),
        )
    }

    pub fn left_ray(from: Position) -> Self {
        Self::full()
            .mask_files(from.file().left_all())
            .mask_rank(from.rank())
    }

    pub fn up_left_ray(from: Position) -> Self {
        Self::full().mask_positions(
            from.file()
                .left_all()
                .zip(from.rank().up_all())
                .map(|(file, rank)| Position::new(file, rank)),
        )
    }

    pub fn white_pawn_targets(pos: Position, black_occupancy: Self, white_occupancy: Self) -> Self {
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

    pub fn king_targets(pos: Position) -> Self {
        let king_bb = Bitboard::with_one(pos);
        let up = king_bb << 8;
        let up_right = (king_bb << 9).clear_file(File::A);
        let right = (king_bb << 1).clear_file(File::A);
        let down_right = (king_bb >> 7).clear_file(File::A);
        let down = king_bb >> 8;
        let down_left = (king_bb >> 9).clear_file(File::H);
        let left = king_bb >> 1;
        let up_left = (king_bb << 7).clear_file(File::H);

        up | up_right | right | down_right | down | down_left | left | up_left
    }

    pub fn knight_targets(pos: Position) -> Self {
        let knight_bb = Bitboard::with_one(pos);
        let up_2_right_1 = (knight_bb << 17).clear_file(File::A);
        let right_2_up_1 = (knight_bb << 10).clear_files([File::A, File::B]);
        let right_2_down_1 = (knight_bb >> 6).clear_files([File::A, File::B]);
        let down_2_right_1 = (knight_bb >> 15).clear_file(File::A);
        let down_2_left_1 = (knight_bb >> 17).clear_file(File::H);
        let left_2_down_1 = (knight_bb >> 10).clear_files([File::G, File::H]);
        let left_2_up_1 = (knight_bb << 6).clear_files([File::G, File::H]);
        let up_2_left_1 = (knight_bb << 15).clear_file(File::H);

        up_2_right_1
            | right_2_up_1
            | right_2_down_1
            | down_2_right_1
            | down_2_left_1
            | left_2_down_1
            | left_2_up_1
            | up_2_left_1
    }

    pub fn bishop_targets(from: Position, occupancy: Self) -> Self {
        (Self::line_targets(from, occupancy, LineAttack::Slash)
            | Self::line_targets(from, occupancy, LineAttack::Backslash))
        .clear_position(from)
    }

    pub fn rook_targets(from: Position, occupancy: Self) -> Self {
        (Self::line_targets(from, occupancy, LineAttack::File)
            | Self::line_targets(from, occupancy, LineAttack::Rank))
        .clear_position(from)
    }

    pub fn queen_targets(from: Position, occupancy: Self) -> Self {
        (Self::bishop_targets(from, occupancy) | Self::rook_targets(from, occupancy))
            .clear_position(from)
    }

    pub fn first_position(self) -> Option<Position> {
        let zeros = self.0.trailing_zeros();
        if zeros == 64 {
            None
        } else {
            Some(position::consts::INCREASING_A1_B1[zeros as usize])
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
                    Self::full().mask_rank(from.rank())
                } else {
                    bb.mask_rank(from.rank())
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
                    Self::full().mask_file(from.file())
                } else {
                    bb.mask_file(from.file())
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
    Rank,
    File,
    Slash,
    Backslash,
}

pub(crate) fn bitboard_index_of(pos: Position) -> usize {
    // A1 is index 0, H8 is index 63
    usize::from(8 * u8::from(pos.rank()) + u8::from(pos.file()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use position::consts::*;

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
        let file_b_only = INITIAL_STATE.mask_file(File::B);
        assert_eq!(
            file_b_only,
            Bitboard::construct(0b0000001000000010000000000000000000000000000000000000001000000010)
        );
    }

    #[test]
    fn mask_files_test() {
        let file_b_and_e = INITIAL_STATE.mask_files([File::B, File::E]);
        assert_eq!(
            file_b_and_e,
            Bitboard::construct(0b0001001000010010000000000000000000000000000000000001001000010010)
        );

        assert_eq!(Bitboard::empty(), Bitboard::full().mask_files([]));

        assert_eq!(Bitboard::full(), Bitboard::full().mask_files(ALL_FILES));
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
        let rank_2_only = INITIAL_STATE.mask_rank(Rank::Two);
        assert_eq!(
            rank_2_only,
            Bitboard::construct(0b0000000000000000000000000000000000000000000000001111111100000000)
        );
    }

    #[test]
    fn mask_ranks_test() {
        let rank_2_and_8 = INITIAL_STATE.mask_ranks([Rank::Two, Rank::Eight]);
        assert_eq!(
            rank_2_and_8,
            Bitboard::construct(0b1111111100000000000000000000000000000000000000001111111100000000)
        );

        assert_eq!(Bitboard::empty(), Bitboard::full().mask_ranks([]));

        assert_eq!(Bitboard::full(), Bitboard::full().mask_ranks(ALL_RANKS));
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
        let targets_of_e2 = Bitboard::king_targets(E2);
        assert_eq!(
            targets_of_e2,
            Bitboard::full().mask_positions([E3, F3, F2, F1, E1, D1, D2, D3])
        );

        let targets_of_g8 = Bitboard::king_targets(G8);
        assert_eq!(
            targets_of_g8,
            Bitboard::full().mask_positions([H8, H7, G7, F7, F8])
        );
    }

    #[test]
    fn knight_targets_test() {
        let targets_of_e5 = Bitboard::knight_targets(E5);
        assert_eq!(
            targets_of_e5,
            Bitboard::full().mask_positions([F7, G6, G4, F3, D3, C4, C6, D7])
        );

        let targets_of_a1 = Bitboard::knight_targets(A1);
        assert_eq!(targets_of_a1, Bitboard::full().mask_positions([B3, C2]));
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
                Bitboard::full().mask_ranks([Rank::Seven, Rank::Eight]),
                Bitboard::full().mask_ranks([Rank::One, Rank::Two])
            ),
            Bitboard::with_ones([E3, E4])
        );

        assert_eq!(
            Bitboard::white_pawn_targets(
                E6,
                Bitboard::full().mask_ranks([Rank::Seven, Rank::Eight]),
                Bitboard::empty()
            ),
            Bitboard::with_ones([D7, F7])
        );
    }

    #[test]
    fn black_pawn_targets_test() {
        assert_eq!(
            Bitboard::black_pawn_targets(
                A7,
                INITIAL_STATE.mask_ranks([Rank::One, Rank::Two]),
                INITIAL_STATE.mask_ranks([Rank::Seven, Rank::Eight])
            ),
            Bitboard::with_ones([A6, A5])
        );

        assert_eq!(
            Bitboard::black_pawn_targets(
                H6,
                INITIAL_STATE.mask_ranks([Rank::One, Rank::Two]),
                Bitboard::empty()
            ),
            Bitboard::with_ones([H5])
        );

        assert_eq!(
            Bitboard::black_pawn_targets(
                H3,
                INITIAL_STATE.mask_ranks([Rank::One, Rank::Two]),
                Bitboard::empty()
            ),
            Bitboard::with_ones([G2])
        );
    }

    #[test]
    fn bishop_targets_test() {
        assert_eq!(
            Bitboard::bishop_targets(E4, INITIAL_STATE),
            Bitboard::with_ones([B7, H7, C6, G6, D5, F5, D3, F3, C2, G2])
        );

        assert_eq!(
            Bitboard::bishop_targets(F4, INITIAL_STATE),
            Bitboard::with_ones([D2, H2, E3, G3, E5, G5, D6, H6, C7])
        );

        let pawn_moved = Bitboard::with_ones([
            A1, B1, C1, D1, E1, F1, G1, H1, A2, B2, C2, D2, E3, F2, G2, H2,
        ]);
        assert_eq!(
            Bitboard::bishop_targets(D1, pawn_moved),
            Bitboard::with_ones([C2, E2, F3, G4, H5])
        );

        let pawn_moved = Bitboard::with_ones([
            A1, B1, C1, D1, E1, F1, G1, H1, A2, B2, C2, D2, E3, F2, G2, H2,
        ]);
        assert_eq!(
            Bitboard::bishop_targets(F1, pawn_moved),
            Bitboard::with_ones([G2, E2, D3, C4, B5, A6])
        );
    }

    #[test]
    fn rook_targets_test() {
        // assert_eq!(
        //     Bitboard::rook_targets(E4, INITIAL_STATE),
        //     Bitboard::with_ones([E2, E3, E5, E6, E7, A4, B4, C4, D4, F4, G4, H4])
        // );

        // assert_eq!(
        //     Bitboard::rook_targets(A1, INITIAL_STATE),
        //     Bitboard::with_ones([A2, B1])
        // );

        assert_eq!(
            Bitboard::rook_targets(H1, Bitboard::with_ones([A1, B1, C1, D1, F1, H1, H2])),
            Bitboard::with_ones([F1, G1, H2])
        );
    }

    #[test]
    fn queen_targets_test() {
        assert_eq!(
            Bitboard::queen_targets(E4, INITIAL_STATE),
            Bitboard::with_ones([
                B7, E7, H7, C6, E6, G6, D5, E5, F5, A4, B4, C4, D4, F4, G4, H4, D3, E3, F3, C2, E2,
                G2,
            ])
        );
        assert_eq!(
            Bitboard::queen_targets(A4, INITIAL_STATE),
            Bitboard::with_ones([
                A7, D7, A6, C6, A5, B5, B4, C4, D4, E4, F4, G4, H4, A3, B3, A2, C2
            ])
        );
        let pawn_moved = Bitboard::with_ones([
            A1, B1, C1, D1, E1, F1, G1, H1, A2, B2, C2, D2, E3, F2, G2, H2,
        ]);

        assert_eq!(
            Bitboard::queen_targets(F1, pawn_moved),
            Bitboard::with_ones([A6, B5, C4, D3, E2, E1, F2, G2, G1])
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
