use position::*;
use std::{
    fmt::{Debug, Display},
    ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not, Shl, Shr},
};

pub mod position;

pub const INITIAL_STATE: Bitboard =
    Bitboard(0b1111111111111111000000000000000000000000000000001111111111111111);

const FILE_A: u64 = 0b0000000100000001000000010000000100000001000000010000000100000001;
pub const FILE_A_INCLUDE: Bitboard = Bitboard::construct(FILE_A);
pub const FILE_A_CLEAR: Bitboard = Bitboard::construct(!FILE_A);

const FILE_B: u64 = 0b0000001000000010000000100000001000000010000000100000001000000010;
pub const FILE_B_INCLUDE: Bitboard = Bitboard::construct(FILE_B);
pub const FILE_B_CLEAR: Bitboard = Bitboard::construct(!FILE_B);

const FILE_C: u64 = 0b0000010000000100000001000000010000000100000001000000010000000100;
pub const FILE_C_INCLUDE: Bitboard = Bitboard::construct(FILE_C);
pub const FILE_C_CLEAR: Bitboard = Bitboard::construct(!FILE_C);

const FILE_D: u64 = 0b0000100000001000000010000000100000001000000010000000100000001000;
pub const FILE_D_INCLUDE: Bitboard = Bitboard::construct(FILE_D);
pub const FILE_D_CLEAR: Bitboard = Bitboard::construct(!FILE_D);

const FILE_E: u64 = 0b0001000000010000000100000001000000010000000100000001000000010000;
pub const FILE_E_INCLUDE: Bitboard = Bitboard::construct(FILE_E);
pub const FILE_E_CLEAR: Bitboard = Bitboard::construct(!FILE_E);

const FILE_F: u64 = 0b0010000000100000001000000010000000100000001000000010000000100000;
pub const FILE_F_INCLUDE: Bitboard = Bitboard::construct(FILE_F);
pub const FILE_F_CLEAR: Bitboard = Bitboard::construct(!FILE_F);

const FILE_G: u64 = 0b0100000001000000010000000100000001000000010000000100000001000000;
pub const FILE_G_INCLUDE: Bitboard = Bitboard::construct(FILE_G);
pub const FILE_G_CLEAR: Bitboard = Bitboard::construct(!FILE_G);

const FILE_H: u64 = 0b1000000010000000100000001000000010000000100000001000000010000000;
pub const FILE_H_INCLUDE: Bitboard = Bitboard::construct(FILE_H);
pub const FILE_H_CLEAR: Bitboard = Bitboard::construct(!FILE_H);

const RANK_1: u64 = 0b0000000000000000000000000000000000000000000000000000000011111111;
pub const RANK_1_INCLUDE: Bitboard = Bitboard::construct(RANK_1);
pub const RANK_1_CLEAR: Bitboard = Bitboard::construct(!RANK_1);

const RANK_2: u64 = 0b0000000000000000000000000000000000000000000000001111111100000000;
pub const RANK_2_INCLUDE: Bitboard = Bitboard::construct(RANK_2);
pub const RANK_2_CLEAR: Bitboard = Bitboard::construct(!RANK_2);

const RANK_3: u64 = 0b0000000000000000000000000000000000000000111111110000000000000000;
pub const RANK_3_INCLUDE: Bitboard = Bitboard::construct(RANK_3);
pub const RANK_3_CLEAR: Bitboard = Bitboard::construct(!RANK_3);

const RANK_4: u64 = 0b0000000000000000000000000000000011111111000000000000000000000000;
pub const RANK_4_INCLUDE: Bitboard = Bitboard::construct(RANK_4);
pub const RANK_4_CLEAR: Bitboard = Bitboard::construct(!RANK_4);

const RANK_5: u64 = 0b0000000000000000000000001111111100000000000000000000000000000000;
pub const RANK_5_INCLUDE: Bitboard = Bitboard::construct(RANK_5);
pub const RANK_5_CLEAR: Bitboard = Bitboard::construct(!RANK_5);

const RANK_6: u64 = 0b0000000000000000111111110000000000000000000000000000000000000000;
pub const RANK_6_INCLUDE: Bitboard = Bitboard::construct(RANK_6);
pub const RANK_6_CLEAR: Bitboard = Bitboard::construct(!RANK_6);

const RANK_7: u64 = 0b0000000011111111000000000000000000000000000000000000000000000000;
pub const RANK_7_INCLUDE: Bitboard = Bitboard::construct(RANK_7);
pub const RANK_7_CLEAR: Bitboard = Bitboard::construct(!RANK_7);

const RANK_8: u64 = 0b1111111100000000000000000000000000000000000000000000000000000000;
pub const RANK_8_INCLUDE: Bitboard = Bitboard::construct(RANK_8);
pub const RANK_8_CLEAR: Bitboard = Bitboard::construct(!RANK_8);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Bitboard(u64);

impl Bitboard {
    const fn construct(v: u64) -> Self {
        Self(v)
    }

    pub fn full() -> Self {
        Self::construct(!0)
    }

    pub fn empty() -> Self {
        Self::construct(0)
    }

    pub fn with_one(pos: Position) -> Self {
        let v = 1 << bitboard_index_of(pos);
        Self::construct(v)
    }

    pub fn with_ones(positions: &[Position]) -> Self {
        let mut s = Self(0);
        for &pos in positions {
            s = s | Self::with_one(pos);
        }

        s
    }

    pub fn bit_at(&self, pos: Position) -> bool {
        let idx = bitboard_index_of(pos);
        let b = ((1 << idx) & self.0) > 0;
        b
    }

    pub fn include_position(self, pos: Position) -> Self {
        self & Self::with_one(pos)
    }

    pub fn include_positions(self, positions: &[Position]) -> Self {
        let mut include = Self::empty();
        for pos in positions {
            include |= Self::with_one(*pos);
        }

        self & include
    }

    pub fn clear_position(self, pos: Position) -> Self {
        self & !Self::with_one(pos)
    }

    pub fn clear_positions(self, positions: &[Position]) -> Self {
        let mut clear = Self::full();
        for pos in positions {
            clear &= !Self::with_one(*pos);
        }

        self & clear
    }

    fn include_file_bitboard(file: File) -> Self {
        match file {
            File::A => FILE_A_INCLUDE,
            File::B => FILE_B_INCLUDE,
            File::C => FILE_C_INCLUDE,
            File::D => FILE_D_INCLUDE,
            File::E => FILE_E_INCLUDE,
            File::F => FILE_F_INCLUDE,
            File::G => FILE_G_INCLUDE,
            File::H => FILE_H_INCLUDE,
        }
    }

    pub fn include_file(self, file: File) -> Self {
        self & Self::include_file_bitboard(file)
    }

    pub fn include_files(self, files: &[File]) -> Self {
        let mut include = Bitboard::empty();
        for file in files {
            include |= Self::include_file_bitboard(*file)
        }

        self & include
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

    pub fn clear_files(self, files: &[File]) -> Self {
        let mut clear = Bitboard::full();
        for file in files {
            clear &= Self::clear_file_bitboard(*file)
        }

        self & clear
    }

    fn include_rank_bitboard(rank: Rank) -> Self {
        match rank {
            Rank::One => RANK_1_INCLUDE,
            Rank::Two => RANK_2_INCLUDE,
            Rank::Three => RANK_3_INCLUDE,
            Rank::Four => RANK_4_INCLUDE,
            Rank::Five => RANK_5_INCLUDE,
            Rank::Six => RANK_6_INCLUDE,
            Rank::Seven => RANK_7_INCLUDE,
            Rank::Eight => RANK_8_INCLUDE,
        }
    }

    pub fn include_rank(self, rank: Rank) -> Self {
        self & Self::include_rank_bitboard(rank)
    }

    pub fn include_ranks(self, ranks: &[Rank]) -> Self {
        let mut include = Bitboard::empty();
        for rank in ranks {
            include |= Self::include_rank_bitboard(*rank)
        }

        self & include
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

    pub fn clear_ranks(self, ranks: &[Rank]) -> Self {
        let mut clear = Bitboard::full();
        for rank in ranks {
            clear &= Self::clear_rank_bitboard(*rank);
        }

        self & clear
    }

    pub fn knight_targets(&self, pos: Position) -> Self {
        let knight_bb = Bitboard::with_one(pos);
        let up_2_right_1 = (knight_bb << 17_u64).clear_file(File::A);
        let right_2_up_1 = (knight_bb << 10_u64).clear_files(&[File::A, File::B]);
        let right_2_down_1 = (knight_bb >> 6_u64).clear_files(&[File::A, File::B]);
        let down_2_right_1 = (knight_bb >> 15_u64).clear_file(File::A);
        let down_2_left_1 = (knight_bb >> 17_u64).clear_file(File::H);
        let left_2_down_1 = (knight_bb >> 10_u64).clear_files(&[File::G, File::H]);
        let left_2_up_1 = (knight_bb << 6_u64).clear_files(&[File::G, File::H]);
        let up_2_left_1 = (knight_bb << 15_u64).clear_file(File::H);

        up_2_right_1
            | right_2_up_1
            | right_2_down_1
            | down_2_right_1
            | down_2_left_1
            | left_2_down_1
            | left_2_up_1
            | up_2_left_1
    }

    fn data(&self) -> u64 {
        self.0
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

impl BitAnd for Bitboard {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self::construct(self.0.bitand(rhs.0))
    }
}

impl BitAnd<u64> for Bitboard {
    type Output = Self;

    fn bitand(self, rhs: u64) -> Self::Output {
        Self::construct(self.data().bitor(rhs))
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

impl Debug for Bitboard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

impl Display for Bitboard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let chars: Vec<char> = format!("{:064b}", self.data()).chars().collect();

        let mut output = vec![String::from("  abcdefgh")];
        for (idx, chunk) in chars.chunks(8).enumerate() {
            let mut line = String::with_capacity(10);
            line.push_str(&format!("{} ", 8 - idx));
            line.push_str(&chunk.iter().rev().collect::<String>());
            output.push(line);
        }

        write!(f, "bitboard:\n{}", output.join("\n"))
    }
}

macro_rules! impl_shl {
    ($type:ty) => {
        impl Shl<$type> for Bitboard {
            type Output = Self;

            fn shl(self, rhs: $type) -> Self {
                Self(self.data().shl(rhs))
            }
        }
    };
}

impl_shl!(u8);
impl_shl!(u16);
impl_shl!(u32);
impl_shl!(u64);
impl_shl!(u128);

impl_shl!(i8);
impl_shl!(i16);
impl_shl!(i32);
impl_shl!(i64);
impl_shl!(i128);

impl_shl!(usize);
impl_shl!(isize);

macro_rules! impl_shr {
    ($type:ty) => {
        impl Shr<$type> for Bitboard {
            type Output = Self;

            fn shr(self, rhs: $type) -> Self {
                Self(self.data().shr(rhs))
            }
        }
    };
}

impl_shr!(u8);
impl_shr!(u16);
impl_shr!(u32);
impl_shr!(u64);
impl_shr!(u128);

impl_shr!(i8);
impl_shr!(i16);
impl_shr!(i32);
impl_shr!(i64);
impl_shr!(i128);

impl_shr!(usize);
impl_shr!(isize);

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
    fn include_file_test() {
        let file_b_only = INITIAL_STATE.include_file(File::B);
        assert_eq!(
            file_b_only,
            Bitboard::construct(0b0000001000000010000000000000000000000000000000000000001000000010)
        );
    }

    #[test]
    fn include_files_test() {
        let file_b_and_e = INITIAL_STATE.include_files(&[File::B, File::E]);
        assert_eq!(
            file_b_and_e,
            Bitboard::construct(0b0001001000010010000000000000000000000000000000000001001000010010)
        )
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
        let no_file_b_or_e = INITIAL_STATE.clear_files(&[File::B, File::E]);
        assert_eq!(
            no_file_b_or_e,
            Bitboard::construct(0b1110110111101101000000000000000000000000000000001110110111101101)
        );
    }

    #[test]
    fn include_rank_test() {
        let rank_2_only = INITIAL_STATE.include_rank(Rank::Two);
        assert_eq!(
            rank_2_only,
            Bitboard::construct(0b0000000000000000000000000000000000000000000000001111111100000000)
        );
    }

    #[test]
    fn include_ranks_test() {
        let rank_2_and_8 = INITIAL_STATE.include_ranks(&[Rank::Two, Rank::Eight]);
        assert_eq!(
            rank_2_and_8,
            Bitboard::construct(0b1111111100000000000000000000000000000000000000001111111100000000)
        );
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
        let no_rank_2_or_7 = INITIAL_STATE.clear_ranks(&[Rank::Two, Rank::Seven]);
        assert_eq!(
            no_rank_2_or_7,
            Bitboard::construct(0b1111111100000000000000000000000000000000000000000000000011111111)
        );
    }

    #[test]
    fn knight_targets_test() {
        let targets_of_e5 = Bitboard::empty().knight_targets(E5);
        assert_eq!(
            targets_of_e5,
            Bitboard::full().include_positions(&[F7, G6, G4, F3, D3, C4, C6, D7])
        );

        let targets_of_a1 = Bitboard::empty().knight_targets(A1);
        assert_eq!(targets_of_a1, Bitboard::full().include_positions(&[B3, C2]));
    }
}
