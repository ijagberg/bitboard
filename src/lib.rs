use position::*;
use std::{
    fmt::{Debug, Display},
    ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not, Shl, Shr},
};

pub mod position;

pub const INITIAL_STATE: Bitboard =
    Bitboard(0b1111111111111111000000000000000000000000000000001111111111111111);

const FILE_A: u64 = 0b0000000100000001000000010000000100000001000000010000000100000001;
pub const FILE_A_MASK: Bitboard = Bitboard::construct(FILE_A);
pub const FILE_A_CLEAR: Bitboard = Bitboard::construct(!FILE_A);

const FILE_B: u64 = 0b0000001000000010000000100000001000000010000000100000001000000010;
pub const FILE_B_MASK: Bitboard = Bitboard::construct(FILE_B);
pub const FILE_B_CLEAR: Bitboard = Bitboard::construct(!FILE_B);

const FILE_C: u64 = 0b0000010000000100000001000000010000000100000001000000010000000100;
pub const FILE_C_MASK: Bitboard = Bitboard::construct(FILE_C);
pub const FILE_C_CLEAR: Bitboard = Bitboard::construct(!FILE_C);

const FILE_D: u64 = 0b0000100000001000000010000000100000001000000010000000100000001000;
pub const FILE_D_MASK: Bitboard = Bitboard::construct(FILE_D);
pub const FILE_D_CLEAR: Bitboard = Bitboard::construct(!FILE_D);

const FILE_E: u64 = 0b0001000000010000000100000001000000010000000100000001000000010000;
pub const FILE_E_MASK: Bitboard = Bitboard::construct(FILE_E);
pub const FILE_E_CLEAR: Bitboard = Bitboard::construct(!FILE_E);

const FILE_F: u64 = 0b0010000000100000001000000010000000100000001000000010000000100000;
pub const FILE_F_MASK: Bitboard = Bitboard::construct(FILE_F);
pub const FILE_F_CLEAR: Bitboard = Bitboard::construct(!FILE_F);

const FILE_G: u64 = 0b0100000001000000010000000100000001000000010000000100000001000000;
pub const FILE_G_MASK: Bitboard = Bitboard::construct(FILE_G);
pub const FILE_G_CLEAR: Bitboard = Bitboard::construct(!FILE_G);

const FILE_H: u64 = 0b1000000010000000100000001000000010000000100000001000000010000000;
pub const FILE_H_MASK: Bitboard = Bitboard::construct(FILE_H);
pub const FILE_H_CLEAR: Bitboard = Bitboard::construct(!FILE_H);

const RANK_1: u64 = 0b0000000000000000000000000000000000000000000000000000000011111111;
pub const RANK_1_MASK: Bitboard = Bitboard::construct(RANK_1);
pub const RANK_1_CLEAR: Bitboard = Bitboard::construct(!RANK_1);

const RANK_2: u64 = 0b0000000000000000000000000000000000000000000000001111111100000000;
pub const RANK_2_MASK: Bitboard = Bitboard::construct(RANK_2);
pub const RANK_2_CLEAR: Bitboard = Bitboard::construct(!RANK_2);

const RANK_3: u64 = 0b0000000000000000000000000000000000000000111111110000000000000000;
pub const RANK_3_MASK: Bitboard = Bitboard::construct(RANK_3);
pub const RANK_3_CLEAR: Bitboard = Bitboard::construct(!RANK_3);

const RANK_4: u64 = 0b0000000000000000000000000000000011111111000000000000000000000000;
pub const RANK_4_MASK: Bitboard = Bitboard::construct(RANK_4);
pub const RANK_4_CLEAR: Bitboard = Bitboard::construct(!RANK_4);

const RANK_5: u64 = 0b0000000000000000000000001111111100000000000000000000000000000000;
pub const RANK_5_MASK: Bitboard = Bitboard::construct(RANK_5);
pub const RANK_5_CLEAR: Bitboard = Bitboard::construct(!RANK_5);

const RANK_6: u64 = 0b0000000000000000111111110000000000000000000000000000000000000000;
pub const RANK_6_MASK: Bitboard = Bitboard::construct(RANK_6);
pub const RANK_6_CLEAR: Bitboard = Bitboard::construct(!RANK_6);

const RANK_7: u64 = 0b0000000011111111000000000000000000000000000000000000000000000000;
pub const RANK_7_MASK: Bitboard = Bitboard::construct(RANK_7);
pub const RANK_7_CLEAR: Bitboard = Bitboard::construct(!RANK_7);

const RANK_8: u64 = 0b1111111100000000000000000000000000000000000000000000000000000000;
pub const RANK_8_MASK: Bitboard = Bitboard::construct(RANK_8);
pub const RANK_8_CLEAR: Bitboard = Bitboard::construct(!RANK_8);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Bitboard(u64);

impl Bitboard {
    const fn construct(v: u64) -> Self {
        Self(v)
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

    pub fn with_ones(positions: impl Iterator<Item = Position>) -> Self {
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

    pub fn mask_positions(self, positions: impl Iterator<Item = Position>) -> Self {
        let mut include = Self::empty();
        for pos in positions {
            include |= Self::with_one(pos);
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

    pub fn mask_files(self, files: impl Iterator<Item = File>) -> Self {
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

    pub fn clear_files(self, files: impl Iterator<Item = File>) -> Self {
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

    pub fn mask_ranks(self, ranks: impl Iterator<Item = Rank>) -> Self {
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

    pub fn clear_ranks(self, ranks: &[Rank]) -> Self {
        let mut clear = Bitboard::full();
        for rank in ranks {
            clear &= Self::clear_rank_bitboard(*rank);
        }

        self & clear
    }

    pub fn king_targets(pos: Position) -> Self {
        let king_bb = Bitboard::with_one(pos);
        let up = king_bb << 8_u64;
        let up_right = (king_bb << 9_u64).clear_file(File::A);
        let right = (king_bb << 1_u64).clear_file(File::A);
        let down_right = (king_bb >> 7_u64).clear_file(File::A);
        let down = king_bb >> 8_u64;
        let down_left = (king_bb >> 9_u64).clear_file(File::H);
        let left = king_bb >> 1_u64;
        let up_left = (king_bb << 7_u64).clear_file(File::H);

        up | up_right | right | down_right | down | down_left | left | up_left
    }

    pub fn knight_targets(pos: Position) -> Self {
        let knight_bb = Bitboard::with_one(pos);
        let up_2_right_1 = (knight_bb << 17_u64).clear_file(File::A);
        let right_2_up_1 = (knight_bb << 10_u64).clear_files([File::A, File::B].into_iter());
        let right_2_down_1 = (knight_bb >> 6_u64).clear_files([File::A, File::B].into_iter());
        let down_2_right_1 = (knight_bb >> 15_u64).clear_file(File::A);
        let down_2_left_1 = (knight_bb >> 17_u64).clear_file(File::H);
        let left_2_down_1 = (knight_bb >> 10_u64).clear_files([File::G, File::H].into_iter());
        let left_2_up_1 = (knight_bb << 6_u64).clear_files([File::G, File::H].into_iter());
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

    pub fn up_ray(from: Position) -> Self {
        Self::full()
            .mask_ranks(from.rank().up_all())
            .mask_file(from.file())
    }

    pub fn up_occupancy_ray(from: Position) -> Self {
        Self::up_ray(from).clear_rank(Rank::Eight)
    }

    pub fn up_right_ray(from: Position) -> Self {
        Self::full().mask_positions(
            from.file()
                .right_all()
                .zip(from.rank().up_all())
                .map(|(file, rank)| Position::new(file, rank)),
        )
    }

    pub fn up_right_occupancy_ray(from: Position) -> Self {
        Self::up_right_ray(from)
            .clear_file(File::H)
            .clear_rank(Rank::Eight)
    }

    pub fn right_ray(from: Position) -> Self {
        Self::full()
            .mask_files(from.file().right_all())
            .mask_rank(from.rank())
    }

    pub fn right_occupancy_ray(from: Position) -> Self {
        Self::right_ray(from).clear_file(File::H)
    }

    pub fn down_right_ray(from: Position) -> Self {
        Self::full().mask_positions(
            from.file()
                .right_all()
                .zip(from.rank().down_all())
                .map(|(file, rank)| Position::new(file, rank)),
        )
    }

    pub fn down_right_occupancy_ray(from: Position) -> Self {
        Self::down_right_ray(from)
            .clear_file(File::H)
            .clear_rank(Rank::One)
    }

    pub fn down_ray(from: Position) -> Self {
        Self::full()
            .mask_ranks(from.rank().down_all())
            .mask_file(from.file())
    }

    pub fn down_occupancy_ray(from: Position) -> Self {
        Self::down_ray(from).clear_rank(Rank::One)
    }

    pub fn down_left_ray(from: Position) -> Self {
        Self::full().mask_positions(
            from.file()
                .left_all()
                .zip(from.rank().down_all())
                .map(|(file, rank)| Position::new(file, rank)),
        )
    }

    pub fn down_left_occupancy_ray(from: Position) -> Self {
        Self::down_left_ray(from)
            .clear_file(File::A)
            .clear_rank(Rank::One)
    }

    pub fn left_ray(from: Position) -> Self {
        Self::full()
            .mask_files(from.file().left_all())
            .mask_rank(from.rank())
    }

    pub fn left_occupancy_ray(from: Position) -> Self {
        Self::left_ray(from).clear_file(File::A)
    }

    pub fn up_left_ray(from: Position) -> Self {
        Self::full().mask_positions(
            from.file()
                .left_all()
                .zip(from.rank().up_all())
                .map(|(file, rank)| Position::new(file, rank)),
        )
    }

    pub fn up_left_occupancy_ray(from: Position) -> Self {
        Self::up_left_ray(from)
            .clear_rank(Rank::Eight)
            .clear_file(File::A)
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
    fn bitor_test() {
        let a4 = Bitboard::with_one(A4);
        let b3 = Bitboard::with_one(B3);
        let a4_or_b3 = a4 | b3;

        assert_eq!(a4_or_b3, Bitboard::with_ones([A4, B3].into_iter()));

        let mut empty = Bitboard::empty();
        empty |= a4;
        assert_eq!(empty, Bitboard::with_one(A4));
    }

    #[test]
    fn bitand_test() {
        let a4 = Bitboard::with_one(A4);
        let a4_and_g6 = Bitboard::with_ones([A4, G6].into_iter());
        let only_a4 = a4 & a4_and_g6;

        assert_eq!(only_a4, a4);

        let mut full = Bitboard::full();
        full &= a4;
        assert_eq!(full, Bitboard::with_one(A4));
    }

    #[test]
    fn bitxor_test() {
        let a4_and_b3 = Bitboard::with_ones([A4, B3].into_iter());
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
        let file_b_and_e = INITIAL_STATE.mask_files([File::B, File::E].into_iter());
        assert_eq!(
            file_b_and_e,
            Bitboard::construct(0b0001001000010010000000000000000000000000000000000001001000010010)
        );

        assert_eq!(
            Bitboard::empty(),
            Bitboard::full().mask_files([].into_iter())
        );

        assert_eq!(
            Bitboard::full(),
            Bitboard::full().mask_files(ALL_FILES.into_iter())
        );
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
        let no_file_b_or_e = INITIAL_STATE.clear_files([File::B, File::E].into_iter());
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
        let rank_2_and_8 = INITIAL_STATE.mask_ranks([Rank::Two, Rank::Eight].into_iter());
        assert_eq!(
            rank_2_and_8,
            Bitboard::construct(0b1111111100000000000000000000000000000000000000001111111100000000)
        );

        assert_eq!(
            Bitboard::empty(),
            Bitboard::full().mask_ranks([].into_iter())
        );

        assert_eq!(
            Bitboard::full(),
            Bitboard::full().mask_ranks(ALL_RANKS.into_iter())
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

        assert_eq!(
            Bitboard::empty(),
            Bitboard::full().clear_files(ALL_FILES.into_iter())
        );
    }

    #[test]
    fn king_targets_test() {
        let targets_of_e2 = Bitboard::king_targets(E2);
        assert_eq!(
            targets_of_e2,
            Bitboard::full().mask_positions([E3, F3, F2, F1, E1, D1, D2, D3].into_iter())
        );

        let targets_of_g8 = Bitboard::king_targets(G8);
        assert_eq!(
            targets_of_g8,
            Bitboard::full().mask_positions([H8, H7, G7, F7, F8].into_iter())
        );
    }

    #[test]
    fn knight_targets_test() {
        let targets_of_e5 = Bitboard::knight_targets(E5);
        assert_eq!(
            targets_of_e5,
            Bitboard::full().mask_positions([F7, G6, G4, F3, D3, C4, C6, D7].into_iter())
        );

        let targets_of_a1 = Bitboard::knight_targets(A1);
        assert_eq!(
            targets_of_a1,
            Bitboard::full().mask_positions([B3, C2].into_iter())
        );
    }

    #[test]
    fn up_ray_test() {
        assert_eq!(
            Bitboard::up_ray(C5),
            Bitboard::with_ones([C6, C7, C8].into_iter())
        );
    }

    #[test]
    fn occupancy_up_ray_test() {
        assert_eq!(
            Bitboard::up_occupancy_ray(C5),
            Bitboard::with_ones([C6, C7].into_iter())
        );
    }

    #[test]
    fn up_right_ray_test() {
        assert_eq!(
            Bitboard::up_right_ray(E4),
            Bitboard::with_ones([F5, G6, H7].into_iter())
        );

        assert_eq!(
            Bitboard::up_right_ray(H7),
            Bitboard::with_ones([].into_iter())
        );
    }

    #[test]
    fn up_right_occupancy_ray_test() {
        assert_eq!(
            Bitboard::up_right_occupancy_ray(E4),
            Bitboard::with_ones([F5, G6].into_iter())
        );
    }

    #[test]
    fn right_ray_test() {
        assert_eq!(
            Bitboard::right_ray(A7),
            Bitboard::with_ones([B7, C7, D7, E7, F7, G7, H7].into_iter())
        );
    }

    #[test]
    fn right_occupancy_ray_test() {
        assert_eq!(
            Bitboard::right_occupancy_ray(A7),
            Bitboard::with_ones([B7, C7, D7, E7, F7, G7].into_iter())
        );
    }

    #[test]
    fn down_right_ray_test() {
        assert_eq!(
            Bitboard::down_right_ray(C6),
            Bitboard::with_ones([D5, E4, F3, G2, H1].into_iter())
        );
    }

    #[test]
    fn down_right_occupancy_ray_test() {
        assert_eq!(
            Bitboard::down_right_occupancy_ray(C6),
            Bitboard::with_ones([D5, E4, F3, G2].into_iter())
        );
    }

    #[test]
    fn down_ray_test() {
        assert_eq!(
            Bitboard::down_ray(G5),
            Bitboard::with_ones([G4, G3, G2, G1].into_iter())
        );
    }

    #[test]
    fn down_occupancy_ray_test() {
        assert_eq!(
            Bitboard::down_occupancy_ray(G5),
            Bitboard::with_ones([G4, G3, G2].into_iter())
        );
    }

    #[test]
    fn down_left_ray_test() {
        assert_eq!(
            Bitboard::down_left_ray(C7),
            Bitboard::with_ones([B6, A5].into_iter())
        );
    }

    #[test]
    fn down_left_occupancy_ray_test() {
        assert_eq!(
            Bitboard::down_left_occupancy_ray(C7),
            Bitboard::with_ones([B6].into_iter())
        );
    }

    #[test]
    fn left_ray_test() {
        assert_eq!(Bitboard::left_ray(A7), Bitboard::empty());

        assert_eq!(
            Bitboard::left_ray(E4),
            Bitboard::with_ones([D4, C4, B4, A4].into_iter())
        );
    }

    #[test]
    fn left_occupancy_ray_test() {
        assert_eq!(Bitboard::left_occupancy_ray(A7), Bitboard::empty());
        assert_eq!(
            Bitboard::left_occupancy_ray(E4),
            Bitboard::with_ones([D4, C4, B4].into_iter())
        );
    }

    #[test]
    fn up_left_ray_test() {
        assert_eq!(
            Bitboard::up_left_ray(H1),
            Bitboard::with_ones([G2, F3, E4, D5, C6, B7, A8].into_iter())
        );
    }

    #[test]
    fn up_left_occupancy_ray_test() {
        assert_eq!(
            Bitboard::up_left_occupancy_ray(H1),
            Bitboard::with_ones([G2, F3, E4, D5, C6, B7].into_iter())
        );
    }
}
