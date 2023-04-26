use std::{
    fmt::{Debug, Display},
    str::FromStr,
};

use File::*;
use Rank::*;

pub const A1: Position = Position(A, One);
pub const A2: Position = Position(A, Two);
pub const A3: Position = Position(A, Three);
pub const A4: Position = Position(A, Four);
pub const A5: Position = Position(A, Five);
pub const A6: Position = Position(A, Six);
pub const A7: Position = Position(A, Seven);
pub const A8: Position = Position(A, Eight);
pub const B1: Position = Position(B, One);
pub const B2: Position = Position(B, Two);
pub const B3: Position = Position(B, Three);
pub const B4: Position = Position(B, Four);
pub const B5: Position = Position(B, Five);
pub const B6: Position = Position(B, Six);
pub const B7: Position = Position(B, Seven);
pub const B8: Position = Position(B, Eight);
pub const C1: Position = Position(C, One);
pub const C2: Position = Position(C, Two);
pub const C3: Position = Position(C, Three);
pub const C4: Position = Position(C, Four);
pub const C5: Position = Position(C, Five);
pub const C6: Position = Position(C, Six);
pub const C7: Position = Position(C, Seven);
pub const C8: Position = Position(C, Eight);
pub const D1: Position = Position(D, One);
pub const D2: Position = Position(D, Two);
pub const D3: Position = Position(D, Three);
pub const D4: Position = Position(D, Four);
pub const D5: Position = Position(D, Five);
pub const D6: Position = Position(D, Six);
pub const D7: Position = Position(D, Seven);
pub const D8: Position = Position(D, Eight);
pub const E1: Position = Position(E, One);
pub const E2: Position = Position(E, Two);
pub const E3: Position = Position(E, Three);
pub const E4: Position = Position(E, Four);
pub const E5: Position = Position(E, Five);
pub const E6: Position = Position(E, Six);
pub const E7: Position = Position(E, Seven);
pub const E8: Position = Position(E, Eight);
pub const F1: Position = Position(F, One);
pub const F2: Position = Position(F, Two);
pub const F3: Position = Position(F, Three);
pub const F4: Position = Position(F, Four);
pub const F5: Position = Position(F, Five);
pub const F6: Position = Position(F, Six);
pub const F7: Position = Position(F, Seven);
pub const F8: Position = Position(F, Eight);
pub const G1: Position = Position(G, One);
pub const G2: Position = Position(G, Two);
pub const G3: Position = Position(G, Three);
pub const G4: Position = Position(G, Four);
pub const G5: Position = Position(G, Five);
pub const G6: Position = Position(G, Six);
pub const G7: Position = Position(G, Seven);
pub const G8: Position = Position(G, Eight);
pub const H1: Position = Position(H, One);
pub const H2: Position = Position(H, Two);
pub const H3: Position = Position(H, Three);
pub const H4: Position = Position(H, Four);
pub const H5: Position = Position(H, Five);
pub const H6: Position = Position(H, Six);
pub const H7: Position = Position(H, Seven);
pub const H8: Position = Position(H, Eight);
pub const FILE_A: [Position; 8] = [A1, A2, A3, A4, A5, A6, A7, A8];
pub const FILE_B: [Position; 8] = [B1, B2, B3, B4, B5, B6, B7, B8];
pub const FILE_C: [Position; 8] = [C1, C2, C3, C4, C5, C6, C7, C8];
pub const FILE_D: [Position; 8] = [D1, D2, D3, D4, D5, D6, D7, D8];
pub const FILE_E: [Position; 8] = [E1, E2, E3, E4, E5, E6, E7, E8];
pub const FILE_F: [Position; 8] = [F1, F2, F3, F4, F5, F6, F7, F8];
pub const FILE_G: [Position; 8] = [G1, G2, G3, G4, G5, G6, G7, G8];
pub const FILE_H: [Position; 8] = [H1, H2, H3, H4, H5, H6, H7, H8];
pub const RANK_ONE: [Position; 8] = [A1, B1, C1, D1, E1, F1, G1, H1];
pub const RANK_TWO: [Position; 8] = [A2, B2, C2, D2, E2, F2, G2, H2];
pub const RANK_THREE: [Position; 8] = [A3, B3, C3, D3, E3, F3, G3, H3];
pub const RANK_FOUR: [Position; 8] = [A4, B4, C4, D4, E4, F4, G4, H4];
pub const RANK_FIVE: [Position; 8] = [A5, B5, C5, D5, E5, F5, G5, H5];
pub const RANK_SIX: [Position; 8] = [A6, B6, C6, D6, E6, F6, G6, H6];
pub const RANK_SEVEN: [Position; 8] = [A7, B7, C7, D7, E7, F7, G7, H7];
pub const RANK_EIGHT: [Position; 8] = [A8, B8, C8, D8, E8, F8, G8, H8];
pub const INCREASING_A1_A2: [Position; 64] = [
    A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8, C1, C2, C3, C4, C5, C6, C7, C8,
    D1, D2, D3, D4, D5, D6, D7, D8, E1, E2, E3, E4, E5, E6, E7, E8, F1, F2, F3, F4, F5, F6, F7, F8,
    G1, G2, G3, G4, G5, G6, G7, G8, H1, H2, H3, H4, H5, H6, H7, H8,
];
pub const INCREASING_A1_B1: [Position; 64] = [
    A1, B1, C1, D1, E1, F1, G1, H1, A2, B2, C2, D2, E2, F2, G2, H2, A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4, A5, B5, C5, D5, E5, F5, G5, H5, A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7, A8, B8, C8, D8, E8, F8, G8, H8,
];

pub(crate) const ALL_FILES: [File; 8] = [
    File::A,
    File::B,
    File::C,
    File::D,
    File::E,
    File::F,
    File::G,
    File::H,
];
pub(crate) const ALL_RANKS: [Rank; 8] = [
    Rank::One,
    Rank::Two,
    Rank::Three,
    Rank::Four,
    Rank::Five,
    Rank::Six,
    Rank::Seven,
    Rank::Eight,
];

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Position(File, Rank);

impl Position {
    fn construct(file: File, rank: Rank) -> Self {
        Self(file, rank)
    }

    pub fn new(file: File, rank: Rank) -> Self {
        Self::construct(file, rank)
    }

    pub fn file_iter(file: File) -> impl Iterator<Item = Self> {
        ALL_RANKS
            .iter()
            .map(move |&rank| Self::construct(file, rank))
    }

    pub fn rank_iter(rank: Rank) -> impl Iterator<Item = Self> {
        ALL_FILES
            .iter()
            .map(move |&file| Self::construct(file, rank))
    }

    pub fn increasing_iter() -> impl Iterator<Item = Self> {
        INCREASING_A1_A2.iter().copied()
    }

    pub fn file(&self) -> File {
        self.0
    }

    pub fn rank(&self) -> Rank {
        self.1
    }

    pub fn manhattan_distance_to(&self, other: Self) -> usize {
        let file_dist = u8::from(self.file()).abs_diff(u8::from(other.file()));
        let rank_dist = u8::from(self.rank()).abs_diff(u8::from(other.rank()));
        (file_dist + rank_dist) as usize
    }

    pub fn up(&self) -> Option<Self> {
        let file = self.file();
        let rank = self.rank().up()?;
        Some(Self::construct(file, rank))
    }

    pub fn up_right(&self) -> Option<Self> {
        let file = self.file().right()?;
        let rank = self.rank().up()?;
        Some(Self::construct(file, rank))
    }

    pub fn right(&self) -> Option<Self> {
        let file = self.file().right()?;
        let rank = self.rank();
        Some(Self::construct(file, rank))
    }

    pub fn down_right(&self) -> Option<Self> {
        let file = self.file().right()?;
        let rank = self.rank().down()?;
        Some(Self::construct(file, rank))
    }

    pub fn down(&self) -> Option<Self> {
        let file = self.file();
        let rank = self.rank().down()?;
        Some(Self::construct(file, rank))
    }

    pub fn down_left(&self) -> Option<Self> {
        let file = self.file().left()?;
        let rank = self.rank().down()?;
        Some(Self::construct(file, rank))
    }

    pub fn left(&self) -> Option<Self> {
        let file = self.file().left()?;
        let rank = self.rank();
        Some(Self::construct(file, rank))
    }

    pub fn up_left(&self) -> Option<Self> {
        let file = self.file().left()?;
        let rank = self.rank().up()?;
        Some(Self::construct(file, rank))
    }
}

impl Debug for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.file(), self.rank())
    }
}

impl FromStr for Position {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut chars = s.chars();
        let file: File = chars.next().ok_or(())?.try_into()?;
        let rank: Rank = chars.next().ok_or(())?.try_into()?;

        Ok(Self::new(file, rank))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum File {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
}

impl File {
    pub fn left(&self) -> Option<Self> {
        let v = u8::from(*self);
        if v > 0 {
            Self::try_from(v - 1).ok()
        } else {
            None
        }
    }

    pub fn left_all(&self) -> impl Iterator<Item = Self> {
        ALL_FILES[..=usize::from(u8::from(*self))]
            .iter()
            .copied()
            .rev()
    }

    pub fn right(&self) -> Option<Self> {
        Self::try_from(u8::from(*self) + 1).ok()
    }

    pub fn right_all(&self) -> impl Iterator<Item = Self> {
        ALL_FILES[usize::from(u8::from(*self))..].iter().copied()
    }

    pub fn add_offset(&self, offset: i32) -> Option<Self> {
        let v = (u8::from(*self)) as i32 + offset;
        Self::try_from(u8::try_from(v).ok()?).ok()
    }
}

impl Display for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            A => "A",
            B => "B",
            C => "C",
            D => "D",
            E => "E",
            F => "F",
            G => "G",
            H => "H",
        };
        write!(f, "{}", s)
    }
}

impl TryFrom<char> for File {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        Ok(match value {
            'a' | 'A' => Self::A,
            'b' | 'B' => Self::B,
            'c' | 'C' => Self::C,
            'd' | 'D' => Self::D,
            'e' | 'E' => Self::E,
            'f' | 'F' => Self::F,
            'g' | 'G' => Self::G,
            'h' | 'H' => Self::H,
            _ => return Err(()),
        })
    }
}

impl From<File> for u8 {
    fn from(value: File) -> Self {
        match value {
            A => 0,
            B => 1,
            C => 2,
            D => 3,
            E => 4,
            F => 5,
            G => 6,
            H => 7,
        }
    }
}

impl From<File> for usize {
    fn from(value: File) -> Self {
        usize::from(u8::from(value))
    }
}

impl TryFrom<u8> for File {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use File::*;
        Ok(match value {
            0 => A,
            1 => B,
            2 => C,
            3 => D,
            4 => E,
            5 => F,
            6 => G,
            7 => H,
            _ => return Err(()),
        })
    }
}

impl PartialOrd for File {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for File {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        u8::from(*self).cmp(&u8::from(*other))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Rank {
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
}

impl Rank {
    pub fn up(&self) -> Option<Self> {
        Self::try_from(u8::from(*self) + 1).ok()
    }

    pub fn up_all(&self) -> impl Iterator<Item = Self> {
        ALL_RANKS[usize::from(u8::from(*self))..].iter().copied()
    }

    pub fn down(&self) -> Option<Self> {
        let v = u8::from(*self);
        if v > 0 {
            Self::try_from(v - 1).ok()
        } else {
            None
        }
    }

    pub fn down_all(&self) -> impl Iterator<Item = Self> {
        ALL_RANKS[..=usize::from(u8::from(*self))]
            .iter()
            .copied()
            .rev()
    }
}

impl Display for Rank {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            One => "1",
            Two => "2",
            Three => "3",
            Four => "4",
            Five => "5",
            Six => "6",
            Seven => "7",
            Eight => "8",
        };
        write!(f, "{}", s)
    }
}

impl TryFrom<char> for Rank {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        Ok(match value {
            '1' => Self::One,
            '2' => Self::Two,
            '3' => Self::Three,
            '4' => Self::Four,
            '5' => Self::Five,
            '6' => Self::Six,
            '7' => Self::Seven,
            '8' => Self::Eight,
            _ => return Err(()),
        })
    }
}

impl From<Rank> for u8 {
    fn from(value: Rank) -> Self {
        match value {
            One => 0,
            Two => 1,
            Three => 2,
            Four => 3,
            Five => 4,
            Six => 5,
            Seven => 6,
            Eight => 7,
        }
    }
}

impl From<Rank> for usize {
    fn from(value: Rank) -> Self {
        usize::from(u8::from(value))
    }
}

impl TryFrom<u8> for Rank {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use Rank::*;
        Ok(match value {
            0 => One,
            1 => Two,
            2 => Three,
            3 => Four,
            4 => Five,
            5 => Six,
            6 => Seven,
            7 => Eight,
            _ => return Err(()),
        })
    }
}

impl PartialOrd for Rank {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Rank {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        u8::from(*self).cmp(&u8::from(*other))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn directions_test() {
        assert_eq!(E4.up(), Some(E5));
        assert_eq!(E4.up_right(), Some(F5));
        assert_eq!(E4.right(), Some(F4));
        assert_eq!(E4.down_right(), Some(F3));
        assert_eq!(E4.down(), Some(E3));
        assert_eq!(E4.down_left(), Some(D3));
        assert_eq!(E4.left(), Some(D4));
        assert_eq!(E4.up_left(), Some(D5));
    }

    #[test]
    fn file_directions_test() {
        assert_eq!(
            File::E.left_all().collect::<Vec<_>>(),
            vec![File::E, File::D, File::C, File::B, File::A]
        );
    }

    #[test]
    fn file_iter_test() {
        let positions: Vec<Position> = Position::file_iter(File::C).collect();
        assert_eq!(positions, vec![C1, C2, C3, C4, C5, C6, C7, C8]);
    }

    #[test]
    fn rank_iter_test() {
        let positions: Vec<Position> = Position::rank_iter(Rank::Seven).collect();
        assert_eq!(positions, vec![A7, B7, C7, D7, E7, F7, G7, H7]);
    }

    #[test]
    fn manhattan_distance_to_test() {
        assert_eq!(E4.manhattan_distance_to(E3), 1);
        assert_eq!(E4.manhattan_distance_to(A1), 7);
        assert_eq!(E4.manhattan_distance_to(A8), 8);
        assert_eq!(E4.manhattan_distance_to(H1), 6);
        assert_eq!(E4.manhattan_distance_to(H8), 7);
    }

    #[test]
    fn parse_position_test() {
        assert_eq!(E4, "e4".parse().unwrap());
        assert!("abc".parse::<Position>().is_err());
    }
}
