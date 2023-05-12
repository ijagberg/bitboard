use consts::*;
use std::{
    fmt::{Debug, Display},
    str::FromStr,
};
use File::*;
use Rank::*;

pub mod consts;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
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

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
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
            A => "a",
            B => "b",
            C => "c",
            D => "d",
            E => "e",
            F => "f",
            G => "g",
            H => "h",
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

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
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
