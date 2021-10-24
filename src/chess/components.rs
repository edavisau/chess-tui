use std::fmt::Display;

use super::BOARD_SIZE;

pub(crate) const FILES: [char; 8] = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];

pub(crate) fn parse_file(value: &char) -> Option<usize> {
    FILES.iter().position(|x| x == value)
}

pub(crate) fn parse_rank(value: &char) -> Option<usize> {
    match value.to_digit(10) {
        Some(x) if 1 <= x && x <= 8 => Some((x-1) as usize),
        _ => None,
    }
}

#[derive(Debug)]
pub(crate) struct Piece {
    pub(crate) kind: PieceType,
    pub(crate) colour: Colour,
    pub(crate) position: Position,
    pub(crate) history: Vec<u8>,
}

pub(crate) fn piece_as_char(kind: PieceType, colour: Colour) -> char {
    match (kind, colour) {
        (PieceType::Pawn, Colour::White) => '\u{2659}',
        (PieceType::Pawn, Colour::Black) => '\u{265F}',
        (PieceType::Rook, Colour::White) => '\u{2656}',
        (PieceType::Rook, Colour::Black) => '\u{265C}',
        (PieceType::Knight, Colour::White) => '\u{2658}',
        (PieceType::Knight, Colour::Black) => '\u{265E}',
        (PieceType::Bishop, Colour::White) => '\u{2657}',
        (PieceType::Bishop, Colour::Black) => '\u{265D}',
        (PieceType::Queen, Colour::White) => '\u{2655}',
        (PieceType::Queen, Colour::Black) => '\u{265B}',
        (PieceType::King, Colour::White) => '\u{2654}',
        (PieceType::King, Colour::Black) => '\u{265A}',
    }
}

impl Piece {
    pub(crate) fn new(kind: PieceType, colour: Colour, position: Position) -> Self {
        Self { kind, colour, position, history: Vec::new() }
    }

    pub(crate) fn as_char(&self) -> char {
        piece_as_char(self.kind, self.colour)
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Colour {
    White,
    Black
}

impl Display for Colour {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Colour::White => write!(f, "White"),
            Colour::Black => write!(f, "Black"),
        }
    }
}

impl Colour {
    pub fn flip(&self) -> Self {
        match self {
            Colour::White => Colour::Black,
            Colour::Black => Colour::White,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PieceType {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct Position(pub(super) usize, pub(super) usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) struct PosDiff(pub(super) isize, pub(super) isize);

impl Position {
    fn is_valid(value: (usize, usize)) -> bool {
        (value.0 < BOARD_SIZE) && (value.1 < BOARD_SIZE)
    }

    pub(super) fn add(&self, diff: PosDiff) -> Result<Self, &'static str> {
        let (new_x, new_y) = (self.0 as isize + diff.0, self.1 as isize + diff.1);
        if new_x >= 0 && new_y >= 0 {
            Position::try_from((new_x as usize, new_y as usize))
        } else {
            Err("New coordinates are not positive")
        }
    }

    /// Finds the difference between two positions
    pub(super) fn diff(pos1: Position, pos2: Position) -> PosDiff {
        let x: isize = pos1.0 as isize - pos2.0 as isize;
        let y: isize = pos1.1 as isize - pos2.1 as isize;
        PosDiff(x, y)
    }

    pub fn as_white(self, colour: Colour) -> Self {
        match colour {
            Colour::White => self,
            Colour::Black => Self(BOARD_SIZE - self.0 - 1, BOARD_SIZE - self.1 - 1),
        }
    }

    pub fn to_string(&self) -> String {
        let file = FILES[self.0];
        let rank = self.1 + 1;
        return format!("{}{}", file, rank);
    }
}

impl TryFrom<(usize, usize)> for Position {
    type Error = &'static str;

    fn try_from(value: (usize, usize)) -> Result<Self, Self::Error> {
        if Position::is_valid(value) {
            Ok(Position(value.0, value.1))
        } else {
            Err("Invalid coordinates. Both must be in range 0..=7.")
        }
    }
}

impl TryFrom<&str> for Position {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut value = value.chars();

        let c = value.next().ok_or("Invalid length")?;
        let x: usize = match parse_file(&c) {
            Some(x) => x,
            None => return Err("Column must be a-h (lowercase)")
        };

        let y = match usize::from_str_radix(value.as_str(), 10) {
            Ok(y) if (1 <= y) && (y <= 8) => y - 1,
            _ => return Err("Row must be 1-8")
        };
        Ok(Self (x, y))
    }
}

impl PosDiff {
    pub fn as_white(self, colour: Colour) -> Self {
        match colour {
            Colour::White => self,
            Colour::Black => Self(-self.0, -self.1),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_try_from_str() {
        let good1 = "c3".to_string();
        assert_eq!(Position::try_from(&good1[..]), Ok(Position(2, 2)));
        let good2 = "h8".to_string();
        assert_eq!(Position::try_from(&good2[..]), Ok(Position(7, 7)));
        let bad1 = "a9".to_string();
        assert!(Position::try_from(&bad1[..]).is_err());
        let bad2 = "k1".to_string();
        assert!(Position::try_from(&bad2[..]).is_err());
        let bad3 = "asfd".to_string();
        assert!(Position::try_from(&bad3[..]).is_err());
    }

    #[test]
    fn test_position_try_from_usize_pair() {
        assert_eq!(Position::try_from((2, 2)), Ok(Position(2, 2)));
        assert_eq!(Position::try_from((7, 7)), Ok(Position(7, 7)));
        assert!(Position::try_from((1, 9)).is_err());
    }

    #[test]
    fn test_position_add() {
        let position = Position::try_from("g8").unwrap();
        assert_eq!(position.add(PosDiff(-3, -2)).unwrap(), Position::try_from("d6").unwrap());
        assert!(position.add(PosDiff(2, 1)).is_err());
    }

    #[test]
    fn test_position_diff() {
        let pos1 = Position::try_from("g8").unwrap();
        let pos2 = Position::try_from("a2").unwrap();
        let pos3 = Position::try_from("c1").unwrap();
        assert_eq!(Position::diff(pos1, pos2), PosDiff(6, 6));
        assert_eq!(Position::diff(pos2, pos1), PosDiff(-6, -6));
        assert_eq!(Position::diff(pos3, pos2), PosDiff(2, -1));
    }

    #[test]
    fn test_position_as_white() {
        let pos1 = Position::try_from("g8").unwrap();
        assert_eq!(pos1.as_white(Colour::Black), Position::try_from("b1").unwrap());
        assert_eq!(pos1.as_white(Colour::White), pos1);
    }

    #[test]
    fn test_posdiff_as_white() {
        let diff = PosDiff(3, -2);
        assert_eq!(diff.as_white(Colour::Black), PosDiff(-3, 2));
        assert_eq!(diff.as_white(Colour::White), diff);
    }
}