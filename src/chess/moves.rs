use std::convert::TryFrom;
use serde::{Serialize, Deserialize};
use super::BOARD_SIZE;
use super::game::{PieceType, Colour};


fn parse_file(value: &char) -> Option<usize> {
    vec!['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'].iter().position(|x| x == value)
}

fn parse_rank(value: &char) -> Option<usize> {
    match value.to_digit(10) {
        Some(x) if 1 <= x && x <= 8 => Some((x-1) as usize),
        _ => None,
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub(super) struct Move {
    pub(super) kind: MoveType,
    pub(super) piece: PieceType,
}

impl Move {
    pub fn new(piece: PieceType, kind: MoveType) -> Self {
        Self {
            kind,
            piece,
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum MoveError {
    InvalidMovement, // Rook tries to move in a diagonal
    NoPiece, // there is no piece at position
    InvalidColour, // using white on black's turn, for example
    AttackingSameColour, // white attacking white, for example
    PositionBlocked, // Can't reach position because another piece is in the way
    InvalidCastleConditions,
    InvalidEnPassantConditions,
    CausesCheck,
    InvalidMoveRequest,
}

#[derive(Debug, PartialEq)]
pub(crate) enum MoveResult {
    MovePlayed,
    Check(Colour),
    Checkmate(Colour)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub(super) enum MoveType {
    Standard(Position, Position),
    Castle(CastleType),
    Promotion(Position, Position, PieceType),
    EnPassant(Position, Position)
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone, Copy)]
pub(super) enum CastleType {
    Long,
    Short,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
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

#[derive(Debug, PartialEq)]
pub(super) struct MoveRequestSAN {
    pub(super) piece: PieceType,
    pub(super) end_pos: Option<Position>,
    pub(super) start_file: Option<usize>,
    pub(super) start_rank: Option<usize>,
    pub(super) castle: Option<CastleType>,
    pub(super) promotion: Option<PieceType>,
}

impl TryFrom<&str> for MoveRequestSAN {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        // Check for castling
        match value {
            "O-O-O" => return Ok(MoveRequestSAN {piece: PieceType::King, end_pos: None, start_file: None, start_rank: None, castle: Some(CastleType::Long), promotion: None}),
            "O-O" => return Ok(MoveRequestSAN {piece: PieceType::King, end_pos: None, start_file: None, start_rank: None, castle: Some(CastleType::Short), promotion: None}),
            _ => (), 
        }

        // Now for regular moves
        let chars: Vec<char> = value.chars().collect();
        if chars.len() < 2 {
            return Err("Input is empty or not long enough!");
        }

        let mut slice_start: usize = 0;
        let mut slice_end: usize = chars.len();

        // Piece type
        let piece_type = match chars[slice_start] {
            'K' => {slice_start += 1; PieceType::King},
            'Q' => {slice_start += 1; PieceType::Queen},
            'R' => {slice_start += 1; PieceType::Rook},
            'B' => {slice_start += 1; PieceType::Bishop},
            'N' => {slice_start += 1; PieceType::Knight},
            'P' => {slice_start += 1; PieceType::Pawn},
            _ => PieceType::Pawn, // e.g. "e4" defaults to Pawn
        };

        // Check and checkmate
        if chars[slice_end - 1] == '+' || chars[slice_end - 1] == '#' {
            // Ignore for now. TODO send a warning if this is not true.
            slice_end -= 1;
        }

        let promotion: Option<PieceType> = if slice_end - slice_start >= 2 {
            let promotion_keys = vec!['Q', 'R', 'B', 'N'];
            let promotion_pieces = vec![PieceType::Queen, PieceType::Rook, PieceType::Bishop, PieceType::Knight];
            match promotion_keys.iter().position(|&x| x == chars[slice_end - 1]) {
                Some(i) => {
                    if chars[slice_end - 2] != '=' {
                        return Err("Promotion must have an equals sign.")
                    }
                    slice_end -= 2;
                    Some(promotion_pieces[i])
                },
                None => None,
            }
        } else {
            return Err("Value does not meet the minimum size.")
        };

        // End position
        let end_pos: Position = if slice_end - slice_start >= 2 {
            let try_str: &str = &chars[slice_end - 2..slice_end].iter().collect::<String>();
            Position::try_from(try_str)?
        } else {
            return Err("Cannot detect end position")
        };
        slice_end -= 2;

        // "x" representing piece taking another
        if slice_end - slice_start >= 1 && chars[slice_end - 1] == 'x' {
            // Ignore for now, TODO send a warning if this is not true.
            slice_end -= 1;
        }

        let rank: Option<usize> = if slice_end - slice_start >= 1 {
            let rank = parse_rank(&chars[slice_end - 1]);
            if let Some(_) = rank {
                slice_end -= 1;
            }
            rank
        } else {
            None
        };
        
        let file: Option<usize> = if slice_end - slice_start >= 1 {
            let file = parse_file(&chars[slice_end - 1]);
            if let Some(_) = file {
                slice_end -= 1;
            }
            file
        } else {
            None
        };

        if slice_start != slice_end {
            return Err("Detected invalid characters")
        }

        Ok(Self {
            piece: piece_type,
            end_pos: Some(end_pos),
            start_file: file,
            start_rank: rank,
            castle: None,
            promotion
        })
    }
}

#[test]
fn test_san_moves() {
    // Castling
    assert_eq!(
        MoveRequestSAN::try_from("O-O-O").unwrap(), 
        MoveRequestSAN {piece: PieceType::King, end_pos: None, start_file: None, start_rank: None, castle: Some(CastleType::Long), promotion: None} 
    );
    assert_eq!(
        MoveRequestSAN::try_from("O-O").unwrap(), 
        MoveRequestSAN {piece: PieceType::King, end_pos: None, start_file: None, start_rank: None, castle: Some(CastleType::Short), promotion: None} 
    );
    // Normal moves
    assert_eq!(
        MoveRequestSAN::try_from("Re5").unwrap(), 
        MoveRequestSAN {piece: PieceType::Rook, end_pos: Position::try_from("e5").ok(), start_file: None, start_rank: None, castle: None, promotion: None} 
    );
    assert_eq!(
        MoveRequestSAN::try_from("Ba1").unwrap(), 
        MoveRequestSAN {piece: PieceType::Bishop, end_pos: Position::try_from("a1").ok(), start_file: None, start_rank: None, castle: None, promotion: None} 
    );
    assert_eq!(
        MoveRequestSAN::try_from("e4#").unwrap(), 
        MoveRequestSAN {piece: PieceType::Pawn, end_pos: Position::try_from("e4").ok(), start_file: None, start_rank: None, castle: None, promotion: None} 
    );
    assert_eq!(
        MoveRequestSAN::try_from("gxh7").unwrap(), 
        MoveRequestSAN {piece: PieceType::Pawn, end_pos: Position::try_from("h7").ok(), start_file: Some(6), start_rank: None, castle: None, promotion: None} 
    );
    assert_eq!(
        MoveRequestSAN::try_from("Nb1xc3").unwrap(), 
        MoveRequestSAN {piece: PieceType::Knight, end_pos: Position::try_from("c3").ok(), start_file: Some(1), start_rank: Some(0), castle: None, promotion: None} 
    );
    assert_eq!(
        MoveRequestSAN::try_from("e8=Q").unwrap(), 
        MoveRequestSAN {piece: PieceType::Pawn, end_pos: Position::try_from("e8").ok(), start_file: None, start_rank: None, castle: None, promotion: Some(PieceType::Queen)} 
    );
}