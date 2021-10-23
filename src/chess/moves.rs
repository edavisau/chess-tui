use std::convert::TryFrom;
use serde::{Serialize, Deserialize};
use super::components::{PieceType, Colour, Position, parse_rank, parse_file};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub(super) struct Move {
    pub(super) pos: Position,
    pub(super) kind: MoveType,
}

impl Move {
    pub fn new(pos: Position, kind: MoveType) -> Self {
        Self {
            kind,
            pos,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum MoveError {
    InvalidMovement, // Rook tries to move in a diagonal
    NoPiece, // there is no piece at position
    InvalidColour, // using white on black's turn, for example
    AttackingSameColour, // white attacking white, for example
    PositionBlocked, // Can't reach position because another piece is in the way
    InvalidCastleConditions,
    InvalidEnPassantConditions,
    CausesCheck,
    InvalidMoveRequest(String),
}

#[derive(Debug, PartialEq)]
pub enum MoveResult {
    MovePlayed,
    Check(Colour),
    Checkmate(Colour),
    Stalemate(Colour),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub(super) enum MoveType {
    Standard(Position),
    Castle(CastleType),
    PawnAttack(Direction, Option<PieceType>),
    PawnPush(Option<PieceType>),
    PawnStart,
    EnPassant(Direction)
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone, Copy)]
pub(super) enum Direction {
    Left,
    Right
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone, Copy)]
pub(super) enum CastleType {
    Long,
    Short,
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

#[cfg(test)]
mod tests {
    use super::*;
    
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
}