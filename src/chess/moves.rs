use super::{components::{Colour, Piece, PieceType, Position, parse_file, parse_rank, piece_as_char, FILES}, game::Game};
use std::convert::TryFrom;

pub(super) struct Move {
    pub(super) turn: u8,
    pub(super) kind: MoveType,
    pub(super) pos1: Position,
    pub(super) pos2: Position,
    pub(super) piece_type: PieceType,
    pub(super) colour: Colour,
    pub(super) captures: bool,
    pub(super) move_result: Option<MoveResult>,
    pub(super) promotion_callback: Option<Box<dyn Promotion>>,
    pub(super) promotion_piece: Option<PieceType>,
    pub(super) san_string: Option<String>,
}

impl Move {
    pub(crate) fn compute_string(&mut self, game: &Game) {
        if self.turn != game.get_current_count() {
            panic!("Can only run this function during the current turn!");
        }

        if let Some(_) = &self.san_string {
            // Already computed
            return
        } else if let MoveType::Castle(castle_type) = self.kind {
            let string = castle_type.as_string();
            self.san_string = Some(string);
        } else {
            let piece_char = match self.piece_type {
                PieceType::Pawn => "".to_string(),
                piece_type => piece_as_char(piece_type, self.colour).to_string()
            };

            let captures = if self.captures { "x" } else { "" };

            // Look for other moves
            let other_possibilities: Vec<Position> = game.get_all_colour_pieces(self.colour)
                .filter(|&x| x.kind == self.piece_type && x.position != self.pos1)
                .filter(|&x| game.find_move(x.position, self.pos2).is_ok())
                .map(|x| x.position)
                .collect();

            let mut start_file: String = match other_possibilities.iter().map(|x| x.1).find(|&x| x == self.pos1.1) {
                Some(_) => FILES[self.pos1.0].to_string(),
                _ => "".to_string()
            };
            let start_rank: String = match other_possibilities.iter().map(|x| x.0).find(|&x| x == self.pos1.0) {
                Some(_) => format!("{}", self.pos1.1 + 1),
                _ => "".to_string(),
            };

            // Pawn attacks should always include the file
            if let MoveType::PawnAttack | MoveType::EnPassant = self.kind {
                if start_file.eq("") {
                    start_file = FILES[self.pos1.0].to_string()
                }
            }

            let end_condition: &str = match self.move_result {
                Some(MoveResult::Check(_)) => "+",
                Some(MoveResult::Checkmate(_)) => "#",
                _ => "",
            };
            let promotion: String = if let Some(piece_type) = self.promotion_piece {
                format!("={}", piece_as_char(piece_type, self.colour))
            } else {
                "".to_string()
            };
            let end_pos = self.pos2.to_string();

            let result = format!("{}{}{}{}{}{}{}", piece_char, start_file, start_rank, captures, end_pos, promotion, end_condition);
            self.san_string = Some(result);
        }
    }

    pub fn get_string(&self) -> String {
        self.san_string.as_ref().unwrap().clone()
    }
}

#[derive(Debug, PartialEq)]
pub enum MoveError {
    InvalidMovement,     // Rook tries to move in a diagonal
    NoPiece,             // there is no piece at position
    InvalidColour,       // using white on black's turn, for example
    AttackingSameColour, // white attacking white, for example
    PositionBlocked,     // Can't reach position because another piece is in the way
    InvalidCastleConditions,
    InvalidEnPassantConditions,
    CausesCheck,
    InvalidMoveRequest(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum MoveResult {
    MovePlayed,
    Check(Colour),
    Checkmate(Colour),
    Stalemate(Colour),
}

#[derive(Debug, PartialEq)]
pub(super) enum MoveType {
    Standard,
    Castle(CastleType),
    PawnAttack,
    PawnPush,
    PawnStart,
    EnPassant,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum CastleType {
    Long,
    Short,
}

impl CastleType {
    /// Determines end position of king for a castle
    fn end_pos(&self, colour: Colour) -> Position {
        match (self, colour) {
            (CastleType::Long, Colour::White) => (2, 0).try_into().unwrap(),
            (CastleType::Long, Colour::Black) => (2, 7).try_into().unwrap(),
            (CastleType::Short, Colour::White) => (6, 0).try_into().unwrap(),
            (CastleType::Short, Colour::Black) => (6, 7).try_into().unwrap(),
        }
    }

    /// Writes castle moves in SAN
    fn as_string(&self) -> String {
        match self {
            CastleType::Long => "O-O-O".into(),
            CastleType::Short => "O-O".into(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(super) struct SANRequest {
    pub(super) piece: PieceType,
    pub(super) end_pos: Option<Position>,
    pub(super) start_file: Option<usize>,
    pub(super) start_rank: Option<usize>,
    pub(super) castle: Option<CastleType>,
    pub(super) promotion: Option<PieceType>,
}

impl TryFrom<&str> for SANRequest {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        // Check for castling
        match value {
            "O-O-O" => {
                return Ok(SANRequest {
                    piece: PieceType::King,
                    end_pos: None,
                    start_file: None,
                    start_rank: None,
                    castle: Some(CastleType::Long),
                    promotion: None,
                })
            }
            "O-O" => {
                return Ok(SANRequest {
                    piece: PieceType::King,
                    end_pos: None,
                    start_file: None,
                    start_rank: None,
                    castle: Some(CastleType::Short),
                    promotion: None,
                })
            }
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
            'K' | '\u{2654}' | '\u{265A}' => {
                slice_start += 1;
                PieceType::King
            }
            'Q' | '\u{2655}' | '\u{265B}' => {
                slice_start += 1;
                PieceType::Queen
            }
            'R' | '\u{2656}' | '\u{265C}' => {
                slice_start += 1;
                PieceType::Rook
            }
            'B' | '\u{2657}' | '\u{265D}' => {
                slice_start += 1;
                PieceType::Bishop
            }
            'N' | '\u{2658}' | '\u{265E}' => {
                slice_start += 1;
                PieceType::Knight
            }
            'P' | '\u{2659}' | '\u{265F}' => {
                slice_start += 1;
                PieceType::Pawn
            }
            _ => PieceType::Pawn, // e.g. "e4" defaults to Pawn
        };

        // Check and checkmate
        if chars[slice_end - 1] == '+' || chars[slice_end - 1] == '#' {
            // Ignore for now. TODO send a warning if this is not true.
            slice_end -= 1;
        }

        let promotion: Option<PieceType> = if slice_end - slice_start >= 2 {
            let piece_type = match chars[slice_end - 1] {
                'Q' | '\u{2655}' | '\u{265B}' => Some(PieceType::Queen),
                'R' | '\u{2656}' | '\u{265C}' => Some(PieceType::Rook),
                'B' | '\u{2657}' | '\u{265D}' => Some(PieceType::Bishop),
                'N' | '\u{2658}' | '\u{265E}' => Some(PieceType::Knight),
                _ => None,
            };

            if let Some(piece_type) = piece_type {
                if chars[slice_end - 2] != '=' {
                    return Err("Promotion must have an equals sign.");
                }
                slice_end -= 2;
                Some(piece_type)
            } else {
                None
            }
        } else {
            return Err("Value does not meet the minimum size.");
        };

        // End position
        let end_pos: Position = if slice_end - slice_start >= 2 {
            let try_str: &str = &chars[slice_end - 2..slice_end].iter().collect::<String>();
            Position::try_from(try_str)?
        } else {
            return Err("Cannot detect end position");
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
            return Err("Detected invalid characters");
        }

        Ok(Self {
            piece: piece_type,
            end_pos: Some(end_pos),
            start_file: file,
            start_rank: rank,
            castle: None,
            promotion,
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
            SANRequest::try_from("O-O-O").unwrap(),
            SANRequest {
                piece: PieceType::King,
                end_pos: None,
                start_file: None,
                start_rank: None,
                castle: Some(CastleType::Long),
                promotion: None
            }
        );
        assert_eq!(
            SANRequest::try_from("O-O").unwrap(),
            SANRequest {
                piece: PieceType::King,
                end_pos: None,
                start_file: None,
                start_rank: None,
                castle: Some(CastleType::Short),
                promotion: None
            }
        );
        // Normal moves
        assert_eq!(
            SANRequest::try_from("Re5").unwrap(),
            SANRequest {
                piece: PieceType::Rook,
                end_pos: Position::try_from("e5").ok(),
                start_file: None,
                start_rank: None,
                castle: None,
                promotion: None
            }
        );
        assert_eq!(
            SANRequest::try_from("Ba1").unwrap(),
            SANRequest {
                piece: PieceType::Bishop,
                end_pos: Position::try_from("a1").ok(),
                start_file: None,
                start_rank: None,
                castle: None,
                promotion: None
            }
        );
        assert_eq!(
            SANRequest::try_from("e4#").unwrap(),
            SANRequest {
                piece: PieceType::Pawn,
                end_pos: Position::try_from("e4").ok(),
                start_file: None,
                start_rank: None,
                castle: None,
                promotion: None
            }
        );
        assert_eq!(
            SANRequest::try_from("gxh7").unwrap(),
            SANRequest {
                piece: PieceType::Pawn,
                end_pos: Position::try_from("h7").ok(),
                start_file: Some(6),
                start_rank: None,
                castle: None,
                promotion: None
            }
        );
        assert_eq!(
            SANRequest::try_from("Nb1xc3").unwrap(),
            SANRequest {
                piece: PieceType::Knight,
                end_pos: Position::try_from("c3").ok(),
                start_file: Some(1),
                start_rank: Some(0),
                castle: None,
                promotion: None
            }
        );
        assert_eq!(
            SANRequest::try_from("e8=Q").unwrap(),
            SANRequest {
                piece: PieceType::Pawn,
                end_pos: Position::try_from("e8").ok(),
                start_file: None,
                start_rank: None,
                castle: None,
                promotion: Some(PieceType::Queen)
            }
        );
    }
}

#[derive(Debug)]
pub(super) struct AbsoluteRequest<F: Promotion> {
    pub(super) pos1: Position,
    pub(super) pos2: Position,
    pub(super) promotion_callback: Option<F>,
}

impl<F: Promotion> AbsoluteRequest<F> {
    pub(super) fn new(pos1: Position, pos2: Position, promotion_callback: Option<F>) -> Self {
        Self {
            pos1,
            pos2,
            promotion_callback,
        }
    }
}

pub(super) trait PartialRequest {
    fn find_move(self, game: &Game) -> Result<Move, MoveError>;
}

impl PartialRequest for SANRequest {
    fn find_move(self, game: &Game) -> Result<Move, MoveError> {
        let colour = game.get_current_colour();

        // Check for castling
        if let Some(castle_type) = self.castle {
            if game.can_castle(castle_type, colour) {
                let king_pos = game.get_king_pos(colour);
                return Ok(Move {
                    turn: game.get_current_count(),
                    kind: MoveType::Castle(castle_type),
                    pos1: king_pos,
                    pos2: castle_type.end_pos(colour),
                    piece_type: PieceType::King,
                    colour,
                    captures: false,
                    move_result: None,
                    promotion_callback: None,
                    san_string: Some(castle_type.as_string()),
                    promotion_piece: None,
                })
            }
        }

        let end_pos = self.end_pos
            .ok_or(MoveError::InvalidMoveRequest("Must have end position if not castling".into()))?;
        
        let promotion: Option<Box<dyn Promotion>> = if self.piece == PieceType::Pawn && end_pos.as_white(colour).1 == 7 {
            if let Some(promotion) = self.promotion {
                Some(Box::new(promotion))
            } else {
                return Err(MoveError::InvalidMoveRequest("Must supply promotion piece".into()))
            }
        } else {
            None
        };

        let pieces: Vec<&Piece> = game
            .get_all_colour_pieces(colour)
            .filter(|&x| x.kind == self.piece)
            .filter(|&x| if let Some(file) = self.start_file { x.position.0 == file } else { true } )
            .filter(|&x| if let Some(rank) = self.start_rank { x.position.1 == rank } else { true } )
            .collect();
        if pieces.len() == 0 {
            return Err(MoveError::NoPiece)
        }

        let mut potential: Vec<(&Piece, MoveType)> = pieces
            .iter()
            .map(|&x| (x, game.find_move(x.position, end_pos)))
            .filter(|&(_, ref result)| result.is_ok())
            .map(|(x, result)| (x, result.unwrap()))
            .collect();

        let (piece, move_type) = match potential.len() {
            0 => return Err(MoveError::InvalidMovement),
            1 => potential.remove(0),
            _ => return Err(MoveError::InvalidMoveRequest("This move has multiple possibilities. Please make it more specific.".into()))
        };

        let captures = match game.get_piece(end_pos).as_ref() {
            Some(piece) if piece.colour != colour && move_type != MoveType::PawnPush && move_type != MoveType::PawnPush => true,
            _ => false,
        };

        Ok(Move {
            kind: move_type,
            pos1: piece.position,
            pos2: end_pos,
            piece_type: self.piece,
            captures,
            move_result: None,
            promotion_callback: promotion,
            turn: game.get_current_count(),
            colour,
            san_string: None,
            promotion_piece: None,
        })
    }
}

impl<F> PartialRequest for AbsoluteRequest<F>
where
    F: Promotion + 'static,
{
    fn find_move(self, game: &Game) -> Result<Move, MoveError> {
        if self.pos1 == self.pos2 {
            return Err(MoveError::InvalidMovement);
        }

        let colour = game.get_current_colour();
        let piece: &Piece = match game.get_piece(self.pos1).as_ref() {
            Some(piece) if piece.colour == colour => piece,
            Some(_) => return Err(MoveError::InvalidColour),
            None => return Err(MoveError::NoPiece),
        };

        let move_type = game.find_move(self.pos1, self.pos2)?;

        let captures = match game.get_piece(self.pos2).as_ref() {
            Some(piece) if piece.colour != colour && move_type != MoveType::PawnPush => true,
            _ => false,
        };

        let promotion: Option<Box<dyn Promotion>> =
            if piece.kind == PieceType::Pawn && self.pos2.as_white(colour).0 == 7 {
                if let Some(promotion) = self.promotion_callback {
                    Some(Box::new(promotion))
                } else {
                    return Err(MoveError::InvalidMoveRequest(
                        "Promotion callback missing.".into(),
                    ));
                }
            } else {
                None
            };

        Ok(Move {
            kind: move_type,
            pos1: self.pos1,
            pos2: self.pos2,
            piece_type: piece.kind,
            captures,
            move_result: None,
            promotion_callback: promotion,
            turn: game.get_current_count(),
            colour,
            san_string: None,
            promotion_piece: None,
        })
    }
}

pub trait Promotion {
    fn get_promotion_piece(&self) -> PieceType;
}

impl Promotion for PieceType {
    fn get_promotion_piece(&self) -> PieceType {
        self.clone()
    }
}

impl<F> Promotion for F
where
    F: Fn() -> PieceType,
{
    fn get_promotion_piece(&self) -> PieceType {
        self()
    }
}
