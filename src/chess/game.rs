use std::convert::{TryFrom, TryInto};
use std::fmt::Display;
use std::fs;

use super::BOARD_SIZE;
use super::moves::*;
use super::components::*;

use itertools::Itertools;

/// Chess game representing the main interface for the game.
pub struct Game {
    /// 8x8 board containing a piece or an empty square.
    board: Vec<Vec<Option<Piece>>>,
    /// The colour whose turn it is to move
    current_turn: Colour,
    /// The current numbers of moves performed in the game
    count: u8,
    /// A list of historical moves. Acts as a stack to allow undoing moves in the right order.
    moves: Vec<Move>,
    /// A list of pieces which have been captured by the other team.
    captured: Vec<Piece>,
    /// Indicates whether the colour to next move is in check
    in_check: bool,
    /// Indicates which colour has offered to draw. If both colours offer draw (i.e. the other colour accepts), then the game ends.
    draw_offer: Option<Colour>
}

impl Default for Game {
    fn default() -> Self {
        let mut board: Vec<Vec<Option<Piece>>> = Vec::new();

        fn piece_order() -> Vec<PieceType> {
            vec![PieceType::Rook, PieceType::Knight, PieceType::Bishop, PieceType::Queen, 
                    PieceType::King, PieceType::Bishop, PieceType::Knight, PieceType::Rook]
        }

        board.push(piece_order().into_iter().enumerate().map(|(i, x)| Some(Piece::new(x, Colour::White, (i, 0).try_into().unwrap()))).collect());
        board.push((0..BOARD_SIZE).map(|i| Some(Piece::new(PieceType::Pawn, Colour::White, (i, 1).try_into().unwrap()))).collect());
        for _ in 0..4 {
            board.push((0..BOARD_SIZE).map(|_| None).collect());
        }
        board.push((0..BOARD_SIZE).map(|i| Some(Piece::new(PieceType::Pawn, Colour::Black, (i, 6).try_into().unwrap()))).collect());
        board.push(piece_order().into_iter().enumerate().map(|(i, x)| Some(Piece::new(x, Colour::Black, (i, 7).try_into().unwrap()))).collect());
        Self { 
            board,
            current_turn: Colour::White, 
            count: 0, 
            moves: Vec::new(),
            captured: Vec::new(),
            in_check: false,
            draw_offer: None,
        }
    }
}

impl Game {
    /// Initialises a brand new chess game
    pub fn new() -> Self {
        Self::default()
    }

    fn try_move_internal(&mut self, mut move_: Move) -> Result<MoveResult, MoveError> {
        self.check_move(&mut move_, true)?;
        move_.compute_string(self);
        self.do_move(&mut move_);

        // Update game
        self.count += 1;
        self.current_turn = self.current_turn.flip();
        self.in_check = self.is_in_check(self.current_turn);
        let moves_left = self.any_moves_left();
        let result = match (self.in_check, moves_left) {
            (true, true) => MoveResult::Check(self.current_turn),
            (true, false) => MoveResult::Checkmate(self.current_turn),
            (false, true) => MoveResult::MovePlayed,
            (false, false) => MoveResult::Stalemate(self.current_turn.flip()),
        };
        move_.move_result = Some(result);
        self.moves.push(move_);
        return Ok(result)
    }

    pub fn try_move_positions<F>(&mut self, pos1: &str, pos2: &str, promotion_callback: Option<F>) -> Result<MoveResult, MoveError>
    where
        F: Promotion + 'static
    {
        let pos1: Position = pos1.try_into().map_err(|_| MoveError::InvalidMoveRequest(format!("Invalid position: {}", pos1)))?;
        let pos2: Position = pos2.try_into().map_err(|_| MoveError::InvalidMoveRequest(format!("Invalid position: {}", pos2)))?;
        let partial_request = Box::new(AbsoluteRequest::new(pos1, pos2, promotion_callback));
        let move_: Move = partial_request.find_move(self)?;
        return self.try_move_internal(move_)
    }

    pub fn try_move_san(&mut self, input: &str) -> Result<MoveResult, MoveError> {
        let san_request = SANRequest::try_from(input).map_err(|_| MoveError::InvalidMoveRequest(format!("Invalid SAN input: {}", input)))?;
        let move_: Move = san_request.find_move(self)?;
        return self.try_move_internal(move_)
    }

    pub fn get_current_count(&self) -> u8 {
        self.count
    }

    pub fn get_current_colour(&self) -> Colour {
        self.current_turn
    }

    pub fn save_game(&self, filename: &str) {
        let data: String = self
            .display_moves()
            .join("\n");
        fs::write(filename, data).expect("Unable to write file");
    }

    pub fn load_game(filename: &str) -> Result<Self, String> {
        let moves = fs::read_to_string(filename)
            .expect("Unable to read file");
        
        let moves = moves
            .trim()
            .split_whitespace()
            .collect::<Vec<&str>>();
        return Game::from_san_moves(moves);
    }

    /// Offers a draw. Returns true if the both sides have agreed to a draw.
    pub fn offer_draw(&mut self) -> bool {
        match self.draw_offer {
            Some(colour) if colour == self.current_turn => return false,
            Some(_) => return true,
            None => {
                self.draw_offer = Some(self.current_turn);
                return false;
            },
        }
    }

    /// Cancels a previously-offered request to draw.
    pub fn cancel_draw(&mut self) {
        if let Some(colour) = self.draw_offer {
            if colour == self.current_turn {
                self.draw_offer = None;
            }
        }
    }

    pub(crate) fn get_piece(&self, pos: Position) -> &Option<Piece> {
        &self.board[pos.1][pos.0]
    }

    fn get_piece_mut(&mut self, pos: Position) -> &mut Option<Piece> {
        &mut self.board[pos.1][pos.0]
    }

    // Remove piece from the board and add to captured pile
    fn capture_piece(&mut self, pos: Position) {
        if let Some(piece) = self.get_piece_mut(pos).take() {
            self.captured.push(piece);
        }
    }

    fn uncapture_piece(&mut self, pos: Position) {
        if let Some(piece) = self.captured.last() {
            if piece.position == pos {
                *self.get_piece_mut(pos) = self.captured.pop();
            }
        }
    }

    /// Swaps two positions on the board. Positions do not have to contain pieces.
    fn swap_pieces(&mut self, pos1: Position, pos2: Position) {
        // Swap pieces
        let piece1 = self.get_piece_mut(pos1).take();
        let piece2 = self.get_piece_mut(pos2).take();
        *self.get_piece_mut(pos1) = piece2;
        *self.get_piece_mut(pos2) = piece1;
        if let Some(piece) = self.get_piece_mut(pos2).as_mut() {
            piece.position = pos2;
        }
    }

    /// Displays the board from the perspective of a colour
    pub fn display_board_as_colour(&self, colour: Colour) -> String {
        let rows: Vec<(usize, String)> = if colour == Colour::White {
            self.get_board_rows()
                .rev()
                .enumerate()
                .map(|(i, x)| (8 - (i as usize), x))
                .collect::<Vec<(usize, String)>>()
        } else {
            self.get_board_rows()
                .enumerate()
                .map(|(i, x)| (i as usize + 1, x))
                .collect::<Vec<(usize, String)>>()
        };
        

        let mut result: Vec<String> = Vec::new();
        // Top row
        result.push("  a b c d e f g h".to_owned());
        // Middle rows
        for (row_num, row) in rows {
            result.push(format!("{} {}  {}", row_num, row, row_num));
        }
        // Bottom row
        result.push("  a b c d e f g h".to_owned());

        return result.join("\n");
    }

    /// Returns a list of all previous moves in standard notation
    pub fn display_moves(&self) -> Vec<String> {
        self.moves.iter()
            .map(|record| record.get_string())
            .collect::<Vec<String>>()
    }

    fn get_board_rows(&self) -> impl Iterator<Item = String> + '_ + DoubleEndedIterator{
        self.board.iter()
            .map(|row| {
                row.iter()
                    .map(|tile| if let Some(piece) = tile { piece.as_char() } else { ' ' })
                    .join(" ")
            })
    }

    fn do_move(&mut self, move_: &mut Move) {
        match &move_.kind {
            MoveType::Standard => {
                self.capture_piece(move_.pos2);
                self.swap_pieces(move_.pos1, move_.pos2);
            },
            MoveType::Castle(castle_type) => {
                let row = if self.current_turn == Colour::White { 0 } else { 7 };
                match castle_type {
                    CastleType::Long => {
                        self.swap_pieces((4, row).try_into().unwrap(), (2, row).try_into().unwrap());
                        self.swap_pieces((0, row).try_into().unwrap(), (3, row).try_into().unwrap());
                    }
                    CastleType::Short => {
                        self.swap_pieces((4, row).try_into().unwrap(), (6, row).try_into().unwrap());
                        self.swap_pieces((7, row).try_into().unwrap(), (5, row).try_into().unwrap());
                    },
                }
            },
            MoveType::PawnAttack => {
                self.capture_piece(move_.pos2);
                self.swap_pieces(move_.pos1, move_.pos2);

                if let Some(promotion_callback) = &move_.promotion_callback {
                    let chosen_piece = promotion_callback.get_promotion_piece();
                    move_.promotion_piece = Some(chosen_piece);
                    self.get_piece_mut(move_.pos2).as_mut().unwrap().kind = chosen_piece;

                }
            },
            MoveType::EnPassant => {
                self.swap_pieces(move_.pos1, move_.pos2);
                let diff = PosDiff(0, -1).as_white(self.current_turn);
                self.capture_piece(move_.pos2.add(diff).unwrap());
            },
            MoveType::PawnPush => {
                self.swap_pieces(move_.pos1, move_.pos2);
                if let Some(promotion_callback) = &move_.promotion_callback {
                    let chosen_piece = if let Some(chosen_piece) = move_.promotion_piece {
                        chosen_piece
                    } else {
                        let chosen_piece = promotion_callback.get_promotion_piece();
                        move_.promotion_piece = Some(chosen_piece);
                        chosen_piece
                    };
                    self.get_piece_mut(move_.pos2).as_mut().unwrap().kind = chosen_piece;
                }
            },
            MoveType::PawnStart => {
                self.swap_pieces(move_.pos1, move_.pos2);
            },
        }
    }

    fn undo_move(&mut self, move_: &Move) {
        match &move_.kind {
            MoveType::Standard => {
                self.swap_pieces(move_.pos2, move_.pos1);
                self.uncapture_piece(move_.pos2);
            },
            MoveType::Castle(castle_type) => {
                let row = if self.current_turn == Colour::White { 0 } else { 7 };
                match castle_type {
                    CastleType::Long => {
                        self.swap_pieces((2, row).try_into().unwrap(), (4, row).try_into().unwrap());
                        self.swap_pieces((3, row).try_into().unwrap(), (0, row).try_into().unwrap());
                    },
                    CastleType::Short => {
                        self.swap_pieces((6, row).try_into().unwrap(), (4, row).try_into().unwrap());
                        self.swap_pieces((5, row).try_into().unwrap(), (7, row).try_into().unwrap());
                    },
                }
            },
            MoveType::PawnAttack => {
                if let Some(_) = move_.promotion_callback {
                    self.get_piece_mut(move_.pos2).as_mut().unwrap().kind = PieceType::Pawn;
                }
                self.swap_pieces(move_.pos2, move_.pos1);
                self.uncapture_piece(move_.pos2);
            },
            MoveType::PawnPush => {
                if let Some(_) = move_.promotion_callback {
                    self.get_piece_mut(move_.pos2).as_mut().unwrap().kind = PieceType::Pawn;
                }
                self.swap_pieces(move_.pos2, move_.pos1);
                self.uncapture_piece(move_.pos2);
            }
            MoveType::EnPassant => {
                let diff = PosDiff(0, -1).as_white(self.current_turn);
                self.uncapture_piece(move_.pos2.add(diff).unwrap());
                self.swap_pieces(move_.pos2, move_.pos1);
            },
            MoveType::PawnStart => {
                self.swap_pieces(move_.pos2, move_.pos1);
                self.uncapture_piece(move_.pos2);
            },
        }
    }

    fn check_move(&mut self, move_: &mut Move, undo: bool) -> Result<(), MoveError> {
        self.do_move(move_);
        if self.is_in_check(self.current_turn) {
            self.undo_move(move_);
            return Err(MoveError::CausesCheck)
        }
        if undo {
            self.undo_move(move_)
        }
        Ok(())
    }

    /// Checks if there are any moves left for the current turn. If false, the game is over by checkmate or stalemate depending on whether the colour is in check.
    fn any_moves_left(&mut self) -> bool {
        let potential_moves = self.get_all_colour_pieces(self.current_turn)
            .map(|x| self.get_available_moves(x.position))
            .flatten()
            .map(|(pos1, pos2)| AbsoluteRequest { pos1, pos2, promotion_callback: Some(|| PieceType::Queen) }.find_move(self) )
            .filter_map(|x| x.ok())
            .collect::<Vec<Move>>();

        return potential_moves.into_iter()
            .map(|mut x| self.check_move(&mut x, true))
            .find_map(|x| x.ok())
            .is_some();
    }

    fn get_available_moves(&self, pos: Position) -> Vec<(Position, Position)> {
        let piece = match self.get_piece(pos) {
            Some(piece) => piece,
            None => return vec![],
        };

        fn explore_sides(base: Position, diag: bool, row_col: bool) -> Vec<(Position, Position)> {
            let mut directions = Vec::new();
            if diag {
                directions.append(&mut vec![(-1, -1), (-1, 1), (1, -1), (1, 1)])
            }
            if row_col {
                directions.append(&mut vec![(0, 1), (1, 0), (0, -1), (-1, 0)])
            }
            directions.iter()
                .map(|(x, y)| {
                    let mut result: Vec<Position> = Vec::new();
                    let mut k = 1;
                    loop {
                        let test_pos = base.add(PosDiff(k*x, k*y));
                        if test_pos.is_ok() {
                            result.push(test_pos.unwrap());
                            k += 1;
                        } else {
                            break;
                        }
                    }
                    result
                })
                .flatten()
                .map(|x| (base, x))
                .collect::<Vec<(Position, Position)>>()
        }

        match piece.kind {
            PieceType::Pawn => { // TODO optimise based on pawn position
                vec![(0, 1), (-1, 1), (1, 1), (0, 2)].iter()
                    .map(|(x, y)| PosDiff(*x, *y))
                    .map(|x| piece.position.add(x))
                    .filter_map(|x| x.ok())
                    .map(|x| (piece.position, x))
                    .collect::<Vec<(Position, Position)>>()
            },
            PieceType::Rook => explore_sides(piece.position, false, true),
            PieceType::Knight => {
                vec![(2,1), (2,-1), (-2,1), (-2,-1), (1,2), (-1,2), (1,-2), (-1,-2)].iter()
                    .map(|(x, y)| PosDiff(*x, *y))
                    .map(|x| piece.position.add(x))
                    .filter_map(|x| x.ok())
                    .map(|x| (piece.position, x))
                    .collect::<Vec<(Position, Position)>>()
            },
            PieceType::Bishop => explore_sides(piece.position, true, false),
            PieceType::Queen => explore_sides(piece.position, true, true),
            PieceType::King => {
                vec![(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1,-1)].iter()
                    .map(|(x, y)| PosDiff(*x, *y))
                    .map(|x| piece.position.add(x))
                    .filter_map(|x| x.ok())
                    .map(|x| (piece.position, x))
                    .collect::<Vec<(Position, Position)>>()
            },
        }
    }

    fn iterate_pieces(&self) -> impl Iterator<Item = &Piece> {
        self.board.iter().flatten().filter_map(|x| x.as_ref())
    }

    pub(crate) fn get_all_colour_pieces(&self, colour: Colour) -> impl Iterator<Item = &Piece> {
        self.iterate_pieces().filter(move |&x| x.colour == colour)
    }

    // Checks if a piece can perform a move, excluding check conditions
    pub(super) fn find_move(&self, pos1: Position, pos2: Position) -> Result<MoveType, MoveError> {
        let piece = self.get_piece(pos1).as_ref().expect("Piece not found.");
        let diff = Position::diff(pos2, pos1);
        if let Some(piece2) = self.get_piece(pos2).as_ref() {
            if piece.colour == piece2.colour {
                return Err(MoveError::AttackingSameColour)
            }
        }

        fn can_reach_between(game: &Game, pos: Position, diff: PosDiff) -> Result<(), MoveError> {
            let inc = if (diff.0 == diff.1) || (diff.0 == -diff.1) || (diff.0 == 0) || (diff.1 == 0) {
                (diff.0.signum(), diff.1.signum())
            } else {
                return Err(MoveError::InvalidMovement);
            };

            let max_k = if diff.0 == 0 { diff.1.abs() } else { diff.0.abs() };
            for k in 1..max_k {
                if let Some(_) = game.get_piece(pos.add(PosDiff(inc.0 * k, inc.1 * k)).unwrap()) {
                    return Err(MoveError::PositionBlocked);
                }
            }
            Ok(())
        }

        match piece.kind {
            PieceType::Pawn => {
                // Makes matching easier for black movements (0, -2) instead of (0, 2)
                let pos_as_white = piece.position.as_white(piece.colour);
                let diff_as_white = diff.as_white(piece.colour);
                match diff_as_white { // Starting move
                    PosDiff(0, 2) if pos_as_white.1 == 1 => {
                        if let &Some(_) = self.get_piece(pos2) { return Err(MoveError::PositionBlocked) }
                        can_reach_between(self, pos1, diff)?;
                        return Ok(MoveType::PawnStart);
                    },
                    PosDiff(0, 1) => { // Move forward
                        if let &Some(_) = self.get_piece(pos2) { 
                            return Err(MoveError::PositionBlocked) 
                        } else {
                            return Ok(MoveType::PawnPush)
                        }
                    }, 
                    PosDiff(1|-1, 1) => {  // Attack or En Passant
                        if let &Some(_) = self.get_piece(pos2) { // Standard attack
                            return Ok(MoveType::PawnAttack);
                        } else { // En Passant
                            let last_move = self.moves.last()
                                .ok_or(MoveError::InvalidEnPassantConditions)?;
                            if let Move { pos1: last_pos, kind: MoveType::PawnStart, .. } = last_move {
                                if *last_pos == pos1.add(PosDiff(diff.0, diff.1 * 2)).unwrap() {
                                    return Ok(MoveType::EnPassant)
                                }
                            }
                            return Err(MoveError::InvalidEnPassantConditions);
                        }
                    },
                    _ => return Err(MoveError::InvalidMovement),
                }
            },
            PieceType::Rook => {
                if !((diff.0 == 0) || (diff.1 == 0)) {return Err(MoveError::InvalidMovement)}
                can_reach_between(self, pos1, diff)?;
                return Ok(MoveType::Standard);
            },
            PieceType::Knight => {
                if !((diff.0.abs() == 2 && diff.1.abs() == 1) || (diff.0.abs() == 1 && diff.1.abs() == 2)) {return Err(MoveError::InvalidMovement)}
                return Ok(MoveType::Standard);
            },
            PieceType::Bishop => {
                if !((diff.0 == -diff.1) || (diff.0 == diff.1)) {return Err(MoveError::InvalidMovement)}
                can_reach_between(self, pos1, diff)?;
                return Ok(MoveType::Standard);
            },
            PieceType::Queen => {
                can_reach_between(self, pos1, diff)?;
                return Ok(MoveType::Standard);
            },
            PieceType::King => {
                match diff {
                    PosDiff(0|-1|1, 0|-1|1) => return Ok(MoveType::Standard),
                    PosDiff(direction @ (-2|2), 0) => { // Castle
                        let castle_type = if direction < 0 { CastleType::Long} else { CastleType::Short };
                        if self.can_castle(castle_type, piece.colour) {
                            return Ok(MoveType::Castle(castle_type));
                        } else {
                            return Err(MoveError::InvalidCastleConditions)
                        }
                    },
                    _ => return Err(MoveError::InvalidMovement),
                }
            },
        }
    }

    /// Checks if a square is being attacked. Colour is the side being attacked.
    fn is_attacked(&self, colour: Colour, position: Position) -> bool {
        self.get_all_colour_pieces(colour.flip())
            .map(|x| self.find_move(x.position, position))
            .find_map(|x| x.ok())
            .is_some()
    }

    pub(crate) fn get_king_pos(&self, colour: Colour) -> Position {
        self.iterate_pieces()
            .filter(move |&x| x.colour == colour && x.kind == PieceType::King)
            .next()
            .unwrap()
            .position
    }

    /// Checks if a colour is in check
    fn is_in_check(&self, colour: Colour) -> bool {
        let king_pos = self.get_king_pos(colour); 
        return self.is_attacked(colour, king_pos);
    }

    pub(crate) fn can_castle(&self, castle_type: CastleType, colour: Colour) -> bool {
        let row = if self.current_turn == Colour::White { 0 } else { 7 };
        let king = self.get_piece(self.get_king_pos(colour)).as_ref().unwrap(); // King has to exist
        let rook = self.get_piece(match castle_type {
            CastleType::Long => (0, row).try_into().unwrap(),
            CastleType::Short => (7, row).try_into().unwrap(),
        }).as_ref().unwrap();

        // King and rook must not have moved
        if king.history.len() > 0 || rook.history.len() > 0 {
            return false;
        }

        // No pieces between king and rook
        let files_to_check = if castle_type == CastleType::Long { 1..=3 } else { 5..=6 };
        for pos in files_to_check.map(|x| Position::try_from((x, row)).unwrap()) {
            if let Some(_) = self.get_piece(pos) {
                return false;
            }
        }

        // King must not be in check or move through a square in check
        let files_to_check = if castle_type == CastleType::Long { 2..=4 } else { 4..=6 };
        for pos in files_to_check.map(|x| Position::try_from((x, row)).unwrap()) {
            if self.is_attacked(colour, pos) {
                return false;
            }
        }
        return true;
    }

    pub fn from_san_moves(input: Vec<&str>) -> Result<Self, String> {
        let mut game = Game::new();
        for (i, x) in input.into_iter().enumerate() {

            if game.try_move_san(x).is_err() {
                return Err(format!("Cannot parse move no.{}: {}", i+1, x))
            }
        }
        return Ok(game);
    }
}

impl Display for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = self.display_board_as_colour(Colour::White);
        write!(f, "{}", result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_move() {
        let game = Game::new();
    
        // Pawn E2 -> E4
        assert_eq!(
            game.find_move("e2".try_into().unwrap(), "e4".try_into().unwrap()).unwrap(),
            MoveType::PawnStart
        );
        // Pawn E2 -> E3
        assert_eq!(
            game.find_move("e2".try_into().unwrap(), "e3".try_into().unwrap()).unwrap(),
            MoveType::PawnPush
        );
        // Knight B1 -> C3
        assert_eq!(
            game.find_move("b1".try_into().unwrap(), "c3".try_into().unwrap()).unwrap(),
            MoveType::Standard
        );
        // Queen D1 -> B3
        assert_eq!(
            game.find_move("d1".try_into().unwrap(), "b3".try_into().unwrap()).err().unwrap(),
            MoveError::PositionBlocked
        );
        // Bishop F8 -> A4
        assert_eq!(
            game.find_move("f8".try_into().unwrap(), "a4".try_into().unwrap()).err().unwrap(),
            MoveError::InvalidMovement
        );
        // Queen D1 -> E1
        assert_eq!(
            game.find_move("d1".try_into().unwrap(), "e1".try_into().unwrap()).err().unwrap(),
            MoveError::AttackingSameColour
        );

        let game = Game::from_san_moves(vec!["e4", "h5", "Nf3", "h4", "Ba6", "h3", "e5", "d5"]).unwrap();
        // Short Castle: E1 -> G1
        assert_eq!(
            game.find_move("e1".try_into().unwrap(), "g1".try_into().unwrap()).unwrap(),
            MoveType::Castle(CastleType::Short)
        );
        // En Passant: E5 -> D6
        assert_eq!(
            game.find_move("e5".try_into().unwrap(), "d6".try_into().unwrap()).unwrap(),
            MoveType::EnPassant
        );
    }
    
    #[test]
    fn test_try_move_san() {
        let mut game = Game::from_san_moves(vec!["Nc3", "e5", "Nf3", "a6", "Nb5", "a5"]).unwrap();
        // Normal moves
        assert!(game.try_move_san("Nd4").is_err());
        assert!(game.try_move_san("Nbd4").is_ok());
    
        // Promotions
        let mut game = Game::from_san_moves(vec!["c4", "b5", "cxb5", "c5", "c6", "Bb7", "cxb7", "a6"]).unwrap();
        assert!(game.try_move_san("bxa8").is_err());
        assert!(game.try_move_san("bxa8=Q").is_ok());
    }

    #[test]
    fn test_is_attacked() {
        let game = Game::from_san_moves(vec!["e4", "d5"]).unwrap();
        assert!(game.is_attacked(Colour::Black, "d5".try_into().unwrap()));
        assert!(!game.is_attacked(Colour::White, "c7".try_into().unwrap()));
    }

    #[test]
    fn test_is_in_check() {
        let game = Game::from_san_moves(vec!["e4", "e5", "Qh5", "h6", "Qe5"]).unwrap();
        assert!(game.is_in_check(Colour::Black));
        assert!(!game.is_in_check(Colour::White));
    }

    #[test]
    fn test_stalemate() {
        let mut game = Game::from_san_moves(vec!["e3", "a5", "Qh5", "Ra6", "Qxa5", "h5", "h4", "Rah6",
                                                        "Qxc7", "f6", "Qxd7+", "Kf7", "Qxb7", "Qd3", "Qxb8", "Qh7",
                                                        "Qxc8", "Kg6"]).unwrap();
        assert_eq!(
            game.try_move_san("Qe6").unwrap(),
            MoveResult::Stalemate(Colour::White)
        );
    }
}