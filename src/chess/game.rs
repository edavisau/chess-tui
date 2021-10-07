use std::convert::{TryFrom, TryInto};
use std::fmt::Display;
use std::fs;

use super::BOARD_SIZE;
use super::moves::*;

use serde::{Serialize, Deserialize};

/// Chess game representing the main interface for the game.
#[derive(Serialize, Deserialize)]
pub(crate) struct Game {
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
        }
    }
}

impl Game {
    /// Initialises a brand new chess game
    pub fn new() -> Self {
        Self::default()
    }

    pub fn try_move_positions(&mut self, pos1: String, pos2: String, promotion_callback: fn() -> PieceType) -> Result<MoveResult, MoveError> {
        let pos1: Position = Position::try_from(&pos1[..]).map_err(|x| MoveError::InvalidMoveRequest(x.to_owned()))?;
        let pos2: Position = Position::try_from(&pos2[..]).map_err(|x| MoveError::InvalidMoveRequest(x.to_owned()))?;
        
        if pos1 == pos2 {
            return Err(MoveError::InvalidMovement);
        }

        let mut found_move: Move = match self.get_piece(pos1).as_ref() {
            Some(piece) if piece.colour == self.current_turn => self.find_move(pos1, pos2)?,
            Some(_) => return Err(MoveError::InvalidColour),
            None => return Err(MoveError::NoPiece),
        };

        // Specify promotion piece
        if let MoveType::PawnPush(ref mut promotion_piece) | MoveType::PawnAttack(_, ref mut promotion_piece) = found_move.kind {
            *promotion_piece = Some(promotion_callback());
        }

        return self.try_move(found_move);
    }

    #[allow(dead_code)]
    pub fn from_san_moves(input: Vec<&str>) -> Option<Self> {
        let mut game = Game::new();
        for x in input {
            if game.try_move_san(x).is_err() {
                return None;
            }
        }
        return Some(game);
    }

    pub fn try_move_san(&mut self, value: &str) -> Result<MoveResult, MoveError> {
        let move_request = match MoveRequestSAN::try_from(value) {
            Ok(move_request) => move_request,
            Err(message) => return Err(MoveError::InvalidMoveRequest(message.to_owned()))
        };

        let mut found_move = self.check_san_request(&move_request).map_err(|x| MoveError::InvalidMoveRequest(x.to_owned()))?;
        
        // Specify promotion piece
        if let MoveType::PawnAttack(_, Some(piece_type)) | MoveType::PawnPush(Some(piece_type)) = &mut found_move.kind {
            *piece_type = move_request.promotion.unwrap();
        }

        return self.try_move(found_move);
    }

    pub fn get_current_count(&self) -> u8 {
        self.count
    }

    pub fn get_current_colour(&self) -> Colour {
        self.current_turn
    }

    pub fn save_game(&self, filename: &str) {
        let serialised = serde_json::to_string(self).expect("JSON encoding error");
        fs::write(filename, serialised).expect("Unable to write file");
    }

    pub fn load_game(filename: &str) -> Self {
        let serialised = fs::read_to_string(filename).expect("Unable to read file");
        serde_json::from_str(&serialised).unwrap()
    }

    fn get_piece(&self, pos: Position) -> &Option<Piece> {
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

    fn do_move(&mut self, move_: &Move) {
        match &move_.kind {
            MoveType::Standard(p2) => {
                self.capture_piece(*p2);
                self.swap_pieces(move_.pos, *p2);
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
            MoveType::PawnAttack(direction, promotion) => {
                let diff = if direction == &Direction::Left { 
                    PosDiff(-1, 1).as_white(self.get_current_colour())
                } else { 
                    PosDiff(1, 1).as_white(self.get_current_colour()) 
                };
                let pos2 = move_.pos.add(diff).unwrap();
                self.capture_piece(pos2);
                self.swap_pieces(move_.pos, pos2);

                if let Some(piece_type) = promotion {
                    self.get_piece_mut(pos2).as_mut().unwrap().kind = *piece_type;
                }
            },
            MoveType::EnPassant(direction) => {
                let diff = if direction == &Direction::Left { 
                    PosDiff(-1, 1).as_white(self.get_current_colour())
                } else { 
                    PosDiff(1, 1).as_white(self.get_current_colour()) 
                };
                let pos2 = move_.pos.add(diff).unwrap();
                self.swap_pieces(move_.pos, pos2);
                let diff = PosDiff(0, -1).as_white(self.current_turn);
                self.capture_piece(pos2.add(diff).unwrap());
            },
            MoveType::PawnPush(promotion) => {
                let diff = PosDiff(0, 1).as_white(self.get_current_colour());
                let pos2 = move_.pos.add(diff).unwrap();
                self.swap_pieces(move_.pos, pos2);
                if let Some(piece_type) = promotion {
                    self.get_piece_mut(pos2).as_mut().unwrap().kind = *piece_type;
                }
            },
            MoveType::PawnStart => {
                let diff = PosDiff(0, 2).as_white(self.get_current_colour());
                let pos2 = move_.pos.add(diff).unwrap();
                self.swap_pieces(move_.pos, pos2);
            },
        }
    }

    fn undo_move(&mut self, move_: &Move) {
        match &move_.kind {
            MoveType::Standard(p2) => {
                self.swap_pieces(*p2, move_.pos);
                self.uncapture_piece(*p2);
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
            MoveType::PawnAttack(direction, promotion) => {
                let diff = if direction == &Direction::Left {
                    PosDiff(-1, 1).as_white(self.get_current_colour())
                } else {
                    PosDiff(1, 1).as_white(self.get_current_colour())
                };
                let p2 = move_.pos.add(diff).unwrap();
                if let Some(_) = promotion {
                    self.get_piece_mut(p2).as_mut().unwrap().kind = PieceType::Pawn;
                }
                self.swap_pieces(p2, move_.pos);
                self.uncapture_piece(p2);
            },
            MoveType::PawnPush(promotion) => {
                let p2 = move_.pos.add(PosDiff(0, 1).as_white(self.get_current_colour())).unwrap();
                if let Some(_) = promotion {
                    self.get_piece_mut(p2).as_mut().unwrap().kind = PieceType::Pawn;
                }
                self.swap_pieces(p2, move_.pos);
                self.uncapture_piece(p2);
            }
            MoveType::EnPassant(direction) => {
                let diff = if direction == &Direction::Left {
                    PosDiff(-1, 1).as_white(self.get_current_colour())
                } else {
                    PosDiff(1, 1).as_white(self.get_current_colour())
                };
                let p2 = move_.pos.add(diff).unwrap();
                self.uncapture_piece(p2.add(diff).unwrap());
                self.swap_pieces(p2, move_.pos);
            },
            MoveType::PawnStart => {
                let p2 = move_.pos.add(PosDiff(0, 2).as_white(self.get_current_colour())).unwrap();
                self.swap_pieces(p2, move_.pos);
                self.uncapture_piece(p2);
            },
        }
    }

    fn check_move(&mut self, move_: &Move, undo: bool) -> Result<(), MoveError> {
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
            .map(|(pos1, pos2)| self.find_move(pos1, pos2))
            .filter_map(|x| x.ok())
            .collect::<Vec<Move>>();

        return potential_moves.iter()
            .map(|x| self.check_move(x, true))
            .filter_map(|x| x.ok())
            .next()
            .is_some();
    }

    fn try_move(&mut self, move_: Move) -> Result<MoveResult, MoveError> {
        self.check_move(&move_, false)?;

        // Update game
        self.moves.push(move_);
        self.count += 1;
        self.current_turn = self.current_turn.flip();
        self.in_check = self.is_in_check(self.current_turn);
        let moves_left = self.any_moves_left();
        match (self.in_check, moves_left) {
            (true, true) => return Ok(MoveResult::Check(self.current_turn)),
            (true, false) => return Ok(MoveResult::Checkmate(self.current_turn)),
            (false, true) => return Ok(MoveResult::MovePlayed),
            (false, false) => return Ok(MoveResult::Stalemate(self.current_turn.flip())),
        }
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

    fn get_all_colour_pieces(&self, colour: Colour) -> impl Iterator<Item = &Piece> {
        self.iterate_pieces().filter(move |&x| x.colour == colour)
    }

    // Checks if a piece can perform a move, excluding check conditions
    fn find_move(&self, pos1: Position, pos2: Position) -> Result<Move, MoveError> {
        let piece = self.get_piece(pos1).as_ref()
            .expect(&format!("No piece at {:?}. Must ensure piece exists before calling this function!", pos1));
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
                let promotion = pos_as_white.1 == 6;
                let diff_as_white = diff.as_white(piece.colour);
                match diff_as_white { // Starting move
                    PosDiff(0, 2) if pos_as_white.1 == 1 => {
                        if let &Some(_) = self.get_piece(pos2) { return Err(MoveError::PositionBlocked) }
                        can_reach_between(self, pos1, diff)?;
                        return Ok(Move::new(pos1, MoveType::PawnStart));
                    },
                    PosDiff(0, 1) => { // Move forward
                        if let &Some(_) = self.get_piece(pos2) { return Err(MoveError::PositionBlocked) }
                        if promotion {
                            return Ok(Move::new(pos1, MoveType::PawnPush(Some(PieceType::Queen))));
                        } else {
                            return Ok(Move::new(pos1, MoveType::PawnPush(None)));
                        }
                    }, 
                    PosDiff(1|-1, 1) => {  // Attack or En Passant
                        if let &Some(_) = self.get_piece(pos2) { // Standard attack
                            let direction = if diff_as_white.0 == -1 { Direction::Left } else { Direction::Right };
                            if promotion {
                                return Ok(Move::new(pos1, MoveType::PawnAttack(direction, Some(PieceType::Queen))));
                            } else {
                                return Ok(Move::new(pos1, MoveType::PawnAttack(direction,  None)));
                            }
                        } else { // En Passant
                            let last_move = self.moves.last().ok_or(MoveError::InvalidEnPassantConditions)?;
                            let direction = if diff_as_white.0 == -1 { Direction::Left } else { Direction::Right };
                            if let Move { pos: last_pos, kind: MoveType::PawnStart } = last_move {
                                if *last_pos == pos1.add(PosDiff(diff.0, diff.1 * 2)).unwrap() {
                                    return Ok(Move::new(pos1, MoveType::EnPassant(direction)))
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
                return Ok(Move::new(pos1, MoveType::Standard(pos2)));
            },
            PieceType::Knight => {
                if !((diff.0.abs() == 2 && diff.1.abs() == 1) || (diff.0.abs() == 1 && diff.1.abs() == 2)) {return Err(MoveError::InvalidMovement)}
                return Ok(Move::new(pos1, MoveType::Standard(pos2)));
            },
            PieceType::Bishop => {
                if !((diff.0 == -diff.1) || (diff.0 == diff.1)) {return Err(MoveError::InvalidMovement)}
                can_reach_between(self, pos1, diff)?;
                return Ok(Move::new(pos1, MoveType::Standard(pos2)));
            },
            PieceType::Queen => {
                can_reach_between(self, pos1, diff)?;
                return Ok(Move::new(pos1, MoveType::Standard(pos2)));
            },
            PieceType::King => {
                match diff {
                    PosDiff(0|-1|1, 0|-1|1) => return Ok(Move::new(pos1, MoveType::Standard(pos2))),
                    PosDiff(-2|2, 0) => { // Castle
                        let castle_type = match diff.0 {
                            -2 => CastleType::Long,
                            2 => CastleType::Short,
                            _ => unreachable!(),
                        };
                        if self.can_castle(castle_type, piece.colour) {
                            return Ok(Move::new(pos1, MoveType::Castle(castle_type)));
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

    fn get_king_pos(&self, colour: Colour) -> Position {
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

    fn can_castle(&self, castle_type: CastleType, colour: Colour) -> bool {
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

    fn check_san_request(&mut self, move_request: &MoveRequestSAN) -> Result<Move, &'static str> {
        // Most moves can be found by looking at the position and using Game::found_move.
        // However castling and promotions can be handled differently

        // Castling
        if let Some(castle_type) = move_request.castle {
            if self.can_castle(castle_type, self.get_current_colour()) {
                let king_pos = self.get_king_pos(self.get_current_colour());
                return Ok(Move::new(king_pos, MoveType::Castle(castle_type)));
            } else {
                return Err("Invalid castle conditions");
            }
        }

        // Promotions
        if move_request.piece == PieceType::Pawn && move_request.end_pos
                .unwrap()
                .as_white(self.get_current_colour()).1 == 7 {
            if move_request.promotion.is_none() {
                return Err("Must provide promotion piece when doing promotions")
            }
        }

        let pieces: Vec<&Piece> = self.get_all_colour_pieces(self.get_current_colour())
            .filter(|&x| x.kind == move_request.piece)
            .filter(|&x| if let Some(file) = move_request.start_file { x.position.0 == file } else { true } )
            .filter(|&x| if let Some(rank) = move_request.start_rank { x.position.1 == rank } else { true } )
            .collect();
        if pieces.len() == 0 {
            return Err("No piece from the move request has been found on the board.")
        }

        let mut potential_moves: Vec<Move> = pieces.into_iter()
            .map(|x| self.find_move(x.position, move_request.end_pos.unwrap()))
            .filter_map(|x| x.ok())
            .collect();

        return match potential_moves.len() {
            0 => Err("No such move is possible."),
            1 => Ok(potential_moves.remove(0)),
            _ => Err("This move has multiple possibilities. Please make it more specific."),
        }
    }
}

impl Display for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = self.board.iter().rev().map(|row| {
            row.iter()
            .map(|x| {
                if let Some(Piece{ kind: p, colour: c, ..}) = x {
                    let char = match (p, c) {
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
                    };
                    format!("{}", char)
                } else {
                    " ".to_string()
                }
            })
            .collect::<Vec<String>>()
            .join(" ")
        }).collect::<Vec<String>>()
        .join("\n");
        write!(f, "{}", result)
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct Piece {
    kind: PieceType,
    colour: Colour,
    position: Position,
    history: Vec<u8>,
}

impl Piece {
    fn new(kind: PieceType, colour: Colour, position: Position) -> Self {
        Self { kind, colour, position, history: Vec::new() }
    }
}

#[derive(PartialEq, Debug, Clone, Copy, Serialize, Deserialize)]
pub(crate) enum Colour {
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

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum PieceType {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
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
            Move::new("e2".try_into().unwrap(), MoveType::PawnStart)
        );
        // Pawn E2 -> E3
        assert_eq!(
            game.find_move("e2".try_into().unwrap(), "e3".try_into().unwrap()).unwrap(),
            Move::new("e2".try_into().unwrap(), MoveType::PawnPush(None))
        );
        // Knight B1 -> C3
        assert_eq!(
            game.find_move("b1".try_into().unwrap(), "c3".try_into().unwrap()).unwrap(),
            Move::new("b1".try_into().unwrap(), MoveType::Standard("c3".try_into().unwrap()))
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
            Move::new("e1".try_into().unwrap(), MoveType::Castle(CastleType::Short))
        );
        // En Passant: E5 -> D6
        assert_eq!(
            game.find_move("e5".try_into().unwrap(), "d6".try_into().unwrap()).unwrap(),
            Move::new("e5".try_into().unwrap(), MoveType::EnPassant(Direction::Left))
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