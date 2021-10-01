#![allow(dead_code, unused_variables)]

use std::convert::TryFrom;
use std::io::{self, BufRead, Write};

use crate::chess::PieceType;

mod chess {
    use std::fmt::Display;
    use std::convert::{TryFrom, TryInto};

    const BOARD_SIZE: usize = 8;

    pub(crate) struct Game {
        board: Vec<Vec<Option<Piece>>>,
        current_turn: Colour,
        count: u8,
        moves: Vec<Move>,
        captured: Vec<Piece>,
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
        pub fn new() -> Self {
            Self::default()
        }

        pub fn get_current_count(&self) -> u8 {
            self.count
        }

        pub fn get_current_colour(&self) -> Colour {
            self.current_turn
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
                MoveType::Standard(p1, p2) => {
                    self.capture_piece(*p2);
                    self.swap_pieces(*p1, *p2);
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
                MoveType::Promotion(p1, p2, piece_type) => {
                    self.capture_piece(*p2);
                    self.swap_pieces(*p1, *p2);

                    // Update piece type
                    self.get_piece_mut(*p2).as_mut().unwrap().kind = *piece_type;
                },
                MoveType::EnPassant(p1, p2) => {
                    self.swap_pieces(*p1, *p2);
                    let diff = PosDiff(0, -1).as_white(self.current_turn);
                    self.capture_piece(p2.add(diff).unwrap());
                },
            }
        }

        fn undo_move(&mut self, move_: &Move) {
            match &move_.kind {
                MoveType::Standard(p1, p2) => {
                    self.swap_pieces(*p2, *p1);
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
                MoveType::Promotion(p1, p2, piece_type) => {
                    self.get_piece_mut(*p2).as_mut().unwrap().kind = PieceType::Pawn;
                    self.swap_pieces(*p2, *p1);
                    self.uncapture_piece(*p2);
                },
                MoveType::EnPassant(p1, p2) => {
                    let diff = PosDiff(0, -1).as_white(self.current_turn);
                    self.uncapture_piece(p2.add(diff).unwrap());
                    self.swap_pieces(*p2, *p1);
                },
            }
        }

        pub fn try_move_positions(&mut self, pos1: Position, pos2: Position, promotion_callback: fn() -> PieceType) -> Result<(), MoveError> {
            if pos1 == pos2 {
                return Err(MoveError::InvalidMovement);
            }

            let mut found_move: Move = match self.get_piece(pos1).as_ref() {
                Some(piece) => {
                    if piece.colour == self.current_turn {
                        self.find_move(pos1, pos2)?
                    } else {
                        return Err(MoveError::InvalidColour)
                    }
                },
                None => return Err(MoveError::NoPiece),
            };

            // Specify promotion piece
            if let MoveType::Promotion(_, _, promotion_piece) = &mut found_move.kind {
                *promotion_piece = promotion_callback();
            } 

            self.try_move(found_move)?;
            Ok(())
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

        fn try_move(&mut self, move_: Move) -> Result<(), MoveError> {
            self.check_move(&move_, false)?;

            // Update game
            self.moves.push(move_);
            self.count += 1;
            self.current_turn = self.current_turn.flip();
            self.in_check = self.is_in_check(self.current_turn);
            if self.in_check { // Is it checkmate???
                let potential_moves = self.get_all_colour_pieces(self.current_turn)
                    .map(|x| self.get_available_moves(x.position))
                    .flatten()
                    .map(|(pos1, pos2)| self.find_move(pos1, pos2))
                    .filter_map(|x| x.ok())
                    .collect::<Vec<Move>>();

                let checkmate = potential_moves.iter()
                    .map(|x| self.check_move(x, true))
                    .filter_map(|x| x.ok())
                    .next()
                    .is_none();
                if checkmate {
                    println!("CHECKMATE FOR {}", self.current_turn);
                } else {
                    println!("{} is in check, but not checkmate.", self.current_turn);
                }
            }

            Ok(())
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
                            return Ok(Move::new(piece.kind, MoveType::Standard(pos1, pos2)));
                        },
                        PosDiff(0, 1) => { // Move forward
                            if let &Some(_) = self.get_piece(pos2) { return Err(MoveError::PositionBlocked) }
                            if promotion {
                                return Ok(Move::new(piece.kind, MoveType::Promotion(pos1, pos2, PieceType::Queen)));
                            } else {
                                return Ok(Move::new(piece.kind, MoveType::Standard(pos1, pos2)));
                            }
                        }, 
                        PosDiff(1|-1, 1) => {  // Attack or En Passant
                            if let &Some(_) = self.get_piece(pos2) { // Standard attack
                                if promotion {
                                    return Ok(Move::new(piece.kind, MoveType::Promotion(pos1, pos2, PieceType::Queen)));
                                } else {
                                    return Ok(Move::new(piece.kind, MoveType::Standard(pos1, pos2)));
                                }
                            } else { // En Passant
                                let last_move = self.moves.last().ok_or(MoveError::InvalidEnPassantConditions)?;
                                if pos_as_white.1 == 4 && last_move.piece == PieceType::Pawn {
                                    if let MoveType::Standard(last_pos1, last_pos2) = last_move.kind {
                                        if last_pos1 == pos1.add(PosDiff(diff.0, diff.1 * 2)).unwrap() &&
                                                last_pos2 == pos1.add(PosDiff(diff.0, 0)).unwrap() {
                                            return Ok(Move::new(piece.kind, MoveType::EnPassant(pos1, pos2)));
                                        }
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
                    return Ok(Move::new(piece.kind, MoveType::Standard(pos1, pos2)));
                },
                PieceType::Knight => {
                    if !((diff.0.abs() == 2 && diff.1.abs() == 1) || (diff.0.abs() == 1 && diff.1.abs() == 2)) {return Err(MoveError::InvalidMovement)}
                    return Ok(Move::new(piece.kind, MoveType::Standard(pos1, pos2)));
                },
                PieceType::Bishop => {
                    if !((diff.0 == -diff.1) || (diff.0 == diff.1)) {return Err(MoveError::InvalidMovement)}
                    can_reach_between(self, pos1, diff)?;
                    return Ok(Move::new(piece.kind, MoveType::Standard(pos1, pos2)));
                },
                PieceType::Queen => {
                    can_reach_between(self, pos1, diff)?;
                    return Ok(Move::new(piece.kind, MoveType::Standard(pos1, pos2)));
                },
                PieceType::King => {
                    match diff {
                        PosDiff(0|-1|1, 0|-1|1) => return Ok(Move::new(piece.kind, MoveType::Standard(pos1, pos2))),
                        PosDiff(-2|2, 0) => { // Castle
                            let castle_type = match diff.0 {
                                -2 => CastleType::Long,
                                2 => CastleType::Short,
                                _ => unreachable!(),
                            };
                            let row = if self.current_turn == Colour::White { 0 } else { 7 };
                            let rook_position: Position = match castle_type {
                                CastleType::Long => (0, row).try_into().unwrap(),
                                CastleType::Short => (7, row).try_into().unwrap(),
                            };

                            // King must not have moved
                            if piece.history.len() > 0 {
                                return Err(MoveError::InvalidCastleConditions);
                            }
                            // Rook must not have moved
                            match self.get_piece(rook_position) {
                                Some(p) if p.history.len() == 0 => (),
                                _ => return Err(MoveError::InvalidCastleConditions),
                            }
                            // No pieces between king and rook
                            for pos in (if castle_type == CastleType::Long { 1..=3 } else { 5..=6 })
                                    .map(|x| Position::try_from((x, row)).unwrap()) {
                                if let Some(_) = self.get_piece(pos) {
                                    return Err(MoveError::InvalidCastleConditions)
                                }
                            }
                            // King is not in check
                            if self.in_check {
                                return Err(MoveError::InvalidCastleConditions)
                            }
                            // Square next to king is in check
                            let intermediate_square_position: Position = if castle_type == CastleType::Long { 
                                (3, row).try_into().unwrap()
                            } else { 
                                (5, row).try_into().unwrap()
                            };
                            if self.is_attacked(piece.colour, intermediate_square_position) {
                                return Err(MoveError::InvalidCastleConditions)
                            }
                            // We can ignore checking if end position is in check. It will be checked later.
                            return Ok(Move::new(piece.kind, MoveType::Castle(castle_type)))
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

        /// Checks if a colour is in check
        fn is_in_check(&self, colour: Colour) -> bool {
            let king_pos = self.iterate_pieces()
                .filter(move |&x| x.colour == colour && x.kind == PieceType::King)
                .next()
                .unwrap()
                .position;
            return self.is_attacked(colour, king_pos);
        }
    }

    #[test]
    fn test_find_move() {
        let game = Game::new();

        // Pawn E2 -> E4
        assert_eq!(
            game.find_move("E2".try_into().unwrap(), "E4".try_into().unwrap()).unwrap(),
            Move::new(PieceType::Pawn, MoveType::Standard("E2".try_into().unwrap(), "E4".try_into().unwrap()))
        );

        // Queen D1 -> B3
        assert_eq!(
            game.find_move("D1".try_into().unwrap(), "B3".try_into().unwrap()).err().unwrap(),
            MoveError::PositionBlocked
        );

        // Bishop F8 -> A3
        assert_eq!(
            game.find_move("F8".try_into().unwrap(), "A3".try_into().unwrap()).err().unwrap(),
            MoveError::PositionBlocked
        );

        // Bishop F8 -> A4
        assert_eq!(
            game.find_move("F8".try_into().unwrap(), "A4".try_into().unwrap()).err().unwrap(),
            MoveError::InvalidMovement
        );

        // Queen D1 -> E1
        assert_eq!(
            game.find_move("D1".try_into().unwrap(), "E1".try_into().unwrap()).err().unwrap(),
            MoveError::AttackingSameColour
        );

    }

    impl Display for Game {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let result = self.board.iter().rev().map(|row| {
                row.iter()
                .map(|x| {
                    if let Some(Piece{ kind: p, colour: c, ..}) = x {
                        let p = match p {
                            &PieceType::Pawn => 'P',
                            &PieceType::Rook => 'R',
                            &PieceType::Knight => 'N',
                            &PieceType::Bishop => 'B',
                            &PieceType::Queen => 'Q',
                            &PieceType::King => 'K',
                        };
                        let c = match c {
                            &Colour::White => 'W',
                            &Colour::Black => 'B',
                        };
                        format!("{}{}", p, c)
                    } else {
                        "  ".to_string()
                    }
                })
                .collect::<Vec<String>>()
                .join(" ")
            }).collect::<Vec<String>>()
            .join("\n");
            write!(f, "{}", result)
        }
    }

    #[derive(Debug)]
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

    #[derive(PartialEq, Debug, Clone, Copy)]
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
        fn flip(&self) -> Self {
            match self {
                Colour::White => Colour::Black,
                Colour::Black => Colour::White,
            }
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq)]
    pub(crate) enum PieceType {
        Pawn,
        Rook,
        Knight,
        Bishop,
        Queen,
        King,
    }

    #[derive(Debug, PartialEq)]
    struct Move {
        kind: MoveType,
        piece: PieceType,
    }

    impl Move {
        fn new(piece: PieceType, kind: MoveType) -> Self {
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
    }

    #[derive(Debug, PartialEq)]
    enum MoveType {
        Standard(Position, Position),
        Castle(CastleType),
        Promotion(Position, Position, PieceType),
        EnPassant(Position, Position)
    }

    #[derive(Debug, PartialEq)]
    enum CastleType {
        Long,
        Short,
    }

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub struct Position(usize, usize);

    #[derive(Clone, Copy, Debug, PartialEq)]
    pub struct PosDiff(isize, isize);

    impl Position {
        fn is_valid(value: (usize, usize)) -> bool {
            (value.0 < BOARD_SIZE) && (value.1 < BOARD_SIZE)
        }

        fn add(&self, diff: PosDiff) -> Result<Self, &'static str> {
            let (new_x, new_y) = (self.0 as isize + diff.0, self.1 as isize + diff.1);
            if new_x >= 0 && new_y >= 0 {
                Position::try_from((new_x as usize, new_y as usize))
            } else {
                Err("New coordinates are not positive")
            }
        }

        /// Finds the difference between two positions
        fn diff(pos1: Position, pos2: Position) -> PosDiff {
            let x: isize = pos1.0 as isize - pos2.0 as isize;
            let y: isize = pos1.1 as isize - pos2.1 as isize;
            PosDiff(x, y)
        }

        fn as_white(self, colour: Colour) -> Self {
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

            let alphabet = vec!['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'];
            let c = value.next().ok_or("Invalid length")?;
            let x: usize = match alphabet.iter().position(|&x| x == c) {
                Some(x) => x,
                None => return Err("Column must be A-Z (capitals)")
            };

            let y = match usize::from_str_radix(value.as_str(), 10) {
                Ok(y) if (1 <= y) && (y <= 8) => y - 1,
                _ => return Err("Row must be 1-8")
            };
            Ok(Self (x, y))
        }
    }

    impl PosDiff {
        fn as_white(self, colour: Colour) -> Self {
            match colour {
                Colour::White => self,
                Colour::Black => Self(-self.0, -self.1),
            }
        }
    }

    #[test]
    fn test_position_try_from_str() {
        let good1 = "C3".to_string();
        assert_eq!(Position::try_from(&good1[..]), Ok(Position(2, 2)));
        let good2 = "H8".to_string();
        assert_eq!(Position::try_from(&good2[..]), Ok(Position(7, 7)));
        let bad1 = "A9".to_string();
        assert!(Position::try_from(&bad1[..]).is_err());
        let bad2 = "K1".to_string();
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
        let position = Position::try_from("G8").unwrap();
        assert_eq!(position.add(PosDiff(-3, -2)).unwrap(), Position::try_from("D6").unwrap());
        assert!(position.add(PosDiff(2, 1)).is_err());
    }

    #[test]
    fn test_position_diff() {
        let pos1 = Position::try_from("G8").unwrap();
        let pos2 = Position::try_from("A2").unwrap();
        let pos3 = Position::try_from("C1").unwrap();
        assert_eq!(Position::diff(pos1, pos2), PosDiff(6, 6));
        assert_eq!(Position::diff(pos2, pos1), PosDiff(-6, -6));
        assert_eq!(Position::diff(pos3, pos2), PosDiff(2, -1));
    }

    #[test]
    fn test_position_as_white() {
        let pos1 = Position::try_from("G8").unwrap();
        assert_eq!(pos1.as_white(Colour::Black), Position::try_from("B1").unwrap());
        assert_eq!(pos1.as_white(Colour::White), pos1);
    }

    #[test]
    fn test_posdiff_as_white() {
        let diff = PosDiff(3, -2);
        assert_eq!(diff.as_white(Colour::Black), PosDiff(-3, 2));
        assert_eq!(diff.as_white(Colour::White), diff);
    }
    
}


fn play_game() {
    fn choose_promotion() -> chess::PieceType {
        println!("Choose a piece to promote to: 1.Knight, 2.Bishop, 3.Rook, 4.Queen", );
        let stdin = io::stdin();
        let selection = stdin.lock().lines().next().expect("There was no line.").expect("The line could not be read.")
                .trim()
                .parse::<usize>(); 
        match selection {
            Ok(1) => PieceType::Knight,
            Ok(2) => PieceType::Bishop,
            Ok(3) => PieceType::Rook,
            Ok(4) => PieceType::Queen,
            _ => {
                println!("Invalid choice but we will give you queen anyway.");
                PieceType::Queen
            }
        }
    }
    let mut game = chess::Game::new();
    loop {
        println!("{}", game);
        println!("{}. {} Turn", game.get_current_count() + 1, game.get_current_colour());
        let mut positions: Vec<chess::Position>;
        loop {
            print!("Move: ");
            io::stdout().flush().expect("Could not flush!");
            
            let stdin = io::stdin();
            positions = match stdin.lock()
                .lines()
                .next()
                .expect("There was no line.")
                .expect("The line could not be read.")
                .trim()
                .split_whitespace()
                .map(|x| chess::Position::try_from(x))
                .collect() {
                    Ok(positions) => positions,
                    Err(e) => {
                        println!("Errors: {}", e);
                        continue;
                    }
            };
            if positions.len() != 2 {
                println!("Must write two positions separated by whitespace!");
                continue;
            }
            
            if let Err(e) = game.try_move_positions(positions[0], positions[1], choose_promotion) {
                println!("Invalid move: {:?}", e);
                continue;
            }

            break;
        }
    }
}

fn main() {
    play_game();
}