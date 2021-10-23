use std::io::{self, BufRead};

use clap::load_yaml;
use crossterm::event::{self, KeyModifiers, KeyCode};

mod chess;
use chess::components::PieceType;
use chess::moves::{MoveResult, MoveError};

mod ui;
mod app;
use app::{App, AppState, GameResult};

enum UserAction {
    TryPlayPositions(String, String),
    TryPlaySAN(String),
    SaveGame(String),
    QuitGame,
    Resign,
    OfferDraw,
    CancelDraw,
}

fn choose_promotion() -> PieceType {
    todo!("Choose promotion through the UI");
    println!("Choose a piece to promote to: 1.Queen, 2.Rook, 3.Knight, 4.Bishop");
    let stdin = io::stdin();
    let selection = stdin.lock().lines().next().expect("There was no line.").expect("The line could not be read.")
            .trim()
            .parse::<usize>(); 
    match selection {
        Ok(1) => PieceType::Queen,
        Ok(2) => PieceType::Rook,
        Ok(3) => PieceType::Knight,
        Ok(4) => PieceType::Bishop,
        _ => {
            println!("Invalid choice but we will give you queen anyway.");
            PieceType::Queen
        }
    }
}

fn parse_player_input(player_input: &String) -> Result<UserAction, &'static str> {
    let mut player_input = player_input
        .trim()
        .split_whitespace()
        .map(|x| x.to_string())
        .collect::<Vec<String>>();

    if player_input.len() == 0 {
        return Err("No command written!");
    }

    if player_input[0].starts_with("/") {
        match &player_input[0][1..] {
            "q" => return Ok(UserAction::QuitGame),
            "s" => {
                if player_input.len() >= 2 {
                    return Ok(UserAction::SaveGame(player_input[1].to_string()))
                } else {
                    return Err("Must enter a filename to save game.");
                }
            },
            "r" => return Ok(UserAction::Resign),
            "od" => return Ok(UserAction::OfferDraw),
            "cd" => return Ok(UserAction::CancelDraw),
            _ => return Err("Invalid command. Commands are 's' or 'q'")
        }
    }

    return match player_input.len() {
        0 => unreachable!(),
        1 => Ok(UserAction::TryPlaySAN(player_input.remove(0))),
        2 => {
            let mut player_input = player_input.into_iter();
            let (a, b) = (player_input.next().unwrap(), player_input.next().unwrap());
            Ok(UserAction::TryPlayPositions(a, b))
        },
        _ => Err("Too many tokens.")
    }
}

fn handle_player_input(app: &mut App, player_input: &String) -> Option<Result<String, String>> {
    let action = match parse_player_input(player_input) {
        Ok(action) => action,
        Err(e) => {
            return Some(Err(format!("Invalid input: {}", e)))
        }
    };

    let move_attempted: Option<Result<MoveResult, MoveError>>;
    match action {
        UserAction::TryPlayPositions(p1, p2) => { move_attempted = Some(app.game.try_move_positions(p1, p2, choose_promotion)) },
        UserAction::TryPlaySAN(value) => { move_attempted = Some(app.game.try_move_san(&value[..])) },
        UserAction::SaveGame(filename) => {
            app.game.save_game(&filename);
            return Some(Ok("Saved game".into()))
        },
        UserAction::QuitGame => {
            app.state = AppState::Finished(GameResult::Interrupted);
            return Some(Ok("Saved game".into()))
        },
        UserAction::Resign => {
            let winner = app.game.get_current_colour().flip();
            app.state = AppState::Finished(GameResult::Resignation(winner));
            return Some(Ok(format!("{} has resigned. {} wins!", app.game.get_current_colour(), winner)));
            
        },
        UserAction::OfferDraw => {
            let game_ended = app.game.offer_draw();
            if game_ended {
                app.state = AppState::Finished(GameResult::Draw);
                return Some(Ok("Both sides have agreed to a draw.".into()));
            } else {
                return None;
            }
        },
        UserAction::CancelDraw => {
            app.game.cancel_draw();
            return None;
        }
    }
    
    if let Some(result) = move_attempted {
        match result {
            Ok(MoveResult::MovePlayed) => (),
            Ok(MoveResult::Check(colour)) => return Some(Ok(format!("{} is in check.", colour))),
            Ok(MoveResult::Checkmate(colour)) => {
                app.state = AppState::Finished(GameResult::Checkmate(colour.flip()));
                return Some(Ok(format!("Game over. {} has been checkmated! {} is the winner!", colour, colour.flip())));
            },
            Ok(MoveResult::Stalemate(colour)) => {
                app.state = AppState::Finished(GameResult::Stalemate);
                return Some(Ok(format!("Game over, {} has caused a stalemate. This is a draw.", colour)));
            },
            Err(e) => {
                return Some(Err(format!("Invalid move: {:?}", e)))
            },
        }
    }
    None
}

impl App {
    fn play(&mut self) {
        self.state = AppState::Running;
        loop {
            ui::update_ui(self).unwrap();

            let event = event::read().expect("Encountered crossterm error.");

            // If help screen is currently being showed, ignore event and redraw main layout.
            if self.show_help_screen {
                self.show_help_screen = false;
                continue;
            } else if let AppState::Finished(_) = self.state {
                return;
            }

            match event {
                event::Event::Key(event) => {
                    match (event.code, event.modifiers) {
                        (KeyCode::Char('?'), _) => self.show_help_screen = true,
                        (KeyCode::Char(c), KeyModifiers::NONE | KeyModifiers::SHIFT) => {
                            self.add_input_char(c);
                        },
                        (KeyCode::Backspace, _) => {
                            self.pop_input_char();
                        },
                        (KeyCode::Enter, _) => {
                            let player_input = std::mem::replace(&mut self.input, String::new()); 
                            self.status = handle_player_input(self, &player_input);
                        },
                        _ => ()
                    }
                },
                _ => ()
            }
        }
    }
}

fn main() {
    let yaml = load_yaml!("cli.yml");
    let matches = clap::App::from_yaml(yaml).get_matches();

    if matches.is_present("play") {
        App::default().play()
    } else if let Some(matches) = matches.subcommand_matches("load") {
        let file = matches.value_of("file").unwrap();
        App::from_file(file).play()
    } else {
        unreachable!();
    }
}