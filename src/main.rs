use std::io::{self, BufRead, Write};

use chess::moves::MoveResult;
use clap::{App, load_yaml};

mod chess;
use chess::game;
use chess::moves;

use crate::chess::moves::MoveError;

enum Action {
    TryPlayPositions(String, String),
    TryPlaySAN(String),
    SaveGame(String),
    QuitGame,
}


fn choose_promotion() -> game::PieceType {
    println!("Choose a piece to promote to: 1.Queen, 2.Rook, 3.Knight, 4.Bishop");
    let stdin = io::stdin();
    let selection = stdin.lock().lines().next().expect("There was no line.").expect("The line could not be read.")
            .trim()
            .parse::<usize>(); 
    match selection {
        Ok(1) => game::PieceType::Queen,
        Ok(2) => game::PieceType::Rook,
        Ok(3) => game::PieceType::Knight,
        Ok(4) => game::PieceType::Bishop,
        _ => {
            println!("Invalid choice but we will give you queen anyway.");
            game::PieceType::Queen
        }
    }
}

fn request_action() -> Result<Action, &'static str> {
    let stdin = io::stdin();
    let mut player_input = stdin.lock()
        .lines()
        .next()
        .expect("There was no line.")
        .expect("The line could not be read.")
        .trim()
        .split_whitespace()
        .map(|x| x.to_string())
        .collect::<Vec<String>>();

    if player_input.len() == 0 {
        return Err("No command written!");
    }

    if player_input[0].starts_with("/") {
        match &player_input[0][1..] {
            "q" => return Ok(Action::QuitGame),
            "s" => {
                if player_input.len() >= 2 {
                    return Ok(Action::SaveGame(player_input[1].to_string()))
                } else {
                    return Err("Must enter a filename to save game.");
                }
            }
            _ => return Err("Invalid command. Commands are 's' or 'q'")
        }
    }

    return match player_input.len() {
        0 => unreachable!(),
        1 => Ok(Action::TryPlaySAN(player_input.remove(0))),
        2 => {
            let b = player_input.remove(1); // unpacking backwards because Vec is array-based. There must be a nicer way.
            let a = player_input.remove(0);
            Ok(Action::TryPlayPositions(a, b))
        },
        _ => Err("Too many tokens.")
    }
}

fn play_game(mut game: game::Game) {
    loop {
        println!("{}", game);
        println!("{}. {}'s Turn", game.get_current_count() + 1, game.get_current_colour());
        loop {
            print!("Move: ");
            io::stdout().flush().expect("Could not flush!");
            
            let action = match request_action() {
                Ok(action) => action,
                Err(e) => {
                    println!("Invalid input: {}", e);
                    continue;
                }
            };

            let mut move_attempted: Option<Result<MoveResult, MoveError>> = None;
            match action {
                Action::TryPlayPositions(p1, p2) => { move_attempted = Some(game.try_move_positions(p1, p2, choose_promotion)) },
                Action::TryPlaySAN(value) => { move_attempted = Some(game.attempt_san_move(&value[..], choose_promotion)) },
                Action::SaveGame(filename) => game.save_game(&filename),
                Action::QuitGame => return,
            }

            if let Some(result) = move_attempted {
                match result {
                    Ok(moves::MoveResult::MovePlayed) => (),
                    Ok(moves::MoveResult::Check(colour)) => println!("{} is in check.", colour),
                    Ok(moves::MoveResult::Checkmate(colour)) => {
                        println!("{}", game);
                        println!("Game over, {} has been checkmated! {} is the winner!", colour, colour.flip());
                        return;
                    }
                    Err(e) => {
                        println!("Invalid move: {:?}", e);
                        continue;
                    }
                }
            }
            
            println!(""); // Blank line
            break;
        }
    }
}

fn main() {
    let yaml = load_yaml!("cli.yml");
    let matches = App::from_yaml(yaml).get_matches();

    if matches.is_present("play") {
        play_game(game::Game::new());
    } else if let Some(matches) = matches.subcommand_matches("load") {
        let file = matches.value_of("file").unwrap();
        play_game(game::Game::load_game(file));
    } else {
        unreachable!();
    }
}