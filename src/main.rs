use std::convert::TryFrom;
use std::io::{self, BufRead, Write};

use clap::{App, load_yaml};

mod chess;
use chess::game;
use chess::moves;

fn choose_promotion() -> game::PieceType {
    println!("Choose a piece to promote to: 1.Knight, 2.Bishop, 3.Rook, 4.Queen", );
    let stdin = io::stdin();
    let selection = stdin.lock().lines().next().expect("There was no line.").expect("The line could not be read.")
            .trim()
            .parse::<usize>(); 
    match selection {
        Ok(1) => game::PieceType::Knight,
        Ok(2) => game::PieceType::Bishop,
        Ok(3) => game::PieceType::Rook,
        Ok(4) => game::PieceType::Queen,
        _ => {
            println!("Invalid choice but we will give you queen anyway.");
            game::PieceType::Queen
        }
    }
}

fn request_action() -> Result<chess::game::Action, &'static str> {
    let stdin = io::stdin();
    let player_input = stdin.lock()
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
            "q" => return Ok(game::Action::QuitGame),
            "s" => {
                if player_input.len() >= 2 {
                    return Ok(game::Action::SaveGame(player_input[1].to_string()))
                } else {
                    return Err("Must enter a filename to save game.");
                }
            }
            _ => return Err("Invalid command. Commands are 's' or 'q'")
        }
    }

    let positions: Vec<moves::Position> = match player_input.into_iter()
            .map(|x| moves::Position::try_from(&x[..]))
            .collect() {
        Ok(positions) => positions,
        _ => return Err("Invalid positions entered."),
    };
    if positions.len() != 2 {
        return Err("Must write only two positions separated by whitespace!");
    }
    return Ok(game::Action::TryPlay(positions[0], positions[1]));
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

            match action {
                chess::game::Action::TryPlay(p1, p2) => {
                    match game.try_move_positions(p1, p2, choose_promotion) {
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
                },
                game::Action::SaveGame(filename) => game.save_game(&filename),
                game::Action::QuitGame => return,
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