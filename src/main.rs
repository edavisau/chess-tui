use std::io::{self, BufRead, Write};

use clap::{App, load_yaml};

mod chess;
use chess::game::{Game, PieceType};
use chess::moves::{MoveResult, MoveError};

enum Action {
    TryPlayPositions(String, String),
    TryPlaySAN(String),
    SaveGame(String),
    QuitGame,
    PrintHelp,
    Resign,
    OfferDraw,
    CancelDraw,
}

fn print_help() {
    println!("How to use: enter a move using the notation below or a command starting with '/'\n\
    Move Notation:\n\
    \tAbsolute positions: 'e2 e4', 'b8 c6', 'a5 b6'\n\
    \tStandard notation: 'Nc3', 'd5', 'fxg6', 'Q3xf4#'\n\
    \n\
    Commands:\n\
    \t/h: print help\n\
    \t/s <file>: save game to file\n\
    \t/q: quit game\n");
}

fn choose_promotion() -> PieceType {
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
            },
            "h" => return Ok(Action::PrintHelp),
            "r" => return Ok(Action::Resign),
            "od" => return Ok(Action::OfferDraw),
            "cd" => return Ok(Action::CancelDraw),
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

fn play_game(mut game: Game) {
    print_help();
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

            let move_attempted: Option<Result<MoveResult, MoveError>>;
            match action {
                Action::TryPlayPositions(p1, p2) => { move_attempted = Some(game.try_move_positions(p1, p2, choose_promotion)) },
                Action::TryPlaySAN(value) => { move_attempted = Some(game.try_move_san(&value[..])) },
                Action::SaveGame(filename) => {
                    game.save_game(&filename);
                    continue;
                },
                Action::QuitGame => return,
                Action::PrintHelp => {
                    print_help(); 
                    continue;
                },
                Action::Resign => {
                    println!("You have resigned. {} wins!", game.get_current_colour().flip());
                    return
                },
                Action::OfferDraw => {
                    let game_ended = game.offer_draw();
                    if game_ended {
                        println!("Both sides have agreed to a draw.");
                        return;
                    } else {
                        continue;
                    }
                },
                Action::CancelDraw => {
                    game.cancel_draw();
                    continue;
                }
            }

            if let Some(result) = move_attempted {
                match result {
                    Ok(MoveResult::MovePlayed) => (),
                    Ok(MoveResult::Check(colour)) => println!("{} is in check.", colour),
                    Ok(MoveResult::Checkmate(colour)) => {
                        println!("{}", game);
                        println!("Game over, {} has been checkmated! {} is the winner!", colour, colour.flip());
                        return;
                    },
                    Ok(MoveResult::Stalemate(colour)) => {
                        println!("{}", game);
                        println!("Game over, {} has caused a stalemate. This is a draw.", colour);
                        return;
                    },
                    Err(e) => {
                        println!("Invalid move: {:?}", e);
                        continue;
                    },
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
        play_game(Game::new());
    } else if let Some(matches) = matches.subcommand_matches("load") {
        let file = matches.value_of("file").unwrap();
        play_game(Game::load_game(file));
    } else {
        unreachable!();
    }
}