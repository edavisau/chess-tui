use std::io::Stdout;

use tui::{Terminal, backend::CrosstermBackend, layout::Rect};

use crate::chess::game::{Colour, Game};
use crate::ui::Layout;

const INPUT_MAX_CHARS: usize = 20;

pub struct App {
    pub state: AppState,
    pub game: Game,
    pub input: String,
    pub size: Option<Rect>,
    pub terminal: Terminal<CrosstermBackend<Stdout>>,
    pub layout: Option<Layout>,
    pub status: Option<Result<String, String>>,
    pub show_help_screen: bool,
}

impl App {
    pub fn from_file(filename: &str) -> Self {
        App { game: Game::load_game(filename), ..Default::default() }
    }

    pub fn add_input_char(&mut self, c: char) {
        if self.input.len() < INPUT_MAX_CHARS {
            self.input.push(c);
        }
    }

    pub fn pop_input_char(&mut self) {
        let _ = self.input.pop();
    }
}

impl Default for App {
    fn default() -> Self {
        let stdout = std::io::stdout();
        let backend = CrosstermBackend::new(stdout);
        let mut terminal = Terminal::new(backend).expect("Cannot create terminal.");
        terminal.clear().expect("Couldn't clear terminal.");
        terminal.hide_cursor().expect("Couldn't hide cursor.");

        Self {
            state: AppState::Unstarted,
            game: Default::default(), 
            input: Default::default(),
            size: None, 
            terminal,
            layout: None,
            status: None,
            show_help_screen: false,
        }
    }
}

pub enum GameResult {
    Checkmate(Colour),
    Draw,
    Stalemate,
    Interrupted,
    Resignation(Colour),
}

pub enum AppState {
    Unstarted,
    Running,
    Finished(GameResult),
}