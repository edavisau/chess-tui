use std::io::Stdout;

use tui::{Terminal, backend::CrosstermBackend, layout::Rect};

use crate::chess::game::Game;
use crate::chess::components::Colour;
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
    pub history_scroll: usize,
}

impl App {
    pub fn from_file(filename: &str) -> Self {
        App { game: Game::load_game(filename).unwrap(), ..Default::default() }
    }

    pub fn add_input_char(&mut self, c: char) {
        if self.input.len() < INPUT_MAX_CHARS {
            self.input.push(c);
        }
    }

    pub fn pop_input_char(&mut self) {
        let _ = self.input.pop();
    }

    /// Ensures that history scroll isn't too large. Corrects it if it is.
    pub fn check_history_scroll(&mut self) {
        let num_lines: usize = (self.game.get_current_count() as f32 / 2_f32).ceil() as usize;
        let layout_height: usize = self.layout.unwrap().history.height as usize - 2;
        if self.history_scroll > num_lines.checked_sub(layout_height).unwrap_or(0) {
            self.reset_history_scroll();
        }
    }

    /// Resets history scroll so that the latest moves are shown.
    pub fn reset_history_scroll(&mut self) {
        let num_lines: usize = (self.game.get_current_count() as f32 / 2_f32).ceil() as usize;
        let layout_height: usize = self.layout.unwrap().history.height as usize - 2;
        self.history_scroll = num_lines.checked_sub(layout_height).unwrap_or(0);
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
            history_scroll: 0,
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