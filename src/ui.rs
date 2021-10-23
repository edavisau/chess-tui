use std::io;
use tui::Frame;
use tui::backend::Backend;
use tui::widgets::{Block, Borders, Paragraph};
use tui::layout::{Constraint, Direction, Rect, Alignment};
use tui::style::Style;
use tui::text::Spans;

const BOARD_PRINT_WIDTH: u16 = 20;
const BOARD_PRINT_HEIGHT: u16 = 10;

const MIN_WIDTH: u16 = 50;
const MIN_HEIGHT: u16 = 24;

use crate::app::App;

#[derive(Clone, Copy)]
pub struct Layout {
    pub board: Rect,
    pub history: Rect,
    pub status: Rect,
    pub command: Rect,
}

impl Layout {
    fn new(size: Rect) -> Self {
        let vertical_chunks = tui::layout::Layout::default()
            .direction(Direction::Vertical)
            .margin(2)
            .constraints(
                [
                    Constraint::Min(12),
                    Constraint::Length(3),
                    Constraint::Length(4),
                ].as_ref()
            )
            .split(size);
        
        let horizontal_chunks = tui::layout::Layout::default()
            .direction(Direction::Horizontal)
            .constraints(
                [
                    Constraint::Min(12),
                    Constraint::Length(20),
                ].as_ref()
            )
            .margin(0)
            .split(vertical_chunks[0]);
        
        Self {
            board: horizontal_chunks[0],
            history: horizontal_chunks[1],
            status: vertical_chunks[1],
            command: vertical_chunks[2],
        }
    }
}


pub fn update_ui(app: &mut App) -> Result<(), io::Error> {
    // Handle resize
    if let Ok(size) = app.terminal.backend().size() {
        if app.size.is_none() || (app.size.unwrap() != size) {
            app.terminal.clear()?;
            app.size = Some(size);
            app.layout = Some(Layout::new(size));
        }
    }

    let size = app.size.unwrap();
    if size.width < MIN_WIDTH {
        app.terminal.draw(|f| draw_too_small(f, size, "Width of the screen is too small. Please make it wider."))?;
    }
    else if size.height < MIN_HEIGHT {
        app.terminal.draw(|f| draw_too_small(f, size, "Height of the screen is too small. Please make it taller."))?;
    } else if app.show_help_screen { 
        app.terminal.draw(|f| draw_help_screen(f, size))?;
    } else {
        let layout = app.layout.unwrap();
        app.terminal.draw(|f| {
            draw_board_block(f, layout.board, app.game.display_board_as_colour(app.game.get_current_colour()));
            draw_history_block(f, layout.history, app.game.display_moves());
            draw_status_block(f, layout.status, &app.status);
            draw_command_block(f, layout.command, app.game.get_current_colour().to_string(), &app.input);
        })?;
    }
    Ok(())
}

fn center_inside_rect(rect: Rect, width: u16, height: u16) -> Rect {
    let padding_x = (rect.width - width) / 2;
    let padding_y = (rect.height - height) / 2;
    Rect::new(
        rect.x + padding_x,
        rect.y + padding_y,
        width,
        height
    )
}

fn draw_command_block<B>(f: &mut Frame<B>, rect: Rect, colour: String, input: &String) 
where
    B: Backend 
{
    let block = Block::default()
        .title("Command")
        .borders(Borders::ALL);
    let p = Paragraph::new(format!("Enter a command (press ? for help)\n{} > {}", colour, input.as_str()))
        .block(block);
    f.render_widget(p, rect);
}

fn draw_board_block<B>(f: &mut Frame<B>, rect: Rect, board: String)
where
    B: Backend 
{ 
    let empty_block = Block::default()
        .title("Board")
        .style(Style::default().fg(tui::style::Color::White))
        .borders(Borders::ALL);
    f.render_widget(empty_block, rect);

    let center_rect = center_inside_rect(rect, BOARD_PRINT_WIDTH, BOARD_PRINT_HEIGHT);

    // colour tiles
    for i in 0..8 {
        for j in 0..8 {
            let tile = Rect::new(
                center_rect.x + 2 + i * 2,
                center_rect.y + 1 + j,
                2,
                1
            );
            let color = if (i + j) % 2 == 0 { tui::style::Color::White } else { tui::style::Color::DarkGray };
            let block = Block::default()
                .style(Style::default().bg(color).fg(tui::style::Color::Black));
            f.render_widget(block, tile)
        }
    }

    let p = Paragraph::new(board)
        .alignment(Alignment::Left);
    f.render_widget(p, center_rect);
}

fn draw_history_block<B>(f: &mut Frame<B>, rect: Rect, mut moves: Vec<String>)
where
    B: Backend 
{
    let block = Block::default()
        .title("History")
        .borders(Borders::ALL);

    if moves.len() % 2 != 0 { moves.push("".to_string()) }

    let mut lines: Vec<Spans> = Vec::new();
    for i in 0..moves.len() / 2 {
        lines.push(Spans::from(
            format!(" {}: {:5} {:5} ", i+1, moves[i*2], moves[i*2 + 1])
        ));
    }

    let p = Paragraph::new(lines)
        .block(block)
        .alignment(Alignment::Left);
    f.render_widget(p, rect);
}

fn draw_status_block<B>(f: &mut Frame<B>, rect: Rect, status: &Option<Result<String, String>>)
where
    B: Backend 
{
    let (status_text, is_err): (&str, bool) = match status {
        Some(Ok(ref message)) => (message.as_str(), false),
        Some(Err(ref message)) => (message.as_str(), true),
        None => ("", false),
    };

    let block = Block::default()
        .title("Status")
        .borders(Borders::ALL)
        .style(Style::default().bg(if is_err { tui::style::Color::Red } else { tui::style::Color::Black } ));
    let p = Paragraph::new(status_text)
        .block(block);
    f.render_widget(p, rect);
}

fn draw_too_small<B>(f: &mut Frame<B>, rect: Rect, message: &str)
where
    B: Backend 
{
    let p = Paragraph::new(message);
    f.render_widget(p, rect);
}


fn draw_help_screen<B>(f: &mut Frame<B>, rect: Rect)
where
    B: Backend 
{
    const HELP_STR: &'static str = "press any key to exit\n\
    \n\
    How to use: enter a move using the notation below or a command starting with '/'\n\
    Move Notation:\n\
    \tAbsolute positions: 'e2 e4', 'b8 c6', 'a5 b6'\n\
    \tStandard notation: 'Nc3', 'd5', 'fxg6', 'Q3xf4#'\n\
    \n\
    Commands:\n\
    \t/h: print help\n\
    \t/s <file>: save game to file\n\
    \t/q: quit game\n";
    let p = Paragraph::new(HELP_STR);
    f.render_widget(p, rect);
}