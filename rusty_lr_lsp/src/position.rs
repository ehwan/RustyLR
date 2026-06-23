use lsp_types::{Position, Range};

/// Converts a 0-indexed byte offset in `content` into an LSP `Position` (line, character).
/// The LSP character index represents the UTF-16 code unit offset on that line.
pub fn offset_to_position(content: &str, offset: usize) -> Position {
    let mut line = 0;
    let mut character = 0;
    let mut current_offset = 0;

    for c in content.chars() {
        if current_offset >= offset {
            break;
        }
        let char_len = c.len_utf8();
        if current_offset + char_len > offset {
            break;
        }
        current_offset += char_len;

        if c == '\n' {
            line += 1;
            character = 0;
        } else {
            character += c.len_utf16() as u32;
        }
    }
    Position::new(line, character)
}

/// Converts an LSP `Position` (line, character) back into a 0-indexed byte offset in `content`.
pub fn position_to_offset(content: &str, pos: Position) -> usize {
    let mut line = 0;
    let mut character = 0;
    let mut byte_offset = 0;

    for c in content.chars() {
        if line == pos.line && character >= pos.character {
            break;
        }
        byte_offset += c.len_utf8();

        if c == '\n' {
            line += 1;
            character = 0;
        } else {
            character += c.len_utf16() as u32;
        }
    }
    byte_offset
}

/// Converts a `std::ops::Range<usize>` byte range into an LSP `Range`.
pub fn range_to_lsp_range(content: &str, range: std::ops::Range<usize>) -> Range {
    Range::new(
        offset_to_position(content, range.start),
        offset_to_position(content, range.end),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_conversion() {
        let content = "hello\nworld\n안녕 하세요\n😀 hello";

        // Test ASCII character
        let pos = offset_to_position(content, 0);
        assert_eq!(pos.line, 0);
        assert_eq!(pos.character, 0);
        assert_eq!(position_to_offset(content, pos), 0);

        let pos = offset_to_position(content, 6); // 'w' in 'world'
        assert_eq!(pos.line, 1);
        assert_eq!(pos.character, 0);
        assert_eq!(position_to_offset(content, pos), 6);

        // Test multi-byte UTF-8 character (Korean '안' is 3 bytes in UTF-8, 1 code unit in UTF-16)
        let pos = offset_to_position(content, 12); // start of '안'
        assert_eq!(pos.line, 2);
        assert_eq!(pos.character, 0);
        assert_eq!(position_to_offset(content, pos), 12);

        let pos = offset_to_position(content, 15); // after '안', start of '녕'
        assert_eq!(pos.line, 2);
        assert_eq!(pos.character, 1);
        assert_eq!(position_to_offset(content, pos), 15);

        // Test Emoji (😀 is 4 bytes in UTF-8, 2 code units in UTF-16)
        let pos = offset_to_position(content, 29); // start of emoji
        assert_eq!(pos.line, 3);
        assert_eq!(pos.character, 0);
        assert_eq!(position_to_offset(content, pos), 29);

        let pos = offset_to_position(content, 33); // after emoji, before space
        assert_eq!(pos.line, 3);
        assert_eq!(pos.character, 2);
        assert_eq!(position_to_offset(content, pos), 33);
    }
}
