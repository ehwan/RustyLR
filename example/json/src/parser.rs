// https://www.json.org/json-en.html

%% 

%tokentype char;
%start Json;
%eof '\0';

Json: Element;

Value: Object
     | Array
     | String
     | Number
     | "true"
     | "false"
     | "null"
     ;

Object: '{' WS '}'
      | '{' Members '}';

Members: Member
       | Member ',' Members
       ;

Member: WS String WS ':' Element;

Array: '[' WS ']'
     | '[' Elements ']'
     ;

Elements: Element
        | Element ',' Elements
        ;

Element: WS Value WS;

String: '"' Characters '"';

Characters: Character*;

// WIP
Character:
          '\\' Escape
        | ['\u{0020}'-'\u{FF}'] - '"' - '\\'
        ;

Escape: '"'
      | '\\'
      | '/'
      | 'b'
      | 'f'
      | 'n'
      | 'r'
      | 't'
      | 'u' Hex Hex Hex Hex
      ;

Hex: Digit
   | ['A'-'F']
   | ['a'-'f']
   ;

Number: Integer Fraction Exponent;

Integer: Digit
       | Onenine Digits
       | '-' Digit
       | '-' Onenine Digits
       ;

Digits: Digit+;

Digit: ['0'-'9'];

Onenine: ['1'-'9'];

Fraction: ('.' Digits)?;

Exponent: ""
        | 'E' Sign Digits
        | 'e' Sign Digits
        ;

Sign: "" | '+' | '-';

WS: ""
  | '\u{0020}' WS
  | '\u{000A}' WS
  | '\u{000D}' WS
  | '\u{0009}' WS
  ;