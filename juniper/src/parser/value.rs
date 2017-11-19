use ast::InputValue;

use parser::{ParseError, ParseResult, Parser, Spanning, Token};

pub fn parse_value_literal(
    parser: &mut Parser,
    is_const: bool,
) -> ParseResult<InputValue> {
    match parser.peek().clone() {
        Spanning {
            item: Token::BracketOpen,
            ..
        } => parse_list_literal(parser, is_const),
        Spanning {
            item: Token::CurlyOpen,
            ..
        } => parse_object_literal(parser, is_const),
        Spanning {
            item: Token::Dollar,
            ..
        } if !is_const =>
        {
            parse_variable_literal(parser)
        }
        Spanning {
            item: Token::Int(i),
            ..
        } => Ok(parser.next()?.map(|_| InputValue::int(i))),
        Spanning {
            item: Token::Float(f),
            ..
        } => Ok(parser.next()?.map(|_| InputValue::float(f))),
        Spanning {
            item: Token::String(_),
            ..
        } => Ok(parser.next()?.map(|t| if let Token::String(s) = t {
            InputValue::string(s)
        } else {
            panic!("Internal parser error");
        })),
        Spanning {
            item: Token::Name(ref n),
            ..
        } if *n == "true" => Ok(parser.next()?.map(|_| InputValue::boolean(true))),
        Spanning {
            item: Token::Name(ref n),
            ..
        } if *n == "false" => Ok(parser.next()?.map(|_| InputValue::boolean(false))),
        Spanning {
            item: Token::Name(ref n),
            ..
        } if *n == "null" => Ok(parser.next()?.map(|_| InputValue::null())),
        Spanning {
            item: Token::Name(ref name),
            ..
        } => Ok(
            parser
                .next()?
                .map(|_| InputValue::enum_value(name)),
        ),
        _ => Err(parser.next()?.map(ParseError::UnexpectedToken)),
    }
}

fn parse_list_literal(parser: &mut Parser, is_const: bool) -> ParseResult<InputValue> {
    Ok(
        try!(parser.delimited_list(
            &Token::BracketOpen,
            |p| parse_value_literal(p, is_const),
            &Token::BracketClose
        )).map(InputValue::parsed_list),
    )
}

fn parse_object_literal(
    parser: &mut Parser,
    is_const: bool,
) -> ParseResult<InputValue> {
    Ok(
        try!(parser.delimited_list(
            &Token::CurlyOpen,
            |p| parse_object_field(p, is_const),
            &Token::CurlyClose
        )).map(|items| {
            InputValue::parsed_object(items.into_iter().map(|s| s.item).collect())
        }),
    )
}

fn parse_object_field(
    parser: &mut Parser,
    is_const: bool,
) -> ParseResult<(Spanning<String>, Spanning<InputValue>)> {
    let key = try!(parser.expect_name());

    try!(parser.expect(&Token::Colon));

    let value = try!(parse_value_literal(parser, is_const));

    Ok(Spanning::start_end(
        &key.start.clone(),
        &value.end.clone(),
        (key.map(|s| s.to_string()), value),
    ))
}

fn parse_variable_literal(parser: &mut Parser) -> ParseResult<InputValue> {
    let Spanning {
        start: start_pos, ..
    } = try!(parser.expect(&Token::Dollar));
    let Spanning {
        item: name,
        end: end_pos,
        ..
    } = try!(parser.expect_name());

    Ok(Spanning::start_end(
        &start_pos,
        &end_pos,
        InputValue::variable(name.to_owned()),
    ))
}
