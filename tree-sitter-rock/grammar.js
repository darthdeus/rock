/**
 * @file Rock is good language
 * @author Jakub Arnold <darthdeus@gmail.com>
 * @license MPL
 */

function sepBy1(sep, rule) {
  return seq(rule, repeat(seq(sep, rule)));
}
function sepBy(sep, rule) {
  return optional(sepBy1(sep, rule));
}
function commaSep(rule) {
  return optional(seq(rule, repeat(seq(",", rule))));
}
function semicolonSep(rule) {
  return optional(seq(rule, repeat(seq(";", rule))));
}

const PREC = {
  generic: 16,
  range: 15,
  call: 14,
  field: 13,
  unary: 12,
  cast: 11,
  multiplicative: 10,
  additive: 9,
  shift: 8,
  bitand: 7,
  bitxor: 6,
  bitor: 5,
  comparative: 4,
  and: 3,
  or: 2,
  assign: 0,
  closure: -1,
};

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check
module.exports = grammar({
  name: "rock",

  rules: {
    source_file: ($) => repeat($._item),

    _item: ($) =>
      choice(
        $.function_def,
        $.struct,
        $.enum,
        $.global,
        // $.macro_def,
        // $.macro_expr,
        $.impl_block,
        $.comment,
        $.statement,
      ),

    function_def: ($) =>
      seq(
        "fn",
        field("name", $.identifier),
        field("parameters", $.parameter_list),
        optional(seq("->", field("return_type", $._type_expr))),
        field("body", $.block),
      ),

    struct: ($) =>
      seq("struct", field("name", $.identifier), field("fields", $.fields)),

    enum: ($) =>
      seq(
        "enum",
        field("name", $.identifier),
        "{",
        field("variants", $.enum_variants),
        "}",
      ),

    global: ($) =>
      seq(
        "global",
        field("name", $.identifier),
        ":",
        field("type", $._type_expr),
        "=",
        field("value", $.expression),
        ";",
      ),

    // macro_def: ($) =>
    //   seq(
    //     "macro",
    //     "#",
    //     field("name", $.identifier),
    //     field("params", $.macro_params),
    //     optional(seq("->", field("return_type", $._type_expr))),
    //     $.macro_body,
    //   ),

    // macro_body: ($) =>
    //   seq(
    //     "{",
    //     // repeat(choose($._item),
    //     repeat($.macro_expr),
    //     "}",
    //   ),
    //
    // macro_params: ($) =>
    //   seq("(", commaSep(seq("$", $.identifier, ":", $.identifier)), ")"),
    //
    // macro_expr: ($) => seq("#", field("name", $.identifier), $.macro_expr_args),
    //
    // macro_expr_args: ($) =>
    //   seq("(", commaSep(choice($.identifier, $.macro_expr)), ")"),

    impl_block: ($) =>
      seq(
        "impl",
        field("type", $._type_expr),
        "{",
        field("methods", repeat($.function_def)),
        "}",
      ),

    enum_variants: ($) =>
      seq(
        // sepBy1(",", seq($.identifier, optional($._type_expr))),
        sepBy1(
          ",",
          seq(
            field("variant", $.identifier),
            optional(seq(":", field("type", $._type_expr))),
          ),
        ),
        optional(","),
      ),

    expression: ($) =>
      choice(
        $.identifier,
        $.number,
        $.string,
        $.function_call,
        $.unary_op,
        $.binary_op,
        $.typecast,
        $.field_access,
        $.method_call,
        $.index,
        $.paren_expr,
        $.struct_literal,
        $.array_literal,
        $.enum_variant,
        $.block,
        $.if,
      ),

    // _expression_no_if: ($) =>
    //   choice(
    //     $.identifier,
    //     $.number,
    //     $.string,
    //     $.function_call,
    //     $.unary_op,
    //     $.binary_op,
    //     $.typecast,
    //     $.field_access,
    //     $.method_call,
    //     $.index,
    //     $.paren_expr,
    //     $.struct_literal,
    //     $.array_literal,
    //     $.enum_variant,
    //     $.block,
    //     $.if,
    //   ),

    _expression_no_struct: ($) =>
      prec(
        5,
        choice(
          $.identifier,
          $.number,
          $.string,
          $.function_call,
          $.unary_op,
          $.binary_op,
          $.typecast,
          $.field_access,
          $.method_call,
          $.index,
          $.paren_expr,
          // $.struct_literal,
          $.array_literal,
          $.enum_variant,
          $.block,
          $.if,
        ),
      ),

    enum_variant: ($) => seq($.identifier, "::", $.identifier),

    index: ($) =>
      prec(
        5,
        seq(
          field("base", $.expression),
          "[",
          field("index", $.expression),
          "]",
        ),
      ),

    self: ($) => "self",

    parameter_list: ($) =>
      seq(
        "(",
        commaSep(choice($.self, choice($.typed_param, $.untyped_param))),
        ")",
      ),

    typed_param: ($) =>
      seq(
        field("identifier", $.identifier),
        ":",
        field("type_expr", $._type_expr),
      ),
    untyped_param: ($) => $.identifier,

    statement: ($) =>
      prec(
        1000,
        choice(
          $.for,
          $.while,
          $.comment,
          $.if,
          seq(
            choice(
              $.expression,
              $.return,
              $.let,
              $.assignment,
              // $.macro_expr
            ),
            ";",
          ),
        ),
      ),

    for: ($) =>
      seq(
        "for",
        field("ident", $.identifier),
        "in",
        // TODO: should be expression, but fine for now
        field("from", choice($.integer, $.identifier)),
        "..",
        field("to", $.expression),
        field("body", $.block),
      ),

    while: ($) =>
      seq(
        "while",
        field("condition", $._expression_no_struct),
        field("body", $.block),
      ),

    block: ($) =>
      seq(
        "{",
        field("statements", repeat($.statement)),
        field("return_expr", optional($.expression)),
        "}",
      ),

    if: ($) =>
      seq(
        "if",
        field("condition", $._expression_no_struct),
        $.block,
        optional(seq("else", field("else_branch", $.block))),
      ),

    function_call: ($) =>
      prec(
        PREC.call,
        seq(
          field("ident", $.identifier),
          "(",
          field("args", commaSep($.expression)),
          ")",
        ),
      ),

    struct_literal: ($) => prec(1, seq($.identifier, $.field_values)),

    array_literal: ($) => seq("[", repeat(seq($.expression)), "]"),

    field_values: ($) =>
      seq(
        "{",
        repeat(seq($.identifier, ":", $.expression, optional(","))),
        "}",
      ),

    fields: ($) => seq("{", repeat($.field), "}"),
    field: ($) =>
      seq(
        field("name", $.identifier),
        ":",
        field("type", $._type_expr),
        optional(","),
      ),

    bin_op: ($) =>
      choice(
        "+",
        "-",
        "*",
        "/",
        "&&",
        "||",
        ">",
        "<",
        "==",
        ">=",
        "<=",
        "!=",
        ">>",
        "<<",
      ),

    un_op: ($) => choice("-", "*", "&"),

    unary_op: ($) => prec(PREC.unary, seq($.un_op, $.expression)),
    binary_op: ($) =>
      prec.left(
        1,
        seq(
          field("left", $.expression),
          field("op", $.bin_op),
          field("right", $._expression_no_struct),
        ),
      ),

    typecast: ($) => seq($.expression, "as", $._type_expr),

    comment: ($) => seq("//", /.*/),

    field_access: ($) =>
      prec(
        PREC.field,
        seq(field("base", $.expression), ".", field("field", $.identifier)),
      ),

    method_call: ($) =>
      prec(
        PREC.call,
        seq(
          field("base", $.expression),
          ".",
          field("method", $.identifier),
          "(",
          field("args", commaSep($.expression)),
          ")",
        ),
      ),

    _type_expr: ($) => choice($.identifier, $.primitive_type, $.generic_type),

    generic_type: ($) =>
      prec(
        PREC.generic,
        seq(
          field("type", $.identifier),
          "<",
          field("type_args", sepBy1(",", $.identifier), optional(",")),
          ">",
        ),
      ),

    paren_expr: ($) => seq("(", $.expression, ")"),
    return: ($) => seq("return", field("expr", $.expression)),

    let: ($) =>
      seq(
        "let",
        field("ident", $.identifier),
        field("type", optional(seq(":", $._type_expr))),
        "=",
        field("expr", $.expression),
      ),

    assignment: ($) => seq($.expression, "=", $.expression),

    primitive_type: ($) =>
      choice(
        "num",
        "string",
        // "i8",
        // "i16",
        // "i32",
        // "i64",
        // "u8",
        // "u16",
        // "u32",
        // "u64",
        // "f32",
        // "f64",
        "bool",
      ),

    string: ($) =>
      seq(optional("$"), '"', repeat(choice(/[^"\\]/, /\\./)), '"'),

    identifier: ($) => /\$?[a-zA-Z_][a-zA-Z0-9_]*/,
    // identifier: ($) => /(?!\bif\b)[a-zA-Z_][a-zA-Z0-9_]*/,

    integer: ($) => /-?\d+/,

    number: ($) => {
      const decimal_digits = /\d(_?\d)*/;
      const signed_operators = optional(/[-\+]/);
      const signed_integer = seq(signed_operators, decimal_digits);
      const decimal_integer_literal = seq(
        choice(
          "0",
          seq(
            optional("0"),
            /[1-9]/,
            optional(seq(optional("_"), decimal_digits)),
          ),
        ),
      );
      const hex_literal = seq(choice("0x", "0X"), /[\da-fA-F](_?[\da-fA-F])*/);

      const exponent_part = seq(choice("e", "E"), signed_integer);
      const binary_literal = seq(choice("0b", "0B"), /[0-1](_?[0-1])*/);
      const octal_literal = seq(choice("0o", "0O"), /[0-7](_?[0-7])*/);
      const bigint_literal = seq(
        choice(hex_literal, binary_literal, octal_literal, decimal_digits),
        "n",
      );

      const decimal_literal = choice(
        seq(
          signed_operators,
          decimal_integer_literal,
          ".",
          optional(decimal_digits),
          optional(exponent_part),
        ),
        seq(signed_operators, ".", decimal_digits, optional(exponent_part)),
        seq(signed_operators, decimal_integer_literal, exponent_part),
        seq(signed_operators, decimal_digits),
      );

      return token(
        choice(
          hex_literal,
          decimal_literal,
          binary_literal,
          octal_literal,
          bigint_literal,
        ),
      );
    },
  },
});
