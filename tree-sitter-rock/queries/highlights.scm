[
  (primitive_type)
  (identifier)
  "fn"
  "let"
  "return"
  "as"
  "extern"
  "struct"
  "impl"
] @keyword

; Comments
(comment) @comment

; ; Function definitions and calls
(function (identifier) @function)
; (function_call (identifier) @function)

; Variables
(parameter (identifier) @variable.parameter)
(field (identifier) @variable.field)
(let (identifier) @variable)

; Type Expressions
(primitive_type) @type

; Constants
(string) @string
(number) @number

(extern_modifier) @string

(generic_type
  "<" @punctuation.bracket
  ">" @punctuation.bracket)

; Binary operations
(binary_op (bin_op) @operator)

; Unary operations
(unary_op (un_op) @operator)

; Keywords
"fn" @keyword.function
"let" @keyword.declaration
"global" @keyword.declaration
"return" @keyword.control
"as" @keyword.operator
"extern" @keyword.directive
"struct" @keyword.type
"impl" @keyword.directive
