[
  (primitive_type)
  (identifier)
  "fn"
  "let"
  "return"
  "as"
  "struct"
  "impl"
] @keyword

; Comments
(comment) @comment

; ; Function definitions and calls
(function_def (identifier) @function)
(function_call (identifier) @function)

; Variables
(typed_param (identifier) @variable.parameter)
(untyped_param (identifier) @variable.parameter)
(field (identifier) @variable.field)
(let (identifier) @variable)

; Type Expressions
(primitive_type) @type

; Constants
(string) @string
(number) @number

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
"struct" @keyword.type
"impl" @keyword.directive
