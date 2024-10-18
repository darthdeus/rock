(function
  "fn" @append_space
  name: (identifier) @append_space)

(parameter (identifier) ":" @append_space (primitive_type))
("," @append_space)
("->" @append_space)

(parameter_list ")" @append_space)
(function
return_type: (primitive_type) @append_space)

; ("{" @append_space @append_indent_start)
; ("}" @prepend_hardline @prepend_indent_end @append_hardline)


