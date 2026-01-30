module Either_u = Either_u
module Fake_tuple = Fake_tuple

module Kind_tag = Kind_tag
[@@alert
  template_specialization
    "Using kind tags often involves template specialization that won't be subsumed by \
     layout polymorphism. Consider how your code will be updated in the future."]

module Kind_witness = Kind_witness
[@@alert
  template_specialization
    "Using kind witnesses often involves template specialization that won't be subsumed \
     by layout polymorphism. Consider how your code will be updated in the future."]

module Result_u = Result_u
module Or_error_u = Or_error_u
module Option_u = Option_u
