open! Core
module Handle = Bonsai_web_test.Handle
module Result_spec = Bonsai_web_test.Result_spec

include struct
  open Bonsai_web
  module Node = Vdom.Node
  module Attr = Vdom.Attr
end
