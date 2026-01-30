type ('a : any mod separable) t = 'a array

external length
  : ('a : any mod separable).
  ('a array[@local_opt]) @ contended -> int
  @@ portable
  = "%array_length"
[@@layout_poly]

external unsafe_set
  : ('a : any mod separable).
  ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
  @@ portable
  = "%array_unsafe_set"
[@@layout_poly]

external%template unsafe_get
  : ('a : any mod separable).
  ('a array[@local_opt]) @ m -> (int[@local_opt]) -> 'a @ m
  @@ portable
  = "%array_unsafe_get"
[@@mode m = (uncontended, shared)] [@@layout_poly]

[%%template
  external create
    : ('a : any mod separable).
    len:int -> 'a -> 'a array @ m
    @@ portable
    = "%makearray_dynamic"
  [@@alloc __ @ m = (heap_global, stack_local)] [@@layout_poly]]

external magic_create_uninitialized
  : ('a : any mod separable).
  len:int -> ('a array[@local_opt])
  @@ portable
  = "%makearray_dynamic_uninit"
[@@layout_poly]
