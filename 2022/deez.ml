open Base

module IntPair = struct
  module T = struct
    type t = int * int [@@deriving sexp_of, compare, hash, equal]
  end
  include T
  include Base.Comparator.Make(T)
end
