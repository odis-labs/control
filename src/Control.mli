(* Copyright (c) 2018 Rizo I <rizo@odis.io>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

(** Functional control abstractions.. *)

(** {1:types Types} *)

(** A module with an abstract monomorphic type. *)
module type Type0 = sig
  type t
end

(** A module with an abstract polymorphic unary type. *)
module type Type1 = sig
  type 'a t
end

(** A module with an abstract polymorphic binary type. *)
module type Type2 = sig
  type ('a, 'b) t
end

(** A module with an abstract polymorphic ternary type. *)
module type Type3 = sig
  type ('a, 'b, 'c) t
end

(** Default alias for [Type0]. *)
module type Type = Type0


(** {1 Semigroup}

    A type is a [Semigroup] if it implements an associative operation.

    Instances are required to satisfy the following law:

    - {e Associativity}: [(a <+> b) <+> c = a <+> (b <+> c)]

    {3 Examples}
{[
assert (List.combine [1; 2] [3; 4; 5] = [1; 2; 3; 4; 5]);
assert (String.combine "abc" "def" = "abcdef");
assert (Option.combine String.combine (Some "foo") (Some "bar") = Some "foobar"));
assert (Option.combine String.combine None (Some "hello") = Some "foobar"));
]} *)

module type Semigroup0 = sig
  type t

  val (<+>) : t -> t -> t
  (** An associative binary operation that combines two values into one. *)
end

module type Semigroup1 = sig
  type 'a t

  val (<+>) : 'a t -> 'a t -> 'a t
  (** An associative binary operation that combines two values into one. *)
end

module type Semigroup = Semigroup0


(** {1 Monoid}

    Types that implement an associative binary operation, along with a
    neutral element for that operation.

    Instances are required to satisfy the following law:

    {ul
    {- {e Associativity of [<+>]}: [(a <+> b) <+> c = a <+> (b <+> c)].}
    {- {e Neutral for [<+>]}: [a <+> neutral = a] and [neutral <+> = a].}
    } *)

module type Monoid0 = sig
  type t

  include Semigroup0 with type t := t

  val neutral : t
end

module type Monoid1 = sig
  type 'a t

  include Semigroup1 with type 'a t := 'a t

  val neutral : 'a t
end

module type Monoid = Monoid0


(** {1 Functor}

    A type is a [Functor] if it provides some context or structure for values
    of type ['a] and allows the values to be mapped over and transformed into
    type ['b].

    The mapping function may only see each itm independently - not the whole
    structure; it may also not change the structure, only the values being
    mapped. *)

module type Functor1 = sig
  type 'a t
  (** The type that can be mapped over. *)

  val map : ('a -> 'b ) -> 'a t -> 'b t
end

module Functor1 : sig
  module type Extension = sig
    include Functor1
    val (<@>) : ('a -> 'b) -> 'a t -> 'b t
  end

  module Extend (Instance : Functor1) : Extension
    with type 'a t := 'a Instance.t
end

module type Functor0 = sig
  type t

  type item
  val map : (item -> item) -> t -> t
end

module Functor0 : sig
  module type Extension = sig
    include Functor0
    val (<@>) : (item -> item) -> t -> t
  end

  module Extend (Instance : Functor0) : Extension
    with type t := Instance.t
end

module type Functor2 = sig
  type ('a, 'x) t

  val map : ('a -> 'b) -> ('a, 'x) t -> ('b, 'x) t
end

module Functor2 : sig
  module type Extension = sig
    include Functor2
    val (<@>) : ('a -> 'b ) -> ('a, 'x) t -> ('b, 'x) t
  end

  module Extend (Instance : Functor2) : Extension
    with type ('a, 'x) t := ('a, 'x) Instance.t
end

module type Functor = Functor1
module Functor = Functor1


(** {1 Applicatives}

    A type is an [Applicative] if it is a [Functor], and also permits arbitrary
    values and a mapping function to be exist in its context. More simply, this
    allows for chaining of computations inside some context.

    For example, if the functor in question is the List type and we have a list
    of functions which take an integer and produce a boolean, and another list
    of integers, then we can write:

{[
func_list <*> int_list
]}

    to apply all the functions in the first list to all the ints in the second list
    to produce a list of all results. *)

module type Applicative1 = sig
  type 'a t

  val pure : 'a -> 'a t

  val apply : ('a -> 'b) t -> 'a t -> 'b t
end


module Applicative1 : sig
  module type Extension = sig
    type 'a t

    val pure : 'a -> 'a t

    val apply : ('a -> 'b) t -> 'a t -> 'b t

    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

    val ( <* ) : 'a t -> 'b t -> 'a t

    val ( *> ) : 'a t -> 'b t -> 'b t

    val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  end

  module Extend (Instance : Applicative1) : Extension
    with type 'a t := 'a Instance.t

  module To_functor (Instance : Applicative1) : Functor1
    with type 'a t = 'a Instance.t
end


module type Applicative2 = sig
  type ('a, 'x) t

  val pure : 'a -> ('a, 'x) t

  val apply : ('a -> 'b, 'x) t -> ('a, 'x) t -> ('b, 'x) t
end

module Applicative2 : sig
  module type Extension = sig
    type ('a, 'x) t

    val pure : 'a -> ('a, 'x) t

    val apply : ('a -> 'b, 'x) t -> ('a, 'x) t -> ('b, 'x) t

    val ( <*> ) : ('a -> 'b, 'x) t -> ('a, 'x) t -> ('b, 'x) t

    val ( <* ) : ('a, 'x) t -> ('b, 'x) t -> ('a, 'x) t

    val ( *> ) : ('a, 'x) t -> ('b, 'x) t -> ('b, 'x) t

    val lift2 : ('a -> 'b -> 'c) -> ('a, 'x) t -> ('b, 'x) t -> ('c, 'x) t
  end

  module Extend (Instance : Applicative2) : Extension
    with type ('a, 'x) t := ('a, 'x) Instance.t

  module To_functor (Instance : Applicative2) : Functor2
    with type ('a, 'x) t = ('a, 'x) Instance.t
end

module type Applicative = Applicative1
module Applicative = Applicative1


(** {1 Alternative}

    A type is an [Alternative] if it is also an [Applicative]. The name
   "Alternative" suggests a common use case for this interface: functor
    computations which allow for a limited form of choice. How an [Alternative]
    instance decides between two values is what defines it. *)

module type Alternative1 = sig
  type 'a t

  val empty : 'a t

  val (<|>) : 'a t -> 'a t -> 'a t
end

module Alternative1 : sig
  module type Extension = sig
    type 'a t

    val empty : 'a t
    (** [empty] is the identity of [<|>] *)

    val (<|>) : 'a t -> 'a t -> 'a t
    (** [a <|> b] in associative binary operation of [a] and [b]. *)

    val some : 'a t -> 'a list t
    (** [some a] is one or more occurrences of [a]. *)

    val many : 'a t -> 'a list t
    (** [many a] is zero or more occurrences of [a]. *)
  end

  module Extend
      (Instance : Alternative1)
      (Applicative : Applicative1 with type 'a t := 'a Instance.t)
    : Extension with type 'a t := 'a Instance.t
  (** Functor building an instance of {!Alternative} given a
      {!Alternative.Base} and {!Applicative} implementation. *)
end

module type Alternative2 = sig
  type ('a, 'x) t

  val empty : ('a, 'x) t

  val (<|>) : ('a, 'x) t -> ('a, 'x) t -> ('a, 'x) t
end

module Alternative2 : sig
  module type Extension = sig
    type ('a, 'x) t

    val empty : ('a, 'x) t
    (** [empty] is the identity of [<|>] *)

    val (<|>) : ('a, 'x) t -> ('a, 'x) t -> ('a, 'x) t
    (** [a <|> b] in associative binary operation of [a] and [b]. *)

    val some : ('a, 'x) t -> ('a list, 'x) t
    (** [some a] is one or more occurrences of [a]. *)

    val many : ('a, 'x) t -> ('a list, 'x) t
    (** [many a] is zero or more occurrences of [a]. *)
  end

  module Extend
      (Instance : Alternative2)
      (Applicative : Applicative2 with type ('a, 'x) t := ('a, 'x) Instance.t)
    : Extension with type ('a, 'x) t := ('a, 'x) Instance.t
  (** Functor building an instance of {!Alternative2} given a
      {!Alternative2.Base} and {!Applicative2} implementation. *)
end

module type Alternative = Alternative1
module Alternative = Alternative1


(** {1 Monad}

    Types that support sequential composition. *)

module type Monad1 = sig
  type 'a t
  (** The type of the monadic values. *)

  val return : 'a -> 'a t
  (** [return x] injects a value [x] into a monadic value. *)

  val bind : ('a -> 'b t) -> 'a t -> 'b t
  (** [bind f m] sequentially composes two computations, passing any value
      produced by [m] as an argument to [f]. *)
end

module Monad1 : sig
  module type Extension = sig
    type 'a t
    (** The type of the monadic values. *)

    val return : 'a -> 'a t
    (** [return x] injects the value [x] into the monadic type. *)

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( =<< ) : ('a -> 'b t) -> 'a t -> 'b t
    val bind : ('a -> 'b t) -> 'a t -> 'b t
    (** Sequentially compose two actions, passing any value produced by the first
        as an argument to the second.

        [>>=] and [=<<] are the infix versions of bind. *)

    val ( >> ) : 'a t -> (unit -> 'b t) -> 'b t
    (** [m1 >> fun () -> m2] sequentially composes two actions, discarding any
        value produced by [m1], like sequencing operators (such as the semicolon)
        in imperative languages. *)

    val sequence : 'a t list -> 'a list t
    (** [sequence l] evaluates each monadic action in [l] from left to right, and
        collects the results. *)

    val join : 'a t t -> 'a t
    (** [join m] removes one level of monadic structure, projecting its bound
        argument into the outer level. *)
  end

  module Extend (Self : Monad1) : Extension
    with type 'a t := 'a Self.t

  module To_functor (Monad : Monad1) : Functor1
    with type 'a t = 'a Monad.t

  module To_applicative (Monad : Monad1) : Applicative1
    with type 'a t = 'a Monad.t
end


(** {2 Monad2} *)

module type Monad2 = sig
  type ('a, 'x) t
  (** The type of the monadic values. *)

  val return : 'a -> ('a, 'x) t
  (** [pure x] injects the value [x] into the monadic type. *)

  val bind  : ('a -> ('b, 'x) t) -> ('a, 'x) t -> ('b, 'x) t
  (** [bind f m] sequentially composes two actions, passing any value
      produced by [m] as an argument to [f]. *)
end

module Monad2 : sig
  module type Extension = sig
    type ('a, 'x) t
    (** The type of the monadic values. *)

    val return : 'a -> ('a, 'x) t
    (** [return x] injects the value [x] into the monadic type. *)

    val bind : ('a -> ('b, 'x) t) -> ('a, 'x) t -> ('b, 'x) t
    val ( >>= ) : ('a, 'x) t -> ('a -> ('b, 'x) t) -> ('b, 'x) t
    val ( =<< ) : ('a -> ('b, 'x) t) -> ('a, 'x) t -> ('b, 'x) t
    (** Sequentially compose two actions, passing any value produced by the first
        as an argument to the second.

        [>>=] and [=<<] are the infix versions of bind. *)

    val ( >> ) : ('a, 'x) t -> ('b, 'x) t Lazy.t -> ('b, 'x) t
    (** [m1 >> fun () -> m2] sequentially composes two actions, discarding any
        value produced by [m1], like sequencing operators (such as the semicolon)
        in imperative languages. *)

    val sequence : ('a, 'x) t list -> ('a list, 'x) t
    (** [sequence l] evaluates each monadic action in [l] from left to right, and
        collects the results. *)

    val join : (('a, 'x) t, 'x) t -> ('a, 'x) t
    (** [join m] removes one level of monadic structure, projecting its bound
        argument into the outer level. *)
  end

  module Extend (Self : Monad2) : Extension
    with type ('a, 'x) t := ('a, 'x) Self.t

  module To_functor (Monad : Monad2) : Functor2
    with type ('a, 'x) t = ('a, 'x) Monad.t
  (** Produces an instance of {!Functor2} given a {!Monad2} instance. *)

  module To_applicative (Monad : Monad2) : Applicative2
    with type ('a, 'x) t = ('a, 'x) Monad.t
end

module type Monad = Monad1
module Monad = Monad1


(** {1 State Monad} *)

module type State = sig
  type state

  type 'a t = state -> ('a * state)

  include Monad1.Extension with type 'a t := 'a t

  val get : state t
  val put : state -> unit t
  val run : 'a t -> state -> 'a * state
end

module State (S : Type) : State with type state = S.t


module type State1 = sig
  type 'x state

  type ('a, 'x) t = 'x state -> ('a * 'x state)

  include Monad2.Extension with type ('a, 'x) t := ('a, 'x) t

  val get : ('x state, 'x) t
  val put : 'x state -> (unit, 'x) t
  val run : ('a, 'x) t -> 'x state -> 'a * 'x state
end


module State1 (S : Type1) : State1 with type 'x state = 'x S.t


module type StateT = sig
  type state
  type 'a monad
  type 'a t = state -> ('a * state) monad

  include Monad.Extension with type 'a t := 'a t

  val run : 'a t -> state -> ('a * state) monad
  val get : state t
  val put : state -> unit t

  val state : (state -> ('a * state)) -> 'a t
  (** Embed a simple state action into the monad. *)

  val modify : (state -> state) -> unit t
  (* Maps an old state to a new state inside a state monad. The old state is
     thrown away. *)
end


module StateT (S : Type) (M : Monad1) :
  StateT with type state = S.t
          and type 'a monad = 'a M.t


module type State1T = sig
  type 'x state
  type 'a monad
  type ('a, 'x) t = 'x state -> ('a * 'x state) monad

  include Monad2.Extension with type ('a, 'x) t := ('a, 'x) t

  val run : ('a, 'x) t -> 'x state -> ('a * 'x state) monad
  val get : ('x state, 'x) t
  val put : 'x state -> (unit, 'x) t

  val state : ('x state -> ('a * 'x state)) -> ('a, 'x) t
  (** Embed a simple state action into the monad. *)

  val modify : ('x state -> 'x state) -> (unit, 'x) t
  (* Maps an old state to a new state inside a state monad. The old state is
     thrown away. *)
end

module State1T (S : Type1) (M : Monad1) :
  State1T with type 'x state = 'x S.t
           and type 'a monad = 'a M.t


(* Indexed State Monad *)

module IndexedState : sig
  type ('a, 's1, 's2) t = 's1 -> 'a * 's2

  val return : 'a -> ('a, 's1, 's1) t
  val bind : ('a -> ('b, 's2, 's3) t) -> ('a, 's1, 's2) t -> ('b, 's1, 's3) t

  val get : ('s1, 's1, 's1) t
  val put : 's2 -> (unit, 's1, 's2) t
end


(** A container type with the ability to fold on itself.

    The [Foldable] interface describes the operations that sequentially iterate
    over the elements in a parameterised unary type ['a t] and, starting with
    an initial value [init], combine the elements togather, using a provided
    function [f], into a single result. *)
module type Foldable = sig
  type 'a t

  val foldl : ('a -> 'r -> 'r) -> 'r -> 'a t -> 'r
  (** [foldl f init self] uses [f] to sequentially, from left to right, combine
      the elements of the container [self] with an accumulator value [init].

      {[
        assert (Array.foldl (+) 0 [|1; 2; 3|] = 6);
        assert (List.foldl List.add [] [1; 2; 3] = [3; 2; 1]);
      ]} *)

  val foldr : ('a -> 'r -> 'r) -> 'r -> 'a t -> 'r
  (** [foldr f init self] is the same as [foldl] but performs the folding from
      right to left.

      {[
        assert (Array.foldr (+) 0 [|1; 2; 3|] = 6);
        assert (List.foldr List.add [] [1; 2; 3] = [1; 2; 3]);
      ]} *)

  val foldk : ('a -> 'r -> ('r -> 'r) -> 'r) -> 'r -> 'a t -> 'r
  (** [foldk f init self] is the same as [foldl] but with early termination
      support. The function [f] is given a continuation argument [('r -> 'r)]
      that [f] can call to keep folding with an intermediate accumulator, or
      return the accumulator to immediately stop without consuming more
      elements.

      {[
        let count_until_0 =
          List.foldk
            (fun x count continue ->
               if x = 0 then count
               else continue (count + 1))
            0
            [5; 22; 10; 0; 4; 3; 14; 72; 92]
        in assert (count_until_0 = 3);
      ]} *)
end


(* module Exception : sig *)
(*   type t = exn *)

(*   val raise : ?trace: bool -> t -> 'a *)
(*   (** [raise ?trace exn] raises the exception [exn]. If [trace] is [false] no *)
(*       backtrace will be recorded, resulting in faster execution. *)

(*       {[ *)
(*         let check_list l = *)
(*           if List.is_empty l then *)
(*             raise Not_found ~trace:false *)
(*           else *)
(*             print "list is not empty" in *)

(*         assert (try check_list []; false *)
(*                 with Not_found -> true) *)
(*       ]} *) *)

(*   val raises : ?only: t -> (unit -> 'a) -> bool *)
(*   (** [raises ?only f] is [true] if the exception [exn] is raised while calling *)
(*       [f], [false] otherwise. If no [exn] is given, will return [true] on any *)
(*       exception. *)

(*       {[ *)
(*         assert (raises (fun () -> fail "yes")); *)
(*         assert (raises (fun () -> Option.force None) ~only:No_value); *)
(*         assert (not (raises (fun () -> "no"))) *)
(*       ]} *) *)

(*   val show : t -> string *)
(*   (** [show exn] is a string representation of the exception [exn]. *) *)
(* end *)


(* module Function : sig *)
(*   type ('a, 'b) t = 'a -> 'b *)

(*   val identity : 'a -> 'a *)

(*   val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c *)

(*   val apply : ('a -> 'b) -> 'a -> 'b *)

(*   val (@) : ('a -> 'b) -> 'a -> 'b *)

(*   val (|>) : 'a -> ('a -> 'b) -> 'b *)

(*   val always : 'a -> 'b -> 'a *)

(*   val curry : (('a * 'b) -> 'c) -> 'a -> 'b -> 'c *)

(*   val uncurry : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c *)

(*   val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c *)
(*   val ( << )  : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c *)
(*   val ( >> )  : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)

(*   val map : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)
(* end *)

(** Trivial identity type module. *)
(* TODO: Movo to individual classes *)
(* module Identity : sig *)
(*   type 'a t = 'a *)

(*   include Monad   with type 'a t := 'a t *)
(*   include Functor with type 'a t := 'a t *)
(* end *)


val scoped : (('a -> 'b) -> 'a) -> 'a
(** [scoped f] is the result of invocation of the function [f] with the
    ability to interrupt its execution and return some value. A special
    function [break] will be passed to [f] as an argument that can capture a
    value by (internally) raising an exception.

    {[
      let result =
        scoped (fun break ->
          break "hello";
          print "this will not be printed") in
      assert (result = "hello")
    ]} *)

