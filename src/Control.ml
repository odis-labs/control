
let cons x xs = x :: xs

(* Types *)

module type Type0 = sig
  type t
end

module type Type1 = sig
  type 'a t
end

module type Type2 = sig
  type ('a, 'b) t
end

module type Type3 = sig
  type ('a, 'b, 'c) t
end

module type Type = Type0


(* Semigroup *)

module type Semigroup0 = sig
  type t

  val (<+>) : t -> t -> t
end

module type Semigroup1 = sig
  type 'a t

  val (<+>) : 'a t -> 'a t -> 'a t
end

module type Semigroup = Semigroup0


(* Monoid *)

module type Monoid0 = sig
  include Semigroup0

  val neutral : t
end

module type Monoid1 = sig
  include Semigroup1

  val neutral : 'a t
end

module type Monoid = Monoid0


(* Functor *)

module type Functor0 = sig
  type t
  type item

  val map : (item -> item) -> t -> t
end

module Functor0 = struct
  module type Extension = sig
    type t
    type item

    val map : (item -> item) -> t -> t
    val (<@>) : (item -> item) -> t -> t
  end

  module Extend (Instance : Functor0) = struct
    include Instance

    let ( <@> ) f m = map f m
  end
end

module type Functor1 = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Functor1 = struct
  module type Extension = sig
    type 'a t

    val map : ('a -> 'b) -> 'a t -> 'b t
    val (<@>) : ('a -> 'b) -> 'a t -> 'b t
  end

  module Extend (Instance : Functor1) = struct
    include Instance

    let ( <@> ) f m = map f m
  end
end

module type Functor2 = sig
  type ('a, 'x) t

  val map : ('a -> 'b) -> ('a, 'x) t -> ('b, 'x) t
end

module Functor2 = struct
  module type Extension = sig
    type ('a, 'x) t

    val map : ('a -> 'b) -> ('a, 'x) t -> ('b, 'x) t
    val (<@>) : ('a -> 'b ) -> ('a, 'x) t -> ('b, 'x) t
  end

  module Extend (Instance : Functor2) = struct
    include Instance

    let ( <@> ) f m =
      map f m
  end
end

module type Functor = Functor1
module Functor = Functor1


(* Applicative *)

module type Applicative1 = sig
  type 'a t

  val pure : 'a -> 'a t

  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

module Applicative1 = struct
  module type Extension = sig
    type 'a t

    val pure : 'a -> 'a t

    val apply : ('a -> 'b) t -> 'a t -> 'b t

    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

    val ( <* ) : 'a t -> 'b t -> 'a t

    val ( *> ) : 'a t -> 'b t -> 'b t

    val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  end

  module To_functor (Instance : Applicative1) = struct
    type 'a t = 'a Instance.t

    let map f ma =
      Instance.apply (Instance.pure f) ma
  end

  module Extend (Instance : Applicative1) = struct
    include Instance

    module Functor = To_functor (Instance)

    let ( <*> ) fab fa =
      Instance.apply fab fa

    let lift2 f fa fb =
      Functor.map f fa <*> fb

    let ( <* ) fa fb =
      lift2 (fun x _ -> x) fa fb

    let ( *> ) _fa _fb =
      failwith "TODO"
  end

end

module type Applicative2 = sig
  type ('a, 'x) t

  val pure : 'a -> ('a, 'x) t

  val apply : (('a -> 'b), 'x) t -> ('a, 'x) t -> ('b, 'x) t
end

module Applicative2 = struct
  module type Extension = sig
    type ('a, 'x) t

    val pure : 'a -> ('a, 'x) t

    val apply : ('a -> 'b, 'x) t -> ('a, 'x) t -> ('b, 'x) t

    val ( <*> ) : ('a -> 'b, 'x) t -> ('a, 'x) t -> ('b, 'x) t

    val ( <* ) : ('a, 'x) t -> ('b, 'x) t -> ('a, 'x) t

    val ( *> ) : ('a, 'x) t -> ('b, 'x) t -> ('b, 'x) t

    val lift2 : ('a -> 'b -> 'c) -> ('a, 'x) t -> ('b, 'x) t -> ('c, 'x) t
  end

  module To_functor (Instance : Applicative2) = struct
    type ('a, 'x) t = ('a, 'x) Instance.t

    let map f ma =
      Instance.apply (Instance.pure f) ma
  end


  module Extend (Instance : Applicative2)  = struct
    include Instance

    module Functor = To_functor(Instance)

    let ( <*> ) fab fa =
      Instance.apply fab fa

    let lift2 f fa fb =
      Functor.map f fa <*> fb

    let ( <* ) fa fb =
      lift2 (fun x _ -> x) fa fb

    (* TODO *)
    let ( *> ) _fa _fb =
      failwith "TODO"
  end
end

module type Applicative = Applicative1
module Applicative = Applicative1


(* Alternative *)

module type Alternative1 = sig
  type 'a t

  val empty : 'a t

  val (<|>) : 'a t -> 'a t -> 'a t
end

module Alternative1 = struct
  module type Extension = sig
    type 'a t

    val empty : 'a t

    val (<|>) : 'a t -> 'a t -> 'a t

    val some : 'a t -> 'a list t

    val many : 'a t -> 'a list t
  end

  module Extend
      (T : Alternative1)
      (A : Applicative1 with type 'a t := 'a T.t) = struct

    module F = Applicative1.To_functor(struct
        type 'a t = 'a T.t
        include A
      end)

    include T
    open A

    let ( <*> ) fab fa =
      A.apply fab fa

    let some v =
      let rec many_v () = some_v () <|> pure []
      and some_v () = F.map cons v <*> many_v () in
      some_v ()

    let many v =
      let rec many_v () = some_v () <|> pure []
      and some_v () = F.map cons v <*> many_v () in
      many_v ()
  end
end

module type Alternative2 = sig
  type ('a, 'x) t

  val empty : ('a, 'x) t

  val (<|>) : ('a, 'x) t -> ('a, 'x) t -> ('a, 'x) t
end

module Alternative2 = struct
  module type Extension = sig
    type ('a, 'x) t

    val empty : ('a, 'x) t

    val (<|>) : ('a, 'x) t -> ('a, 'x) t -> ('a, 'x) t

    val some : ('a, 'x) t -> ('a list, 'x) t

    val many : ('a, 'x) t -> ('a list, 'x) t
  end

  module Extend
      (T : Alternative2)
      (A : Applicative2 with type ('a, 'b) t := ('a, 'b) T.t) = struct

    module F = Applicative2.To_functor(struct
        type ('a, 'b) t = ('a, 'b) T.t
        include A
      end)

    include T
    open A

    let ( <*> ) fab fa =
      A.apply fab fa

    let some v =
      let rec many_v () = some_v () <|> pure []
      and some_v () = F.map cons v <*> many_v () in
      some_v ()

    let many v =
      let rec many_v () = some_v () <|> pure []
      and some_v () = F.map cons v <*> many_v () in
      many_v ()
  end
end

module type Alternative = Alternative1
module Alternative = Alternative1


(* Monad *)

module type Monad1 = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : ('a -> 'b t) -> 'a t -> 'b t
end

module Monad1 = struct
  module type Extension = sig
    type 'a t

    val return : 'a -> 'a t

    val ( >>= )  : 'a t -> ('a -> 'b t) -> 'b t
    val ( =<< )  : ('a -> 'b t) -> 'a t -> 'b t
    val bind     : ('a -> 'b t) -> 'a t -> 'b t

    val ( >> ) : 'a t -> (unit -> 'b t) -> 'b t

    val sequence : 'a t list -> 'a list t

    val join : 'a t t -> 'a t
  end

  module Extend (Self : Monad1)
    : Extension with type 'a t := 'a Self.t = struct

    include Self

    let ( >>= ) m f =
      bind f m

    let ( =<< ) f m =
      bind f m

    let ( >> ) m1 m2' =
      m1 >>= fun _ -> m2' ()

    let join maa =
      maa >>= fun ma -> ma

    let sequence mas =
      List.fold_right
        (fun ma ms ->
           ma >>= fun a ->
           ms >>= fun s ->
           return (a :: s))
        mas
        (return [])

    (* XXX: Export this *)
    (* let sequence_unit mas = *)
    (*   sequence mas >>= fun _ -> B.return () *)
  end

  module To_functor (Instance : Monad1) = struct
    type 'a t = 'a Instance.t

    let map f ma =
      Instance.bind (fun a -> Instance.return (f a)) ma
  end

  module To_applicative (Instance : Monad1) = struct
    type 'a t = 'a Instance.t

    let pure =
      Instance.return

    let (>>=) m f = Instance.bind f m

    let apply fab fa =
      fab >>= fun f ->
      fa >>= fun a ->
      Instance.return (f a)
  end
end

module type Monad2 = sig
  type ('a, 'x) t

  val return : 'a -> ('a, 'x) t

  val bind : ('a -> ('b, 'x) t) -> ('a, 'x) t -> ('b, 'x) t
end

module Monad2 = struct
  module type Extension = sig
    type ('a, 'x) t

    val return : 'a -> ('a, _) t

    val bind : ('a -> ('b, 'x) t) -> ('a, 'x) t -> ('b, 'x) t
    val ( >>= ) : ('a, 'x) t -> ('a -> ('b, 'x) t) -> ('b, 'x) t
    val ( =<< ) : ('a -> ('b, 'x) t) -> ('a, 'x) t -> ('b, 'x) t

    val ( >> ) : ('a, 'x) t -> ('b, 'x) t Lazy.t -> ('b, 'x) t

    val sequence : ('a, 'x) t list -> ('a list, 'x) t

    val join : (('a, 'x) t, 'x) t -> ('a, 'x) t
  end

  module Extend (Self : Monad2) : Extension
    with type ('a, 'x) t := ('a, 'x) Self.t = struct
    include Self

    let ( >>= ) m f =
      bind f m

    let ( =<< ) f m =
      bind f m

    let ( >> ) m1 m2 =
      m1 >>= fun _ -> Lazy.force m2

    let join maa =
      maa >>= fun ma -> ma

    let sequence mas =
      List.fold_right
        (fun ma ms ->
           ma >>= fun a ->
           ms >>= fun s ->
           return (a :: s))
        mas
        (return [])
  end

  module To_functor (Instance : Monad2) = struct
    type ('a, 'x) t = ('a, 'x) Instance.t
    let map f ma =
      Instance.bind (fun a -> Instance.return (f a)) ma
  end

  module To_applicative (Instance : Monad2) = struct
    type ('a, 'x) t = ('a, 'x) Instance.t

    let pure =
      Instance.return

    let (>>=) m f = Instance.bind f m

    let apply fab fa =
      fab >>= fun f ->
      fa >>= fun a ->
      Instance.return (f a)
  end
end

module Monad = Monad1
module type Monad = Monad1


(* State Monad *)

module type State = sig
  type state

  type 'a t = state -> ('a * state)

  include Monad1.Extension with type 'a t := 'a t

  val get : state t
  val put : state -> unit t

  val run : 'a t -> state -> 'a * state
end


module State (S : Type) : State with type state = S.t = struct
  type state = S.t

  type 'a t = state -> ('a * state)

  include Monad1.Extend(struct
      type nonrec 'a t = 'a t

      let return a = fun s -> (a, s)

      let bind f (m : 'a t) = fun s ->
        let a, s' = m s in
        f a s'
    end)

  let put s = fun _ -> ((), s)
  let get   = fun s -> (s, s)

  let run self s = self s
end


module type State1 = sig
  type 'x state

  type ('a, 'x) t = 'x state -> ('a * 'x state)

  include Monad2.Extension with type ('a, 'x) t := ('a, 'x) t

  val get : ('x state, 'x) t
  val put : 'x state -> (unit, 'x) t

  val run : ('a, 'x) t -> 'x state -> 'a * 'x state
end


module State1 (S : Type1) : State1 with type 'x state = 'x S.t = struct
  type 'x state = 'x S.t

  type ('a, 'x) t = 'x state -> ('a * 'x state)

  include Monad2.Extend(struct
      type nonrec ('a, 'x) t = ('a, 'x) t

      let return a = fun s -> (a, s)

      let bind f (m : ('a, 'x) t) = fun s ->
        let a, s' = m s in
        f a s'
    end)

  let put s = fun _ -> ((), s)
  let get   = fun s -> (s, s)

  let run self s = self s
end



module type StateT = sig
  type state
  type 'a monad
  type 'a t = state -> ('a * state) monad

  include Monad1.Extension with type 'a t := 'a t

  val run : 'a t -> state -> ('a * state) monad
  val get : state t
  val put : state -> unit t

  val state : (state -> ('a * state)) -> 'a t

  val modify : (state -> state) -> unit t
end


module StateT (S : Type) (M : Monad1) = struct
  type state = S.t
  type 'a monad = 'a M.t
  type 'a t = state -> ('a * state) monad

  include Monad1.Extend(struct
      type nonrec 'a t = 'a t

      let return a = fun s ->
        M.return (a, s)

      let bind f xf s =
        M.bind (fun (x, s') -> (f x) s') (xf s)
    end)

  let run self s = self s

  let put s = fun _ -> M.return ((), s)
  let get   = fun s -> M.return (s, s)

  let state f =
    get >>= fun s ->
    let (a, s') = f s in
    put s' >>= fun _ -> (return a)

  let modify f =
    state (fun s -> ((), f s))
end


module type State1T = sig
  type 'x state
  type 'a monad
  type ('a, 'x) t = 'x state -> ('a * 'x state) monad

  include Monad2.Extension with type ('a, 'x) t := ('a, 'x) t

  val run : ('a, 'x) t -> 'x state -> ('a * 'x state) monad
  val get : ('x state, 'x) t
  val put : 'x state -> (unit, 'x) t

  val state : ('x state -> ('a * 'x state)) -> ('a, 'x) t

  val modify : ('x state -> 'x state) -> (unit, 'x) t
end


module State1T (S : Type1) (M : Monad1) = struct
  type 'x state = 'x S.t
  type 'a monad = 'a M.t
  type ('a, 'x) t = 'x state -> ('a * 'x state) monad

  include Monad2.Extend(struct
      type nonrec ('a, 'x) t = ('a, 'x) t

      let return a = fun s ->
        M.return (a, s)

      let bind f xf s =
        M.bind (fun (x, s') -> (f x) s') (xf s)
    end)

  let run self s = self s

  let put s = fun _ -> M.return ((), s)
  let get   = fun s -> M.return (s, s)

  let state f =
    get >>= fun s ->
    let (a, s') = f s in
    put s' >>= fun _ -> return a

  let modify f =
    state (fun s -> ((), f s))
end


module IndexedState = struct
  type ('a, 's1, 's2) t = 's1 -> 'a * 's2

  let return a : ('a, 's1, 's2) t = fun s1 -> (a, s1)

  let bind f m =
    fun i ->
      let (a, j) = m i in
      let m' = f a in
      m' j

  let get   = fun s -> (s,  s)
  let put s = fun _ -> ((), s)
end


module type Foldable = sig
  type 'a t

  val foldl : ('a -> 'r -> 'r) -> 'r -> 'a t -> 'r
  val foldr : ('a -> 'r -> 'r) -> 'r -> 'a t -> 'r
  val foldk : ('a -> 'r -> ('r -> 'r) -> 'r) -> 'r -> 'a t -> 'r
end


(* module Exception = struct *)
(*   type t = exn *)

(*   let show = Printexc.to_string *)

(*   let raise = Pervasives.raise *)
(*   let raises = Kernel.raises *)
(* end *)


(* module Function = struct *)
(*   type ('a, 'b) t = 'a -> 'b *)

(*   let identity = Kernel.identity *)
(*   let flip     = Kernel.flip *)
(*   let curry    = Kernel.curry *)
(*   let uncurry  = Kernel.uncurry *)
(*   let compose  = Kernel.compose *)
(*   let always   = Kernel.always *)
(*   let (<<)     = Kernel.(<<) *)
(*   let (>>)     = Kernel.(>>) *)

(*   let apply f x = f x *)

(*   let map f x = compose f x *)

(*   external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply" *)
(*   external (@) : ('a -> 'b) -> 'a -> 'b = "%apply" *)
(* end *)


(* module Identity = struct *)
(*   type 'a t = 'a *)

(*   module Monad_base = struct *)
(*     type nonrec 'a t = 'a t *)
(*     let return x = x *)
(*     let bind f m = f m *)
(*   end *)
(*   include Monad.Make(Monad_base) *)

(*   include Functor.With_monad(Monad_base) *)
(* end *)


let scoped (type a) f =
  let module Scope = struct
    exception Break of a
  end in
  let break a =
    raise_notrace (Scope.Break a) in
  try
    f break
  with Scope.Break a -> a


