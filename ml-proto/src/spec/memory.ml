(*
 * (c) 2015 Andreas Rossberg
 *)

open Bigarray
open Types
open Values


(* Types and view types *)

type address = int
type size = address
type offset = address
type mem_size = Mem8 | Mem16 | Mem32
type extension = SX | ZX
type segment = {addr : address; data : string}
type value_type = Types.value_type
type value = Values.value

type memory' = (int, int8_unsigned_elt, c_layout) Array1.t
type memory = memory' ref
type t = memory


(* Creation and initialization *)

exception Type
exception Bounds
exception Address

let create' n =
  let mem = Array1.create Int8_unsigned C_layout n in
  Array1.fill mem 0;
  mem

let create n =
  ref (create' n)

let init_seg mem seg =
  (* There currently is no way to blit from a string. *)
  for i = 0 to String.length seg.data - 1 do
    !mem.{seg.addr + i} <- Char.code seg.data.[i]
  done

let init mem segs =
  try List.iter (init_seg mem) segs with Invalid_argument _ -> raise Bounds


let size mem =
  Array1.dim !mem

let resize mem n =
  let before = !mem in
  let after = create' n in
  let min = min (Array1.dim before) n in
  Array1.blit (Array1.sub before 0 min) (Array1.sub after 0 min);
  mem := after

open Values

(* TODO: The conversion to int could overflow *)
let address_of_value = function
  | Int32 i -> Int32.to_int (I32.to_int32 i)
  | _ -> raise Address


(* Load and store *)

let effective_address a o =
  if max_int - a < o then raise Bounds;
  a + o

let rec loadn mem n ea =
  assert (n > 0 && n <= 8);
  let byte = try Int64.of_int !mem.{ea} with Invalid_argument _ -> raise Bounds in
  if n = 1 then
    byte
  else
    Int64.logor byte (Int64.shift_left (loadn mem (n-1) (ea+1)) 8)

let rec storen mem n ea v =
  assert (n > 0 && n <= 8);
  let byte = (Int64.to_int v) land 255 in
  (try !mem.{ea} <- byte with Invalid_argument _ -> raise Bounds);
  if (n > 1) then
    storen mem (n-1) (ea+1) (Int64.shift_right v 8)

let load mem a o t =
  let ea = effective_address a o in
  match t with
  | Int32Type -> Int32 (Int64.to_int32 (loadn mem 4 ea))
  | Int64Type -> Int64 (loadn mem 8 ea)
  | Float32Type -> Float32 (F32.of_bits (Int64.to_int32 (loadn mem 4 ea)))
  | Float64Type -> Float64 (F64.of_bits (loadn mem 8 ea))

let store mem o a v =
  let ea = effective_address a o in
  match v with
  | Int32 x -> storen mem 4 ea (Int64.of_int32 x)
  | Int64 x -> storen mem 8 ea x
  | Float32 x -> storen mem 4 ea (Int64.of_int32 (F32.to_bits x))
  | Float64 x -> storen mem 8 ea (F64.to_bits x)

let loadn_sx mem n ea =
  assert (n > 0 && n <= 8);
  let v = loadn mem n ea in
  let shift = 64 - (8 * n) in
  Int64.shift_right (Int64.shift_left v shift) shift

let load_extend mem a o sz ext t =
  let ea = effective_address a o in
  match sz, ext, t with
  | Mem8,  ZX, Int32Type -> Int32 (Int64.to_int32 (loadn    mem 1 ea))
  | Mem8,  SX, Int32Type -> Int32 (Int64.to_int32 (loadn_sx mem 1 ea))
  | Mem8,  ZX, Int64Type -> Int64 (loadn mem 1 ea)
  | Mem8,  SX, Int64Type -> Int64 (loadn_sx mem 1 ea)
  | Mem16, ZX, Int32Type -> Int32 (Int64.to_int32 (loadn    mem 2 ea))
  | Mem16, SX, Int32Type -> Int32 (Int64.to_int32 (loadn_sx mem 2 ea))
  | Mem16, ZX, Int64Type -> Int64 (loadn    mem 2 ea)
  | Mem16, SX, Int64Type -> Int64 (loadn_sx mem 2 ea)
  | Mem32, ZX, Int64Type -> Int64 (loadn    mem 4 ea)
  | Mem32, SX, Int64Type -> Int64 (loadn_sx mem 4 ea)
  | _ -> raise Type

let store_trunc mem a o sz v =
  let ea = effective_address a o in
  match sz, v with
  | Mem8,  Int32 x -> storen mem 1 ea (Int64.of_int32 x)
  | Mem8,  Int64 x -> storen mem 1 ea x
  | Mem16, Int32 x -> storen mem 2 ea (Int64.of_int32 x)
  | Mem16, Int64 x -> storen mem 2 ea x
  | Mem32, Int64 x -> storen mem 4 ea x
  | _ -> raise Type
