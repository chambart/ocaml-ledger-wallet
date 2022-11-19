(* let vendor_id = 0x2C97
 * 
 * let product_id = 0x0001 *)

let possible_connections = Ledgerwallet.Transport.enumerate ()

let path =
  match possible_connections with [] -> failwith "NO ledger" | hd :: _ -> hd

let with_connection f = Ledgerwallet.Transport.with_connection_path path f

let hard x = Int32.logor x 0x8000_0000l
let derivation = [hard 44l; hard 1729l]

(* let micheline =
 *   "0509020000006d0200000041020000000002000000370509010000000873617563697373650406000000074070726f75746506090100000006626f7564696e0000000a40616e74696c6c6169730200000010020000000b0509010000000463616361082103210721032103210000000440647570" *)

(*
OK [4a c8 94 cc 05 97 1e 2e  c0 7a 15 32 24 9b 2d 15
    86 8d 06 2e 9d d6 b7 72  68 5f 51 35 bf b8 e3 90
    ]
*)

let micheline =
  "0509020000012802000000b9020000000002000000af0509010000002d736175636973736564736c686a6664736b6a686664736c6b6a6668736c6b716a6668736c6b6a68716c6b66717304060000001e4070726f757465666b736a7168666b64736a6873716c6b6a66686473716606090100000042626f7564696e206e6177616b207175616e64206d656d652063652071756520636120666169742065637269726520746f757465732063657320636f6e6e65726965730000000a40616e74696c6c616973020000003d0200000038050901000000316361636120626f7575757575757564696e73206465206c61206d6f72742071756920707565207361206d6565656565726508210321092100000012032103210321032103210321032103210321000000000000000440647570"

(*
OK [0e 81 ff 67 c8 c6 75 ed  ad 25 80 1f ec 4f 16 71
    8b d8 27 26 7e aa 36 de  86 49 b9 58 10 a2 3e cb
    ]
*)

let msg = Cstruct.concat [
    (* Cstruct.of_string "\x03"; *)
    Cstruct.of_hex micheline]

(* let msg =
 *   let s = String.init 1024 (fun i -> Char.chr (i mod 256)) in
 *   Cstruct.concat
 *     [
 *       Cstruct.of_string "\x03"; Cstruct.of_string "saucisse"; Cstruct.of_string s;
 *     ] *)

let test_sign_stuff t = Ledgerwallet_tezos.sign_and_hash t Ed25519 derivation msg


let () =
  match with_connection test_sign_stuff with
  | None -> failwith "NONNE"
  | Some res -> (
      match res with
      | Ok (signed, _hash) ->
          Format.printf "OK [%a]@." Cstruct.hexdump_pp signed ;
          ()
      | Error err ->
          Format.printf "Error %a@." Ledgerwallet.Transport.pp_error err ;
          ())
