(* Regression tester on top of cpdftest *)
open Pdfutil

let debug = ref true

let command s =
  if !debug then Printf.printf "%s\n%!" s;
  match Sys.command s with
  | 0 | 1 -> () (* Sadly, diff exit code 1 means "differences" not errors *)
  | n -> failwith ("command exited with code " ^ string_of_int n)

(* 1. call ../cpdftest -cpdf oldexe -command >oldfoo
   2. copy ../PDFResults locally to /OldPDFResults
   3. call ../cpdftest -cpdf newexe -command >newfoo
   4. diff oldfoo and newfoo to foo
   5. diff ../PDFResults and /OldPDFResults *)
let test newexe oldexe op n =
  command ("cd ..; ./cpdftest -cpdf " ^ oldexe ^ " " ^ op ^ " " ^ n ^ " >oldfoo 2>&1");
  command ("cp -R ../PDFResults OldPDFResults");
  command ("cd ..; ./cpdftest -cpdf " ^ newexe ^ " " ^ op ^ " " ^ n ^ " >newfoo 2>&1");
  command ("diff -ua ../oldfoo ../newfoo >foo.diff");
  command ("diff -r --brief OldPDFResults ../PDFResults >>foo.diff")

let _ =
  match Sys.argv with
  | [|_; newexe; oldexe; op; n|] ->
      test newexe oldexe op n
  | [|_; oldexe; op; n|] ->
      test "cpdf" oldexe op n
  | _ ->
     print_string "Syntax:\n\
     cpdfr newexe oldexe command\n\
     cpdfr oldexe command (default newexe is 'cpdf')\n";
     exit 2
