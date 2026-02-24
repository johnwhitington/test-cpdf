(* FIXME: Add a test for -output-image - will need to be custom. *)
(* FIXME: Add test for -extract-all-metadata - will need to be custom. *)
(* FIXME: Add test for -extract-single-image - custom? *)
(* A very simple tester for cpdf. *)
open Pdfutil

let loud = true

let source = ref (match Sys.getenv_opt "CPDFTEST_SOURCE" with Some x -> x | None -> "PDFTests")

let destination = ref (match Sys.getenv_opt "CPDFTEST_DEST" with Some x -> x | None -> "PDFResults")

let command_and_print s = (*Printf.printf "%s%!" s;*) Sys.command s

let strip_name s = implode (dropwhile (neq ' ') (explode s))

(* Do a single test, returning the name / exit code pair. *)
let test executable range prename postname name =
  Printf.printf "TEST pre=%s, post=%s\n%!" prename postname;
  let command_line =
    executable ^ " " (*^ " -debug-stderr-to-stdout "*) ^ prename ^ " \"" ^ name ^ "\" " ^ range ^ (if String.starts_with ~prefix:"-extract-images" name || String.starts_with ~prefix:"-extract-font" name then "" else " -o " ^ postname)
  in
    if loud then (print_string (strip_name command_line); print_newline ());
    let f, r = name, Sys.command command_line in
      if r > 0 && prename = "-squeeze" then
        begin
          Printf.printf "Copying failed file %s to %s\n" name postname;
          ignore (command_and_print ("cp " ^ "\"" ^ name ^ "\"" ^ " " ^ postname));
        end;
      (f, r)

(* Get a directory listing for a given directory. *)
let dir_listing directory =
  Array.to_list (Sys.readdir directory)

(* Explanation for each code. *)
let name_of_code = function
  | 0 -> "Success"
  | n -> "Test Failed with code " ^ string_of_int n

(* Double seperator line *)
let line =
  implode (many '=' 80) ^ "\n"

(* Does a string represent a plausible PDF file name? *)
let ispdf s =
  match rev (explode s) with
  | 'f'::'d'::'p'::'.'::_
  | 'F'::'D'::'P'::'.'::_ -> true
  | _ -> false

let exec = if Sys.os_type = "Unix" then ref "cpdf" else ref "cpdf.exe"
and out leaf = "\"" ^ !destination ^ "/" ^ leaf ^ "\""

(* Read and write *)
let tests =
  [(* Chapters 1 & 2 *)
   "readwrite",
    ("", "");
   "-i",
    ("-i hello.pdf", "");
   "-idir",
    ("-idir idir", "");
   "version",
    ("-version", "");
   "firstpage",
    ("-process-struct-trees", "1");
   "reverse",
    ("", "reverse");
   "pagelabel",
    ("", "'[1]-[1]'");
   "createobjstm",
    ("-create-objstm -no-preserve-objstm", "");
   "preservecreateobjstm",
   ("-create-objstm", "");
   "-change-id", ("-change-id", "");
   "-remove-id", ("-remove-id", "");
   "-l", ("-l", "");
   "-l2", ("-cpdflin /usr/local/bin/cpdflin -l", "");
   "-args", ("-args args.txt", "");
   "-args-json", ("-args-json args.json", "");
   "AND", ("", "-add-text \"FOO\" AND -add-text \"BAR\"");
   "-split-max", ("-split-max 200kb -decrypt-force -recrypt", "");
   (* Chapter 3: Pages *)
   "-scale-to-fit", ("-scale-to-fit a3portrait -scale-to-fit-scale 0.9", "");
   "-stretch", ("-stretch a4landscape", "");
   "-center-to-fit", ("-center-to-fit a3landscape -prerotate", "");
   "-scale-contents", ("-scale-contents 2", "");
   "-mediabox", ("-mediabox \"200 200 400 500\"", "");
   "-scale-page",
    ("-scale-page \"2 3\"", "");
   "-shift",
    ("-shift \"20mm 0mm\"", "");
   "-shift-boxes",
    ("-shift-boxes \"20mm 0mm\"", "");
   "-rotate",
    ("-rotate 90", "");
   "-rotate-by",
    ("-rotateby 90", "");
   "-rotate-contents",
    ("-rotate-contents 30", "");
   "-upright",
    ("-upright", "");
   "-hflip",
    ("-hflip", "");
   "-vflip",
    ("-vflip", "");
   "-crop",
    ("-crop \" 100 100 200 200\" -keep-version -range 1-1", "");
   "-remove-crop",
    ("-remove-crop", "");
   "-frombox",
    ("-frombox /CropBox -tobox /MediaBox -mediabox-if-missing", "");
   (* Chapter 4: Encryption *)
   "-encrypt-40",
   ("-encrypt 40bit owner \"\"", "");
   "-encrypt-128",
   ("-encrypt 128bit owner \"\"", "");
   "-encrypt-aes",
   ("-encrypt AES owner \"\"", "");
   "-encrypt-aes-256",
   ("-encrypt AES256 owner \"\"", "");
   "-encrypt-aes-256-iso",
   ("-encrypt AES256ISO owner \"\"", "");
   "-decrypt-aes",
   ("-decrypt owner=password", "");
   (* Chapter 5: Compression *)
   "-compress",
    ("-compress", "");
   "-decompress",
    ("-decompress", "");
   "-decompress-with-jbig2",
    ("-decompress -jbig2dec jbig2dec", "");
   "-squeeze", ("-squeeze", "");
   "-squeeze-log-to", ("-squeeze -squeeze-log-to PDFResults/squeeze_log.txt", "");
   (* Chapter 6: Bookmarks *)
   "-list-bookmarks",
    ("-list-bookmarks -utf8", "");
   "-remove-bookmarks",
    ("-remove-bookmarks", "");
   "-add-bookmarks",
    ("-add-bookmarks bookmarks.txt -keep-l", "");
   "-list-bookmarks-json", ("-list-bookmarks-json", "");
   "-list-bookmarks-json-preserve-actions", ("-list-bookmarks-json -preserve-actions", "");
   (* Chapter 7: Presentations *)
   "-presentation",
    ("-presentation -trans Split -duration 10", "");
   (* Chapter 8: Logos, watermarks and stamps *)
   "-stamp-on",
    ("-stamp-on hello.pdf", "");
   "-stamp-on-fast",
    ("-stamp-on hello.pdf -fast", "");
   "-stamp-under",
    ("-stamp-under hello.pdf -scale-stamp-to-fit", "");
   "-stamp-under-fast",
    ("-stamp-under hello.pdf -fast -scale-stamp-to-fit", "");
   "-combine-pages",
    ("-combine-pages logo.pdf", "");
   "-combine-pages-fast",
    ("-combine-pages logo.pdf -fast", "");
   "-add-text",
    ("-add-text \"%Bookmark1 UTF8: è - bates %Bates, %URL[coherentpdf|https://www.coherentpdf.com/] %Label of %EndPage %Roman %roman %a %A %b %B %d\
%e %H %I %j %m %M %p %S %T %u %w %Y %%  -\" -bates 10 \
-bates-pad-to 3 -font Times-Bold -topleft \"10 20\" -prerotate -progress", " -bates-at-range 1 ");
   "-add-text-multiline",
    ("-add-text \"This is\\nmultline\\ntext\" -justify-right -topline -center \
    -underneath -line-spacing 10 -relative-to-cropbox -decrypt-force", "");
    ("-add-text-ttf",
    ("-load-ttf 'A=./fonts/NotoSans-Black.ttf' -font A -add-text foo ", ""));
    ("-embed-std14",
    ("-embed-std14 fonts/ -add-text foo ", ""));
   "-remove-text",
    ("-remove-text", "");
   ("-add-rectangle",
    ("-add-rectangle \"300 200\"", ""));
   (* Chapter 9: Multipage facilities *)
   "-twoup", ("-twoup", "");
   "-twoup-fast", ("-twoup -fast", "");
   "-pad-before", ("-pad-before", "1");
   "-pad-after", ("-pad-after", "1");
   "-twoup-stack", ("-twoup-stack", "");
   "-twoup-stack-fast", ("-twoup-stack -fast", "");
   "-pad-every", ("-pad-every 2", "");
   "-pad-with", ("-pad-every 2 -pad-with hello.pdf", "");
   "-pad-multiple", ("-pad-multiple 8", "");
   "-pad-multiple-before", ("-pad-multiple-before 8", "");
   "-redact", ("-redact -process-struct-trees", "1");
   (* Chapter 10: Annotations *)
   "-list-annotations",
    ("-list-annotations", "");
   "-copy-annotations",
    ("-copy-annotations annots.pdf", "");
   "-remove-annotations",
    ("-remove-annotations", "");
   "-set-annotations",
    ("-set-annotations annots.json", "");
   (* Chapter 11: Document Information and Metadata *)
   "-list-fonts",
    ("-list-fonts", "");
   "-list-fonts-json",
    ("-list-fonts-json", "");
   "-info",
    ("-info -gs gs -gs-malformed ", "-raw");
   "-info-json",
    ("-info-json -mm", "-raw");
   "-set-title-also",
    ("-set-title frøgs -also-set-xmp", "");
   "-set-title-just",
    ("-set-title frogs -just-set-xmp", "");
   "-set-titleUTF8",
    ("-set-title frogs -utf8", "");
   "-set-titleraw",
    ("-set-title frogs -raw", "");
   "-set-titlestripped",
    ("-set-title frogs -stripped", "");
   "-set-create", ("-set-create D:19970915110347-08'00' -also-set-xmp", "");
   "-set-language", ("-set-language 'en-GB'", "");
   "-set-page-layout",
    ("-set-page-layout TwoColumnLeft", "");
   "-set-page-mode",
    ("-set-page-mode FullScreen", "");
   "-set-non-full-screen-page-mode", ("-set-non-full-screen-page-mode UseOutlines", "");
   "-open-at-page-custom", ("-open-at-page-custom \"[1 /Fit]\"", "");
   "-hide-toolbar",
    ("-hide-toolbar true", "");
   "-set-metadata",
    ("-set-metadata bookmarks.txt", "");
   "-remove-metadata",
    ("-remove-metadata", "");
   "-remove-all-metadata",
    ("-remove-all-metadata", "");
   "-print-metadata",
    ("-print-metadata", "");
   "-page-info", ("-page-info -in", "");
   "-page-info-json", ("-page-info-json", "");
   "-pages", ("-pages", ""); 
   "-add-page-labels", ("-add-page-labels -label-style UppercaseRoman", ""); 
   "-remove-page-labels", ("-remove-page-labels", ""); 
   "-composition", ("-composition", "");
   "-composition-json", ("-composition-json", "");
   (* Chapter 12: File attachments *)
   "-attach-file",
    ("-attach-file bookmarks.txt", "");
   "-remove-files",
    ("-remove-files","");
   "-list-attached-files", ("-list-attached-files -error-on-malformed", "");
   "-list-attached-files-json", ("-list-attached-files -json -error-on-malformed", "");
   "-dump-attachments", ("-dump-attachments", "");
   (* Chapter 13. Images *)
   "-image-resolution", ("-image-resolution 400", "");
   "-image-resolution-json", ("-image-resolution-json 400", "");
   "-image-resolution-max", ("-image-resolution 400000000", "");
   "-extract-images", ("-extract-images -dedup-perpage", "");
   "-extract-images-merge-masks", ("-extract-images -dedup-perpage -merge-masks", "");
   "-extract-images-inline", ("-extract-images -inline -dedup-perpage", "");
   "-extract-images-raw", ("-extract-images -raw", "");
   "-extract-images-p2p", ("-extract-images -dedup", "");
   "-list-images", ("-list-images", "");
   "-list-images-inline", ("-list-images -inline", "");
   "-list-images-json", ("-list-images-json", "");
   "-list-images-used", ("-list-images-used", "");
   "-list-images-used-inline", ("-list-images-used -inline", "");
   "-list-images-used-json", ("-list-images-used-json", "");
   "-process-images-jpeg", ("-decrypt-force -im magick -process-images-info -process-images -jpeg-to-jpeg 65 -lossless-to-jpeg 65", "");
   "-process-images-jpeg2000", ("-decrypt-force -im magick -process-images-info -process-images -jpeg2000-to-jpeg2000 65 -lossless-to-jpeg2000 100", "");
   "-process-images-jpeg-resample", ("-decrypt-force -im magick -process-images-info -process-images -jpeg-to-jpeg 70 -jpeg-to-jpeg-scale 50", "");
   "-process-images-jpeg-resample-dpi", ("-decrypt-force -im magick -process-images-force -process-images-info -process-images -jpeg-to-jpeg 70 -jpeg-to-jpeg-dpi 100", "");
   "-process-images-lossless", ("-resample-interpolate -process-images-info -decrypt-force -im magick -process-images -lossless-resample 50", "");
   "-process-images-lossless-dpi", ("-resample-interpolate -process-images-info -decrypt-force -im magick -process-images -lossless-resample-dpi 50", "");
   "-process-images-jbig2-lossless", ("-process-images -process-images-info -decrypt-force -1bpp-method JBIG2 -jbig2enc jbig2", "");
   "-process-images-jbig2-lossy", ("-process-images -process-images-info -decrypt-force -jbig2-lossy-threshold 0.8 -1bpp-method JBIG2Lossy -jbig2enc jbig2", "");
   "-process-images-ccitt", ("-process-images -process-images-info -decrypt-force -1bpp-method CCITT -jbig2dec jbig2dec", "");
   "-process-images-ccittg4", ("-process-images -process-images-info -decrypt-force -1bpp-method CCITTG4", "");
   (* Chapter 14. Fonts *)
   "-remove-fonts", ("-remove-fonts", "");
   "-missing-fonts", ("-missing-fonts", "");
   "-remove-fonts", ("-remove-fonts", "");
   "-extract-font", ("-extract-font 1,/F1", "");
   (* Chapter 15. PDF and JSON *)
   "-output-json", ("-output-json", "");
   "-output-json-utf8", ("-output-json -utf8", "");
   "-output-json-parse", ("-output-json -output-json-parse-content-streams", "");
   (* Chapter 16. Optional Content Groups *)
   "-ocg-list", ("-ocg-list", "");
   "-ocg-list-json", ("-ocg-list-json", "");
   (* Chapter 17. Creating new PDFs *)
   "-png", ("-subformat PDF/UA-1 -title foo -png images/png.png", "");
   "-jpeg", ("-jpeg images/jpg.jpg", "");
   (* Chapter 18. Drawing on PDFs *)
   "-draw", ("", "-draw -push -to '100 100' -line '200 200' -stroke -pop -bt -text Text -et");
   "-draw-struct-tree", ("", "-draw-struct-tree -draw -push -to '100 100' -line '200 200' -stroke -pop -bt -text Text -et");
   (* Chapter 19. Miscellaneous *)
   "-draft",
    ("-draft -boxes", "");
   "-draft-remove-only",
    ("-draft -draft-remove-only /Foo", "");
   "-blacktext",
    ("-blacktext","");
   "-blacklines",
    ("-blacklines","");
   "-blackfills",
    ("-blackfills","");
   "-thinlines",
    ("-thinlines \"5mm\"","");
   "-clean",
    ("-clean -gs gs -gs-malformed","");
   "-set-version",
    ("-set-version 6","");
   "-copy-id-from",
   ("-copy-id-from hello.pdf","");
   "-list-spot-colors", ("-list-spot-colors", "");
   "-print-page-labels", ("-print-page-labels", "");
   "-print-page-labels-json", ("-print-page-labels-json", "");
   "-remove-dict-entry", ("-remove-dict-entry /Foo", "");
   "-replace-obj", ("-replace-obj /Root/MarkInfo/Marked=true", "");
   "-producer", ("-producer THEPRODUCER", "");
   "-creator", ("-creator THECREATOR", "");
   "-remove-duplicate-fonts", ("-remove-duplicate-fonts", "");
   "-show-boxes", ("-show-boxes", "");
   "-remove-clipping", ("-remove-clipping", "");
   "-create-pdf", ("-create-pdf -create-pdf-papersize a4portrait -create-pdf-pages 20", "");
   "-create-pdf-ua-1", ("-create-pdf-ua-1 doc -create-pdf-papersize a4portrait -create-pdf-pages 20", "");
   "-create-pdf-ua-2", ("-create-pdf-ua-2 doc -create-pdf-papersize a4portrait -create-pdf-pages 20", "");
   "-remove-all-text", ("-remove-all-text", "");
   "-embed-missing-fonts", ("-embed-missing-fonts -gs gs", "");
   "-trim-marks", ("-trim-marks", "");
   "-remove-cropbox", ("-remove-cropbox", "");
   "-remove-trimbox", ("-remove-trimbox", "");
   "-remove-bleedbox", ("-remove-bleedbox", "");
   "-remove-artbox", ("-remove-artbox", "");
   "-hard-box", ("-hard-box /BleedBox", "");
   "-cropbox", ("-cropbox \"200 200 400 400\"", "");
   "-artbox", ("-artbox \"200 200 400 400\"", "");
   "-bleedbox", ("-bleedbox \"200 200 400 400\"", "");
   "-trimbox", ("-trimbox \"200 200 400 400\"", "");
   "-set-metadata-date", ("-set-metadata-date \"2019-01-01T00:00+01:01\"","");
   "-create-metadata", ("-create-metadata", "");
   "-impose-xy", ("-impose-linewidth 2 -impose-margin 20 -impose-spacing 10 -impose-xy \"3 3\"", "");
   "-impose", ("-impose-linewidth 2 -impose-margin 20 -impose-spacing 10 -impose a0landscape", "");
   "-list-annotations-json", ("-list-annotations-json", "");
   "-print-dict-entry", ("-print-dict-entry '/URI'", "");
   "-table-of-contents", ("-table-of-contents -toc-dot-leaders", "");
   "-chop", ("-chop '1 3'", "");
   "-chop-h", ("-chop-h 200", "");
   "-chop-v", ("-chop-v 200", "");
   (* Chapter X. Internal or undocumented or recently added. Move before release.*)
   "-remove-procsets", ("-remove-procsets", "");
   "-remove-output-intents", ("-remove-output-intents", "");
   "-remove-page-piece", ("-remove-page-piece", "");
   "-remove-article-threads", ("-remove-article-threads", "");
   "-remove-web-capture", ("-remove-web-capture", "");
   "-verify-pdfua", ("-verify 'PDF/UA-1(matterhorn)'", "");
   "-verify-pdfua-json", ("-verify 'PDF/UA-1(matterhorn)' -json", "");
   "-verify-pdfua-single", ("-verify 'PDF/UA-1(matterhorn)' -verify-single '28-012' -json", "");
   "-print-struct-tree", ("-print-struct-tree", "");
   "-remove-struct-tree", ("-remove-struct-tree", "");
   "-extract-struct-tree", ("-extract-struct-tree", "");
   "-mark-as", ("-mark-as 'PDF/UA-1'", "");
   "-remove-mark", ("-remove-mark 'PDF/UA-1'", "");
   "-rasterize", ("-rasterize -gs-quiet -gs gs", "");
   "-contains-javascript", ("-contains-javascript", "");
   "-remove-javascript", ("-remove-javascript", "");
   "-revisions", ("-revisions", "");
   "-revision", ("", "-revision 1");
   "-revision2", ("", "-revision 2");
   "-revisionmax", ("", "-revision 20000");
   "-attach-files-json", ("-attach-files-json attachhello.json", "");
   "-add-page-labels-json", ("-add-page-labels-json labels.json", "");
   (*"-output-image", ("-output-image -gs-quiet -gs gs -rasterize-jpeg -rasterize-res 72", "");*)
   (* Specials, for internal testing *)
   "-dup", ("", "5DUP");
   ]

let recrypt_allowed = function
  "-change-id" | "-remove-id" | "-copy-id-from" -> false
| _ -> true

let tests = 
  map 
   (function (x, (y, z)) ->
     let recrypt =
       if recrypt_allowed x then " -recrypt" else ""
     in
       (x, (y ^ recrypt, z)))
   tests

(* Do the test *)
let testit (s, range) todo =
  let files = dir_listing !source in
    let files =
      map (fun f -> (!source ^ "/") ^ f, f) (keep ispdf files)
    in
    let files = if todo < max_int then take files todo else files in
      let results =
        map
          (fun (file, leaf) ->
            if loud then
              begin
                print_newline ();
                Printf.printf "==== Testing file %s\n" file;
                flush stdout
              end;
            test !exec range s (out leaf) file)
          files
      in
        (*if loud then*)
          begin
            Printf.printf "\n\nResults:\n";
            print_string line;
            iter
              (fun (f, r) ->
                 print_string f;
                 print_string (implode (many ' ' (max 1 (50 - String.length f))));
                 print_string (name_of_code r);
                 print_newline ();
                 flush stdout)
              results
          end;
        let successes = length (keep (fun (_, r) -> r = 0) results) in
          Printf.printf "\n\nTests Completed: %i succeeded, %i failed.\n"
            successes (length results - successes)

let test_merge collate todo =
  let files = dir_listing !source in
    let files =
      map (fun f -> "\"" ^ (!source ^ "/") ^ f ^ "\"", f) (keep ispdf files)
    in
      let files = if todo < max_int then take files todo else files in
        let filenames =
          fold_left ( ^ ) "" (interleave " " (map fst files))
        in
        let collate = match collate with
        | 0 -> ""
        | 1 -> " -collate "
        | n -> " -collate-n " ^ string_of_int n ^ " "
        in
          let line = !exec ^ collate ^ " -decrypt-force -merge -merge-add-bookmarks -merge-add-bookmarks-use-titles -retain-numbering " ^ filenames ^ " -o " ^ !destination ^ "/merged.pdf" in
            print_string (line ^ "\n");
            flush stderr;
            ignore (Sys.command line)

(* Copy each file to temp, then merge with itself. *)
let self_merge todo =
  let files = dir_listing !source in
    let files =
      map (fun f -> "\"" ^ (!source ^ "/") ^ f ^ "\"", f) (keep ispdf files)
    in
      let fs, fs2 = split (if todo < max_int then take files todo else files) in
        iter2
          (fun f f2 ->
             let line = "cp " ^ f ^ " temp.pdf" in
              ignore (Sys.command line);
             let line = !exec ^ " -merge -process-struct-trees -decrypt-force temp.pdf " ^ f ^ " -o " ^ "\"" ^ !destination ^ "/" ^ f2 ^ "\"" in
               print_string (line ^ "\n");
               flush stderr;
               ignore (Sys.command line))
          fs fs2

(* Here, todo is the name of the file to test splitting. *)
let test_split_inner todo =
  begin try Unix.mkdir (!destination ^ "/" ^ todo ^ ".split") 0o777 with _ -> () end;
  let line =
    !exec ^ " -split " ^ "\"" ^ !source ^ "/" ^ todo ^ "\"" ^ " -chunk 2 -o " ^
    "\"" ^ !destination ^ "/" ^ todo ^ ".split/%%%.pdf" ^ "\""
  in
    print_string (line ^ "\n");
    flush stderr;
    ignore (Sys.command line)

let test_split todo =
  let files = dir_listing !source in
    let files =
      map (fun f -> f, f) (keep ispdf files)
    in
      let files = if todo < max_int then take files todo else files in
      List.iter (fun (filename, f) -> test_split_inner filename) files

let test_split_bookmarks_inner todo =
  begin try Unix.mkdir (!destination ^ "/" ^ todo ^ ".splitbookmarks") 0o777 with _ -> () end;
  let line =
    !exec ^ " -split-bookmarks 0 " ^ "\"" ^ !source ^ "/" ^ todo ^ "\"" ^ " -o " ^
    "\"" ^ !destination ^ "/" ^ todo ^ ".splitbookmarks/@b10@%%%.pdf" ^ "\""
  in
    print_string (line ^ "\n");
    flush stderr;
    ignore (Sys.command line)

let test_split_bookmarks todo =
  let files = dir_listing !source in
    let files =
      map (fun f -> f, f) (keep ispdf files)
    in
      let files = if todo < max_int then take files todo else files in
      List.iter (fun (filename, f) -> test_split_bookmarks_inner filename) files

let test_pdftest todo =
  let files = dir_listing !source in
    let files =
      map (fun f -> "\"" ^ (!source ^ "/") ^ f ^ "\"", f) (keep ispdf files)
    in
      let files = if todo < max_int then take files todo else files in
        iter
          (function (filename, f) ->
             let line =
               "./rendertest " ^ filename ^ " \"" ^ !destination ^ "/" ^ f ^ "\""
             in
               print_string (line ^ "\n");
               flush stderr;
               ignore (Sys.command line))
          files

let command c =
  print_endline c;
  Sys.command c

let test_roundtrip_bookmarks json todo =
  let add = if json then " -add-bookmarks-json " else " -add-bookmarks " in
  let lst = if json then " -list-bookmarks-json -preserve-actions " else " -list-bookmarks " in
  let files = dir_listing !source in
    let files =
      map (fun f -> "\"" ^ (!source ^ "/") ^ f ^ "\"", f) (keep ispdf files)
    in
      let files = if todo < max_int then take files todo else files in
        iter
          (function (filename, f) ->
             Printf.printf "Processing file %s\n" filename;
             Printf.printf "========================================================================\n";
             flush stdout;
             (* List its bookmarks to file. *)
             ignore (Sys.command (!exec ^ " -utf8 -gs gs -gs-malformed " ^ lst ^ filename ^ " >bar"));
             (* Add those bookmarks back, copying to new pdf *)
             ignore (Sys.command (!exec ^ " -utf8 -gs gs -gs-malformed -recrypt " ^ add ^ "bar " ^ filename ^ " -o out.pdf"));
             ignore (Sys.command ("cp out.pdf PDFResults/roundtripbookmarksjson/" ^ f));
             (* List the new bookmarks to a file *)
             ignore (Sys.command (!exec ^ " -utf8 -gs gs -gs-malformed " ^ lst ^ " out.pdf >bar2"));
             (* Call diff on the two files *)
             ignore (Sys.command ("diff -u bar bar2")))
          files

(* Use the results from the jsonroundtripjson directory, and convert them to PDFs
   and write to the jsonroundtrip directory, then do it again to the second directory. *)
let test_roundtrip_json parse_content utf8 todo =
  Printf.printf "beginning of test_roundtrip_json\n";
  begin try Unix.mkdir ("PDFResults" ^ "/" ^ "jsonroundtrip") 0o777 with _ -> () end;
  begin try Unix.mkdir ("PDFResults" ^ "/" ^ "jsonroundtripjson") 0o777 with _ -> () end;
  begin try Unix.mkdir ("PDFResults" ^ "/" ^ "second") 0o777 with _ -> () end;
  let files = dir_listing !source in
    let files =
      map (fun f -> "\"" ^ (!source ^ "/") ^ f ^ "\"", f) (keep ispdf files)
    in
      let files = if todo < max_int then take files todo else files in
        iter
          (function (filename, f) ->
             Printf.printf "JSON Round trip: processing file %s\n" filename;
             Printf.printf "========================================================================\n";
             flush stdout;
             (* Output the JSON. *)
             let cmd = !exec ^ " " ^ (if utf8 then "-utf8" else "") ^ " -output-json -o \"PDFResults/jsonroundtripjson" ^ "/" ^ f ^ ".json\" " ^ filename ^ (if parse_content then " -output-json-parse-content-streams" else "")
             in
             Printf.printf "cmd: %s\n" cmd;
             ignore (Sys.command cmd);
             (* Read back in to a PDF *)
             Printf.printf "(Reading back in...)\n%!";
             let cmd =
               !exec ^ " -j \"PDFResults/jsonroundtripjson/" ^ f ^ ".json\" -o \"PDFResults/jsonroundtrip/" ^ f ^ "\""
             in
             Printf.printf "cmd: %s\n" cmd;
             ignore (Sys.command cmd);
             (* Output again... *)
             Printf.printf "(Second try...)\n%!";
             let cmd = !exec ^ " " ^ (if utf8 then "-utf8" else "") ^ " -output-json -o \"PDFResults/second" ^ "/" ^ f ^ ".json\" " ^ "PDFResults/jsonroundtrip/" ^ f ^ (if parse_content then " -output-json-parse-content-streams" else "")
             in
             Printf.printf "cmd: %s\n" cmd;
             ignore (Sys.command cmd);
           )
          files

let test_roundtrip_annotations todo =
  begin try Unix.mkdir ("PDFResults" ^ "/" ^ "removedannotations") 0o777 with _ -> () end;
  begin try Unix.mkdir ("PDFResults" ^ "/" ^ "addedannotations") 0o777 with _ -> () end;
  let files = dir_listing !source in
    let files =
      map (fun f -> "\"" ^ (!source ^ "/") ^ f ^ "\"", f) (keep ispdf files)
    in
      let files = if todo < max_int then take files todo else files in
        iter
          (function (filename, f) ->
             Printf.printf "Processing file %s\n" filename;
             Printf.printf "========================================================================\n";
             flush stdout;
             (* Save annotations to bar *)
             ignore (Sys.command (!exec ^ " -list-annotations-json PDFTests/" ^ f ^ " >bar"));
             (* Remove annotations, writing to results *)
             ignore (Sys.command (!exec ^ " -remove-annotations PDFTests/" ^ f ^ " -o PDFResults/removedannotations/" ^ f));
             (* Set the annotations from the JSON *)
             ignore (Sys.command (!exec ^ " -set-annotations bar PDFResults/removedannotations/" ^ f ^ " -o PDFResults/addedannotations/" ^ f))
            )
          files

let test_roundtrip_struct_tree todo =
  begin try Unix.mkdir ("PDFResults" ^ "/" ^ "replacedstructtrees") 0o777 with _ -> () end;
  let files = dir_listing !source in
    let files =
      map (fun f -> "\"" ^ (!source ^ "/") ^ f ^ "\"", f) (keep ispdf files)
    in
      let files = if todo < max_int then take files todo else files in
        iter
          (function (filename, f) ->
             Printf.printf "Processing file %s\n" filename;
             Printf.printf "========================================================================\n";
             flush stdout;
             (* Save structure to out.json *)
             ignore (command (!exec ^ " -extract-struct-tree PDFTests/" ^ f ^ " -o out.json"));
             (* Set the structure tree from the JSON *)
             ignore (command (!exec ^ " -recrypt -replace-struct-tree out.json PDFTests/" ^ f ^ " -o PDFResults/replacedstructtrees/" ^ f))
            )
          files

let test_update todo =
  let files = dir_listing !source in
    let files =
      map (fun f -> "\"" ^ (!source ^ "/") ^ f ^ "\"", f) (keep ispdf files)
    in
      let files = if todo < max_int then take files todo else files in
        iter
          (function (filename, f) ->
             Printf.printf "Processing file %s\n" filename;
             Printf.printf "========================================================================\n";
             flush stdout;
             (* Copy to output directory *)
             let a = command ("cp 'PDFTests/" ^ f ^ "' 'PDFResults/update/" ^ f ^ "'") in
               Printf.printf "cp exit code %i\n" a;
             (* Update in place in output directory. *)
             let b = command (!exec ^ " 'PDFResults/update/" ^ f ^ "' -rotate 90 -recrypt -update " ^ " -o " ^ " 'PDFResults/update/" ^ f ^ "'") in
               Printf.printf "cpdf exit code %i\n" b)
          files

let go src dest testname number =
  source := src;
  destination := dest;
  let todo =
    match number with
    | "all" -> max_int
    | other -> begin try int_of_string other with _ -> 0 end
  in
    match testname with
    | "-merge" -> test_merge 0 todo
    | "-collate" -> test_merge 1 todo
    | "-collate-n" -> test_merge 3 todo
    | "-split" -> test_split todo
    | "-split-bookmarks" -> test_split_bookmarks todo
    | "-roundtrip-bookmarks" -> test_roundtrip_bookmarks false todo
    | "-roundtrip-bookmarks-json" -> test_roundtrip_bookmarks true todo
    | "-roundtrip-json" -> test_roundtrip_json false false todo
    | "-roundtrip-json-utf8" -> test_roundtrip_json false true todo
    | "-roundtrip-json-parse-content" -> test_roundtrip_json true false todo
    | "-roundtrip-json-parse-content-utf8" -> test_roundtrip_json true true todo
    | "-self-merge" -> self_merge todo
    | "-roundtrip-annotations" -> test_roundtrip_annotations todo
    | "-roundtrip-struct-tree" -> test_roundtrip_struct_tree todo
    | "-update" -> test_update todo
    | _ ->
      match lookup testname tests with
      | Some t -> testit t todo
      | None -> failwith "unknown test"

(* Don't include split or merge, since they're special. *)
let testnames =
  map fst tests

let foldername s =
  implode (lose (eq '-') (explode s))

let alltests number =
  iter
    (function t ->
       let dir = "PDFResults/" ^ foldername t in
       (try Unix.mkdir dir 0o777 with _ -> ());
       go "PDFTests" dir t number)
    testnames

let singletest name number =
  let dir = "PDFResults/" ^ foldername name in
    (try Unix.mkdir dir 0o777 with _ -> ());
    go !source dir name number

let _ =
  match Sys.argv with
  | [|_; n|] -> alltests n
  | [|_; "-cpdf"; cpdfname; n|] ->
      exec := cpdfname; alltests n
  | [|_; testname; number|] ->
      singletest testname number
  | [|_; "-cpdf"; cpdfname; testname; number|] ->
      exec := cpdfname;
      singletest testname number
  | _ ->
     print_string "Syntax:\n\
     cpdftest <number or \"all\">\n\
     cpdftest testname number (single test)\n";
     print_string "Available tests are:\n";
     iter (fun (t, _) -> print_string t; print_newline ()) tests; 
     exit 2
