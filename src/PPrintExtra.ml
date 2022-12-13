open PPrint

let print (doc : document) : unit =
  PPrint.ToChannel.pretty 0.9 80 stdout doc;
  flush stdout

let to_string (doc : document) : string =
  let b = Buffer.create 80 in
  PPrint.ToBuffer.pretty 0.9 80 b doc;
  Buffer.contents b
