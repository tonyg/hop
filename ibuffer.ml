type t = {
    mutable pos: int;
    limit: int;
    buf: string;
  }

let create s ofs len = {
  pos = ofs;
  limit = ofs + len;
  buf = s
}

let sub b ofs len =
  if b.pos + ofs + len > b.limit
  then
    raise End_of_file
  else
    { pos = b.pos + ofs;
      limit = b.pos + ofs + len;
      buf = b.buf }

let remaining b = b.limit - b.pos

let next_char b =
  if b.pos < b.limit
  then
    let v = String.get b.buf b.pos in
    b.pos <- b.pos + 1;
    v
  else
    raise End_of_file

let next_byte b = int_of_char (next_char b)

let next_chars b n =
  if remaining b < n
  then
    raise End_of_file
  else
    let dst = String.create n in
    String.blit b.buf b.pos dst 0 n;
    b.pos <- b.pos + n;
    dst

let next_sub b n =
  let v = sub b 0 n in
  b.pos <- b.pos + n;
  v
