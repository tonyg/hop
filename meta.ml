open Sexp

let announce_subscription source filter sink name on_off =
  Node.post_ignore "meta" (Str source)
    (if on_off
    then Message.subscribed (Str source, filter, Str sink, name)
    else Message.unsubscribed (Str source, filter, Str sink, name))
    (Str "")

let init () =
  Node.send_ignore "factory" (Message.create (Str "direct", Arr [Str "meta"], Str "", Str ""))
