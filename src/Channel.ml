type channel = int

let counter = ref (3)

let create_chan_id () =
  let id = !counter in
  counter := (!counter) + 1;
  id

let equal ch1 ch2 = (ch1 == ch2)
