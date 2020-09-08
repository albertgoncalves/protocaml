let () : unit =
    let buffer : Buffer.t = Buffer.create 64 in
    (
        try
            while true do
                Buffer.add_char buffer (input_char stdin)
            done
        with _ -> ()
    );
    Buffer.contents buffer |> Printf.printf "\n%S\n"
