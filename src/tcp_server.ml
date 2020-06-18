type channel_t = {
    input : in_channel;
    output : out_channel;
}

let loop (service : channel_t -> unit) (address : Unix.sockaddr) : unit =
    let domain : Unix.socket_domain = Unix.domain_of_sockaddr address in
    let socket : Unix.file_descr = Unix.socket domain Unix.SOCK_STREAM 0 in
    Unix.bind socket address;
    Unix.listen socket 3;
    while true do
        let (file, _) : (Unix.file_descr * Unix.sockaddr) =
            Unix.accept socket in
        match Unix.fork () with
            | 0 ->
                (
                    if Unix.fork () <> 0 then
                        exit 0;
                    let channels : channel_t = {
                        input = Unix.in_channel_of_descr file;
                        output = Unix.out_channel_of_descr file;
                    } in
                    service channels;
                    close_in channels.input;
                    close_out channels.output;
                    exit 0
                )
            | id ->
                (
                    Unix.close file;
                    Unix.waitpid [] id |> ignore
                )
    done

let init_server (service : channel_t -> unit) : unit =
    if Array.length Sys.argv < 2 then
        Printf.eprintf "USAGE: $ %s <PORT>\n%!" Sys.argv.(0)
    else
        try
            let port : int = int_of_string Sys.argv.(1) in
            let host_name : string = Unix.gethostname () in
            Printf.eprintf
                "INFO: Server running at `%s:%d`\n%!"
                host_name
                port;
            let address : Unix.inet_addr =
                (host_name |> Unix.gethostbyname).Unix.h_addr_list.(0) in
            Unix.ADDR_INET (address, port) |> loop service
        with Failure (_) ->
            Printf.eprintf "ERROR: Bad port number\n"

let to_uppercase (channels : channel_t) : unit =
    try
        Printf.printf "INFO: Client connect\n%!";
        while true do
            Printf.fprintf
                channels.output
                "%s\n%!"
                (input_line channels.input |> String.uppercase_ascii)
        done
    with _ ->
        (
            Printf.printf "INFO: Client exit\n%!";
            exit 0
        )

let () : unit = Unix.handle_unix_error init_server to_uppercase
