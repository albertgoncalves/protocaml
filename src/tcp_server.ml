let loop
        (service : in_channel -> out_channel -> unit)
        (address : Unix.sockaddr) : unit =
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
                    let i : in_channel = Unix.in_channel_of_descr file in
                    let o : out_channel = Unix.out_channel_of_descr file in
                    service i o;
                    close_in i;
                    close_out o;
                    exit 0
                )
            | id ->
                (
                    Unix.close file;
                    ignore (Unix.waitpid [] id)
                )
    done

let init_server (service : in_channel -> out_channel -> unit) : unit =
    if Array.length Sys.argv < 2 then
        Printf.eprintf "USAGE: $ ./tcp_server <PORT>\n%!"
    else
        try
            let port : int = int_of_string Sys.argv.(1) in
            let host_name : string = Unix.gethostname () in
            let address : Unix.inet_addr =
                (Unix.gethostbyname (host_name)).Unix.h_addr_list.(0) in
            loop service (Unix.ADDR_INET (address, port))
        with Failure(_) ->
            Printf.eprintf "ERROR: Bad port number\n"

let to_uppercase (i : in_channel) (o : out_channel) : unit =
    try
        while true do
            Printf.fprintf o "%s\n%!" (String.uppercase_ascii (input_line i))
        done
    with _ ->
        (
            Printf.printf "INFO: Client exit\n%!";
            exit 0
        )

let () : unit = Unix.handle_unix_error init_server to_uppercase
