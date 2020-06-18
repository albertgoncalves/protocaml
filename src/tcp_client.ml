let connect (address : Unix.sockaddr) : (in_channel * out_channel) =
    let domain : Unix.socket_domain = Unix.domain_of_sockaddr address in
    let socket : Unix.file_descr = Unix.socket domain Unix.SOCK_STREAM 0 in
    try
        Unix.connect socket address;
        (Unix.in_channel_of_descr socket, Unix.out_channel_of_descr socket)
    with exn ->
        Unix.close socket;
        raise exn

let shutdown (i : in_channel) : unit =
    Unix.shutdown (Unix.descr_of_in_channel i) Unix.SHUTDOWN_SEND

let client (service : in_channel -> out_channel -> unit) : unit =
    if Array.length Sys.argv < 3 then
        Printf.printf "USAGE: $ ./tcp_client <SERVER> <PORT>\n%!"
    else
        let host_name : string = Sys.argv.(1) in
        let address : Unix.inet_addr =
            try
                Unix.inet_addr_of_string host_name
            with Failure(_) ->
                (
                    try
                        (Unix.gethostbyname host_name).Unix.h_addr_list.(0)
                    with Not_found ->
                        Printf.eprintf
                            "ERROR: Unknown server \"%s\"\n%!"
                            host_name;
                        exit 2
                ) in
        try
            let port : int = int_of_string (Sys.argv.(2)) in
            let (i, o) : (in_channel * out_channel) =
                connect (Unix.ADDR_INET (address, port)) in
            service i o;
            shutdown i
        with Failure(_) ->
            (
                Printf.eprintf "ERROR: Bad port number\n";
                exit 2
            )

let service (i : in_channel) (o : out_channel) : unit =
    try
        while true do
            Printf.printf "> %!";
            let payload : string = input_line stdin in
            if payload = "quit" then
                (
                    shutdown i;
                    raise Exit
                );
            Printf.fprintf o "%s\n%!" payload;
            let response : string = input_line i in
            Printf.printf "  %s\n%!" response;
        done
    with
        | Exit -> exit 0
        | exn ->
            (
                shutdown i;
                raise exn
            )

let () : unit = client service
