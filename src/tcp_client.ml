type channel_t = {
    input : in_channel;
    output : out_channel;
}

let connect (address : Unix.sockaddr) : channel_t =
    let domain : Unix.socket_domain = Unix.domain_of_sockaddr address in
    let socket : Unix.file_descr = Unix.socket domain Unix.SOCK_STREAM 0 in
    try
        Unix.connect socket address;
        {
            input = Unix.in_channel_of_descr socket;
            output = Unix.out_channel_of_descr socket;
        }
    with exn ->
        Unix.close socket;
        raise exn

let shutdown (channels : channel_t) : unit =
    Unix.shutdown (Unix.descr_of_in_channel channels.input) Unix.SHUTDOWN_SEND

let client (service : channel_t -> unit) : unit =
    if Array.length Sys.argv < 3 then
        Printf.printf "USAGE: $ %s <HOST> <PORT>\n%!" Sys.argv.(0)
    else
        let host_name : string = Sys.argv.(1) in
        let address : Unix.inet_addr =
            try
                Unix.inet_addr_of_string host_name
            with Failure (_) ->
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
            let channels : channel_t =
                Unix.ADDR_INET (address, port) |> connect in
            service channels;
            shutdown channels
        with Failure (_) ->
            (
                Printf.eprintf "ERROR: Bad port number\n%!";
                exit 2
            )

let service (channels : channel_t) : unit =
    try
        while true do
            Printf.printf "> %!";
            let payload : string = input_line stdin in
            if payload = "quit" then
                (
                    shutdown channels;
                    raise Exit
                );
            Printf.fprintf channels.output "%s\n%!" payload;
            let response : string = input_line channels.input in
            Printf.printf "  %s\n%!" response;
        done
    with
        | Exit -> exit 0
        | exn ->
            (
                shutdown channels;
                raise exn
            )

let () : unit = client service
