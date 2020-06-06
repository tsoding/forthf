type op_t =
  | Push of int
  | Plus
  | Print

let gen_header: string list = [
    ";;; HEADER BEGIN ;;;";
    "section .text";
    "global _start";

    "print:";
    "    mov rbp, rsp";
    "    dec rsp";
    "    mov BYTE [rsp], 0x0A";
    ".loop:";
    "    mov rbx, 10";
    "    mov rdx, 0";
    "    div rbx";
    "    add dl, '0'";
    "    dec rsp";
    "    mov [rsp], dl";
    ".loop_cond:";
    "    test rax, rax";
    "    jnz .loop";
    "    mov rax, 1";
    "    mov rdi, 1";
    "    mov rsi, rsp";
    "    mov rdx, rbp";
    "    sub rdx, rsp";
    "    syscall";
    "    mov rsp, rbp";
    "    ret";

    "_start:";
    ";;; HEADER END ;;;"]

let gen_footer: string list = [
    ";;; FOOTER BEGIN ;;;";
    "mov rax, 60";
    "mov rdi, 0";
    "syscall";
    "ret";
    ";;; FOOTER END ;;;"]

let rec generate (ops: op_t list): string list =
  match ops with
  | Push x :: Print :: rest_ops ->
      [Printf.sprintf ";; push %d; print" x;
       Printf.sprintf "mov rax, %d" x;
       Printf.sprintf "call print"] @ generate rest_ops
  | Push x :: rest_ops ->
      [Printf.sprintf ";; push %d" x;
       Printf.sprintf "push %d" x] @ generate rest_ops
  | Plus :: Print :: rest_ops ->
      [";; plus; print";
       "pop rax";
       "pop rbx";
       "add rax, rbx";
       "call print"]
  | Plus :: rest_ops ->
      [";; plus";
       "pop rax";
       "pop rbx";
       "add rax, rbx";
       "push rax"] @ generate rest_ops
  | Print :: rest_ops ->
      [";; print";
       "pop rax";
       "call print"] @ generate rest_ops
  | [] -> []

let parse_ops (source : string): op_t list =
  let not_empty x = String.length x != 0 in
  source
  |> String.split_on_char ' '
  |> List.filter not_empty
  |> List.map (fun x ->
       match x with
       | "." -> Print
       | "+" -> Plus
       | x   -> Push (int_of_string x))

let () =
  let source = "1 2 3 + + ." in
  List.concat [gen_header;
               source |> parse_ops |> generate;
               gen_footer]
  |> List.iter print_endline
