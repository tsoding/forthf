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

let parse_ops (source : string): op_t list = []

let () =
  let program0 = [Push 1;
                  Push 2;
                  Push 3;
                  Plus;
                  Plus;
                  Print] in
  let program1 = [Push 1;
                  Push 2;
                  Push 3;
                  Print;
                  Print;
                  Print] in
  let program_print_zero = [Push 0; Print; Push 1; Print; Push 2; Print] in
  List.concat [gen_header;
               generate program_print_zero;
               gen_footer]
  |> List.iter print_endline
