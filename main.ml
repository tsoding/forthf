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

let gen_op op =
  match op with
  | Push x -> [Printf.sprintf ";; push %d" x;
               Printf.sprintf "push %d" x]
  | Plus -> [";; plus";
             "pop rax";
             "pop rbx";
             "add rax, rbx";
             "push rax"]
  | Print -> [";; print";
              "pop rax";
              "call print"]

let generate (ops: op_t list): string list =
  ops |> List.map gen_op |> List.concat

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
  let program_print_zero = [Push 0; Push 1; Push 2; Print; Print; Print] in
  List.concat [gen_header;
               generate program_print_zero;
               gen_footer]
  |> List.iter print_endline
