# -*- mode: gdb-script -*-

break *(&breakpoint_code)

define pstack
  printf "\nParameter Stack:\n\n"
  printf "     TOS: %08x %11d\n", $r4, $r4
  set $psp = $r5
  while ($psp < &stack_bottom)
    printf "%08x: %08x %11d\n", $psp, \
    	*(unsigned int *)$psp, *(unsigned int *)$psp
    set $psp += 4
  end
end

define rstack
  printf "\nReturn Stack:\n\n"
  printf "      IP: %08x = ", $r7
  info symbol $r7
  set $rsp = $r6
  while ($rsp < (cell_t)&rstack_bottom)
    printf "%08x: %08x = ", $rsp, *(unsigned int *)$rsp
    info symbol *(unsigned int *)$rsp
    set $rsp += 4
  end
end

define state
  rstack
  pstack
end

display/i $pc
