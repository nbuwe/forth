# -*- mode: gdb-script -*-

break *(&breakpoint_code)

define pstack
  printf "\nParameter Stack:\n\n"
  printf "     TOS: %08x %11d\n", $ecx, $ecx
  set $psp = $edi
  while ($psp < &stack_bottom)
    printf "%08x: %08x %11d\n", $psp, \
    	*(unsigned int *)$psp, *(unsigned int *)$psp
    set $psp += 4
  end
end

define rstack
  printf "\nReturn Stack:\n\n"
  printf "      IP: %08x = ", $ebx
  info symbol $ebx
  set $rsp = $esi
  while ($rsp < &rstack_bottom)
    printf "%08x: %08x = ", $rsp, *(unsigned int *)$rsp
    info symbol *(unsigned int *)$rsp
    set $rsp += 4
  end
end

define state
  rstack
  pstack
end

display/i $eip
